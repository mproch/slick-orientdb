package pl.mproch.slick.orientdb.direct

import slick.direct._
import slick.session.{Session, PositionedResult}
import reflect.runtime._
import scala.reflect.runtime.universe._
import scala.reflect.runtime.{currentMirror=>cm}
import java.util.{Map => jmap}
import java.util.{List => jlist}
import pl.mproch.slick.orientdb.driver.OrientDBDriver
import slick.lifted.ConstColumn
import slick.{SlickException, ast}
import ast.{Node, FieldSymbol, Library}
import pl.mproch.slick.orientdb.ast.{OrientDBLibrary, NestedSymbol}
import slick.lifted.TypeMapper.StringTypeMapper
import scala.collection.JavaConversions._
import reflect.api.Trees
import java.util

class OrientSlickBackend(mapper: Mapper) extends SlickBackend(OrientDBDriver, mapper) {


  override protected def resultByType( expectedType : Type, rs: PositionedResult, session:Session) : Any = {
    def createInstance( args:Seq[Any] ) = {
      val constructor = expectedType.member( nme.CONSTRUCTOR ).asMethod
      val cls = cm.reflectClass( cm.classSymbol(cm.runtimeClass(expectedType)) )
      cls.reflectConstructor( constructor )( args:_* )
    }
    import TupleTypes.tupleTypes
    (expectedType match {
      case t if typeMappers.isDefinedAt(expectedType.toString) => typeMappers( expectedType.toString )(OrientDBDriver).nextValue(rs)
      case t if tupleTypes.exists( expectedType <:< _ ) =>
        val typeArgs = expectedType match { case TypeRef(_,_,args_) => args_ }
        val args = typeArgs.map{
          tpe => resultByType( tpe, rs, session )
        }
        createInstance( args )

      case t if t.typeSymbol.asClass.isCaseClass && !mapper.isMapped(t) =>
         val map = rs.nextObject().asInstanceOf[jmap[String,Object]]
         caseClassByTypeFromValueMap(expectedType, map)

      //FIXME: do it in some more elegant way ;)
      case t if (t.typeSymbol.asClass.fullName == "scala.collection.immutable.List") => {
        val param = t.asInstanceOf[TypeRefApi].args(0)
        val list = rs.nextObject().asInstanceOf[jlist[Object]]
        param match {
          case p if p.typeSymbol.asClass.isCaseClass =>
            list.map(o=>caseClassByTypeFromValueMap(param, o.asInstanceOf[jmap[String,Object]])).toList
        }
      }

      case t if t.typeSymbol.asClass.isCaseClass =>
        val args = expectedType.member( nme.CONSTRUCTOR ).typeSignature match {
          case MethodType( params, resultType ) => params.map{ // TODO check that the field order is correct
            param =>  resultByType( param.typeSignature, rs, session )
          }
        }
        createInstance( args )
    })
  }


  protected def caseClassByTypeFromValueMap(expectedType: universe.Type, map:jmap[String,Object]) : Any= {
    def createInstance(args: Seq[Any]) = {
      val constructor = expectedType.member(nme.CONSTRUCTOR).asMethod
      val cls = cm.reflectClass(cm.classSymbol(cm.runtimeClass(expectedType)))
      cls.reflectConstructor(constructor)(args: _*)
    }

    val args = expectedType.member(nme.CONSTRUCTOR).typeSignature match {
      case MethodType(params, resultType) => params.map {
        param : Symbol=> resultByTypeFromValueMap(mapper.fieldToColumn(param), param.typeSignature, map)
      }
    }
    createInstance(args)
  }

  protected def resultByTypeFromValueMap(name:String, expectedType: universe.Type, map:jmap[String,Object]) : Any= {
    val value = map.get(name)
    //this is v. strange, but seems that sth has to be evaluated???
    expectedType.toString
    expectedType match {
      case t if t.typeSymbol.asClass.isCaseClass => caseClassByTypeFromValueMap(expectedType, value.asInstanceOf[jmap[String,Object]])
      case _ => value
    }
  }

  import scala.slick.{ast => sq}

  override def toQuery( tree:Tree, scope : Scope = Scope() ) : (Type,Query) = {
    import scala.tools.reflect._
    // external references (symbols) are preserved by reify, so cm suffices (its class loader does not need to load any new classes)
    val toolbox = cm.mkToolBox()//mkConsoleFrontEnd().asInstanceOf[scala.tools.reflect.FrontEnd],"") // FIXME cast
    val typed_tree = toolbox.typeCheck(tree) // TODO: can we get rid of this to remove the compiler dependency?
    ( typed_tree.tpe, scala2scalaquery_typed( removeTypeAnnotations(typed_tree), scope ) )
  }
  private def eval( tree:Tree ) :Any = tree match {
    case Select(from,name) => {
      val i = cm.reflect( eval(from) )
      val m = i.symbol.typeSignature.member( name ).asMethod
      val mm = i.reflectMethod( m )
      mm()
    }
    case ident:Ident => ident.symbol.asFreeTerm.value
  }

  private def scala2scalaquery_typed( tree:Tree, scope : Scope ) : Query = {
    def s2sq( tree:Tree, scope:Scope=scope ) : Query = scala2scalaquery_typed( tree, scope )
    implicit def node2Query(node:sq.Node) = new Query( node, scope )
    try{
      val string_types = List("String","java.lang.String")
      tree match {
        // explicitly state types here until SQ removes type parameters and type mapper from ConstColumn
        case Literal(Constant(x:Int))    => ConstColumn[Int](x)
        case Literal(Constant(x:String)) => ConstColumn[String](x)
        case Literal(Constant(x:Double)) => ConstColumn[Double](x)
        case ident@Ident(name) if !scope.contains(ident.symbol) => // TODO: move this into a separate inlining step in queryable
          ident.symbol.asFreeTerm.value match {
            case q:BaseQueryable[_] => val (tpe,query) = toQuery( q ); query
            case x => s2sq( Literal(Constant(x)) )
          }
        case ident@Ident(name) => scope(ident.symbol)

        case Select( t, term ) if t.tpe.erasure <:< typeOf[BaseQueryable[_]].erasure && term.decoded == "queryable" => s2sq(t)

        // match columns
        /*case Select(from,name) if mapper.isMapped( from.tpe.widen )
        =>
          extractColumn( getConstructorArgs( from.tpe.widen ).filter(_.name==name).head, scope(from.symbol) )
        */
        case Select(from,name) if isNestedColumn(tree) =>
          new Query(extractNestedColumn(tree,scope),scope)

/*
        // TODO: Where is this needed?
        case Select(a:This,b) =>
          val obj = companionInstance( a.symbol )
          val value = invoke( obj, a.tpe.nonPrivateMember(b) )()
          value match{
            case q:BaseQueryable[_] => toQuery( q )
            case x => s2sq( Literal(Constant(x)) )
          }
*/

        case Apply( Select( queryOps, term ), queryable::Nil )
          if queryOps.tpe <:< typeOf[QueryOps.type] && queryable.tpe.erasure <:< typeOf[BaseQueryable[_]].erasure && term.decoded == "query"
        => s2sq( queryable ).node

        // match queryable methods
        case Apply(Select(scala_lhs,term),Function( arg::Nil, body )::Nil)
          if scala_lhs.tpe.erasure <:< typeOf[QueryOps[_]].erasure
        =>
          val sq_lhs = s2sq( scala_lhs ).node
          val sq_symbol = new sq.AnonSymbol
          val new_scope = scope+(arg.symbol -> sq.Ref(sq_symbol))
          val rhs = s2sq(body, new_scope)
          new Query( term.decoded match {
            case "filter"     => sq.Filter( sq_symbol, sq_lhs, rhs.node )
            case "map"        => sq.Bind( sq_symbol, sq_lhs, sq.Pure(rhs.node) )
            case "flatMap"    => sq.Bind( sq_symbol, sq_lhs, rhs.node )
            case e => throw new UnsupportedMethodException( scala_lhs.tpe.erasure+"."+term.decoded )
          },
            new_scope
          )

        // FIXME: this case is required because of a bug, but should be covered by the next case
        case d@Apply(Select(lhs,term),rhs::Nil)
          if {
            /*println("_a__")
            println(showRaw(d))
            println(showRaw(lhs))
            println(rhs.symbol.asInstanceOf[scala.reflect.internal.Symbols#FreeTerm].value)
            println(rhs.tpe)
            println("_b__")*/
            (
              (string_types contains lhs.tpe.widen.toString) //(lhs.tpe <:< typeOf[String])
                && (string_types contains rhs.tpe.widen.toString) // (rhs.tpe <:< typeOf[String] )
                && (List("+").contains( term.decoded ))
              )
          }
        =>
          term.decoded match {
            case "+" => Library.Concat(s2sq( lhs ).node, s2sq( rhs ).node )
          }

        case Apply(op@Select(lhs,term),rhs::Nil) => {
          val actualTypes = lhs.tpe :: rhs.tpe :: Nil //.map(_.tpe).toList
          val matching_ops = ( operatorMap.collect{
              case (str2sym, types)
                    if str2sym.isDefinedAt( term.decoded )
                      && types.zipWithIndex.forall{
                           case (expectedTypes, index) => expectedTypes.exists( actualTypes(index) <:< _ )
                         }
              => str2sym( term.decoded )
          })
          matching_ops.size match{
            case 0 => throw new SlickException("Operator not supported: "+ lhs.tpe +"."+term.decoded+"("+ rhs.tpe +")")
            case 1 => matching_ops.head( s2sq( lhs ).node, s2sq( rhs ).node )
            case _ => throw new SlickException("Internal Slick error: resolution of "+ lhs.tpe +" "+term.decoded+" "+ rhs.tpe +" was ambigious")
          }
        }

        // Tuples
        case Apply(
            Select(Select(Ident(package_), class_), method_),
            components
        )
        if package_.decoded == "scala" && class_.decoded.startsWith("Tuple") && method_.decoded == "apply" // FIXME: match smarter than matching strings
        =>
            sq.ProductNode( components.map(s2sq(_).node) )

        case Select(scala_lhs, term)
          if scala_lhs.tpe.erasure <:< typeOf[QueryOps[_]].erasure && (term.decoded == "length" || term.decoded == "size")
          => sq.Pure( Library.CountAll( s2sq(scala_lhs).node ) )

        case tree if tree.tpe.erasure <:< typeOf[BaseQueryable[_]].erasure
            => val (tpe,query) = toQuery( eval(tree).asInstanceOf[BaseQueryable[_]] ); query

        case tree => throw new Exception( "You probably used currently not supported scala code in a query. No match for:\n" + showRaw(tree) )
      }
    } catch{
      case e:java.lang.NullPointerException => { println("NPE in tree "+showRaw(tree));throw e}
    }
  }

  private def isNestedColumn(tree:Tree) : Boolean = {
    tree match {
      case Select(from,name) if mapper.isMapped( from.tpe.widen ) => true
      case Select(a,_) => isNestedColumn(a)
      case Apply(Select(a,name),arg::Nil) if (name.encoded == "filter") => isNestedColumn(a)
      case _ => false
    }
  }

  private def extractNestedColumn(tree:Tree, scope:Scope, pathN:List[Either[Symbol,String]]=List()) : Node=  {

    tree match {
      case Select(from,name) if mapper.isMapped( from.tpe.widen ) => {

        val column = findColumn(from,name)

        createColumnFromPath(Left(column)::pathN, from,scope)
      }
      case Select(a,name) if (tree.symbol.isMethod && zeroOperatorMethods.containsKey(name.encoded)) => {
        val method = zeroOperatorMethods(name.encoded)
        method.apply(extractNestedColumn(a, scope, pathN))
      }
      case Apply(Select(a,name),arg::Nil) if (name.encoded == "filter") =>  {
        //this is v. provisoric...
        val Function(param::Nil,body) = arg
        val filter = parseFilter(body, param.symbol)

        extractNestedColumn(a, scope,Right("["+filter+"]")::pathN)
      }

      case Select(a,name) => {
        extractNestedColumn(a, scope, Left(findColumn(a,name))::pathN)
      }
    }
  }

  private def findColumn(from:TreeApi, name:Name) = getConstructorArgs( from.tpe.widen ).filter(_.name==name).head

  val zeroOperatorMethods = Map("size"->OrientDBLibrary.Size,"length"->OrientDBLibrary.Size)

  private def createColumnFromPath(path: List[Either[Symbol,String]],from:TreeApi,scope:Scope) = {
    val finalname::finalpath = path.reverse.map(e=>e.fold(mapper.fieldToColumn,identity))
            scala.slick.ast.Select(scope(from.symbol), new FieldSymbol(finalname)(List(), StringTypeMapper) with NestedSymbol {
                  val path = finalpath.reverse
    })
  }

  private def parseFilter(body:TreeApi, paramName:Symbol) : String = body match {
    case Apply(Select(a,name),arg::Nil) => {
      val Literal(Constant(value)) = arg
      parseNestedSelect(a, paramName, List()).reduce(_+"."+_)+" = "+value
    }
  }

  private def parseNestedSelect(body:TreeApi, paramName:Symbol, acc:List[String]) : List[String] = body match {
    case Select(a,name) => {
      (mapper.fieldToColumn(findColumn(a,name)))::acc
    }
    case a if (a == paramName) => acc
  }

}
