package pl.mproch.slick.orientdb.direct

import slick.direct.{Mapper, SlickBackend}
import slick.session.{Session, PositionedResult}
import reflect.runtime._
import scala.reflect.runtime.universe._
import scala.reflect.runtime.{currentMirror=>cm}
import java.util.{Map => jmap}
import pl.mproch.slick.orientdb.driver.OrientDBDriver

class OrientSlickBackend(mapper: Mapper) extends SlickBackend(OrientDBDriver, mapper) {
  override protected def resultByType(expectedType: universe.Type, rs: PositionedResult, session: Session) = {
    expectedType match {
      case t if t.typeSymbol.asClass.isCaseClass && !mapper.isMapped(t) =>
        val map = rs.nextObject().asInstanceOf[jmap[String,Object]]
        caseClassByTypeFromValueMap(expectedType, map)
      case _ => super.resultByType(expectedType, rs, session)
    }
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


}
