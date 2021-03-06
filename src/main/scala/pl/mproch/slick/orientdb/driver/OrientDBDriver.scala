package pl.mproch.slick.orientdb.driver

import slick.driver.{QueryBuilderInput, BasicDriver}
import slick.ast.{Library, Path, Node, Symbol}
import pl.mproch.slick.orientdb.ast.{OrientDBLibrary, NestedSymbol}
import scala.slick.util.MacroSupport.macroSupportInterpolation

trait OrientDBDriver extends BasicDriver {
  driver =>
  override def quoteIdentifier(id: String) = id

  override def createQueryBuilder(input: QueryBuilderInput): QueryBuilder = new QueryBuilder(input)

  class QueryBuilder(input: QueryBuilderInput) extends super.QueryBuilder(input) {

    override def expr(n: Node, skipParens: Boolean) = n match {
      case Path(field :: (rest@(_ :: _))) =>
        b += encodeSymbol(field)
      case OrientDBLibrary.Size(path) => b"$path.size()"


      case _ => super.expr(n, skipParens)
    }

    def encodeSymbol(symbol : Symbol) = symbol match {
      case e:NestedSymbol => (symbolName(symbol)::(e.path.reverse)).reverse.reduce(_+"."+_).replaceAll("\\.\\[","[")
      case _ => symbolName(symbol)
    }

    override protected def buildFrom(n: Node, alias: Option[Symbol], skipParens: Boolean) {
      super.buildFrom(n, None, skipParens)
    }
  }

}

object OrientDBDriver extends OrientDBDriver
