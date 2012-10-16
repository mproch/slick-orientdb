package pl.mproch.slick.orientdb.lifted

import pl.mproch.slick.orientdb.driver.OrientDBDriver
import OrientDBDriver.Table
import slick.ast.{Ref, Node}

abstract class Document[T](_schemaName: Option[String], _tableName: String) extends Table[T](_schemaName, _tableName) with NNested {
  table =>

  def this(_tableName: String) = this(None, _tableName)

  def node = {
    Node(table) match {
      case r: Ref => r
      case _ => Ref(Node(table).nodeIntrinsicSymbol)
    }
  }

  def path = List()
}

