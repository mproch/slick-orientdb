package pl.mproch.slick.orientdb.lifted

import slick.ast.{FieldSymbol, Select, Node}
import slick.lifted.{Column, TypeMapper}
import java.util
import pl.mproch.slick.orientdb.ast.NestedSymbol

trait NNested {
  nested =>

  def path: List[String]

  def node: Node

  def ncolumn[C](n: String)(implicit tm: TypeMapper[C]): Column[C] with NestedProperty[C] = new Column[C] with NestedProperty[C] {
    override def nodeDelegate = Select(node, new FieldSymbol(n)(List(), tm) with NestedSymbol {
      val path = nested.path
    })

    override def toString = node.toString() + "." + nested.path

    def getResult(map: util.Map[String, Object]) = map.get(n).asInstanceOf[C]
  }
}