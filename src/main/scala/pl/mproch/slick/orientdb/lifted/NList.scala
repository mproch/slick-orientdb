package pl.mproch.slick.orientdb.lifted

import java.util
import scala.collection.JavaConversions._
import slick.lifted.{TypeMapperDelegate, BaseTypeMapper, TypeMapper, Column}
import slick.driver.BasicProfile
import slick.session.{PositionedResult, PositionedParameters}
import slick.ast.{FieldSymbol, Select}

object NList {
  implicit def nListToColumn[A <: Product](nested: NList[A]): Column[List[A]] = {

    implicit val mapper: TypeMapper[List[A]] = new BaseTypeMapper[List[A]] with TypeMapperDelegate[List[A]] {

      def apply(v1: BasicProfile) = this

      def zero = ???
      def sqlType = ???
      def sqlTypeName = ???

      def setValue(v: List[A], p: PositionedParameters) {}
      def setOption(v: Option[List[A]], p: PositionedParameters) {}

      def nextValue(r: PositionedResult) = mapToNList(r.nextObject(), nested)

      def updateValue(v: List[A], r: PositionedResult) {}
    }

    new Column[List[A]]() {
      override def nodeDelegate = Select(nested.node, new FieldSymbol(nested.name)(List(), implicitly[TypeMapper[List[A]]]))
    }

  }

  def mapToNList[A<:Product](value:Object, nlist:NList[A]) = {
    val list = Option(value).map(_.asInstanceOf[util.List[util.Map[String, Object]]].toList).toList.flatten
    list.map(nlist.*.getResult).toList
  }
}

abstract class NList[A <: Product](nameP: String, parent: NNested)
  extends NNested with NestedProperty[List[A]] {

  val node = parent.node

  def name = nameP

  def path = parent.path :+ name

  def * : NestedMappedProjection[A, _ <: Product]

  def getResult(map: util.Map[String, Object]) = NList.mapToNList(map.get(nameP), this)
}