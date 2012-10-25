package pl.mproch.slick.orientdb.lifted

import slick.lifted._
import slick.driver.BasicProfile
import slick.session.{PositionedResult, PositionedParameters}
import java.util
import slick.ast.FieldSymbol
import slick.ast.Select
import slick.util.NaturalTransformation2

object Nested {
  implicit def nestedToColumn[A <: Product](nested: Nested[A]): Column[A] = {

    implicit val mapper: TypeMapper[A] = nested.typeMapper

    new Column[A]() {
      override def nodeDelegate = Select(nested.node, new FieldSymbol(nested.name)(List(), implicitly[TypeMapper[A]]))
    }

  }

  def nestedShape[A<:Product]: Shape[Nested[A], A, Nested[A]] = new IdentityShape[Nested[A], A] {
    def linearizer(from: Mixed) = nestedToColumn(from)
    def buildPacked(f: NaturalTransformation2[TypeMapper, ({ type L[X] = Unpacked => X })#L, Column]) = impureShape
  }


}

abstract class Nested[A <: Product](nameP: String, parent: NNested)
  extends NNested with NestedProperty[A] {

  val node = parent.node

  def name = nameP

  def path = parent.path :+ name

  def * : NestedMappedProjection[A, _ <: Product]

  def getResult(map: util.Map[String, Object]) = *.getResult(map.get(nameP).asInstanceOf[util.Map[String, Object]])

  def typeMapper = {
    new BaseTypeMapper[A] with TypeMapperDelegate[A] {

      def apply(v1: BasicProfile) = this

      def zero = ???

      def sqlType = ???

      def sqlTypeName = ???

      def setValue(v: A, p: PositionedParameters) {}

      def setOption(v: Option[A], p: PositionedParameters) {}

      def nextValue(r: PositionedResult) = {
        val map = r.nextObject().asInstanceOf[java.util.Map[String, Object]]
        *.getResult(map)
      }

      def updateValue(v: A, r: PositionedResult) {}
    }
  }

}

