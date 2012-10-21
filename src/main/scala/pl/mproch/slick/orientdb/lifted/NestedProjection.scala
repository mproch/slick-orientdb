package pl.mproch.slick.orientdb.lifted

import java.util


case class NestedMappedProjection[T, P <: Product](f: P => T, g: T => Option[P])(proj: NestedProjection[P]) {
  def getResult(map: java.util.Map[String, Object]): T = f(proj.getResult(map))
}

trait NestedProjection[P <: Product] {

  def getResult(map: java.util.Map[String, Object]): P

  def toProj[T](f: P => T, g: T => Option[P]) = NestedMappedProjection[T, P](f, g)(this)


}

case class NestedProjection2[S1, S2](p1: NestedProperty[S1], p2: NestedProperty[S2]) extends NestedProjection[(S1, S2)] {
  def getResult(map: util.Map[String, Object]) = (p1.getResult(map), p2.getResult(map))

  def <->[S3](pro: NestedProperty[S3]) = NestedProjection3(p1, p2, pro)

  def <>[T](f: (S1, S2) => T, g: T => Option[(S1, S2)]) = toProj(t => f(t._1, t._2), g)
}

case class NestedProjection3[S1, S2, S3](p1: NestedProperty[S1], p2: NestedProperty[S2], p3: NestedProperty[S3])
  extends NestedProjection[(S1, S2, S3)] {
  def getResult(map: util.Map[String, Object]) = (p1.getResult(map), p2.getResult(map), p3.getResult(map))

  def <->[S4](pro: NestedProperty[S4]) = NestedProjection4(p1, p2, p3, pro)

  def <>[T](f: (S1, S2, S3) => T, g: T => Option[(S1, S2, S3)]) = toProj(t => f(t._1, t._2, t._3), g)
}

case class NestedProjection4[S1, S2, S3, S4](p1: NestedProperty[S1], p2: NestedProperty[S2], p3: NestedProperty[S3], p4:NestedProperty[S4])
  extends NestedProjection[(S1, S2, S3, S4)] {
  def getResult(map: util.Map[String, Object]) = (p1.getResult(map), p2.getResult(map), p3.getResult(map), p4.getResult(map))

  def <>[T](f: (S1, S2, S3, S4) => T, g: T => Option[(S1, S2, S3, S4)]) = toProj(t => f(t._1, t._2, t._3, t._4), g)
}
