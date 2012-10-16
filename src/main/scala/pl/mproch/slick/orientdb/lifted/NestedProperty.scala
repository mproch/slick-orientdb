package pl.mproch.slick.orientdb.lifted

trait NestedProperty[S1] {
  def getResult(map: java.util.Map[String, Object]): S1

  def <->[S2](pro: NestedProperty[S2]) = NestedProjection2(this, pro)
}
