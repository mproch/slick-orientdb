package pl.mproch.slick.orientdb.ast

import slick.ast.Library.SqlFunction

object OrientDBLibrary {

  val Size = new SqlFunction("size")

  val Contains = new SqlFunction("contains")


}
