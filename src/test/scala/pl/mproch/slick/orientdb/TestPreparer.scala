package pl.mproch.slick.orientdb

import com.orientechnologies.orient.core.config.OGlobalConfiguration
import com.orientechnologies.orient.core.db.document.{ODatabaseDocument, ODatabaseDocumentTx}
import scala.collection.JavaConverters._
import java.util

trait TestPreparer {

  def prepareDatabase(name:String) {
    OGlobalConfiguration.STORAGE_KEEP_OPEN.setValue(true)
    val tx = new ODatabaseDocumentTx("memory:"+name)

    tx.create[ODatabaseDocument]()
    tx.getMetadata.getSchema.getOrCreateClass("suppliers")

    tx.newInstance("suppliers")
      .field("name", "Henry")
      .field("id", 12)
      .field("trucks",new util.ArrayList[Object](List(
        Map[String, Object]("number" -> "aa", "capacity"->(2:java.lang.Double)).asJava,
        Map[String, Object]("number" -> "bb", "capacity"->(3:java.lang.Double)).asJava).asJava
     ))
      .field("details",
      Map[String, Object]("description" -> "Important supplier", "address" ->
        Map[String, Object]("city" -> "Warsaw", "street" -> "Popularna").asJava).asJava)
      .save()

    tx.newInstance("suppliers")
      .field("name", "James")
      .field("id", 13)
      .field("trucks",new util.ArrayList[Object]())
      .field("details",
      Map[String, Object]("description" -> "Minor supplier", "address" ->
        Map[String, Object]("city" -> "Tallin", "street" -> "Narva").asJava).asJava)
      .save()


    tx.close()

  }

}
