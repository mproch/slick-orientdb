package pl.mproch.slick.orientdb.direct

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest.fixture.FunSpec
import org.scalatest.matchers.MustMatchers
import pl.mproch.slick.orientdb.TestPreparer
import slick.session.Database
import scala.slick.direct._
import scala.slick.direct.AnnotationMapper._
import Database.{threadLocalSession => session}


@RunWith(classOf[JUnitRunner])
class QueryTest extends FunSpec with MustMatchers with TestPreparer {

  type FixtureParam = Database

  protected def withFixture(test: OneArgTest) {
    prepareDatabase(test.name)
    test(Database.forURL("jdbc:orient:memory:"+test.name,
      driver = "com.orientechnologies.orient.jdbc.OrientJdbcDriver",
      user = "admin", password = "admin"))
  }

  describe("Basic tests for direct embedding") {


    it("loads supplier by name") { db =>
      db withSession {
        val backend = new OrientSlickBackend(AnnotationMapper )

        val query = Queryable[Supplier]

        backend.result(query.filter(_.name == "Henry").map(_.details),session) must be === List(Details("Important supplier", Address("Popularna","Warsaw")))
      }
    }

    it("loads supplier by city") { db =>
      db withSession {
        val backend = new OrientSlickBackend(AnnotationMapper )

        val query = Queryable[Supplier]

        val result = backend.result(query.filter(_.details.address.city == "Warsaw").map(_.details),session)
        result must be === List(Details("Important supplier", Address("Popularna","Warsaw")))

      }
    }

    it("loads complete object") { db =>
      db withSession {

        val backend = new OrientSlickBackend(AnnotationMapper )

        val query = Queryable[Supplier]

        backend.result(query.filter(_.name == "James"),session).map(_.id) must be === List(13)
      }
    }

    it("loads trucks") { db =>
      db withSession {

        val backend = new OrientSlickBackend(AnnotationMapper )

        val query = Queryable[Supplier]

        backend.result(query.filter(_.name == "Henry"),session).head.trucks must be === List(Truck("aa",2.0), Truck("bb",3.0))
      }
    }

  }

}

@table("suppliers")
case class Supplier(@column("id") id:Int,
                     @column("name") name:String,
                     @column("details") details:Details,
                     @column("trucks") trucks:List[Truck])

case class Truck(@column("number") number:String, @column("capacity") capacity:Double)

case class Details(@column("description") description:String,
                   @column("address") address : Address)

case class Address(@column("street") street:String,
                   @column("city") city:String)

