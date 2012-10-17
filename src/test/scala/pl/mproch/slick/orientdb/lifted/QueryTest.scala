package pl.mproch.slick.orientdb.lifted

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest.fixture.FunSpec
import org.scalatest.matchers.MustMatchers
import pl.mproch.slick.orientdb.TestPreparer
import slick.session.Database
import Nested._

import Database.threadLocalSession
import pl.mproch.slick.orientdb.driver.OrientDBDriver
import OrientDBDriver.simple._

@RunWith(classOf[JUnitRunner])
class QueryTest extends FunSpec with MustMatchers with TestPreparer {

  describe("Basic tests for lifted embedding") {
    it("Filters and loads Supplier") { db =>
      db withSession {
        val query = for {
          s <- Suppliers
          if (s.name === "Henry" && s.details.address.city === "Warsaw")
        } yield (s)

        query.list().map(_.id) must be === List(12)
      }
    }

    it("Filters and loads cities") { db =>
      db withSession {
        val query = for {
          s <- Suppliers
          if (s.name === "James")
        } yield (s.details.address.city)

        query.list() must be === List("Tallin")
      }
    }
  }

  type FixtureParam = Database

  protected def withFixture(test: OneArgTest) {
    prepareDatabase(test.name)
    test(Database.forURL("jdbc:orient:memory:"+test.name,
      driver = "com.orientechnologies.orient.jdbc.OrientJdbcDriver",
      user = "admin", password = "admin"))
  }
}

object Suppliers extends Document[Supplier]("SUPPLIERS") {

  def id = column[Int]("id")
  def name = column[String]("name")
  def details = {
    val s = this
    new Nested[Details]("details", s) {
      def description = ncolumn[String]("description")

      def address = new Nested[Address]("address", this) {
        def city = ncolumn[String]("city")
        def street = ncolumn[String]("street")
        def * = city <-> street <>(Address, Address.unapply _)
      }
      def * = description <-> address <>(Details.apply _, Details.unapply _)
    }
  }

  def * = id ~ name ~ details <>(Supplier, Supplier.unapply _)
}

case class Details(description: String, address: Address)

case class Address(city: String, street: String)

case class Supplier(id: Int, name: String, details: Details)

