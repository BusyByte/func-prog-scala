package net.nomadicalien.ch4

import org.scalatest.{FunSuite, Matchers}

/**
 * Created by Shawn on 2/4/2015.
 */
class OptionTest extends FunSuite with Matchers {
  test("exercise 4.1, implement map") {
      Some("foo").map(_.length) should be(Some(3))
      None.asInstanceOf[Option[String]].map(_.length) should be(None)
  }

  test("exercise 4.1, implement getOrElse") {
    Some("foo").getOrElse("default") should be("foo")
    None.asInstanceOf[Option[String]].getOrElse("default") should be("default")
  }

  test("exercise 4.1, implement flatMap") {
    def getCount(name: String): Option[Int] = {
      val length = name.length
      if(length > 0) Some(length) else None
    }

    Some("bob").flatMap(getCount) should be(Some(3))
    None.asInstanceOf[Option[String]].flatMap(getCount) should be(None)
  }

  test("exercise 4.1, implement orElse") {
    Some("foo").orElse(Some("default")) should be(Some("foo"))
    None.asInstanceOf[Option[String]].orElse(Some("default")) should be(Some("default"))
  }

  test("exercise 4.1, implement filter") {
    def bobFilter(name: String) = "bob".equalsIgnoreCase(name)
    Some("bob").filter(bobFilter) should be(Some("bob"))
    None.asInstanceOf[Option[String]].filter(bobFilter) should be(None)
  }

  import Option._
  test("exercise 4.3, map via option - Some") {
    val aMaybe = Some(1)
    val bMaybe = Some(2)
    map2(aMaybe,bMaybe)(_ + _) should be (Some(3))
  }

  test("exercise 4.3, map via option - None") {
    val aMaybe = None : Option[Int]
    val bMaybe = Some(2)
    map2(aMaybe,bMaybe)(_ + _) should be (None)
  }

  test("exercise 4.4, sequence - Some") {
    sequence(List(Some(1),Some(2))) should be (Some(List(1,2)))
  }

  test("exercise 4.4, sequence - None") {
    sequence(List(Some(1), None: Option[Int], Some(2))) should be (None)
  }

  test("exercise 4.4, traverse - Some") {
    traverse(List(1,2,3))(Some(_)) should be(Some(List(1,2,3)))
  }

  test("exercise 4.4, traverse - None") {
    traverse(List(1,2,3))(a => None : Option[Int]) should be(None)
  }

  test("exercise 4.5, sequence via traverse - Some") {
    sequence2(List(Some(1),Some(2))) should be (Some(List(1,2)))
  }

  test("exercise 4.5, sequence via travers - None") {
    sequence2(List(Some(1), None: Option[Int], Some(2))) should be (None)
  }



}
