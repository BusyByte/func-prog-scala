package net.nomadicalien.ch4

import org.specs2.mutable.Specification

class OptionSpec extends Specification {
  "exercise 4.1, implement map" in {
      Some("foo").map(_.length) must_== (Some(3))
      None.asInstanceOf[Option[String]].map(_.length) must_== (None)
  }

  "exercise 4.1, implement getOrElse" in {
    Some("foo").getOrElse("default") must_== ("foo")
    None.asInstanceOf[Option[String]].getOrElse("default") must_== ("default")
  }

  "exercise 4.1, implement flatMap" in {
    def getCount(name: String): Option[Int] = {
      val length = name.length
      if(length > 0) Some(length) else None
    }

    Some("bob").flatMap(getCount) must_== (Some(3))
    None.asInstanceOf[Option[String]].flatMap(getCount) must_== (None)
  }

  "exercise 4.1, implement orElse" in {
    Some("foo").orElse(Some("default")) must_== (Some("foo"))
    None.asInstanceOf[Option[String]].orElse(Some("default")) must_== (Some("default"))
  }

  "exercise 4.1, implement filter" in {
    def bobFilter(name: String) = "bob".equalsIgnoreCase(name)
    Some("bob").filter(bobFilter) must_== (Some("bob"))
    None.asInstanceOf[Option[String]].filter(bobFilter) must_== (None)
  }

  import Option._
  "exercise 4.3, map via option - Some" in {
    val aMaybe = Some(1)
    val bMaybe = Some(2)
    map2(aMaybe,bMaybe)(_ + _) must_==  (Some(3))
  }

  "exercise 4.3, map via option - None" in {
    val aMaybe = None : Option[Int]
    val bMaybe = Some(2)
    map2(aMaybe,bMaybe)(_ + _) must_==  (None)
  }

  "exercise 4.4, sequence - Some" in {
    sequence(List(Some(1),Some(2))) must_==  (Some(List(1,2)))
  }

  "exercise 4.4, sequence - None" in {
    sequence(List(Some(1), None: Option[Int], Some(2))) must_==  (None)
  }

  "exercise 4.4, traverse - Some" in {
    traverse(List(1,2,3))(Some(_)) must_== (Some(List(1,2,3)))
  }

  "exercise 4.4, traverse - None" in {
    traverse(List(1,2,3))(a => None : Option[Int]) must_== (None)
  }

  "exercise 4.5, sequence via traverse - Some" in {
    sequence2(List(Some(1),Some(2))) must_==  (Some(List(1,2)))
  }

  "exercise 4.5, sequence via travers - None" in {
    sequence2(List(Some(1), None: Option[Int], Some(2))) must_==  (None)
  }
}
