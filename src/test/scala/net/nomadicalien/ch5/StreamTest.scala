package net.nomadicalien.ch5

import net.nomadicalien.ch4.{None, Some}
import org.scalatest.{Matchers, FunSuite}

/**
 * Created by Shawn on 2/20/2015.
 */
class StreamTest  extends FunSuite with Matchers {

  test("exercise 5.1, toList") {
    Stream(1,2,3,4).toList should be(List(1,2,3,4))
  }
}
