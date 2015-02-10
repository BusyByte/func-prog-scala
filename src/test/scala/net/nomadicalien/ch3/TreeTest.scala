package net.nomadicalien.ch3

import org.scalatest.{FunSuite, Matchers}

/**
 * Created by Shawn on 2/4/2015.
 */
class TreeTest extends FunSuite with Matchers {
  import net.nomadicalien.ch3.Tree._

  test("exercise 3.25, size of tree") {
    Tree.size(Branch(Leaf("a"),Leaf("b"))) should be(3)
    Tree.size(Branch(Leaf("a"),Branch(Leaf("b"),Leaf("c")))) should be(5)
  }
}
