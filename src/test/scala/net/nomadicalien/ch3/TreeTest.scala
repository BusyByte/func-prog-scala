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

  test("exercise 3.26, max of tree") {
    maximum(Branch(Leaf(1),Branch(Leaf(2),Leaf(3)))) should be(3)
  }

  test("exercise 3.27, depth of tree") {
    depth(Branch(Leaf(1),Branch(Leaf(2),Leaf(3)))) should be(3)
  }

  test("exercise 3.28, implement map on tree") {
    map(Branch(Leaf(1),Branch(Leaf(2),Leaf(3))))((a:Int) => a.toString) should be(
    Branch(Leaf("1"), Branch(Leaf("2"),Leaf("3")))
    )
  }
}
