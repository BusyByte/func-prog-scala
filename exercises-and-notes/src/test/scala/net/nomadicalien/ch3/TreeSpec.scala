package net.nomadicalien.ch3

import org.specs2.mutable.Specification

class TreeSpec extends Specification {
  import net.nomadicalien.ch3.Tree.{map => treemap, _}

  "exercise 3.25, size of tree" in {
    Tree.size(Branch(Leaf("a"),Leaf("b"))) must_== (3)
    Tree.size(Branch(Leaf("a"),Branch(Leaf("b"),Leaf("c")))) must_== (5)
  }

  "exercise 3.26, max of tree" in {
    maximum(Branch(Leaf(1),Branch(Leaf(2),Leaf(3)))) must_== (3)
  }

  "exercise 3.27, depth of tree" in {
    depth(Branch(Leaf(1),Branch(Leaf(2),Leaf(3)))) must_== (3)
  }

  "exercise 3.28, implement map on tree" in {
    treemap(Branch(Leaf(1),Branch(Leaf(2),Leaf(3))))((a:Int) => a.toString) must_== (
    Branch(Leaf("1"), Branch(Leaf("2"),Leaf("3")))
    )
  }

  "exercise 3.29, size of tree via fold" in {
    size2(Branch(Leaf("a"),Leaf("b"))) must_== (3)
    size2(Branch(Leaf("a"),Branch(Leaf("b"),Leaf("c")))) must_== (5)
  }

  "exercise 3.29, max of tree via fold" in {
    maximum2(Branch(Leaf(1),Branch(Leaf(2),Leaf(3)))) must_== (3)
  }

  "exercise 3.29, depth of tree via fold" in {
    depth2(Branch(Leaf(1),Branch(Leaf(2),Leaf(3)))) must_== (3)
  }

  "exercise 3.29, implement map on tree via fold" in {
    map2(Branch(Leaf(1),Branch(Leaf(2),Leaf(3))))((a:Int) => a.toString) must_== (
      Branch(Leaf("1"), Branch(Leaf("2"),Leaf("3")))
    )
  }
}
