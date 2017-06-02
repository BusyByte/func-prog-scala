package net.nomadicalien.ch8

import net.nomadicalien.ch7.Nonblocking.Par
import org.specs2.mutable.Specification


class PropertySpec extends Specification {


  "Exercise 8.1: sum should" in {
    def sum(list: List[Int]): Int = {
      list.sum
    }

    "have same sum with reverse list" in {
      val intGen = Gen.choose(1, 10)
      val listGen = Gen.listOfN(10, intGen)
      val prop = Gen.forAllPar(listGen) {
        intList: List[Int] =>
          Par.lazyUnit {
            val sum1 = sum(intList)
            val sum2 = sum(intList.reverse)
            sum1 == sum2
          }
      }
      Prop.run(prop)
      ok
    }

    "have sum X * N if all elements of the list are X and has length N" in {
      val fiveGen = Gen.unit(5)
      val listGen = Gen.listOfN(10, fiveGen)
      val prop = Gen.forAllPar(listGen) {
        intList: List[Int] =>
          Par.lazyUnit {
            val sum1 = sum(intList)
            val expectedSum = intList.headOption.getOrElse(0) * intList.size
            sum1 == expectedSum
          }
      }
      Prop.run(prop)
      ok
    }
  }

  "Exercise 8.2: max should" in {
    def max(list: List[Int]): Option[Int] = {
      if(list.isEmpty) None else Some(list.max)
    }

    "have value equal to sorted last" in {
      val intGen = Gen.choose(1, 10)
      val listGen = Gen.listOfN(10, intGen)
      val prop = Gen.forAllPar(listGen) {
        intList: List[Int] =>
          Par.lazyUnit {
            val max1 = max(intList)
            val sortedLast = intList.sorted.lastOption
            max1 == sortedLast
          }
      }
      Prop.run(prop)
      ok
    }
  }



}
