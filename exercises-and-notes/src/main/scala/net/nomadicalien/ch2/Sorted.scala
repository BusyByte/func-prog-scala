package net.nomadicalien.ch2

import scala.annotation.tailrec

object Sorted {

  def isSorted[A](as: Array[A], ordered: (A,A) => Boolean): Boolean = {
    val arraySize = as.size
    if(arraySize == 0 || arraySize == 1) {
      true
    } else {
      val arraySizeMinus1 = arraySize - 1
      @tailrec
      def isSortedIter(index: Int): Boolean = {
        if(index == arraySizeMinus1) {
          true
        } else {
          ordered(as(index), as(index+1)) && isSortedIter(index + 1)
        }
      }

      isSortedIter(0)
    }
  }


}
