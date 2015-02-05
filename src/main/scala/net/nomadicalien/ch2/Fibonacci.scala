package net.nomadicalien.ch2

import scala.annotation.tailrec

/**
 * Created by Shawn on 2/4/2015.
 */
object Fibonacci {
  def fibbo(n : Int): Int = {
    @tailrec
    def iterFibbo(nMinus2:Int, nMinus1:Int, currentN:Int) :Int = {
      val currentValue = nMinus2 + nMinus1
      if(currentN == n) {
        currentValue
      } else {
        iterFibbo(nMinus1, currentValue, currentN + 1)
      }
    }

    if(n < 0) {
      throw new IllegalStateException(s"n may not be negative; $n was provided")
    }
    if(n == 0){
      0
    } else if(n == 1) {
      1
    } else {
      iterFibbo(0, 1, 2)
    }
  }
}
