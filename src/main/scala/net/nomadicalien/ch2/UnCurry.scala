package net.nomadicalien.ch2

/**
 * Created by Shawn on 2/5/2015.
 */
object UnCurry {
  def uncurry[A,B,C](f: A => B => C): (A ,B) => C = {
   def uncurried(a: A, b: B): C = {
     f(a)(b)
   }
    uncurried
  }
}
