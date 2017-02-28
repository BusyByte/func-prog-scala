package net.nomadicalien.ch2

object UnCurry {
  def uncurry[A,B,C](f: A => B => C): (A ,B) => C = {
   def uncurried(a: A, b: B): C = {
     f(a)(b)
   }
    uncurried
  }
}
