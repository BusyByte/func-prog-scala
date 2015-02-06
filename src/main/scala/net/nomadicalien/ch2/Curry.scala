package net.nomadicalien.ch2

/**
 * Created by Shawn on 2/5/2015.
 */
object Curry {
  def curry[A,B,C](f: (A, B) => C): A => (B => C) = {
   def curried(a : A): B => C = { b: B =>
     f(a,b)
    }
    curried
  }
}
