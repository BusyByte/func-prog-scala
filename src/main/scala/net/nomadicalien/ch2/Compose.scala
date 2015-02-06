package net.nomadicalien.ch2

/**
 * Created by Shawn on 2/5/2015.
 */
object Compose {
  def compose[A,B,C](f: B => C, g: A => B): A => C = {
    def composite(a: A): C = {
      f(g(a))
    }
    composite
  }
}
