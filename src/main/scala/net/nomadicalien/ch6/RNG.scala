package net.nomadicalien.ch6


/**
 * Created by Shawn on 3/7/2015.
 */
trait RNG {
  def nextInt: (Int, RNG)
}

case class SimpleRNG(seed: Long) extends RNG {
  def nextInt: (Int, RNG) = {
    val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL
    val nextRNG = SimpleRNG(newSeed)
    val n = (newSeed >>> 16).toInt
    (n, nextRNG)
  }


}

object RNG {
  type Rand[+A] = RNG => (A, RNG)

  def nonNegativeInt(rng: RNG): (Int, RNG) = {
    val (nextN, nextRng) = rng.nextInt
    if (nextN == Int.MinValue) {
      nonNegativeInt(nextRng)
    } else {
      (Math.abs(nextN), nextRng)
    }
  }

  def double(rng: RNG): (Double, RNG) = {
    val (nextN, nextRng) = nonNegativeInt(rng)
    (nextN.toDouble / Int.MaxValue.toDouble, nextRng)
  }

  def intDouble(rng: RNG): ((Int, Double), RNG) = {
    val (i1, rng1) = nonNegativeInt(rng)
    val (d2, rng2) = double(rng1)
    ((i1, d2), rng2)
  }

  def doubleInt(rng: RNG): ((Double, Int), RNG) = {
    val (p, nextRNG) = intDouble(rng)
    (p.swap, nextRNG)
  }

  def double3(rng: RNG): ((Double, Double, Double), RNG) = {
    val (d1, rng1) = double(rng)
    val (d2, rng2) = double(rng1)
    val (d3, rng3) = double(rng2)
    ((d1, d2, d3), rng3)
  }

  def ints(count: Int)(rng: RNG): (List[Int], RNG) = {
    def i(cnt: Int)(r: RNG)(acc: List[Int]): (List[Int], RNG) = {
      if (cnt == 0) {
        (acc, r)
      } else {
        val (nextRndInt, nextRng) = nonNegativeInt(r)
        i(cnt - 1)(nextRng)(nextRndInt :: acc)
      }
    }
    i(count)(rng)(Nil)
  }

  def unit[A](a: A): Rand[A] =
    rng => (a, rng)

  def map[A, B](s: Rand[A])(f: A => B): Rand[B] =
    rng => {
      val (a, rng2) = s(rng)
      (f(a), rng2)
    }

  def nonNegativeEven: Rand[Int] =
    map(nonNegativeInt)(i => i - i % 2)

  def double2: Rand[Double] =
    map(nonNegativeInt)(i => i.toDouble / Int.MaxValue)

  def map2[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] =
    rng => {
      val (a, rng2) = ra(rng)
      val (b, rng3) = rb(rng2)
      val c = f(a, b)
      (c, rng3)
    }

}