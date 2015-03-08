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
  def nonNegativeInt(rng: RNG): (Int, RNG) = {
    val (nextN, nextRng) = rng.nextInt
    if(nextN == Int.MinValue) {
      nonNegativeInt(nextRng)
    } else {
      (Math.abs(nextN), nextRng)
    }
  }
}