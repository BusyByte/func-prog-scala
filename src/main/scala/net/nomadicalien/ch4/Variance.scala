package net.nomadicalien.ch4

/**
 * Created by Shawn on 2/12/2015.
 */
object Variance {
  def mean(xs: Seq[Double]): Option[Double] =
    if(xs.isEmpty) None else Some(xs.sum/xs.length)

  def variance(xs: Seq[Double]): Option[Double] = {
    mean(xs) flatMap(m => mean(xs.map(x => math.pow(x-m, 2))))
  }
}
