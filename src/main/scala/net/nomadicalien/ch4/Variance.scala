package net.nomadicalien.ch4

object Variance {
  def mean(xs: Seq[Double]): Option[Double] =
    if(xs.isEmpty) None else Some(xs.sum/xs.length)

  def variance(xs: Seq[Double]): Option[Double] = {
    mean(xs) flatMap(m => mean(xs.map(x => math.pow(x-m, 2))))
  }
}
