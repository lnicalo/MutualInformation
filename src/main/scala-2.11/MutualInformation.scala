/**
  * Created by LNICOLAS on 07/10/2016.
  */
package MutualInformation

import breeze.linalg.{*, Axis, DenseMatrix, DenseVector, sum}
import breeze.numerics.{exp, pow, sqrt}

import scala.math.Pi
import breeze.stats.stddev
import breeze.generic.UFunc
object sqDist extends UFunc {

  implicit object implDMDM extends Impl2[DenseMatrix[Double], DenseMatrix[Double], DenseMatrix[Double]] {
    def apply(x1: DenseMatrix[Double], x2: DenseMatrix[Double]): DenseMatrix[Double] = {
      val t1 = -2.0 * (x1.t * x2)
      val t2 = t1(*, ::) + sum(x2 :* x2, Axis._0).t
      t2(::, *) + sum(x1 :* x1, Axis._0).t
    }
  }
}

class MutualInformation[T](x: DenseMatrix[Double], y: Array[T]) {
  def run(): DenseVector[Double] ={
    val classes = y.distinct
    require(classes.length > 2)
    // Priori probabilities
    val p = y.groupBy(x => x).map(p => (p._1, p._2.length / y.length))
    DenseVector[Double](0,0,0)
  }
}

class ParzenWindow(x0: DenseVector[Double], x: DenseVector[Double]) {
  val M = x.length
  val sigma: Double = stddev(x)
  val h: Double = pow((4 / (3 * M)), (1 / 5) * sigma)

  def probability (): DenseVector[Double] = {
    // val dist: DenseMatrix[Double] = pow(sqDist(x0, x), 2.0)
    // val tmp = 1 / (sqrt(2 * Pi) * h * M) * exp(-dist / (2 * pow(h, 2)))
    val tmp = pow(x)
    sum(tmp(*,::))
  }
}

object ParzenWindow {
  def apply(x0: DenseVector[Double], x: DenseMatrix[Double]): ParzenWindow = new ParzenWindow(x0, x)
}