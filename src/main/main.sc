import MutualInformation.ParzenWindow
import breeze.linalg.{DenseMatrix, DenseVector}
import breeze.stats.stddev
val normal01 = breeze.stats.distributions.Gaussian(0, 1)
val x: DenseMatrix[Double] = DenseMatrix.rand(3000,4, normal01)
val x0: DenseVector[Double] = DenseVector[Double](0,0,0,0)

val pw = ParzenWindow(x0, x)
val p = pw.probability()
