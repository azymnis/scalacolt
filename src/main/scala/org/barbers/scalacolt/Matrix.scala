package org.barbers.scalacolt

import cern.colt.matrix.DoubleMatrix2D
import cern.colt.matrix.impl.DenseDoubleMatrix2D
import cern.colt.matrix.linalg.Algebra

object Matrix {
  implicit def iterIterToMatrix(it : Iterable[Iterable[Double]]) : Matrix = {
    val ar = it.map{ _.toArray }.toArray
    new Matrix(new DenseDoubleMatrix2D(ar))
  }

  private[scalacolt] def algebra = Algebra.DEFAULT
}
class Matrix(val cMatrix : DoubleMatrix2D) {
  def rows = cMatrix.rows
  def columns = cMatrix.columns
  def toArray = cMatrix.toArray

  // These are expensive so make them lazy
  lazy val sum = cMatrix.zSum
  lazy val trace = Matrix.algebra.trace(cMatrix)
  lazy val rank = Matrix.algebra.rank(cMatrix)
}
