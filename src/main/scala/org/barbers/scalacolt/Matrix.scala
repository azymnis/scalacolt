package org.barbers.scalacolt

import cern.colt.matrix.DoubleMatrix2D
import cern.colt.matrix.impl.DenseDoubleMatrix2D

object Matrix {
  implicit def iterIterToMatrix(it : Iterable[Iterable[Double]]) : Matrix = {
    val ar = it.map{ _.toArray }.toArray
    new Matrix(new DenseDoubleMatrix2D(ar))
  }
}
class Matrix(val cMatrix : DoubleMatrix2D) {
  def rows = cMatrix.rows
  def columns = cMatrix.columns
}
