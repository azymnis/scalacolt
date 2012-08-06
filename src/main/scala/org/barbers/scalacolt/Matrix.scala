package org.barbers.scalacolt

import cern.colt.function.IntIntDoubleFunction
import cern.colt.matrix.DoubleMatrix2D
import cern.colt.matrix.impl.DenseDoubleMatrix2D
import cern.colt.matrix.linalg.Algebra

import Numeric.Implicits._

object MatrixImplicits {
  implicit def iterIterToMatrix[T : Numeric](it : Iterable[Iterable[T]]) = {
    val ar = it.map{ _.map{el => el.toDouble}.toArray }.toArray
    new Matrix(new DenseDoubleMatrix2D(ar))
  }

  implicit def doubleToDoubleMultiplier(c : Double) = new DoubleMultiplier(c)
}

object Matrix {
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

  def *[T : Numeric](c : T) = {
    val cDoub = c.toDouble
    val multFunc = new IntIntDoubleFunction {
      def apply(first : Int, second : Int, third : Double) = cDoub * third
    }
    val newMat = cMatrix.copy.forEachNonZero(multFunc)
    new Matrix(newMat)
  }

  override def equals(that : Any) = {
    if(that.isInstanceOf[Matrix]) {
      this.cMatrix == that.asInstanceOf[Matrix].cMatrix
    } else {
      false
    }
  }
  override def hashCode = cMatrix.hashCode
  override def toString = cMatrix.toString
}

class DoubleMultiplier[T : Numeric](c : T) {
  def *(m : Matrix) = m * c
}
