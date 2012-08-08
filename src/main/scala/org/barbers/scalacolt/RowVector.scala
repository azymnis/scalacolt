package org.barbers.scalacolt

import cern.colt.matrix.DoubleMatrix1D

import FunctionImplicits._

/** Immutable RowVector
 */
class RowVector(val vect : DoubleMatrix1D) {
  lazy val sum = vect.zSum

  def t : ColVector = new ColVector(vect)
  def *(that : Matrix) : RowVector = {
    // v * A = (A^t * v^t)^t, but for 1D vectors there is no tranpose
    new RowVector(Matrix.algebra.mult(that.cMatrixT, vect))
  }
  def *(that : ColVector) : Double = Matrix.algebra.mult(vect, that.vect)
  def *[N : Numeric](that : N) = {
    val dN = implicitly[Numeric[N]].toDouble(that)
    map { _ * dN }
  }
  def +(other : RowVector) = {
    val out = vect.copy.assign(other.vect, new MatrixAddition)
    new RowVector(out)
  }

  def -(other : RowVector) = {
    val out = vect.copy.assign(other.vect, new MatrixSubtraction)
    new RowVector(out)
  }

  def apply(idx : Int) : Double = vect.get(idx)
  def size = vect.size
  override def toString = vect.toString
  override def equals(that : Any) : Boolean = {
    (that != null) && that.isInstanceOf[RowVector] && {
      vect.equals(that.asInstanceOf[RowVector].vect)
    }
  }
  def map(mapfn : (Double) => Double) : RowVector = {
    new RowVector(vect.copy.assign(mapfn))
  }

  def mapReduce(mapfn : (Double) => Double)(reduce : (Double, Double) => Double) : Double =
    vect.aggregate(reduce, mapfn)

  override def hashCode = vect.hashCode
}
