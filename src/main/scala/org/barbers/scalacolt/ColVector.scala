package org.barbers.scalacolt

import cern.colt.matrix.DoubleMatrix1D
import cern.colt.matrix.impl.{DenseDoubleMatrix2D, SparseDoubleMatrix2D}

import FunctionImplicits._

class ColVector(val vect : DoubleMatrix1D) {

  // expensive to compute, but cheap to store:
  lazy val sum = vect.zSum

  def t : RowVector = new RowVector(vect)

  // This is an outer product
  def *(that : RowVector) : Matrix = {
    // TODO think of a better strategy to choose dense/sparse
    val out = new DenseDoubleMatrix2D(that.size, size)
    Matrix.algebra.multOuter(vect, that.vect, out)
    new Matrix(out)
  }
  def *[N : Numeric](that : N) = {
    val dN = implicitly[Numeric[N]].toDouble(that)
    map { _ * dN }
  }
  def +(other : ColVector) = {
    val out = vect.copy.assign(other.vect, new MatrixAddition)
    new ColVector(out)
  }

  def -(other : ColVector) = {
    val out = vect.copy.assign(other.vect, new MatrixSubtraction)
    new ColVector(out)
  }

  def apply(idx : Int) : Double = vect.get(idx)
  def size = vect.size
  override def toString = vect.toString
  override def equals(that : Any) : Boolean = {
    (that != null) && that.isInstanceOf[ColVector] && {
      vect.equals(that.asInstanceOf[ColVector].vect)
    }
  }
  override def hashCode = vect.hashCode

  def map(mapfn : (Double) => Double) : ColVector = {
    new ColVector(vect.copy.assign(mapfn))
  }
  def mapReduce(mapfn : (Double) => Double)(reduce : (Double, Double) => Double) : Double =
    vect.aggregate(reduce, mapfn)
}
