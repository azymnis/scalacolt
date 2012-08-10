package org.barbers.scalacolt

import cern.colt.matrix.DoubleMatrix1D
import cern.colt.function.{DoubleFunction, DoubleDoubleFunction, IntIntDoubleFunction}
import cern.colt.matrix.impl.{DenseDoubleMatrix2D, SparseDoubleMatrix2D}

import Implicits.funcToColt
import Implicits.func2ToColt

trait Vector[VecT <: Vector[VecT]] {
  val mapfn : Option[(Double) => Double]
  private[scalacolt] val getVect : DoubleMatrix1D

  lazy val vector : DoubleMatrix1D = {
    mapfn.map { fn => getVect.copy.assign(funcToColt(fn)) }
      .getOrElse(getVect)
  }

  lazy val sum = {
    if(mapfn.isEmpty)
      getVect.zSum
    else
      reduce { _ + _ }
  }

  def *[N : Numeric](that : N) : VecT = {
    val dN = implicitly[Numeric[N]].toDouble(that)
    map { _ * dN }
  }

  def map(fn : (Double) => Double) : VecT
  def mergeOp(op : DoubleDoubleFunction, other : VecT) : VecT
  def +(other : VecT) = mergeOp(MatrixAddition, other)
  def -(other : VecT) = mergeOp(MatrixSubtraction, other)

  def reduce(reduce : (Double,Double) => Double) : Double = {
    val thismapfn : DoubleFunction = mapfn.map { funcToColt _ }.getOrElse(IdentityFunc)
    getVect.aggregate(func2ToColt(reduce), thismapfn)
  }

  // This forces evaluation
  def apply(idx : Int) : Double = mapfn.map { fn => fn(getVect.get(idx)) }.getOrElse { getVect.get(idx) }
  def size = getVect.size
  override def toString = getVect.toString
}
