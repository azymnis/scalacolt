package org.barbers.scalacolt

import cern.colt.matrix.DoubleMatrix1D
import cern.colt.function.{DoubleFunction, DoubleDoubleFunction, IntIntDoubleFunction}
import cern.colt.matrix.impl.{DenseDoubleMatrix2D, SparseDoubleMatrix2D}
import cern.colt.matrix.impl.{DenseDoubleMatrix1D, SparseDoubleMatrix1D}

import cern.colt.list.{IntArrayList, DoubleArrayList}

import Implicits.funcToColt
import Implicits.func2ToColt

trait Vector[VecT <: Vector[VecT]] extends Function1[Int,Double] {
  val mapfn : Option[(Double) => Double]
  private[scalacolt] val getVect : DoubleMatrix1D

  lazy val vector : DoubleMatrix1D = {
    mapfn.map { fn => getVect.copy.assign(funcToColt(fn)) }
      .getOrElse(getVect)
  }

  // We calculate the norms here to avoid materializing a new copy (don't touch vector)
  // \sum |x_i|
  lazy val norm1 = map { xi => scala.math.abs(xi) }.sum
  // \sum x_i^2
  lazy val norm2 = map { xi => xi*xi }.sum
  // max_i |x_i|
  lazy val normInf = map { xi => scala.math.abs(xi) }.reduce { _ max _ }

  lazy val sum = {
    if(mapfn.isEmpty)
      getVect.zSum
    else
      reduce { _ + _ }
  }

  lazy val isSparse : Boolean = getVect.isInstanceOf[SparseDoubleMatrix1D]
  lazy val isZero : Boolean = Matrix.property.equals(vector, 0.0)

  def *[N : Numeric](that : N) : VecT = {
    val dN = implicitly[Numeric[N]].toDouble(that)
    map { _ * dN }
  }

  def columns : Int
  def rows : Int

  def toArray : Array[Double] = vector.toArray
  def nonZeros : Iterator[(Int,Double)] = {
    if(isSparse) {
      val ilist = new IntArrayList
      val dlist = new DoubleArrayList
      vector.getNonZeros(ilist, dlist)
      (0 until ilist.size)
        .iterator
        .map { idx => (ilist.get(idx), dlist.get(idx)) }
    }
    else {
      (0 until (columns max rows))
        .iterator
        .map { idx => (idx, vector.getQuick(idx)) }
        .filter { idxV => scala.math.abs(idxV._2) > Matrix.property.tolerance }
    }
  }
  def toMatrix : Matrix = {
    val emptyMat = getVect.like2D(rows, columns)
    if( columns > 1 ) {
      // must be a column vector
      nonZeros.foreach { idxV => emptyMat.setQuick(0, idxV._1, idxV._2) }
    }
    else {
      //Rows *may* be bigger than one
      nonZeros.foreach { idxV => emptyMat.setQuick(idxV._1, 0, idxV._2) }
    }
    new Matrix(emptyMat)
  }

  def map(fn : (Double) => Double) : VecT
  def zipMap(other : VecT)(fn : (Double,Double) => Double) : VecT
  def +(other : VecT) = zipMap(other) { _ + _ }
  def -(other : VecT) = zipMap(other) { _ - _ }

  def reduce(reduce : (Double,Double) => Double) : Double = {
    val thismapfn : DoubleFunction = mapfn.map { funcToColt _ }.getOrElse(IdentityFunc)
    getVect.aggregate(func2ToColt(reduce), thismapfn)
  }

  // This forces evaluation
  override def apply(idx : Int) : Double = mapfn.map { fn => fn(getVect.get(idx)) }.getOrElse { getVect.get(idx) }
  def size = getVect.size
  override def toString = getVect.toString
}
