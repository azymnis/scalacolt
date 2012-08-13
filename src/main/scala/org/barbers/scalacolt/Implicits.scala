package org.barbers.scalacolt

import cern.colt.function.{DoubleFunction, DoubleDoubleFunction, IntIntDoubleFunction}
import cern.colt.matrix.impl.{DenseDoubleMatrix2D, SparseDoubleMatrix2D}
import cern.colt.matrix.DoubleMatrix2D

/**
 * you almost certainly want to do: import Implicits._ in your code.
 *
 * Note: scala Function1/Function2 have primitive specializations, so this should be a zero
 * cost wrapper once the jit gets ahold of it.
 */
object Implicits {
  implicit def toMatrix[T : Numeric](it : Iterable[Iterable[T]]) = Matrix(it)
  implicit def vectToMatrix[VecT <: Vector[VecT]](v : Vector[VecT]) : Matrix = v.toMatrix

  implicit def funcToColt(fn : (Double) => Double) : DoubleFunction = {
    new DoubleFunction { override def apply(x : Double) = fn(x) }
  }
  implicit def func2ToColt(fn : (Double,Double) => Double) : DoubleDoubleFunction = {
    new DoubleDoubleFunction { override def apply(x : Double, y : Double) = fn(x,y) }
  }
  implicit def coltToFunc(fn : DoubleFunction) : (Double) => Double = { (x : Double) => fn.apply(x) }
  implicit def coltToFunc2(fn : DoubleDoubleFunction) : (Double,Double) => Double = {
    (x : Double, y : Double) => fn.apply(x,y)
  }
  implicit def iFnIU2Proc(fn : (Int) => Unit) = new ScalaIntUnitProcedure(fn)
  implicit def iFnIB2Proc(fn : (Int) => Boolean) = new ScalaIntProcedure(fn)

  implicit def doubleToDoubleMultiplier[T : Numeric](c : T) = new DoubleMultiplier(c)
  implicit def doubleMatrix2DToMatrix(d : DoubleMatrix2D) = new Matrix(d)
}
