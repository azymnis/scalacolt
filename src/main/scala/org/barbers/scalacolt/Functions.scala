package org.barbers.scalacolt

import cern.colt.function.{DoubleFunction, DoubleDoubleFunction, IntIntDoubleFunction, IntProcedure}

object IdentityFunc extends DoubleFunction { def apply(x : Double) = x }

object MatrixAddition extends DoubleDoubleFunction {
  def apply(x : Double, y : Double) = x + y
}

object MatrixSubtraction extends DoubleDoubleFunction {
  def apply(x : Double, y : Double) = x - y
}

class MappedDoubleDouble(xfn : (Double) => Double, yfn : (Double) => Double,
  ddfn : DoubleDoubleFunction)
  extends DoubleDoubleFunction {
  def apply(x : Double, y : Double) = ddfn.apply(xfn(x),yfn(y))
}

class ScalaIntUnitProcedure(fn : (Int) => Unit) extends IntProcedure {
  def apply(x : Int) = { fn(x); true }
}

class ScalaIntProcedure(fn : (Int) => Boolean) extends IntProcedure {
  def apply(x : Int) = fn(x)
}
