package org.barbers.scalacolt

import cern.colt.function.{DoubleFunction, DoubleDoubleFunction, IntIntDoubleFunction}

object IdentityFunc extends DoubleFunction { def apply(x : Double) = x }

object MatrixAddition extends DoubleDoubleFunction {
  def apply(x : Double, y : Double) = x + y
}

object MatrixSubtraction extends DoubleDoubleFunction {
  def apply(x : Double, y : Double) = x - y
}
