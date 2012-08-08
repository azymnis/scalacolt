package org.barbers.scalacolt

import cern.colt.function.{DoubleFunction, DoubleDoubleFunction, IntIntDoubleFunction}

object FunctionImplicits {
  implicit def fn1(f : (Double) => Double) = new DoubleFunction {
    def apply(in : Double) = f(in)
  }
  implicit def fn2(f : (Double,Double) => Double) = new DoubleDoubleFunction {
    def apply(in0 : Double, in1 : Double) = f(in0, in1)
  }
}
