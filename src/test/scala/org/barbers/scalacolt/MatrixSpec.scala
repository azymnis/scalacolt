package org.barbers.scalacolt

import org.specs2.mutable._

class MatrixSpec extends Specification {
  import MatrixImplicits._
  val a : Matrix = List(List(1,2.0,3),List(4.0,5,6.0))
  val b : Matrix = List(List(1.0,2.0,3),List(4.0,5.0,6),List(7.0,8,9.0))

  "A Matrix" should {
    "have the right rows and columns" in {
      a.rows must_== 2
      a.columns must_== 3
    }

    "have the right trace" in {
      a.trace must_== 6.0
    }

    "have the right sum" in {
      a.sum must_== 21.0
    }

    // TODO: check why rank fails for rectangular matrices
    "have the right rank" in {
      b.rank must_== 2
    }
  }

  "A scalar" should {
    "implicitly be able to scale a Matrix" in {
      val c = 2 * a
      val res : Matrix = List(List(2.0,4.0,6.0),List(8.0,10.0,12.0))
      c must_== res
    }
  }
}
