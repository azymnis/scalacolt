package org.barbers.scalacolt

import org.specs2.mutable._

class MatrixSpec extends Specification {
  import MatrixImplicits._
  val A : Matrix = List(List(1,2.0,3),List(4.0,5,6.0))
  val B : Matrix = List(List(1.0,2.0,3),List(4.0,5.0,6),List(7.0,8,9.0))
  val D : Matrix = List(List(1,2), List(3,4), List(5,6))

  "A Matrix" should {
    "have the right rows and columns" in {
      A.rows must_== 2
      A.columns must_== 3
    }

    "have the right trace" in {
      A.trace must_== 6.0
    }

    "have the right sum" in {
      A.sum must_== 21.0
    }

    // TODO: check why rank fails for rectangular matrices
    "have the right rank" in {
      B.rank must_== 2
    }
  }

  "A scalar" should {
    "implicitly be able to scale a Matrix" in {
      val C = 2 * A
      val res : Matrix = List(List(2.0,4.0,6.0),List(8.0,10.0,12.0))
      C must_== res
    }
  }

  "A pair of matrices" should {
    "generate a valid multiplication" in {
      val C = A * B
      val res : Matrix = List(List(30, 36, 42),List(66, 81, 96))
      C must_== res
    }
  }

}
