package org.barbers.scalacolt

import org.specs2.mutable._

class MatrixSpec extends Specification {
  import Matrix._
  val a : Matrix = List(List(1.0,2.0,3.0),List(4.0,5.0,6.0))
  val b : Matrix = List(List(1.0,2.0,3.0),List(4.0,5.0,6.0),List(7.0,8.0,9.0))

  "A matrix object" should {
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
}
