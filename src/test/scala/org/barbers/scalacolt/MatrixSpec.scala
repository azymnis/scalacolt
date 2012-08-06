package org.barbers.scalacolt

import org.specs2.mutable._

class MatrixSpec extends Specification {
  "An iterable of iterables" should {
    "implicitly convert to a Matrix" in {
      import Matrix._
      val a : Matrix = List(List(1.0,2.0,3.0),List(4.0,5.0,6.0))
      a.rows must_== 2
      a.columns must_== 3
    }
  }
}
