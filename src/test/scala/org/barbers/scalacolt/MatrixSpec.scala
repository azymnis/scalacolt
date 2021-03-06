package org.barbers.scalacolt

import org.specs2.mutable._

import Implicits._

class MatrixSpec extends Specification {
  val A : Matrix = List(List(1, 2.0, 3), List(4.0, 5, 6.0))
  val B : Matrix = List(List(1.0, 2.0, 3), List(4.0, 5.0, 6), List(7.0, 8, 9.0))
  val D : Matrix = List(List(1, 2), List(3, 4), List(5, 6))
  val E : Matrix = List(List(5, 6, 7), List(1, 2, 3))

  "A Matrix" should {
    "be constructable from iterables" in {
      val m1 = Matrix(List(List(0,1,2),List(2,3,4)))
      m1.cMatrix.get(0,0) must beCloseTo(0, 1e-5)
      m1.cMatrix.get(0,1) must beCloseTo(1, 1e-5)
      m1.cMatrix.get(0,2) must beCloseTo(2, 1e-5)
      m1.cMatrix.get(1,0) must beCloseTo(2, 1e-5)
      m1.cMatrix.get(1,1) must beCloseTo(3, 1e-5)
      m1.cMatrix.get(1,2) must beCloseTo(4, 1e-5)

      val m2 = Matrix.sparse(Map((0,1) -> 1, (0,2) -> 2, (1,0) -> 2, (1,1) -> 3, (1,2) -> 4))
      m2.isSparse must beTrue
      (m2 - m1).normF must beCloseTo(0, 1e-5)
      (m2 - m1).norm1 must beCloseTo(0, 1e-5)
      (m2 - m1).normInf must beCloseTo(0, 1e-5)
    }
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

    "have the right rank" in {
      A.rank must_== 2
      B.rank must_== 2
      D.rank must_== 2
      E.rank must_== 2
    }

    "map to plus 1" in {
      val H = A.map{ x : Double => x + 1}
      val res : Matrix = List(List(2, 3.0, 4), List(5.0, 6, 7.0))
      H must_== res
    }

    "have a correct isRectangular property" in {
      A.isRectangular must_== false
      D.isRectangular must_== true
    }

    "have a transpose" in {
      val At = A.t
      val res : Matrix = List(List(1, 4), List(2, 5), List(3, 6))
      At must_== res
    }
  }

  "Matrix factory methods" should {
    "produce d1/d2 matrices" in {
      (20 to 30).foreach { n =>
        Matrix.d1(n-1) * Matrix.d1(n) must be_==(Matrix.d2(n))
      }
    }
    "produce dense eye" in {
      val I = Matrix.eye(3)
      I.isSparse must beFalse
      val res : Matrix = List(List(1, 0, 0), List(0, 1, 0), List(0, 0, 1))
      I must_== res
    }

    "produce sparse eye" in {
      val I = Matrix.speye(3)
      I.isSparse must beTrue
      val res : Matrix = List(List(1, 0, 0), List(0, 1, 0), List(0, 0, 1))
      I must_== res
    }

    "produce zeros" in {
      val O = Matrix.zeros(2, 2)
      val O2 = Matrix.sparse(2, 2)
      O2.isSparse must beTrue
      val res : Matrix = List(List(0, 0), List(0, 0))
      O must_== res
      O2 must_== res
    }
    "allow stacking" in {
      val sigmax = Matrix.sparse(Map((0,1) -> 1, (1,0) -> 1))
      val sigmaz = Matrix.sparse(Map((0,0) -> 1, (1,1) -> -1))
      val vstacked = Matrix.sparse(Map((0,1) -> 1, (1,0) -> 1, (2,0) -> 1, (3,1) -> -1))
      val m1 = Matrix.vstack(sigmax, sigmaz)
      val m2 = sigmax.appendRows(sigmaz)
      (m1 - m2).normF must beCloseTo(0.0, 1e-6)
      (m1 - vstacked).normF must beCloseTo(0.0, 1e-6)

      val m3 = Matrix.hstack(sigmax, sigmaz)
      val m4 = sigmax.appendCols(sigmaz)
      val hstacked = Matrix.sparse(Map((0,1) -> 1, (1,0) -> 1, (0,2) -> 1, (1,3) -> -1))
      (m3 - m4).normF must beCloseTo(0.0, 1e-6)
      (m3 - hstacked).normF must beCloseTo(0.0, 1e-6)
    }
  }

  "A scalar" should {
    "implicitly be able to scale a Matrix" in {
      val C = 2 * A
      val res : Matrix = List(List(2.0, 4.0, 6.0), List(8.0, 10.0, 12.0))
      C must_== res
    }
  }

  "A pair of matrices" should {
    "multiply" in {
      val C = A * B
      val res : Matrix = List(List(30, 36, 42), List(66, 81, 96))
      C must_== res
    }

    "add up" in {
      val C = A + E
      val res : Matrix = List(List(6, 8, 10), List(5, 7, 9))
      C must_== res
    }

    "subtract" in {
      val prev : Matrix = List(List(1, 2.0, 3), List(4.0, 5, 6.0))
      val C = A - E
      val res : Matrix = List(List(-4, -4, -4), List(3, 3, 3))
      C must_== res
      A must_== prev
    }
  }

  "Row/Column operations" should {
    "be constructable from iterables" in {
      val listIn = List(0,0,1,0,1)
      val r1 = RowVector(listIn)
      r1.isSparse must beFalse
      val r2 = RowVector.sparse(Map(2 -> 1, 4 -> 1))
      r2.isSparse must beTrue
      (r1 - r2).norm2 must beCloseTo(0, 1e-6)
      listIn.zipWithIndex.foreach { vidx => r1.vector.get(vidx._2) must beCloseTo(vidx._1, 1e-5) }

      val c1 = ColVector(listIn)
      c1.isSparse must beFalse
      val c2 = ColVector.sparse(Map(2 -> 1, 4 -> 1))
      c2.isSparse must beTrue
      (c1 - c2).norm2 must beCloseTo(0, 1e-6)
      listIn.zipWithIndex.foreach { vidx => c1.vector.get(vidx._2) must beCloseTo(vidx._1, 1e-5) }

      (c1.t - r1).norm2 must beCloseTo(0, 1e-6)
    }
    "perform dot-product" in {
      val F : Matrix = List(List(1, 2.0, 3), List(4.0, 5, 6.0), List(23.0, 8, 9.0))
      (F.getRow(0) * F.getCol(0)) must beCloseTo(1.0 + 2.0 * 4.0 + 3.0 * 23.0, 1e-5)
    }
    "perform outer-product" in {
      val F : Matrix = List(List(1, 2.0), List(4.0, 5))
      val diff = (F.getCol(0) * F.getRow(0)) - List(List(1.0, 2.0), List(4.0, 8.0))
      diff.sum must beCloseTo(0.0, 1e-5)
      diff.norm1 must beCloseTo(0.0, 1e-5)
      diff.norm2 must beCloseTo(0.0, 1e-5)
      diff.normF must beCloseTo(0.0, 1e-5)
      diff.normInf must beCloseTo(0.0, 1e-5)
    }
    "support map" in {
      val F : Matrix = List(List(1, 2.0), List(4.0, 5))
      (F.getRow(0).map { _ * 2 } - (F.getRow(0) + F.getRow(0))).map { x => x*x }.sum must beCloseTo(0.0, 1e-6)
      (F.getCol(0).map { _ * 2 } - (F.getCol(0) + F.getCol(0))).map { x => x*x }.sum must beCloseTo(0.0, 1e-6)
    }
    "Have correct norms" in {
      val row = RowVector(List(1,2,-3,4,5,6))
      row.norm1 must beCloseTo(row.map { v => scala.math.abs(v) }.sum, 1e-6)
      row.norm2 must beCloseTo(row.map { v => v*v }.sum, 1e-6)
      row.normInf must beCloseTo(row.map { v => scala.math.abs(v) }.reduce { _ max _ }, 1e-6)
    }
    "Correctly add mapped vectors" in {
      val F : Matrix = List(List(1, 2.0), List(4.0, 5))
      ((F.getRow(0).map { _ * 2 } + F.getRow(0).map { _ * 3}) -
        F.getRow(0).map { _ * 5 }).sum must beCloseTo(0.0, 1e-6)
    }
    "support Map/Reduce" in {
      val F : Matrix = List(List(1, 2.0), List(4.0, 5))
      F.getRow(1).map { _ * 2 }.reduce { _ * _ } must beCloseTo(80.0, 1e-5)
      F.getCol(1).map { _ * 2 }.reduce { _ * _ } must beCloseTo(40.0, 1e-5)
    }
  }

  "A matrix vector equation F * h = g" should {
    "have an exact solution if F is square" in {
      val F : Matrix = List(List(1, 2.0, 3), List(4.0, 5, 6.0), List(23.0, 8, 9.0))
      val g : Matrix = List(List(1), List(1), List(1))
      val h = F \ g
      (F * h - g).sum must beCloseTo(0, 1e-3)
    }

    "use Cholesky if F is square and PSD" in {
      val F : Matrix = List(List(3, 1.0, 0.1), List(1.0, 3, 1), List(0.1, 1, 3))
      val g : Matrix = List(List(1), List(1), List(1))
      val h = F \ g
      (F * h - g).sum must beCloseTo(0, 1e-3)
    }

    "work if F is skinny and full rank" in {
      val F : Matrix = List(List(1, 2.0, 3), List(4.0, 5, 6.0), List(23.0, 8, 9.0), List(32.0, -8, 12.0))
      val g : Matrix = List(List(1), List(1), List(1), List(1))
      val h = F \ g
      h.rows must_== 3
    }

    "work if F is fat and full rank" in {
      val F : Matrix = List(List(1, 2.0, 3.0, 4.0), List(4.0, 5, 6.0, 7.0))
      val g : Matrix = List(List(1), List(1))
      val h = F \ g
      (F * h - g).sum must beCloseTo(0, 1e-3)
    }
  }
}
