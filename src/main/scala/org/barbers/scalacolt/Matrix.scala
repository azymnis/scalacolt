package org.barbers.scalacolt

import cern.colt.function.{DoubleFunction, DoubleDoubleFunction, IntIntDoubleFunction}
import cern.colt.matrix.DoubleMatrix2D
import cern.colt.matrix.impl.{DenseDoubleMatrix2D, SparseDoubleMatrix2D}
import cern.colt.matrix.linalg.{Algebra, Property, SingularValueDecomposition}

import Numeric.Implicits._

object MatrixImplicits {
  implicit def iterIterToMatrix[T : Numeric](it : Iterable[Iterable[T]]) = {
    val ar = it.map{ _.map{el => el.toDouble}.toArray }.toArray
    new Matrix(new DenseDoubleMatrix2D(ar))
  }

  implicit def doubleToDoubleMultiplier(c : Double) = new DoubleMultiplier(c)
}

object Matrix {
  private[scalacolt] def algebra = Algebra.DEFAULT
  private[scalacolt] def property = Property.DEFAULT
}
class Matrix(val cMatrix : DoubleMatrix2D) {
  def rows = cMatrix.rows

  def columns = cMatrix.columns

  def toArray = cMatrix.toArray

  // These are expensive so make them lazy
  lazy val sum = cMatrix.zSum

  lazy val trace = Matrix.algebra.trace(cMatrix)

  lazy val rank = if(isRectangular) {
    Matrix.algebra.rank(cMatrix)
  } else {
    Matrix.algebra.rank(cMatrixT)
  }

  lazy val svd = {
    val out = new SingularValueDecomposition(cMatrix)
    (new Matrix(out.getU), new Matrix(out.getS), new Matrix(out.getV))
  }

  lazy val isRectangular = {
    try {
      Matrix.property.checkRectangular(this.cMatrix)
      true
    } catch {
      case(e : IllegalArgumentException) => false
    }
  }

  // Transpose
  def t = new Matrix(cMatrixT)

  def *[T : Numeric](c : T) = {
    val cDoub = c.toDouble
    val multFunc = new IntIntDoubleFunction {
      def apply(first : Int, second : Int, third : Double) = cDoub * third
    }
    val newMat = cMatrix.copy.forEachNonZero(multFunc)
    new Matrix(newMat)
  }

  def *(other : Matrix) = {
    val out = newCMatrix(other, this.rows, other.columns)
    this.cMatrix.zMult(other.cMatrix, out)
    new Matrix(out)
  }

  def +(other : Matrix) = {
    val out = this.cMatrix.copy.assign(other.cMatrix, new MatrixAddition)
    new Matrix(out)
  }

  def -(other : Matrix) = {
    val out = this.cMatrix.copy.assign(other.cMatrix, new MatrixSubtraction)
    new Matrix(out)
  }

  // Same as matlab backslash
  def \(other : Matrix) = if(isRectangular) {
    new Matrix(Matrix.algebra.solve(this.cMatrix, other.cMatrix))
  } else {
    val (v, s, u) = this.t.svd
    val sinv = s.map{ x : Double => if(x != 0.0) 1 / x else 0.0 }
    v * sinv * (u.t) * other
  }

  // Apply fn to each element
  def map(fn : Double => Double) = {
    val dFunc = new DoubleFunction {
      def apply(arg : Double) = fn(arg)
    }
    new Matrix(cMatrix.copy.assign(dFunc))
  }

  override def equals(that : Any) = {
    if(that.isInstanceOf[Matrix]) {
      this.cMatrix == that.asInstanceOf[Matrix].cMatrix
    } else {
      false
    }
  }

  override def hashCode = cMatrix.hashCode

  override def toString = cMatrix.toString

  private[scalacolt] def newCMatrix(other : Matrix, r : Int, c : Int) = {
    (this.cMatrix, other.cMatrix) match {
      case (a : SparseDoubleMatrix2D, b : SparseDoubleMatrix2D) => new SparseDoubleMatrix2D(r, c)
      case _ => new DenseDoubleMatrix2D(r, c)
    }
  }

  private[scalacolt] lazy val cMatrixT = Matrix.algebra.transpose(this.cMatrix)
}

class DoubleMultiplier[T : Numeric](c : T) {
  def *(m : Matrix) = m * c
}

class MatrixAddition extends DoubleDoubleFunction {
  def apply(x : Double, y : Double) = x + y
}

class MatrixSubtraction extends DoubleDoubleFunction {
  def apply(x : Double, y : Double) = x - y
}
