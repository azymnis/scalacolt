package org.barbers.scalacolt

import cern.colt.function.{DoubleFunction, DoubleDoubleFunction, IntIntDoubleFunction}
import cern.colt.matrix.DoubleMatrix2D
import cern.colt.matrix.impl.{DenseDoubleMatrix2D, SparseDoubleMatrix2D}
import cern.colt.matrix.linalg.{Algebra, CholeskyDecomposition, Property, SingularValueDecomposition}

import Numeric.Implicits._

object MatrixImplicits {
  implicit def iterIterToMatrix[T : Numeric](it : Iterable[Iterable[T]]) = {
    val ar = it.map{ _.map{el => el.toDouble}.toArray }.toArray
    new Matrix(new DenseDoubleMatrix2D(ar))
  }

  implicit def doubleToDoubleMultiplier(c : Double) = new DoubleMultiplier(c)

  implicit def doubleMatrix2DToMatrix(d : DoubleMatrix2D) = new Matrix(d)
}

object Matrix {
  private[scalacolt] def algebra = Algebra.DEFAULT
  private[scalacolt] def property = Property.DEFAULT

  // TODO: I think ".like" method can be used here...
  private[scalacolt] def newCMatrix(am : DoubleMatrix2D, bm : DoubleMatrix2D,
    rows : Int, cols : Int) = {
    (am, bm) match {
      case (a : SparseDoubleMatrix2D, b : SparseDoubleMatrix2D) => {
         new SparseDoubleMatrix2D(rows, cols)
      }
      case _ => new DenseDoubleMatrix2D(rows, cols)
    }
  }
}

/** Immutable Matrix.
 * TODO make mutable namespace for inplace modification which will likely
 * be needed for some algorithms at the edge of memory utilization
 */
class Matrix(val cMatrix : DoubleMatrix2D) {
  def rows = cMatrix.rows

  def columns = cMatrix.columns

  def toArray = cMatrix.toArray

  // These are expensive so make them lazy
  lazy val det = Matrix.algebra.det(cMatrix)

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

  lazy val isSquare = Matrix.property.isSquare(this.cMatrix)

  lazy val isSymmetric = Matrix.property.isSymmetric(this.cMatrix)

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

  def *(other : Matrix) : Matrix = {
    val out = Matrix.newCMatrix(this.cMatrix, other.cMatrix, this.rows, other.columns)
    this.cMatrix.zMult(other.cMatrix, out)
    new Matrix(out)
  }
  def *(col : ColVector) : ColVector = {
    new ColVector(Matrix.algebra.mult(cMatrix, col.vect))
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
  // If the matrix is square and symmetric, attempt Cholesky
  // if that fails use QR
  // else if the matrix is rectangular, use QR decomposition
  // else fall back to using SVD
  def \(other : Matrix) = if(isSquare && isSymmetric) {
    try {
      val cd = new CholeskyDecomposition(cMatrix)
      new Matrix(cd.solve(other.cMatrix))
    } catch {
      case(e : IllegalArgumentException) => new Matrix(Matrix.algebra.solve(this.cMatrix, other.cMatrix))
    }
  } else if(isRectangular) {
    new Matrix(Matrix.algebra.solve(this.cMatrix, other.cMatrix))
  } else {
    val (v, s, u) = this.t.svd
    val sinv = s.map{ x : Double => if(x != 0.0) 1 / x else 0.0 }
    // TODO: probably want to be careful where we put the parens here, right?
    v * sinv * (u.t) * other
  }

  def ^(power : Int) : Matrix =
    new Matrix(Matrix.algebra.pow(cMatrix, power))

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

  def getCol(col : Int) : ColVector = new ColVector(cMatrix.viewColumn(col))
  def getRow(row : Int) : RowVector = new RowVector(cMatrix.viewRow(row))

  override def hashCode = cMatrix.hashCode

  override def toString = cMatrix.toString


  // Since this matrix is immutable a view is fine:
  private[scalacolt] lazy val cMatrixT = this.cMatrix.viewDice
}

class DoubleMultiplier[T : Numeric](c : T) {
  def *(m : Matrix) = m * c
  def *(col : ColVector) = col * c
  def *(row : RowVector) = row * c
}

class MatrixAddition extends DoubleDoubleFunction {
  def apply(x : Double, y : Double) = x + y
}

class MatrixSubtraction extends DoubleDoubleFunction {
  def apply(x : Double, y : Double) = x - y
}
