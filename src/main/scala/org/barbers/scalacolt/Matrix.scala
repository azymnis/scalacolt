package org.barbers.scalacolt

import cern.colt.function.{DoubleFunction, DoubleDoubleFunction, IntIntDoubleFunction}
import cern.colt.matrix.DoubleMatrix2D
import cern.colt.matrix.impl.{DenseDoubleMatrix2D, SparseDoubleMatrix2D}
import cern.colt.matrix.linalg.{Algebra, CholeskyDecomposition, Property, SingularValueDecomposition}

import Numeric.Implicits._

// Implicit conversion from function1/2 to colt:
import Implicits.funcToColt
import Implicits.func2ToColt

object Matrix {

  def eye(n : Int) = {
    val cMat = new DenseDoubleMatrix2D(n, n)
    createEye(n, cMat)
  }

  def speye(n : Int) = {
    val cMat = new SparseDoubleMatrix2D(n, n)
    createEye(n, cMat)
  }

  def zeros(n : Int, m : Int) = new Matrix(new DenseDoubleMatrix2D(n, m))
  def sparse(n : Int, m : Int) = new Matrix(new SparseDoubleMatrix2D(n, m))

  private[scalacolt] def createEye(n : Int, cMat : DoubleMatrix2D) = {
    (0 until n).foreach { i => cMat.setQuick(i, i, 1.0) }
    new Matrix(cMat)
  }

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
class Matrix(mat : => DoubleMatrix2D, val mapfn : Option[(Double) => Double] = None) {
  // call-by-name is executed each time, we don't want to do that, so access here:
  private[scalacolt] lazy val getMat = mat

  lazy val cMatrix : DoubleMatrix2D = {
    mapfn.map { fn => getMat.copy.assign(fn) }
      .getOrElse(getMat)
  }

  def rows = getMat.rows

  def columns = getMat.columns

  def toArray = cMatrix.toArray

  // These are expensive so make them lazy
  lazy val det = Matrix.algebra.det(cMatrix)

  lazy val sum = {
    if(mapfn.isEmpty)
      // zSum may be more optimized if there is no mapfn
      getMat.zSum
    else
      reduce { _ + _ }
  }

  lazy val trace = Matrix.algebra.trace(cMatrix)

  lazy val rank = if(isRectangular) {
    Matrix.algebra.rank(cMatrix)
  } else {
    // .viewDice is a view of the transpose
    Matrix.algebra.rank(cMatrix.viewDice)
  }

  lazy val svd = {
    val out = new SingularValueDecomposition(cMatrix)
    (new Matrix(out.getU), new Matrix(out.getS), new Matrix(out.getV))
  }

  lazy val isRectangular = {
    try {
      Matrix.property.checkRectangular(getMat)
      true
    } catch {
      case(e : IllegalArgumentException) => false
    }
  }

  // square cannot be changed by mapping:
  lazy val isSquare = Matrix.property.isSquare(getMat)

  // After mapping, we may be symmetric, even if we weren't originally:
  lazy val isSymmetric = Matrix.property.isSymmetric(cMatrix)

  // Transpose
  lazy val t = new Matrix(getMat.viewDice, mapfn)

  def *[T : Numeric](c : T) = {
    val dc = implicitly[Numeric[T]].toDouble(c)
    map { _ * dc }
  }

  def *(other : Matrix) : Matrix = {
    lazy val newMat = {
      val out = Matrix.newCMatrix(this.cMatrix, other.cMatrix, this.rows, other.columns)
      this.cMatrix.zMult(other.cMatrix, out)
    }
    // Keep it lazy:
    new Matrix(newMat)
  }
  def *(col : ColVector) : ColVector = {
    new ColVector(Matrix.algebra.mult(cMatrix, col.vector))
  }

  def +(other : Matrix) = zipMap(other) { _ + _ }
  def -(other : Matrix) = zipMap(other) { _ - _ }

  // Same as matlab backslash
  // If the matrix is square and symmetric, attempt Cholesky
  // if that fails use QR
  // else if the matrix is rectangular, use QR decomposition
  // else fall back to using SVD
  def \(other : Matrix) = if(isSquare && isSymmetric) {
    try {
      val cd = new CholeskyDecomposition(cMatrix)
      // We need to force this now, else we won't see the exception:
      val soln = cd.solve(other.cMatrix)
      new Matrix(soln)
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
    val newMap = mapfn.map { _.andThen(fn) }.orElse(Some(fn))
    new Matrix(mat, newMap)
  }

  override def equals(that : Any) = {
    (that != null) && (that.isInstanceOf[Matrix]) &&
      (cMatrix == that.asInstanceOf[Matrix].cMatrix)
  }

  def getCol(col : Int) : ColVector = new ColVector(cMatrix.viewColumn(col))
  def getRow(row : Int) : RowVector = new RowVector(cMatrix.viewRow(row))

  def reduce(redfn : (Double,Double) => Double) : Double = {
    val thismapfn : DoubleFunction = mapfn.map { funcToColt _ }.getOrElse(IdentityFunc)
    getMat.aggregate(func2ToColt(redfn), thismapfn)
  }

  override lazy val hashCode = cMatrix.hashCode

  override def toString = cMatrix.toString
  // zip two matrices together, and for each pair (Double,Double) map to a Double
  def zipMap(other : Matrix)(fn : (Double,Double) => Double) = {
    val opfn = new MappedDoubleDouble(mapfn.getOrElse(identity _),
      other.mapfn.getOrElse(identity _), fn)
    new Matrix(getMat.copy.assign(other.getMat, opfn))
  }
}

class DoubleMultiplier[T : Numeric](c : T) {
  def *(m : Matrix) = m * c
  def *(col : ColVector) = col * c
  def *(row : RowVector) = row * c
}

