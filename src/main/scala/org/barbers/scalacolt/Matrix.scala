package org.barbers.scalacolt

import cern.colt.function.{DoubleFunction, DoubleDoubleFunction, IntIntDoubleFunction}
import cern.colt.matrix.DoubleMatrix2D
import cern.colt.matrix.impl.{DenseDoubleMatrix2D, SparseDoubleMatrix2D}
import cern.colt.matrix.linalg.{Algebra, CholeskyDecomposition, Property, SingularValueDecomposition, LUDecomposition, QRDecomposition}
import cern.colt.matrix.DoubleFactory2D.{dense => dense2D, sparse => sparse2D}

import Numeric.Implicits._

// Implicit conversion from function1/2 to colt:
import Implicits.funcToColt
import Implicits.func2ToColt

object Matrix {

  def apply[T : Numeric](it : Iterable[Iterable[T]]) = {
    val num = implicitly[Numeric[T]]
    val ar = it.map{ _.map{el => num.toDouble(el)}.toArray }.toArray
    new Matrix(new DenseDoubleMatrix2D(ar))
  }

  // Use this to convert a Map[(Int,Int),T] to a sparse matrix
  def sparse[T : Numeric](items : Iterable[((Int,Int),T)], rows : Int = -1, cols : Int = -1) = {
    val (rowMax, colMax) = items.foldLeft((-1,-1)) { (rowCol, riciv) =>
      (rowCol._1 max riciv._1._1, rowCol._2 max riciv._1._2)
    }
    assert(rows == -1 || rows > rowMax, "rows <= rowMax: " + rows + " <= " + rowMax)
    assert(cols == -1 || cols > colMax, "cols <= colMax: " + cols + " <= " + colMax)
    val sd2d = new SparseDoubleMatrix2D(if(rows < 0) (rowMax+1) else rows,
        if(cols < 0) (colMax+1) else cols)
    val numT = implicitly[Numeric[T]]
    items.foreach { rcV =>
      sd2d.setQuick(rcV._1._1, rcV._1._2, numT.toDouble(rcV._2))
    }
    new Matrix(sd2d)
  }

  /** Creates the ((n-1) x n) difference matrix:
   * [-1  1  0   ... ]
   * [ 0 -1  1 0   ..]
   * [ 0  0 -1 1 0 ..]
   * hit a column vector with this and x' = d1 x then x'[t] = x[t+1] - x[t]
   */
  def d1(n : Int) = {
    val zn = sparse(n-1,1)
    val thisI = speye(n-1)
    ((thisI * (-1)).appendCols(zn)) + (zn.appendCols(thisI))
  }
  /** Creates the ((n-2) x n) second difference matrix:
   * [ 1 -2  1  0        ... ]
   * [ 0  1 -2  1  0  0    ..]
   * [ 0  0  0  1 -2  1  0 ..]
   * hit a col vector with this, and get the second diff: x'[t] = x[t+2] - 2 x[t] + x[t]
   * Note d1(n-1) * d1(n) == d2(n)
   */
  def d2(n : Int) = {
    val zn = sparse(n-2,1)
    val thisI = speye(n-2)
    (thisI).appendCols(zn, zn) +
      zn.appendCols(thisI * (-2.0), zn) +
      zn.appendCols(zn, thisI)
  }

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

  def spdiag(vec : ColVector) = {
    val cMat = new SparseDoubleMatrix2D(vec.size, vec.size)
    (0 until vec.size).foreach { i => cMat.setQuick(i, i, vec(i)) }
    new Matrix(cMat)
  }

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
  // Stack matrices vertically, same as Matrix.appendRows
  // Named for the numpy function
  def vstack(allMats : Matrix*) : Matrix = {
    val factory = if(allMats.forall { _.isSparse }) sparse2D else dense2D
    new Matrix(factory.compose(allMats.map { mat => Array(mat.cMatrix) }.toArray))
  }
  def hstack(allMats : Matrix*) : Matrix = {
    val factory = if(allMats.forall { _.isSparse }) sparse2D else dense2D
    new Matrix(factory.compose(Array(allMats.map { _.cMatrix }.toArray)))
  }
}

/** Immutable Matrix.
 * TODO make mutable namespace for inplace modification which will likely
 * be needed for some algorithms at the edge of memory utilization
 */
class Matrix(mat : => DoubleMatrix2D, val mapfn : Option[(Double) => Double] = None)
  extends Function2[Int,Int,Double] {
  // call-by-name is executed each time, we don't want to do that, so access here:
  private[scalacolt] lazy val getMat = mat

  lazy val cMatrix : DoubleMatrix2D = {
    mapfn.map { fn => getMat.copy.assign(fn) }
      .getOrElse(getMat)
  }

  def rows = getMat.rows
  // Append the given matrices rows beneath this matrix, must have the same number of cols
  def appendRows(that : Matrix*) : Matrix = Matrix.vstack((List(this) ++ that) : _*)
  def appendCols(that : Matrix*) : Matrix = Matrix.hstack((List(this) ++ that) : _*)

  def columns = getMat.columns

  def toArray = cMatrix.toArray

  // These are expensive so make them lazy
  lazy val det = Matrix.algebra.det(cMatrix)

  // L1 Norm (max abs(sum(col)))
  lazy val norm1 = Matrix.algebra.norm1(cMatrix)
  // L2 Norm, does SVD, max singular value
  lazy val norm2 = Matrix.algebra.norm2(cMatrix)
  // Frobenius Norm = (M * M.t).trace
  lazy val normF = Matrix.algebra.normF(cMatrix)
  // This is the max abs(sum(row))
  lazy val normInf = Matrix.algebra.normInfinity(cMatrix)

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

  lazy val isSparse : Boolean = getMat.isInstanceOf[SparseDoubleMatrix2D]

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

  private[scalacolt] lazy val luDecomp = new LUDecomposition(cMatrix)
  private[scalacolt] lazy val qrDecomp = new QRDecomposition(cMatrix)
  // Cholesky appears really shitty in Colt right now, avoid it.
  private[scalacolt] lazy val cholesky = new CholeskyDecomposition(cMatrix)

  // Transpose
  lazy val t = new Matrix(getMat.viewDice, mapfn)


  override def apply(row : Int, col : Int) : Double = {
    mapfn.map { fn => fn(getMat.get(row, col)) }.getOrElse { getMat.get(row, col) }
  }

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
  // If the matrix is square and symmetric, use LU
  // else if the matrix is rectangular, use QR decomposition
  // else fall back to using SVD
  private lazy val backop : (Matrix) => Matrix = {
    if(isSquare) {
      { (other : Matrix) => new Matrix(luDecomp.solve(other.cMatrix)) }
    }
    else if(isRectangular) {
      { (other : Matrix) => new Matrix(qrDecomp.solve(other.cMatrix)) }
    } else {
      val (v, s, u) = this.t.svd
      val sinv = s.map{ x : Double => if(x != 0.0) 1 / x else 0.0 }
      val prod = (v * sinv * (u.t))
      val fn = { (other : Matrix) => prod * other }
      fn
    }
  }

  def \(other : Matrix) = backop(other)

  def ^(power : Int) : Matrix =
    new Matrix(Matrix.algebra.pow(cMatrix, power))

  // Apply fn to each element
  def map(fn : Double => Double) = {
    val newMap = mapfn.map { _.andThen(fn) }.orElse(Some(fn))
    new Matrix(getMat, newMap)
  }

  override def equals(that : Any) = {
    (that != null) && (that.isInstanceOf[Matrix]) &&
      Matrix.property.equals(cMatrix, that.asInstanceOf[Matrix].cMatrix)
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

