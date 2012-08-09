package org.barbers.scalacolt

import cern.colt.matrix.DoubleMatrix1D
import cern.colt.function.{DoubleFunction, DoubleDoubleFunction, IntIntDoubleFunction}
import cern.colt.matrix.impl.{DenseDoubleMatrix2D, SparseDoubleMatrix2D}

import Implicits._

/** Immutable Lazy RowVector
 */
class RowVector(vect : => DoubleMatrix1D, val mapfn : Option[(Double) => Double] = None) {
  // Due to vect being call by name, we need this. If we didn't have it,
  // vect would be evaluated EVERY time we touch vect
  private[scalacolt] lazy val getVect = vect
  lazy val vector = {
    mapfn.map { fn => getVect.copy.assign(fn) }
      .getOrElse(getVect)
  }

  lazy val sum = {
    if(mapfn.isEmpty)
      getVect.zSum
    else
      reduce { _ + _ }
  }

  def t : ColVector = new ColVector(getVect, mapfn)
  def *(that : Matrix) : RowVector = {
    // v * A = (A^t * v^t)^t, but for 1D vectors there is no tranpose
    new RowVector(Matrix.algebra.mult(that.cMatrixT, vector))
  }
  // This forces evaluation since Double can't be lazy
  def *(that : ColVector) : Double = Matrix.algebra.mult(vector, that.vector)
  def *[N : Numeric](that : N) = {
    val dN = implicitly[Numeric[N]].toDouble(that)
    map { _ * dN }
  }
  def +(other : RowVector) = {
    val addfn = (mapfn, other.mapfn) match {
      case (None, None) => MatrixAddition
      case (None, Some(rightmap)) => {
        new DoubleDoubleFunction {
          override def apply(l : Double, r : Double) = l + rightmap(r)
        }
      }
      case (Some(leftmap), None) => {
        new DoubleDoubleFunction {
          override def apply(l : Double, r : Double) = leftmap(l) + r
        }
      }
      case (Some(leftmap), Some(rightmap)) => {
        new DoubleDoubleFunction {
          override def apply(l : Double, r : Double) = leftmap(l) + rightmap(r)
        }
      }
    }
    // This still doesn't evaluate it, but is lazy:
    new RowVector(getVect.copy.assign(other.getVect, addfn))
  }
  def -(other : RowVector) = {
    val subfn = (mapfn, other.mapfn) match {
      case (None, None) => MatrixSubtraction
      case (None, Some(rightmap)) => {
        new DoubleDoubleFunction {
          override def apply(l : Double, r : Double) = l - rightmap(r)
        }
      }
      case (Some(leftmap), None) => {
        new DoubleDoubleFunction {
          override def apply(l : Double, r : Double) = leftmap(l) - r
        }
      }
      case (Some(leftmap), Some(rightmap)) => {
        new DoubleDoubleFunction {
          override def apply(l : Double, r : Double) = leftmap(l) - rightmap(r)
        }
      }
    }
    // This still doesn't evaluate it, but is lazy:
    new RowVector(getVect.copy.assign(other.getVect, subfn))
  }

  // This forces evaluation
  def apply(idx : Int) : Double = mapfn.map { fn => fn(getVect.get(idx)) }.getOrElse { getVect.get(idx) }
  def size = getVect.size
  override def toString = getVect.toString
  override def equals(that : Any) : Boolean = {
    (that != null) && that.isInstanceOf[RowVector] && {
      vector.equals(that.asInstanceOf[RowVector].vector)
    }
  }
  def map(nextmapfn : (Double) => Double) = {
    new RowVector(getVect, mapfn.map { _.andThen(nextmapfn) }.orElse(Some(nextmapfn)))
  }

  def reduce(reduce : (Double,Double) => Double) : Double = {
    val thismapfn : DoubleFunction = mapfn.map { funcToColt _ }.getOrElse(IdentityFunc)
    getVect.aggregate(reduce, thismapfn)
  }

  override lazy val hashCode = vector.hashCode
}
