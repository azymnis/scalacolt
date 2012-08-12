package org.barbers.scalacolt

import cern.colt.matrix.DoubleMatrix1D
import cern.colt.function.{DoubleFunction, DoubleDoubleFunction, IntIntDoubleFunction}
import cern.colt.matrix.impl.{DenseDoubleMatrix2D, SparseDoubleMatrix2D}

import Implicits._

/** Immutable Lazy RowVector
 */
class RowVector(vect : => DoubleMatrix1D, val mapfn : Option[(Double) => Double] = None)
 extends Vector[RowVector] {
  // Due to vect being call by name, we need this. If we didn't have it,
  // vect would be evaluated EVERY time we touch vect
  private[scalacolt] lazy val getVect = vect

  def t : ColVector = new ColVector(getVect, mapfn)
  def *(that : Matrix) : RowVector = {
    // v * A = (A^t * v^t)^t, but for 1D vectors there is no tranpose
    new RowVector(Matrix.algebra.mult(that.cMatrix.viewDice, vector))
  }
  // This forces evaluation since Double can't be lazy
  def *(that : ColVector) : Double = Matrix.algebra.mult(vector, that.vector)

  override def mergeOp(op : DoubleDoubleFunction, other : RowVector) : RowVector = {
    val opfn = new MappedDoubleDouble(mapfn.getOrElse(identity _),
      other.mapfn.getOrElse(identity _), op)
    // This still doesn't evaluate it, but is lazy:
    new RowVector(getVect.copy.assign(other.getVect, opfn))
  }

  override def equals(that : Any) : Boolean = {
    (that != null) && that.isInstanceOf[RowVector] && {
      vector.equals(that.asInstanceOf[RowVector].vector)
    }
  }
  override def map(nextmapfn : (Double) => Double) = {
    new RowVector(getVect, mapfn.map { _.andThen(nextmapfn) }.orElse(Some(nextmapfn)))
  }

  override lazy val hashCode = vector.hashCode
}
