package org.barbers.scalacolt

import cern.colt.matrix.DoubleMatrix1D
import cern.colt.function.{DoubleFunction, DoubleDoubleFunction, IntIntDoubleFunction}
import cern.colt.matrix.impl.{DenseDoubleMatrix2D, SparseDoubleMatrix2D}

import Implicits._

/** Immutable Lazy ColVector
 */
class ColVector(vect : => DoubleMatrix1D, val mapfn : Option[(Double) => Double] = None)
  extends Vector[ColVector] {
  // Due to vect being call by name, we need this. If we didn't have it,
  // vect would be evaluated EVERY time we touch vect
  private[scalacolt] lazy val getVect = vect

  def t : RowVector = new RowVector(getVect, mapfn)
  // TODO, we should make Matrix lazy and avoid realizing it if we are going to use
  // this in a product next
  def *(that : RowVector) : Matrix = {
    // TODO think of a better strategy to choose dense/sparse
    val out = new DenseDoubleMatrix2D(that.size, size)
    Matrix.algebra.multOuter(vector, that.vector, out)
    new Matrix(out)
  }
  override def mergeOp(op : DoubleDoubleFunction, other : ColVector) : ColVector = {
    val opfn = new MappedDoubleDouble(mapfn.getOrElse(identity _),
      other.mapfn.getOrElse(identity _), op)
    // This still doesn't evaluate it, but is lazy:
    new ColVector(getVect.copy.assign(other.getVect, opfn))
  }

  override def equals(that : Any) : Boolean = {
    (that != null) && that.isInstanceOf[ColVector] && {
      vector.equals(that.asInstanceOf[ColVector].vector)
    }
  }

  override def map(nextmapfn : (Double) => Double) = {
    new ColVector(getVect, mapfn.map { _.andThen(nextmapfn) }.orElse(Some(nextmapfn)))
  }

  override lazy val hashCode = vector.hashCode
}
