package org.barbers.scalacolt

import cern.colt.matrix.DoubleMatrix1D
import cern.colt.function.{DoubleFunction, DoubleDoubleFunction, IntIntDoubleFunction}
import cern.colt.matrix.impl.{DenseDoubleMatrix2D, SparseDoubleMatrix2D}
import cern.colt.matrix.impl.{DenseDoubleMatrix1D, SparseDoubleMatrix1D}

import Implicits._

object ColVector {
  def apply[T : Numeric](items : Iterable[T]) = {
    val numT = implicitly[Numeric[T]]
    val dd1d = new DenseDoubleMatrix1D(items.map { it => numT.toDouble(it) }.toArray)
    new ColVector(dd1d)
  }
  // Make a sparse matrix, optionally give the max index, otherwise taken to be the max in the
  // iterable
  def sparse[T : Numeric](items : Iterable[(Int,T)], size : Int = -1) = {
    if(items.isEmpty) {
      new ColVector(new SparseDoubleMatrix1D(size max 0))
    }
    else {
      val itemMax = items.map { _._1 }.max
      assert(size == -1 || (size > itemMax), "size < itemMax: " + size + " < " + itemMax)
      val sd1d = new SparseDoubleMatrix1D(if(size < 0) (itemMax+1) else size)
      val numT = implicitly[Numeric[T]]
      items.foreach { idxV =>
        sd1d.setQuick(idxV._1, numT.toDouble(idxV._2))
      }
      new ColVector(sd1d)
    }
  }
}

/** Immutable Lazy ColVector
 */
class ColVector(vect : => DoubleMatrix1D, val mapfn : Option[(Double) => Double] = None)
  extends Vector[ColVector] {
  // Due to vect being call by name, we need this. If we didn't have it,
  // vect would be evaluated EVERY time we touch vect
  private[scalacolt] lazy val getVect = vect

  def columns = 1
  def rows = size

  def t : RowVector = new RowVector(getVect, mapfn)
  // TODO, we should make Matrix lazy and avoid realizing it if we are going to use
  // this in a product next
  def *(that : RowVector) : Matrix = {
    // TODO think of a better strategy to choose dense/sparse
    val out = new DenseDoubleMatrix2D(that.size, size)
    Matrix.algebra.multOuter(vector, that.vector, out)
    new Matrix(out)
  }

  override def zipMap(other : ColVector)(op : (Double,Double) => Double) : ColVector = {
    val opfn = new MappedDoubleDouble(mapfn.getOrElse(identity _),
      other.mapfn.getOrElse(identity _), op)
    // This still doesn't evaluate it, but is lazy:
    new ColVector(getVect.copy.assign(other.getVect, opfn))
  }

  override def equals(that : Any) : Boolean = {
    (that != null) && that.isInstanceOf[ColVector] && {
      Matrix.property.equals(vector, that.asInstanceOf[ColVector].vector)
    }
  }

  override def map(nextmapfn : (Double) => Double) = {
    new ColVector(getVect, mapfn.map { _.andThen(nextmapfn) }.orElse(Some(nextmapfn)))
  }

  override lazy val hashCode = vector.hashCode
}
