package org.barbers.scalacolt.filter

import org.barbers.scalacolt._

import scala.annotation.tailrec

import Implicits._
import Matrix.sparse

case class L1TF(
  y : ColVector, // Input
  lambda : Double, // Cost regularization parameter
  alpha : Double = 0.01, // backtracking linesearch parameter (0,0.5]
  beta : Double = 0.5, // backtracking linesearch parameter (0,1)
  mu : Double = 2, // IPM parameter: t update
  maxiter : Int   = 40, // IPM parameter: max iteration of IPM
  maxlsiter : Int = 20, // IPM parameter: max iteration of line search
  tol : Double = 1e-4) /* IPM parameter: tolerance */ {

  val n = y.rows
  val m = n - 2
  val d = Matrix.d2(n)
  val ddt = (d * d.t)
  val dy = (d*y)


  def run : (ColVector, Int) = {
    // Zero vector
    val z = ColVector.sparse(Map[Int,Double](), m)
    val ones = ColVector(Array.fill(m)(1.0))
    mainLoop(maxiter, 1e-10,
      // pretty silly to map zero, but following the matlab code
      (Double.PositiveInfinity,
        z, ones, ones, z.map { _ - lambda }, z.map { -1.0 * _ - lambda }))
  }

  @tailrec
  private def mainLoop(iter : Int, told : Double,
    inTup : (Double, ColVector, ColVector, ColVector, ColVector, ColVector))
    : (ColVector,Int) = {
    val (step, dualVar, dualdual1, dualdual2, dualLam1, dualLam2) = inTup
    if (iter == 0) {
      (y - (d.t * dualVar), 0) //No more iterations
    }
    else {
      //Check the stopping criterion:
      val dtz = (d.t * dualVar)
      val ddtz = (d * dtz)
      val w = dy - (dualdual1 - dualdual2)
      val pobj1 = 0.5 * ((w.t) * (ddt\w).getCol(0)) +
        lambda * ((dualdual1 + dualdual2).sum)
      val dtzNorm2 = dtz.norm2
      val pobj2 = 0.5 * dtzNorm2 + lambda * ((dy - ddtz).norm1)
      val pobj = pobj1 min pobj2
      val dobj = -0.5 * dtzNorm2 + (dy.t * dualVar)
      val gap = pobj - dobj
      if(gap <= tol) {
        //We stop
        (y - (d.t * dualVar), iter) //No more iterations
      }
      else {
        // Go one more step
        val t = if(step >= 0.2) {
          (1.2 * told) max (2 * m * mu / gap)
        }
        else {
          told
        }
        // Calculate Newton Step
        val rz = ddtz - w
        val sdelta = Matrix.hstack(dualdual1.zipMap(dualLam1) { _ / _ } +
          dualdual2.zipMap(dualLam2) { _ / _ }, sparse(m, m-1))
        val s = ddt - sdelta
        val r = ddtz * (-1) + dy + dualLam1.zipMap(dualLam2) { (f1,f2) =>
          (1.0/f1 - 1.0/f2)/t
        }
        val dz = (s \ r).getCol(0)
        def dmu(mu1 : ColVector, f1 : ColVector, factor : Double) : ColVector = {
          mu1 + dz.zipMap(mu1) { (x,y) => factor * x * y + 1/t }
            .zipMap(f1) { _ / _ }
        }
        val dmu1 = dmu(dualdual1, dualLam1, 1.0) * (-1)
        val dmu2 = dmu(dualdual2, dualLam2, -1.0) * (-1)
        def resCent(muv : ColVector, fv : ColVector) : ColVector = {
          muv.zipMap(fv) { (x,y) => - (x * y) - 1/t }
        }
        val resNorm = Matrix.hstack(rz,
          resCent(dualdual1, dualLam1),
          resCent(dualdual2, dualLam2)).norm2

        def stepOf(mu : ColVector, dmu : ColVector) = {
          dmu.map { v => if(v < 0) v else 0.0 }
            .nonZeros
            .map { idxV => -0.99 * mu(idxV._1) / idxV._2 }
        }
        val newStep = (List(1.0) ++ stepOf(dualdual1, dmu1) ++ stepOf(dualdual2, dmu2)).min

        @tailrec
        def lineSearch(lsiter : Int, lsstep : Double) :
          Option[(Double, ColVector, ColVector, ColVector, ColVector, ColVector)] = {
          if(lsiter <= 0) {
            None
          }
          else {
            val newz = dualVar + lsstep * dz
            val newmu1 = dualdual1 + lsstep * dmu1
            val newmu2 = dualdual2 + lsstep * dmu2
            val newf1 = newz.map { - lambda + _ }
            val newf2 = newz.map { - lambda - _ }
            // Update residual
            val newfmax = (newf1.reduce { _ max _ } max newf2.reduce { _ max _ })
            if( newfmax < 0) {
              val newResDual = ddt * newz - dy + newmu1 - newmu2
              val newResidual = Matrix.hstack(newResDual,
                resCent(newmu1, newf1),
                resCent(newmu2, newf2)).norm2
              if( newResidual <= (1 - alpha*lsstep) * resNorm ) {
                //Got it:
                Some(lsstep, newz, newmu1, newmu2, newf1, newf2)
              }
              else {
                // Try again
                lineSearch(lsiter - 1, beta * lsstep)
              }
            }
            else {
              // Try again
              lineSearch(lsiter - 1, beta * lsstep)
            }
          }
        }
        val next = lineSearch(maxlsiter, newStep)
        if( next.isEmpty ) {
          // We couldn't get closer
          (y - (d.t * dualVar), iter) //No more iterations
        }
        else {
          // recurse:
          mainLoop(iter - 1, t, next.get)
        }
      }
    }
  }
}
