package io.f1r3fly.metta2rho.tinyrho

import breeze.linalg.{DenseVector, SparseVector}
import breeze.stats.distributions._
import breeze.stats.distributions.Rand.FixedSeed.randBasis

object HVAlgebra {
  val defaultDimension : Int = 4072
  val Bernie = Bernoulli.distribution(0.5)
  val Uni = Uniform(0,defaultDimension-1)
  def zero() : SparseVector[Boolean] = SparseVector.zeros[Boolean]( defaultDimension )
  def rand(): SparseVector[Boolean]  = {
    val size = Uni.sample().toInt
    val vVals = Bernie.samples.take(size)
    SparseVector[Boolean](vVals.toArray)
  }
  def xOr(v1: SparseVector[Boolean], v2: SparseVector[Boolean]): SparseVector[Boolean]  = ??? //v1.:^^( v2 )
  def perm(v1: SparseVector[Boolean], v2: SparseVector[Boolean]): SparseVector[Boolean] = ??? //v2.permute(v1.toDenseVector)
  def maj(summands: List[SparseVector[Boolean]]): SparseVector[Boolean] =
    ??? //summands.reduce((a, b) => a + b).mapValues(_ >= (vectors.size / 2))
}
