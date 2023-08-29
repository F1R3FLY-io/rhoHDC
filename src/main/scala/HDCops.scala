package io.f1r3fly.metta2rho.tinyrho

import breeze.linalg.{DenseVector, SparseVector}
import breeze.stats.distributions._
import breeze.stats.distributions.Rand.FixedSeed.randBasis

trait HVT[V[_],Q] {
  def zero() : V[Q]
  def rand(): V[Q]
  def xOr(v1: V[Q], v2: V[Q]): V[Q]
  def perm(v1: V[Q], v2: V[Q]): V[Q]
  def maj(summands: List[V[Q]]): V[Q]
}

object HVAlgebra extends HVT[SparseVector, Boolean] {
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

trait HVExpr[V]
case class HVXor[V](l: HVExpr[V], r: HVExpr[V])
    extends HVExpr[V]
case class HVPerm[V](perm: HVExpr[V], hvec: HVExpr[V])
    extends HVExpr[V]
case class HVMaj[V](summands: List[HVExpr[V]])
    extends HVExpr[V]

object HVTermAlgebra extends HVT[HVExpr,Boolean] {
  case object HV0 extends HVExpr[Boolean]
  case object HV1 extends HVExpr[Boolean]
  case object HVRand extends HVExpr[Boolean]
  def zero() : HVExpr[Boolean] = HV0
  def rand(): HVExpr[Boolean] = HV1
  def xOr(
    v1: HVExpr[Boolean],
    v2: HVExpr[Boolean]
  ): HVExpr[Boolean] = HVXor[Boolean]( v1, v2 )
  def perm(
    v1: HVExpr[Boolean],
    v2: HVExpr[Boolean]
  ): HVExpr[Boolean] = HVPerm[Boolean]( v1, v2 )
  def maj(summands: List[HVExpr[Boolean]]): HVExpr[Boolean] = {
    HVMaj[Boolean]( summands )
  }
}
