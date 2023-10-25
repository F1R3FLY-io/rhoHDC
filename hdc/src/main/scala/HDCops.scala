package io.f1r3fly.rhohdc.tinyrho

//import breeze.linalg.{DenseVector, SparseVector}
import hv.*
//import breeze.stats.distributions._
//import breeze.stats.distributions.Rand.FixedSeed.randBasis

trait HVRT [V[_],Q] {
  def rand(): V[Q]
}

trait HVT[V[_],Q] extends HVRT[V,Q] {
  def zero() : V[Q]  
  def xOr(v1: V[Q], v2: V[Q]): V[Q]
  def perm(v1: V[Q], v2: V[Q]): V[Q]
  def maj(summands: Array[V[Q]]): V[Q]
}

trait HVWrapperT[Q] {
  def hv : HyperVector
  def permDecode : Permutation
  def permEncode( p : Permutation ) : HVWrapperT[Q]
}

case class HVWrapper[Q]( hv : HyperVector ) extends HVWrapperT[Q] {
  override def permDecode : Permutation = ???
  override def permEncode( p : Permutation ) = ???
}

trait HVAlgebraT extends HVT[HVWrapperT,Boolean] {
  override def zero() : HVWrapperT[Boolean] = HVWrapper[Boolean]( HyperVector.zero )
  override def rand(): HVWrapperT[Boolean]  = HVWrapper[Boolean]( HyperVector.random )
  override def xOr(v1: HVWrapperT[Boolean], v2: HVWrapperT[Boolean]): HVWrapperT[Boolean]  = {
    ( v1, v2 ) match {
      case ( hvw1 : HVWrapper[Boolean], hvw2 : HVWrapper[Boolean] ) => {
        HVWrapper[Boolean]( hvw1.hv xor hvw2.hv )
      }
      case _ => ???
    }
  }
  override def perm(v1: HVWrapperT[Boolean], v2: HVWrapperT[Boolean]): HVWrapperT[Boolean] = {
    ( v1, v2 ) match {
      case ( hvw1 : HVWrapper[Boolean], hvw2 : HVWrapper[Boolean] ) => {
        HVWrapper[Boolean]( hvw1.permDecode( hvw2.hv ) )
      }
      case _ => ???
    }
  }
  override def maj(summands: Array[HVWrapperT[Boolean]]): HVWrapperT[Boolean] = {
    val hvMajSummands : Array[HyperVector] = summands.map( _.hv )
    //val hvArray : Array[HyperVector] = hvMajSummands.toArray
    //val hvTup : Tuple = Tuple.fromArray(hvArray)
    //HVWrapper[Boolean]( HyperVector.majority(hvTup) )
    // val hvSummand1 = hvMajSummands(0)
    // val hvSummand2 = hvMajSummands(1)
    // val hvMajRslt = HyperVector.majority(hvSummand1,hvSummand2)
    HVWrapper[Boolean]( HyperVector.random )
  }
}

object HVAlgebra extends HVAlgebraT {}

trait HVExpr[V]
case class HVXor[V](l: HVExpr[V], r: HVExpr[V])
    extends HVExpr[V]
case class HVPerm[V](perm: HVExpr[V], hvec: HVExpr[V])
    extends HVExpr[V]
case class HVMaj[V](summands: Array[HVExpr[V]])
    extends HVExpr[V]

trait HVTermAlgebraT extends HVT[HVExpr,Boolean] {
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
  def maj(summands: Array[HVExpr[Boolean]]): HVExpr[Boolean] = {
    HVMaj[Boolean]( summands )
  }
}

object HVTermAlgebra extends HVTermAlgebraT {}
