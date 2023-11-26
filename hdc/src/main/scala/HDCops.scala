package io.f1r3fly.rhohdc.tinyrho

import hv.*
import be.adamv.macroloop.collection.*
import scala.compiletime.ops.int.{%, /}

// A very simple minded polymorphic trampoline for concrete versus syntactic representation
// Extremely disappointed with this implementation. Should rewrite soon.

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
  override def permDecode : Permutation = {
    val perm =
      LehmerCodeVariant.lehmerCodeToPermutation(
        LehmerCodeVariant.bitVectorToLehmerCode( hv )
      )
    //val sv : SizedVector[HyperVectorSize, Int] = SizedVector.wrap( perm )

    //Permutation.fromRaw( sv )
    perm
  }
  override def permEncode( p : Permutation ) : HVWrapperT[Q] = {
    val bitV = 
      LehmerCodeVariant.lehmerCodeToBitVector(
        LehmerCodeVariant.permutationToLehmerCode( p )
      )
    val sv : SizedVector[HyperVectorSize / 32, Int] = SizedVector.wrap( bitV.raw.data )

    HVWrapper( HyperVector.fromRaw( sv ) )
  }
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

trait HVExpr[Q] {
  def toHyperVector() : HyperVector
}
case class HVConst[Q]( vconst: HVWrapper[Q] ) extends HVExpr[Q] {
  override def toHyperVector() : HyperVector = {
    vconst.hv
  }
}
case class HVXor[Q](l: HVExpr[Q], r: HVExpr[Q])
    extends HVExpr[Q] {
  override def toHyperVector() : HyperVector = {
    l.toHyperVector() xor r.toHyperVector()
  }
}
case class HVPerm[Q](permEnc: HVExpr[Q], hvec: HVExpr[Q])
    extends HVExpr[Q] {
  override def toHyperVector() : HyperVector = {
    HVWrapper( permEnc.toHyperVector() ).permDecode( hvec.toHyperVector() )
  }
}
case class HVMaj[Q](summands: Array[HVExpr[Q]])
    extends HVExpr[Q] {
  override def toHyperVector() : HyperVector = {
    HVAlgebra.maj( summands.map( { (s) => { HVWrapper( s.toHyperVector() ) } } ) ).hv
  }
}

trait HVExprWrapperT[Q] extends HVExpr[Q] with HVWrapperT[Q] {
  def hvExpr : HVExpr[Q]
  override def hv = hvExpr.toHyperVector()
  override def toHyperVector() = hvExpr.toHyperVector()
  override def permDecode : Permutation = ???
  override def permEncode( p : Permutation ) = ???
}

case class HVExprWrapper[Q]( hvExpr : HVExpr[Q] ) extends HVExprWrapperT[Q] 

case object HVZeroBool extends HVExpr[Boolean] {
  override def toHyperVector() = HyperVector.zero
}
case object HVOneBool extends HVExpr[Boolean] {
  override def toHyperVector() = HyperVector.one
}
case object HVRandBool extends HVExpr[Boolean] {
  override def toHyperVector() = HyperVector.random
}

case object HV0 extends HVExprWrapperT[Boolean] {
  override def hvExpr = HVZeroBool
  override def toHyperVector() = { HyperVector.zero }
}
case object HV1 extends HVExprWrapperT[Boolean] {
  override def hvExpr = HVOneBool
  override def toHyperVector() = { HyperVector.one }
}
case object HVR extends HVExprWrapperT[Boolean] {
  override def hvExpr = HVRandBool
  override def toHyperVector() = { HyperVector.random }
}

trait HVTermAlgebraT extends HVT[HVExprWrapperT,Boolean] {  
  def zero() : HVExprWrapperT[Boolean] = HV0
  def rand(): HVExprWrapperT[Boolean] = HV1
  def xOr(
    v1: HVExprWrapperT[Boolean],
    v2: HVExprWrapperT[Boolean]
  ): HVExprWrapperT[Boolean] = HVExprWrapper[Boolean]( HVXor[Boolean]( v1.hvExpr, v2.hvExpr ) )
  def perm(
    v1: HVExprWrapperT[Boolean],
    v2: HVExprWrapperT[Boolean]
  ): HVExprWrapperT[Boolean] = HVExprWrapper( HVPerm[Boolean]( v1.hvExpr, v2.hvExpr ) )
  def maj(summands: Array[HVExprWrapperT[Boolean]]): HVExprWrapperT[Boolean] = {
    HVExprWrapper[Boolean]( HVMaj[Boolean]( summands.map(_.hvExpr) ) )
  }
}

object HVTermAlgebra extends HVTermAlgebraT {}
