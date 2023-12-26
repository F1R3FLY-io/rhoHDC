package io.f1r3fly.rhohdc.tinyrho

import hv.*
import be.adamv.macroloop.collection.*
import scala.compiletime.ops.int.{%, /}

// A very simple minded polymorphic trampoline for concrete versus syntactic representation
// Extremely disappointed with this implementation. Should rewrite soon.

trait HVRT [V[_],Q] {
  def rand(): V[Q]
}

trait HVT[V[_],Q,P] extends HVRT[V,Q] {
  def zero() : V[Q]  
  def xOr(v1: V[Q], v2: V[Q]): V[Q]
  def perm(v1: V[Q], v2: V[Q]): V[Q]
  def permute(p: P, v: V[Q]): V[Q]
  def maj(summands: Array[V[Q]]): V[Q]
}

trait HVWrapperT[Q] {
  def hv : HyperVector
  def permDecode[P <: Permutation] : P = ???
  def permEncode[P <: Permutation]( p : P ) : HVWrapperT[Q] = ???
}

case class HVWrapper[Q]( hv : HyperVector ) extends HVWrapperT[Q] {
  override def permDecode[P <: Permutation] : P = {
    val perm =
      LehmerCodeVariant.lehmerCodeToPermutation(
        LehmerCodeVariant.bitVectorToLehmerCode( hv )
      )

    // we should pass perm to some implicit constructor pattern to make a P
    // take a look at some of the design patterns in Cats
    perm.asInstanceOf[P]
  }
  override def permEncode[P <: Permutation]( p : P ) : HVWrapperT[Q] = {
    val perm : Permutation = p
    val bitV = 
      LehmerCodeVariant.lehmerCodeToBitVector(
        LehmerCodeVariant.permutationToLehmerCode( perm )
      )
    val sv : SizedVector[HyperVectorSize / 32, Int] = SizedVector.wrap( bitV.raw.data )

    HVWrapper( HyperVector.fromRaw( sv ) )
  }
}

trait HVPermWrapperT { def p : Permutation }

case class HVPermWrapper( override val p : Permutation ) extends HVPermWrapperT

trait HVAlgebraT extends HVT[HVWrapperT,Boolean,Permutation] {
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
        val p : Permutation = hvw1.permDecode
        val hv : HyperVector =  hvw2.hv
        HVWrapper[Boolean]( p( hv ) )
      }
      case _ => ???
    }
  }
  override def permute(pw: Permutation, v: HVWrapperT[Boolean]): HVWrapperT[Boolean] = {
    v match {
      case HVWrapper[Boolean]( hv ) => {
        HVWrapper[Boolean]( pw( hv ) )
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
trait HVPermExpr {
  def toPermutation() : Permutation
}
case class HVConst[Q]( vconst: HVWrapper[Q] ) extends HVExpr[Q] {
  override def toHyperVector() : HyperVector = {
    vconst.hv
  }
}
case class PermConst( vconst: HVPermWrapper ) extends HVPermExpr {
  override def toPermutation() : Permutation = {
    vconst.p
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
    val p : Permutation = HVWrapper( permEnc.toHyperVector() ).permDecode
    val hv : HyperVector = hvec.toHyperVector()
    p( hv )
  }
}
case class HVPerm2[Q](perm: HVPermExpr, hvec: HVExpr[Q])
    extends HVExpr[Q] {
  override def toHyperVector() : HyperVector = {
    val p : Permutation = perm.toPermutation()
    val hv : HyperVector = hvec.toHyperVector()
    p( hv )
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
  //override def permDecode[P <: Permutation] : Permutation = ???
  //override def permEncode[P <: Permutation]( p : Permutation ) = ???
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

trait HVTermAlgebraT extends HVT[HVExprWrapperT,Boolean,HVPermWrapperT] {  
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
  def permute(
    pw: HVPermWrapperT,
    v: HVExprWrapperT[Boolean]
  ): HVExprWrapperT[Boolean] = HVExprWrapper( HVPerm2[Boolean]( PermConst( HVPermWrapper( pw.p ) ), v ) )
  def maj(summands: Array[HVExprWrapperT[Boolean]]): HVExprWrapperT[Boolean] = {
    HVExprWrapper[Boolean]( HVMaj[Boolean]( summands.map(_.hvExpr) ) )
  }
}

object HVTermAlgebra extends HVTermAlgebraT {}
