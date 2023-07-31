package io.f1r3fly.metta2rho.tinyrho

import breeze.linalg.{DenseVector, SparseVector}
import scala.collection.immutable.HashMap

trait RGrnd
trait Term[Z, N]
trait Name[Z, T]

trait HVGrnd
trait HVTerm[V]

trait RTerm extends Term[RGrnd, Name[RGrnd, RTerm]]

case object RZ                            extends RTerm with RGrnd
trait RFMonoidExpr                        extends RTerm with RGrnd
case class RString(s: String)             extends RFMonoidExpr
case class RHyperVector(v: SparseVector[Boolean])  extends RFMonoidExpr
case class RList(l: List[RTerm])          extends RFMonoidExpr
case class RMap(l: HashMap[RTerm, RTerm]) extends RFMonoidExpr

trait RRingExpr           extends RTerm with RGrnd
case class RBool(b: Boolean) extends RRingExpr
case class RInt(i: Int)   extends RRingExpr
trait RCompare            extends RTerm with RGrnd

trait RHVTerm                                    extends RTerm with HVTerm[RHyperVector] with RGrnd
case object RHV0                                 extends RHVTerm
case object RHV1                                 extends RHVTerm
case object RHVRand                              extends RHVTerm
case class RHVXor(l: RHVTerm, r: RHVTerm)        extends RHVTerm
case class RHVPerm(perm: RHVTerm, hvec: RHVTerm) extends RHVTerm
case class RHVMaj(summands: List[RHVTerm])       extends RHVTerm

// Core rho calculus
// Comprehension: for( param <- chan )cont
case class RF(chan: Name[RGrnd, RTerm], param: Name[RGrnd, RTerm], cont: RTerm) extends RTerm
// Transmission: chan!( msg )
case class RS(chan: Name[RGrnd, RTerm], msg: RTerm) extends RTerm
// Composition: left | right
case class RP(left: RTerm, right: RTerm) extends RTerm
// Evaluation: *name
case class RE(name: Name[RGrnd, RTerm]) extends RTerm

// Quotation: @term
case class RQ(term: RTerm) extends Name[RGrnd, RTerm]

// Arithmetic extensions
case class RMult(l: RRingExpr, r: RRingExpr)  extends RRingExpr
case class RDiv(l: RRingExpr, r: RRingExpr)   extends RRingExpr
case class RAdd(l: RRingExpr, r: RRingExpr)   extends RRingExpr
case class RMinus(l: RRingExpr, r: RRingExpr) extends RRingExpr
case class RNeg(elem: RRingExpr)              extends RRingExpr

case class REq(l: RRingExpr, r: RRingExpr)  extends RCompare
case class RLEq(l: RRingExpr, r: RRingExpr) extends RCompare
case class RGEq(l: RRingExpr, r: RRingExpr) extends RCompare
case class RLt(l: RRingExpr, r: RRingExpr)  extends RCompare
case class RGt(l: RRingExpr, r: RRingExpr)  extends RCompare

case class RAppend(l: RFMonoidExpr, r: RFMonoidExpr) extends RFMonoidExpr

case class RIf(test: RCompare, tbranch: RTerm, fbranch: RTerm) extends RTerm


