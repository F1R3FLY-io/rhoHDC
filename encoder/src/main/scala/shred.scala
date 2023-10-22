package io.f1r3fly.rhohdc.tinyrho

import breeze.linalg.{DenseVector, SparseVector}
import scala.collection.mutable.HashMap

trait ShredPath[L] { def add(l: L): ShredPath[L] }
case class FQN[L](path: List[L]) extends ShredPath[L] {
  override def add(l: L): ShredPath[L] =
    FQN[L](path ++ List[L](l))
  def `++`(l: L): FQN[L] =
    FQN[L](path ++ List[L](l))
}

object VFQN extends FQN[SparseVector[Boolean]](List[SparseVector[Boolean]]())

trait ShredValue

case class TermValue( rTerm : RTerm ) extends ShredValue
case class NameValue( rName : Name[RGrnd, RTerm] ) extends ShredValue
case class HVValue( hv : SparseVector[Boolean] ) extends ShredValue
case class HVTValue( hv : HVExpr[Boolean] ) extends ShredValue

trait Shredder {
  def getLabel(term: RTerm): ShredValue
  def shred(
      chan: Name[RGrnd, RTerm],
      fqn: FQN[ShredValue],
      acc: HashMap[FQN[ShredValue], ShredValue]
  ): HashMap[FQN[ShredValue], ShredValue] =
    acc + (fqn -> NameValue( chan ))
  def shred(term: RTerm, fqn: FQN[ShredValue], acc: HashMap[FQN[ShredValue], ShredValue]): HashMap[FQN[ShredValue], ShredValue] = {
    val nPath = fqn ++ getLabel(term)
    acc.get( fqn ) match {
      case Some( _ ) => acc
      case None => {
        term match {
          case RZ      => acc + (fqn -> getLabel(RZ))
          case RHV0    => acc + (fqn -> getLabel(RHV0))
          case RHV1    => acc + (fqn -> getLabel(RHV1))
          case RHVRand => acc + (fqn -> getLabel(RHVRand))
          case RF(c, v, k) => {
            shred(
              c,
              nPath ++ getLabel(RString("chan")),
              shred(
                v,
                nPath ++ getLabel(RString("param")),
                shred(
                  k,
                  nPath ++ getLabel(RString("k")),
                  acc
                )
              )
            )
          }
          case RS(c, msg) => {
            shred(
              c,
              nPath ++ getLabel(RString("chan")),
              shred(
                msg,
                nPath ++ getLabel(RString("msg")),
                acc
              )
            )
          }
          case RP(l, r) => {
            shred(
              l,
              nPath ++ getLabel(RString("left")),
              shred(
                r,
                nPath ++ getLabel(RString("right")),
                acc
              )
            )
          }
          case RE(t) => {
            shred(
              t,
              nPath ++ getLabel(RString("name")),
              acc
            )
          }
          case RHVXor(l, r) => {
            shred(
              l,
              nPath ++ getLabel(RString("left")),
              shred(
                r,
                nPath ++ getLabel(RString("right")),
                acc
              )
            )
          }
          case RHVPerm(perm, hvec) => {
            shred(
              perm,
              nPath ++ getLabel(RString("perm")),
              shred(
                hvec,
                nPath ++ getLabel(RString("hvec")),
                acc
              )
            )
          }
          case RHVMaj(summands: List[RHVTerm]) => {
            summands.zipWithIndex.foldLeft(acc)(
              { (facc, eIdx) =>
                {
                  val (e, i) = eIdx
                  shred(
                    e,
                    nPath ++ getLabel(RInt(i)),
                    facc
                  )
                }
              }
            )
          }
          case RMult(l, r) => {
            shred(
              l,
              nPath ++ getLabel(RString("left")),
              shred(
                r,
                nPath ++ getLabel(RString("right")),
                acc
              )
            )
          }
          case RDiv(l, r) => {
            shred(
              l,
              nPath ++ getLabel(RString("left")),
              shred(
                r,
                nPath ++ getLabel(RString("right")),
                acc
              )
            )
          }
          case RAdd(l, r) => {
            shred(
              l,
              nPath ++ getLabel(RString("left")),
              shred(
                r,
                nPath ++ getLabel(RString("right")),
                acc
              )
            )
          }
          case RMinus(l, r) => {
            shred(
              l,
              nPath ++ getLabel(RString("left")),
              shred(
                r,
                nPath ++ getLabel(RString("right")),
                acc
              )
            )
          }
          case RNeg(elem) => {
            shred(
              elem,
              nPath ++ getLabel(RString("elem")),
              acc
            )
          }
          case REq(l, r: RRingExpr) => {
            shred(
              l,
              nPath ++ getLabel(RString("left")),
              shred(
                r,
                nPath ++ getLabel(RString("right")),
                acc
              )
            )
          }
          case RLEq(l, r) => {
            shred(
              l,
              nPath ++ getLabel(RString("left")),
              shred(
                r,
                nPath ++ getLabel(RString("right")),
                acc
              )
            )
          }
          case RGEq(l, r) => {
            shred(
              l,
              nPath ++ getLabel(RString("left")),
              shred(
                r,
                nPath ++ getLabel(RString("right")),
                acc
              )
            )
          }
          case RLt(l, r) => {
            shred(
              l,
              nPath ++ getLabel(RString("left")),
              shred(
                r,
                nPath ++ getLabel(RString("right")),
                acc
              )
            )
          }
          case RGt(l, r) => {
            shred(
              l,
              nPath ++ getLabel(RString("left")),
              shred(
                r,
                nPath ++ getLabel(RString("right")),
                acc
              )
            )
          }
          case RAppend(l, r) => {
            shred(
              l,
              nPath ++ getLabel(RString("left")),
              shred(
                r,
                nPath ++ getLabel(RString("right")),
                acc
              )
            )
          }
          case RIf(test, tbranch, fbranch) => {
            shred(
              test,
              nPath ++ getLabel(RString("test")),
              shred(
                tbranch,
                nPath ++ getLabel(RString("tbranch")),
                shred(
                  fbranch,
                  nPath ++ getLabel(RString("fbranch")),
                  acc
                )
              )
            )
          }
        }
      }
    }
  }
}
