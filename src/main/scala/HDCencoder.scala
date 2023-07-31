package io.f1r3fly.metta2rho.tinyrho

import breeze.linalg.{DenseVector, SparseVector}
import scala.collection.mutable.HashMap

import HVAlgebra._
import HDCSymbolTable._

object VEncoder extends Shredder 
{
  override def getLabel(term: RTerm): ShredValue = {
    HVValue( getSymbol(term) )
  }

  def encode(vfqn: FQN[ShredValue], prm: SparseVector[Boolean])(salt: SparseVector[Boolean]): SparseVector[Boolean] =
    maj(
      vfqn.path.map(
        {
          ( shredV ) => {
            shredV match {
              case HVValue( hv ) => hv
              case _ => rand()
            }
          }
        }
      )
    )
  def encode(hmap: HashMap[FQN[ShredValue], ShredValue], prm: SparseVector[Boolean])(salt: SparseVector[Boolean]): SparseVector[Boolean] = {
    val encodedPairs =
      hmap.foldLeft(List[SparseVector[Boolean]]())(
        { (acc, kvPair) =>
          {
            val (k, v) = kvPair
            v match {
              case TermValue( rTrm ) => {
                acc ++ List[SparseVector[Boolean]](xOr(encode(k, prm)(salt), encode(rTrm, prm, zero())(salt)))
              }
              case _ => ???
            }            
          }
        }
      )
    maj(encodedPairs)
  }
  def encode(name: Name[RGrnd, RTerm], prm: SparseVector[Boolean], acc: SparseVector[Boolean])(
      salt: SparseVector[Boolean]
  ): SparseVector[Boolean] = {    
    name match {
      case RQ( t ) => {
        val (rcrdTagV, tagV, termFldV, tV) =
          (getSymbol(RString("RTerm")),
            perm(prm, getSymbol(name)),
            perm(prm, getSymbol(RString("term"))),
            encode(t, prm, acc)( salt ))
        maj(List[SparseVector[Boolean]](xOr(rcrdTagV, tagV), xOr(termFldV, tV)))
      }
      case _ => ???
    }
  }
  def encode(term: RTerm, prm: SparseVector[Boolean], acc: SparseVector[Boolean])(
      salt: SparseVector[Boolean]
  ): SparseVector[Boolean] = {
    val rcrdTagV = getSymbol(RString("RTerm"))
    term match {
      case RZ      => getSymbol(RZ)
      case RHV0    => getSymbol(RHV0)
      case RHV1    => getSymbol(RHV1)
      case RHVRand => getSymbol(RHVRand)
      case RF(c, v, k) => {
        val (tagV, cFldV, cV, vFldV, vV, kFldV, kV) =
          (
            perm(prm, getSymbol(term)),
            perm(prm, getSymbol(RString("chan"))),
            encode(c, prm, acc)( salt ),
            perm(prm, getSymbol(RString("param"))),
            encode(v, prm, acc)( salt ),
            perm(prm, getSymbol(RString("k"))),
            encode(k, prm, acc)( salt )
          )
        maj(List[SparseVector[Boolean]](xOr(rcrdTagV, tagV), xOr(cFldV, cV), xOr(vFldV, vV), xOr(kFldV, kV)))
      }
      case RS(c, msg) => {
        val (tagV, cFldV, cV, msgFldV, msgV) =
          (
            perm(prm, getSymbol(term)),
            perm(prm, getSymbol(RString("chan"))),
            encode(c, prm, acc)( salt ),
            perm(prm, getSymbol(RString("msg"))),
            encode(msg, prm, acc)( salt )
          )
        maj(List[SparseVector[Boolean]](xOr(rcrdTagV, tagV), xOr(cFldV, cV), xOr(msgFldV, msgV)))
      }
      case RP(l, r) => {
        val (tagV, lFldV, lV, rFldV, rV) =
          (
            perm(prm, getSymbol(term)),
            perm(prm, getSymbol(RString("left"))),
            encode(l, prm, acc)( salt ),
            perm(prm, getSymbol(RString("right"))),
            encode(r, prm, acc)( salt )
          )
        maj(List[SparseVector[Boolean]](xOr(rcrdTagV, tagV), xOr(lFldV, lV), xOr(rFldV, rV)))
      }
      case RE(t) => {
        val (tagV, nFldV, nV) =
          (perm(prm, getSymbol(term)), perm(prm, getSymbol(RString("name"))), encode(t, prm, acc)( salt ))
        xOr(nFldV, nV)
      }
      case RHVXor(l, r) => {
        val (tagV, lFldV, lV, rFldV, rV) =
          (
            perm(prm, getSymbol(term)),
            perm(prm, getSymbol(RString("left"))),
            encode(l, prm, acc)( salt ),
            perm(prm, getSymbol(RString("right"))),
            encode(r, prm, acc)( salt )
          )
        xOr(tagV, maj(List[SparseVector[Boolean]](xOr(rcrdTagV, tagV), xOr(lFldV, lV), xOr(rFldV, rV))))
      }
      case RHVPerm(permT, hvec) => {
        val (tagV, pFldV, pV, hvFldV, hvV) =
          (
            perm(prm, getSymbol(term)),
            perm(prm, getSymbol(RString("perm"))),
            encode(permT, prm, acc)( salt ),
            perm(prm, getSymbol(RString("hvec"))),
            encode(hvec, prm, acc)( salt )
          )
        maj(List[SparseVector[Boolean]](xOr(rcrdTagV, tagV), xOr(pFldV, pV), xOr(hvFldV, hvV)))
      }
      case RHVMaj(summands: List[RHVTerm]) => {
        // BUGBUG: should acc be factored in differently?
        val tagV = perm(prm, getSymbol(term))
        val summandVecs : List[SparseVector[Boolean]] =
          summands.zipWithIndex.foldLeft(List[SparseVector[Boolean]]())(
            { (facc, eIdx) =>
              {
                val (e, i) = eIdx
                val iLbl = getLabel(RInt(i))
                val lblV = 
                  iLbl match {
                    case HVValue( hv ) => hv
                    case _ => ???
                  }
                val (tagV, idxV, eV) =
                  (perm(prm, getSymbol(term)), perm(prm, lblV), encode(e, prm, acc)( salt ))
                facc ++ List[SparseVector[Boolean]](maj(List[SparseVector[Boolean]](tagV, xOr(idxV,eV))))
              }
            }
          )
        maj(List[SparseVector[Boolean]](xOr(rcrdTagV, tagV)) ++ summandVecs)
      }
      case RMult(l, r) => {
        val (tagV, lFldV, lV, rFldV, rV) =
          (
            perm(prm, getSymbol(term)),
            perm(prm, getSymbol(RString("left"))),
            encode(l, prm, acc)( salt ),
            perm(prm, getSymbol(RString("right"))),
            encode(r, prm, acc)( salt )
          )
        maj(List[SparseVector[Boolean]](xOr(rcrdTagV, tagV), xOr(lFldV, lV), xOr(rFldV, rV)))
      }
      case RDiv(l, r) => {
        val (tagV, lFldV, lV, rFldV, rV) =
          (
            perm(prm, getSymbol(term)),
            perm(prm, getSymbol(RString("left"))),
            encode(l, prm, acc)( salt ),
            perm(prm, getSymbol(RString("right"))),
            encode(r, prm, acc)( salt )
          )
        maj(List[SparseVector[Boolean]](xOr(rcrdTagV, tagV), xOr(lFldV, lV), xOr(rFldV, rV)))
      }
      case RAdd(l, r) => {
        val (tagV, lFldV, lV, rFldV, rV) =
          (
            perm(prm, getSymbol(term)),
            perm(prm, getSymbol(RString("left"))),
            encode(l, prm, acc)( salt ),
            perm(prm, getSymbol(RString("right"))),
            encode(r, prm, acc)( salt )
          )
        maj(List[SparseVector[Boolean]](xOr(rcrdTagV, tagV), xOr(lFldV, lV), xOr(rFldV, rV)))
      }
      case RMinus(l, r) => {
        val (tagV, lFldV, lV, rFldV, rV) =
          (
            perm(prm, getSymbol(term)),
            perm(prm, getSymbol(RString("left"))),
            encode(l, prm, acc)( salt ),
            perm(prm, getSymbol(RString("right"))),
            encode(r, prm, acc)( salt )
          )
        maj(List[SparseVector[Boolean]](xOr(rcrdTagV, tagV), xOr(lFldV, lV), xOr(rFldV, rV)))
      }
      case RNeg(elem) => {
        val (tagV, elemFldV, elemV) =
          (perm(prm, getSymbol(term)), perm(prm, getSymbol(RString("neg"))), encode(elem, prm, acc)( salt ))
        maj(List[SparseVector[Boolean]](xOr(rcrdTagV, tagV), xOr(elemFldV, elemV)))
      }
      case REq(l, r) => {
        val (tagV, lFldV, lV, rFldV, rV) =
          (
            perm(prm, getSymbol(term)),
            perm(prm, getSymbol(RString("left"))),
            encode(l, prm, acc)( salt ),
            perm(prm, getSymbol(RString("right"))),
            encode(r, prm, acc)( salt )
          )
        maj(List[SparseVector[Boolean]](xOr(rcrdTagV, tagV), xOr(lFldV, lV), xOr(rFldV, rV)))
      }
      case RLEq(l, r) => {
        val (tagV, lFldV, lV, rFldV, rV) =
          (
            perm(prm, getSymbol(term)),
            perm(prm, getSymbol(RString("left"))),
            encode(l, prm, acc)( salt ),
            perm(prm, getSymbol(RString("right"))),
            encode(r, prm, acc)( salt )
          )
        maj(List[SparseVector[Boolean]](xOr(rcrdTagV, tagV), xOr(lFldV, lV), xOr(rFldV, rV)))
      }
      case RGEq(l, r) => {
        val (tagV, lFldV, lV, rFldV, rV) =
          (
            perm(prm, getSymbol(term)),
            perm(prm, getSymbol(RString("left"))),
            encode(l, prm, acc)( salt ),
            perm(prm, getSymbol(RString("right"))),
            encode(r, prm, acc)( salt )
          )
        maj(List[SparseVector[Boolean]](xOr(rcrdTagV, tagV), xOr(lFldV, lV), xOr(rFldV, rV)))
      }
      case RLt(l, r) => {
        val (tagV, lFldV, lV, rFldV, rV) =
          (
            perm(prm, getSymbol(term)),
            perm(prm, getSymbol(RString("left"))),
            encode(l, prm, acc)( salt ),
            perm(prm, getSymbol(RString("right"))),
            encode(r, prm, acc)( salt )
          )
        maj(List[SparseVector[Boolean]](xOr(rcrdTagV, tagV), xOr(lFldV, lV), xOr(rFldV, rV)))
      }
      case RGt(l, r) => {
        val (tagV, lFldV, lV, rFldV, rV) =
          (
            perm(prm, getSymbol(term)),
            perm(prm, getSymbol(RString("left"))),
            encode(l, prm, acc)( salt ),
            perm(prm, getSymbol(RString("right"))),
            encode(r, prm, acc)( salt )
          )
        maj(List[SparseVector[Boolean]](xOr(rcrdTagV, tagV), xOr(lFldV, lV), xOr(rFldV, rV)))
      }
      case RAppend(l, r) => {
        val (tagV, lFldV, lV, rFldV, rV) =
          (
            perm(prm, getSymbol(term)),
            perm(prm, getSymbol(RString("left"))),
            encode(l, prm, acc)( salt ),
            perm(prm, getSymbol(RString("right"))),
            encode(r, prm, acc)( salt )
          )
        maj(List[SparseVector[Boolean]](xOr(rcrdTagV, tagV), xOr(lFldV, lV), xOr(rFldV, rV)))
      }
      case RIf(test, tbranch, fbranch) => {
        val (tagV, testFldV, testV, tbranchFldV, tbranchV, fbranchFldV, fbranchV) =
          (
            perm(prm, getSymbol(term)),
            perm(prm, getSymbol(RString("test"))),
            encode(test, prm, acc)( salt ),
            perm(prm, getSymbol(RString("tbranch"))),
            encode(tbranch, prm, acc)( salt ),
            perm(prm, getSymbol(RString("fbranch"))),
            encode(fbranch, prm, acc)( salt )
          )
        maj(
          List[SparseVector[Boolean]](
            xOr(rcrdTagV, tagV),
            xOr(testFldV, testV),
            xOr(tbranchFldV, tbranchV),
            xOr(fbranchFldV, fbranchV)
          )
        )
      }
    }
  }
  def encode(term: RTerm)(salt: SparseVector[Boolean]): SparseVector[Boolean] = {
    val rel = 
      shred(
        term, 
        FQN[ShredValue](List[ShredValue]()),
        new HashMap[FQN[ShredValue], ShredValue]()
      )
    encode(rel, zero())(salt)
  }
}
