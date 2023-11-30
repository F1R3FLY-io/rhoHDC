package io.f1r3fly.rhohdc.tinyrho

//import breeze.linalg.{DenseVector, SparseVector}
import hv.*
import scala.collection.mutable.HashMap

//import HVAlgebra._
//import HDCSymbolTable._

trait HVEncoder[V[_],Q] extends HVT[V,Q] {
  def encode(vfqn: FQN[ShredValue], prm: V[Q])(salt: V[Q]): V[Q]
  def encode(hmap: HashMap[FQN[ShredValue], ShredValue], prm: V[Q])(salt: V[Q]): V[Q]
  def encode(name: Name[RGrnd, RTerm], prm: V[Q], acc: V[Q])(
      salt: V[Q]
  ): V[Q]
  def encode(term: RTerm, prm: V[Q], acc: V[Q])(
      salt: V[Q]
  ): V[Q]
  def encode(term: RTerm)(salt: V[Q]): V[Q]
}

trait GHVEncoder[V[_],Q] extends HVEncoder[V,Q] with HVT[V,Q] with HVSymbolTableT[V,Q] {
  override def encode(name: Name[RGrnd, RTerm], oprm: V[Q], acc: V[Q])(
      salt: V[Q]
  ): V[Q] = {    
    name match {
      case RQ( t ) => {
        val (rcrdTagV, tagV, termFldV, tV) =
          (getSymbol(RString("RTerm")),
            perm(oprm, getSymbol(name)),
            perm(oprm, getSymbol(RString("term"))),
            encode(t, oprm, acc)( salt ))
        maj(Array[V[Q]](xOr(rcrdTagV, tagV), xOr(termFldV, tV)))
      }
      case _ => ???
    }
  }
}

object VEncoder extends HVEncoder[HVWrapperT,Boolean]
    with Shredder with HVAlgebraT
{
  import HDCSymbolTable._
  override def getLabel(term: RTerm): ShredValue = {
    HVValue( getSymbol(term) )
  }

  def encode(vfqn: FQN[ShredValue], prm: HVWrapperT[Boolean])(salt: HVWrapperT[Boolean]): HVWrapperT[Boolean] =
    maj(
      vfqn.path.toArray.map(
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
  def encode(hmap: HashMap[FQN[ShredValue], ShredValue], prm: HVWrapperT[Boolean])(salt: HVWrapperT[Boolean]): HVWrapperT[Boolean] = {
    val encodedPairs =
      hmap.foldLeft(Array[HVWrapperT[Boolean]]())(
        { (acc, kvPair) =>
          {
            val (k, v) = kvPair
            v match {
              case TermValue( rTrm ) => {
                acc ++ Array[HVWrapperT[Boolean]](xOr(encode(k, prm)(salt), encode(rTrm, prm, zero())(salt)))
              }
              case NameValue( rNm ) => {
                acc ++ Array[HVWrapperT[Boolean]](xOr(encode(k, prm)(salt), encode(rNm, prm, zero())(salt)))
              }
              case HVValue(hvWrap) => {
                val rcrdTagV = getSymbol(RString("RConst"))
                acc ++ Array[HVWrapperT[Boolean]](xOr(encode(k, prm)(salt), xOr(rcrdTagV,hvWrap)))
              }
            }            
          }
        }
      )
    maj(encodedPairs)
  }
  def encode(name: Name[RGrnd, RTerm], prm: HVWrapperT[Boolean], acc: HVWrapperT[Boolean])(
      salt: HVWrapperT[Boolean]
  ): HVWrapperT[Boolean] = {    
    name match {
      case RQ( t ) => {
        val (rcrdTagV, tagV, termFldV, tV) =
          (getSymbol(RString("RTerm")),
            perm(prm, getSymbol(name)),
            perm(prm, getSymbol(RString("term"))),
            encode(t, prm, acc)( salt ))
        maj(Array[HVWrapperT[Boolean]](xOr(rcrdTagV, tagV), xOr(termFldV, tV)))
      }
      case _ => ???
    }
  }
  def encode(term: RTerm, prm: HVWrapperT[Boolean], acc: HVWrapperT[Boolean])(
      salt: HVWrapperT[Boolean]
  ): HVWrapperT[Boolean] = {
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
        maj(Array[HVWrapperT[Boolean]](xOr(rcrdTagV, tagV), xOr(cFldV, cV), xOr(vFldV, vV), xOr(kFldV, kV)))
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
        maj(Array[HVWrapperT[Boolean]](xOr(rcrdTagV, tagV), xOr(cFldV, cV), xOr(msgFldV, msgV)))
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
        maj(Array[HVWrapperT[Boolean]](xOr(rcrdTagV, tagV), xOr(lFldV, lV), xOr(rFldV, rV)))
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
        xOr(tagV, maj(Array[HVWrapperT[Boolean]](xOr(rcrdTagV, tagV), xOr(lFldV, lV), xOr(rFldV, rV))))
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
        maj(Array[HVWrapperT[Boolean]](xOr(rcrdTagV, tagV), xOr(pFldV, pV), xOr(hvFldV, hvV)))
      }
      case RHVMaj(summands: Array[RHVTerm]) => {
        // BUGBUG: should acc be factored in differently?
        val tagV = perm(prm, getSymbol(term))
        val summandVecs : List[HVWrapperT[Boolean]] =
          summands.toList.zipWithIndex.foldLeft(List[HVWrapperT[Boolean]]())(
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
                facc ++ List[HVWrapperT[Boolean]](maj(Array[HVWrapperT[Boolean]](tagV, xOr(idxV,eV))))
              }
            }
          )
        maj(Array[HVWrapperT[Boolean]](xOr(rcrdTagV, tagV)) ++ summandVecs)
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
        maj(Array[HVWrapperT[Boolean]](xOr(rcrdTagV, tagV), xOr(lFldV, lV), xOr(rFldV, rV)))
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
        maj(Array[HVWrapperT[Boolean]](xOr(rcrdTagV, tagV), xOr(lFldV, lV), xOr(rFldV, rV)))
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
        maj(Array[HVWrapperT[Boolean]](xOr(rcrdTagV, tagV), xOr(lFldV, lV), xOr(rFldV, rV)))
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
        maj(Array[HVWrapperT[Boolean]](xOr(rcrdTagV, tagV), xOr(lFldV, lV), xOr(rFldV, rV)))
      }
      case RNeg(elem) => {
        val (tagV, elemFldV, elemV) =
          (perm(prm, getSymbol(term)), perm(prm, getSymbol(RString("neg"))), encode(elem, prm, acc)( salt ))
        maj(Array[HVWrapperT[Boolean]](xOr(rcrdTagV, tagV), xOr(elemFldV, elemV)))
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
        maj(Array[HVWrapperT[Boolean]](xOr(rcrdTagV, tagV), xOr(lFldV, lV), xOr(rFldV, rV)))
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
        maj(Array[HVWrapperT[Boolean]](xOr(rcrdTagV, tagV), xOr(lFldV, lV), xOr(rFldV, rV)))
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
        maj(Array[HVWrapperT[Boolean]](xOr(rcrdTagV, tagV), xOr(lFldV, lV), xOr(rFldV, rV)))
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
        maj(Array[HVWrapperT[Boolean]](xOr(rcrdTagV, tagV), xOr(lFldV, lV), xOr(rFldV, rV)))
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
        maj(Array[HVWrapperT[Boolean]](xOr(rcrdTagV, tagV), xOr(lFldV, lV), xOr(rFldV, rV)))
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
        maj(Array[HVWrapperT[Boolean]](xOr(rcrdTagV, tagV), xOr(lFldV, lV), xOr(rFldV, rV)))
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
          Array[HVWrapperT[Boolean]](
            xOr(rcrdTagV, tagV),
            xOr(testFldV, testV),
            xOr(tbranchFldV, tbranchV),
            xOr(fbranchFldV, fbranchV)
          )
        )
      }
    }
  }
  def encode(term: RTerm)(salt: HVWrapperT[Boolean]): HVWrapperT[Boolean] = {
    val rel = 
      shred(
        term, 
        FQN[ShredValue](List[ShredValue]()),
        new HashMap[FQN[ShredValue], ShredValue]()
      )
    encode(rel, zero())(salt)
  }
}

object VExprEncoder extends HVEncoder[HVExprWrapperT,Boolean]
    with Shredder with HVTermAlgebraT
{
  import HDCExprSymbolTable._
  override def getLabel(term: RTerm): ShredValue = {
    HVValue( getSymbol(term) )
  }

  def encode(vfqn: FQN[ShredValue], prm: HVExprWrapperT[Boolean])(salt: HVExprWrapperT[Boolean]): HVExprWrapperT[Boolean] =
    maj(
      vfqn.path.toArray.map(
        {
          ( shredV ) => {
            shredV match {
              case HVExprValue( hv ) => hv
              case _ => rand()
            }
          }
        }
      )
    )
  def encode(hmap: HashMap[FQN[ShredValue], ShredValue], prm: HVExprWrapperT[Boolean])(salt: HVExprWrapperT[Boolean]): HVExprWrapperT[Boolean] = {
    val encodedPairs =
      hmap.foldLeft(Array[HVExprWrapperT[Boolean]]())(
        { (acc, kvPair) =>
          {
            val (k, v) = kvPair
            v match {
              case TermValue( rTrm ) => {
                acc ++ Array[HVExprWrapperT[Boolean]](xOr(encode(k, prm)(salt), encode(rTrm, prm, zero())(salt)))
              }
              case NameValue( rNm ) => {
                acc ++ Array[HVExprWrapperT[Boolean]](xOr(encode(k, prm)(salt), encode(rNm, prm, zero())(salt)))
              }
              case HVExprValue(hvWrap) => {
                val rcrdTagV = getSymbol(RString("RConst"))
                acc ++ Array[HVExprWrapperT[Boolean]](xOr(encode(k, prm)(salt), xOr(rcrdTagV,hvWrap)))
              }
            }            
          }
        }
      )
    maj(encodedPairs)
  }
  def encode(name: Name[RGrnd, RTerm], prm: HVExprWrapperT[Boolean], acc: HVExprWrapperT[Boolean])(
      salt: HVExprWrapperT[Boolean]
  ): HVExprWrapperT[Boolean] = {    
    name match {
      case RQ( t ) => {
        val (rcrdTagV, tagV, termFldV, tV) =
          (getSymbol(RString("RTerm")),
            perm(prm, getSymbol(name)),
            perm(prm, getSymbol(RString("term"))),
            encode(t, prm, acc)( salt ))
        maj(Array[HVExprWrapperT[Boolean]](xOr(rcrdTagV, tagV), xOr(termFldV, tV)))
      }
      case _ => ???
    }
  }
  def encode(term: RTerm, prm: HVExprWrapperT[Boolean], acc: HVExprWrapperT[Boolean])(
      salt: HVExprWrapperT[Boolean]
  ): HVExprWrapperT[Boolean] = {
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
        maj(Array[HVExprWrapperT[Boolean]](xOr(rcrdTagV, tagV), xOr(cFldV, cV), xOr(vFldV, vV), xOr(kFldV, kV)))
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
        maj(Array[HVExprWrapperT[Boolean]](xOr(rcrdTagV, tagV), xOr(cFldV, cV), xOr(msgFldV, msgV)))
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
        maj(Array[HVExprWrapperT[Boolean]](xOr(rcrdTagV, tagV), xOr(lFldV, lV), xOr(rFldV, rV)))
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
        xOr(tagV, maj(Array[HVExprWrapperT[Boolean]](xOr(rcrdTagV, tagV), xOr(lFldV, lV), xOr(rFldV, rV))))
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
        maj(Array[HVExprWrapperT[Boolean]](xOr(rcrdTagV, tagV), xOr(pFldV, pV), xOr(hvFldV, hvV)))
      }
      case RHVMaj(summands: Array[RHVTerm]) => {
        // BUGBUG: should acc be factored in differently?
        val tagV = perm(prm, getSymbol(term))
        val summandVecs : List[HVExprWrapperT[Boolean]] =
          summands.toList.zipWithIndex.foldLeft(List[HVExprWrapperT[Boolean]]())(
            { (facc, eIdx) =>
              {
                val (e, i) = eIdx
                val iLbl = getLabel(RInt(i))
                val lblV = 
                  iLbl match {
                    case HVExprValue( hv ) => hv
                    case _ => ???
                  }
                val (tagV, idxV, eV) =
                  (perm(prm, getSymbol(term)), perm(prm, lblV), encode(e, prm, acc)( salt ))
                facc ++ List[HVExprWrapperT[Boolean]](maj(Array[HVExprWrapperT[Boolean]](tagV, xOr(idxV,eV))))
              }
            }
          )
        maj(Array[HVExprWrapperT[Boolean]](xOr(rcrdTagV, tagV)) ++ summandVecs)
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
        maj(Array[HVExprWrapperT[Boolean]](xOr(rcrdTagV, tagV), xOr(lFldV, lV), xOr(rFldV, rV)))
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
        maj(Array[HVExprWrapperT[Boolean]](xOr(rcrdTagV, tagV), xOr(lFldV, lV), xOr(rFldV, rV)))
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
        maj(Array[HVExprWrapperT[Boolean]](xOr(rcrdTagV, tagV), xOr(lFldV, lV), xOr(rFldV, rV)))
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
        maj(Array[HVExprWrapperT[Boolean]](xOr(rcrdTagV, tagV), xOr(lFldV, lV), xOr(rFldV, rV)))
      }
      case RNeg(elem) => {
        val (tagV, elemFldV, elemV) =
          (perm(prm, getSymbol(term)), perm(prm, getSymbol(RString("neg"))), encode(elem, prm, acc)( salt ))
        maj(Array[HVExprWrapperT[Boolean]](xOr(rcrdTagV, tagV), xOr(elemFldV, elemV)))
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
        maj(Array[HVExprWrapperT[Boolean]](xOr(rcrdTagV, tagV), xOr(lFldV, lV), xOr(rFldV, rV)))
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
        maj(Array[HVExprWrapperT[Boolean]](xOr(rcrdTagV, tagV), xOr(lFldV, lV), xOr(rFldV, rV)))
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
        maj(Array[HVExprWrapperT[Boolean]](xOr(rcrdTagV, tagV), xOr(lFldV, lV), xOr(rFldV, rV)))
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
        maj(Array[HVExprWrapperT[Boolean]](xOr(rcrdTagV, tagV), xOr(lFldV, lV), xOr(rFldV, rV)))
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
        maj(Array[HVExprWrapperT[Boolean]](xOr(rcrdTagV, tagV), xOr(lFldV, lV), xOr(rFldV, rV)))
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
        maj(Array[HVExprWrapperT[Boolean]](xOr(rcrdTagV, tagV), xOr(lFldV, lV), xOr(rFldV, rV)))
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
          Array[HVExprWrapperT[Boolean]](
            xOr(rcrdTagV, tagV),
            xOr(testFldV, testV),
            xOr(tbranchFldV, tbranchV),
            xOr(fbranchFldV, fbranchV)
          )
        )
      }
    }
  }
  def encode(term: RTerm)(salt: HVExprWrapperT[Boolean]): HVExprWrapperT[Boolean] = {
    val rel = 
      shred(
        term, 
        FQN[ShredValue](List[ShredValue]()),
        new HashMap[FQN[ShredValue], ShredValue]()
      )
    encode(rel, zero())(salt)
  }
}
