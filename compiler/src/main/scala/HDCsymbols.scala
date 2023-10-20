package io.f1r3fly.metta2rho.tinyrho

import breeze.linalg.{DenseVector, SparseVector}
import scala.collection.mutable.HashMap

//import HVAlgebra._

trait HVSymbolTableT[V[_],Q] extends HVRT[V,Q] {
  def symbolMap: HashMap[RTerm, V[Q]]
  def getSymbol(name: Name[RGrnd, RTerm]): V[Q] = {
    name match {
      case _ : RQ => {
        symbolMap.get(RString("RQ")) match {
          case None => {
            val symbol = rand()
            symbolMap += (RString("RQ") -> symbol)
            symbol
          }
          case Some(symbol) => symbol
        }
      }
      case _ => ???
    }
  }
  def getSymbol(term: RTerm): V[Q] = {
    term match {
      case RZ => {
        symbolMap.get(RZ) match {
          case None => {
            val symbol = rand()
            symbolMap += (RZ -> symbol)
            symbol
          }
          case Some(symbol) => symbol
        }
      }
      case RHV0 => {
        symbolMap.get(RHV0) match {
          case None => {
            val symbol = rand()
            symbolMap += (RHV0 -> symbol)
            symbol
          }
          case Some(symbol) => symbol
        }
      }
      case RHV1 => {
        symbolMap.get(RHV1) match {
          case None => {
            val symbol = rand()
            symbolMap += (RHV1 -> symbol)
            symbol
          }
          case Some(symbol) => symbol
        }

      }
      case RHVRand => {
        symbolMap.get(RHVRand) match {
          case None => {
            val symbol = rand()
            symbolMap += (RHVRand -> symbol)
            symbol
          }
          case Some(symbol) => symbol
        }
      }
      case _: RHVXor => {
        symbolMap.get(RString("RHVXor")) match {
          case None => {
            val symbol = rand()
            symbolMap += (RString("RHVXor") -> symbol)
            symbol
          }
          case Some(symbol) => symbol
        }
      }
      case _: RHVPerm => {
        symbolMap.get(RString("RHVPerm")) match {
          case None => {
            val symbol = rand()
            symbolMap += (RString("RHVPerm") -> symbol)
            symbol
          }
          case Some(symbol) => symbol
        }
      }
      case _: RHVMaj => {
        symbolMap.get(RString("RHVMaj")) match {
          case None => {
            val symbol = rand()
            symbolMap += (RString("RHVMaj") -> symbol)
            symbol
          }
          case Some(symbol) => symbol
        }
      }
      case _: RF => {
        symbolMap.get(RString("RF")) match {
          case None => {
            val symbol = rand()
            symbolMap += (RString("RF") -> symbol)
            symbol
          }
          case Some(symbol) => symbol
        }
      }
      case _: RS => {
        symbolMap.get(RString("RS")) match {
          case None => {
            val symbol = rand()
            symbolMap += (RString("RS") -> symbol)
            symbol
          }
          case Some(symbol) => symbol
        }
      }
      case _: RP => {
        symbolMap.get(RString("RP")) match {
          case None => {
            val symbol = rand()
            symbolMap += (RString("RP") -> symbol)
            symbol
          }
          case Some(symbol) => symbol
        }
      }
      case _: RE => {
        symbolMap.get(RString("RE")) match {
          case None => {
            val symbol = rand()
            symbolMap += (RString("RE") -> symbol)
            symbol
          }
          case Some(symbol) => symbol
        }
      }
      case _: RMult => {
        symbolMap.get(RString("RMult")) match {
          case None => {
            val symbol = rand()
            symbolMap += (RString("RMult") -> symbol)
            symbol
          }
          case Some(symbol) => symbol
        }
      }
      case _: RDiv => {
        symbolMap.get(RString("RDiv")) match {
          case None => {
            val symbol = rand()
            symbolMap += (RString("RDiv") -> symbol)
            symbol
          }
          case Some(symbol) => symbol
        }
      }
      case _: RAdd => {
        symbolMap.get(RString("RAdd")) match {
          case None => {
            val symbol = rand()
            symbolMap += (RString("RAdd") -> symbol)
            symbol
          }
          case Some(symbol) => symbol
        }
      }
      case _: RMinus => {
        symbolMap.get(RString("RMinus")) match {
          case None => {
            val symbol = rand()
            symbolMap += (RString("RMinus") -> symbol)
            symbol
          }
          case Some(symbol) => symbol
        }
      }
      case _: RNeg => {
        symbolMap.get(RString("RNeg")) match {
          case None => {
            val symbol = rand()
            symbolMap += (RString("RNeg") -> symbol)
            symbol
          }
          case Some(symbol) => symbol
        }
      }
      case _: REq => {
        symbolMap.get(RString("REq")) match {
          case None => {
            val symbol = rand()
            symbolMap += (RString("REq") -> symbol)
            symbol
          }
          case Some(symbol) => symbol
        }
      }
      case _: RLEq => {
        symbolMap.get(RString("RLEq")) match {
          case None => {
            val symbol = rand()
            symbolMap += (RString("RLEq") -> symbol)
            symbol
          }
          case Some(symbol) => symbol
        }
      }
      case _: RGEq => {
        symbolMap.get(RString("RGEq")) match {
          case None => {
            val symbol = rand()
            symbolMap += (RString("RGEq") -> symbol)
            symbol
          }
          case Some(symbol) => symbol
        }
      }
      case _: RLt => {
        symbolMap.get(RString("RLt")) match {
          case None => {
            val symbol = rand()
            symbolMap += (RString("RLt") -> symbol)
            symbol
          }
          case Some(symbol) => symbol
        }
      }
      case _: RGt => {
        symbolMap.get(RString("RGt")) match {
          case None => {
            val symbol = rand()
            symbolMap += (RString("RGt") -> symbol)
            symbol
          }
          case Some(symbol) => symbol
        }
      }
      case _: RAppend => {
        symbolMap.get(RString("RAppend")) match {
          case None => {
            val symbol = rand()
            symbolMap += (RString("RAppend") -> symbol)
            symbol
          }
          case Some(symbol) => symbol
        }
      }
      case _: RIf => {
        symbolMap.get(RString("RIf")) match {
          case None => {
            val symbol = rand()
            symbolMap += (RString("RIf") -> symbol)
            symbol
          }
          case Some(symbol) => symbol
        }
      }
      case _: RHyperVector => {
        symbolMap.get(RString("RHyperVector")) match {
          case None => {
            val symbol = rand()
            symbolMap += (RString("RHyperVector") -> symbol)
            symbol
          }
          case Some(symbol) => symbol
        }
      }
      case _: RList => {
        symbolMap.get(RString("RList")) match {
          case None => {
            val symbol = rand()
            symbolMap += (RString("RList") -> symbol)
            symbol
          }
          case Some(symbol) => symbol
        }
      }
      case _: RMap => {
        symbolMap.get(RString("RMap")) match {
          case None => {
            val symbol = rand()
            symbolMap += (RString("RMap") -> symbol)
            symbol
          }
          case Some(symbol) => symbol
        }
      }
      case _: RBool => {
        symbolMap.get(RString("RBool")) match {
          case None => {
            val symbol = rand()
            symbolMap += (RString("RBool") -> symbol)
            symbol
          }
          case Some(symbol) => symbol
        }
      }
      case _: RInt => {
        symbolMap.get(RString("RInt")) match {
          case None => {
            val symbol = rand()
            symbolMap += (RString("RInt") -> symbol)
            symbol
          }
          case Some(symbol) => symbol
        }
      }
      case RString("chan") => {
        symbolMap.get(RString("chan")) match {
          case None => {
            val symbol = rand()
            symbolMap += (RString("chan") -> symbol)
            symbol
          }
          case Some(symbol) => symbol
        }
      }
      case RString("param") => {
        symbolMap.get(RString("param")) match {
          case None => {
            val symbol = rand()
            symbolMap += (RString("param") -> symbol)
            symbol
          }
          case Some(symbol) => symbol
        }
      }
      case RString("k") => {
        symbolMap.get(RString("k")) match {
          case None => {
            val symbol = rand()
            symbolMap += (RString("k") -> symbol)
            symbol
          }
          case Some(symbol) => symbol
        }
      }
      case RString("msg") => {
        symbolMap.get(RString("msg")) match {
          case None => {
            val symbol = rand()
            symbolMap += (RString("msg") -> symbol)
            symbol
          }
          case Some(symbol) => symbol
        }
      }
      case RString("left") => {
        symbolMap.get(RString("left")) match {
          case None => {
            val symbol = rand()
            symbolMap += (RString("left") -> symbol)
            symbol
          }
          case Some(symbol) => symbol
        }
      }
      case RString("right") => {
        symbolMap.get(RString("right")) match {
          case None => {
            val symbol = rand()
            symbolMap += (RString("right") -> symbol)
            symbol
          }
          case Some(symbol) => symbol
        }
      }
      case RString("term") => {
        symbolMap.get(RString("term")) match {
          case None => {
            val symbol = rand()
            symbolMap += (RString("term") -> symbol)
            symbol
          }
          case Some(symbol) => symbol
        }
      }
      case RString("test") => {
        symbolMap.get(RString("test")) match {
          case None => {
            val symbol = rand()
            symbolMap += (RString("test") -> symbol)
            symbol
          }
          case Some(symbol) => symbol
        }
      }
      case RString("tbranch") => {
        symbolMap.get(RString("tbranch")) match {
          case None => {
            val symbol = rand()
            symbolMap += (RString("tbranch") -> symbol)
            symbol
          }
          case Some(symbol) => symbol
        }
      }
      case RString("fbranch") => {
        symbolMap.get(RString("fbranch")) match {
          case None => {
            val symbol = rand()
            symbolMap += (RString("fbranch") -> symbol)
            symbol
          }
          case Some(symbol) => symbol
        }
      }
      case _: RString => {
        symbolMap.get(RString("RString")) match {
          case None => {
            val symbol = rand()
            symbolMap += (RString("RString") -> symbol)
            symbol
          }
          case Some(symbol) => symbol
        }
      }
    }
  }
}

trait HVSymbolTable extends HVSymbolTableT[SparseVector,Boolean] 

object HDCSymbolTable extends HVSymbolTable with HVAlgebraT {
  override val symbolMap: HashMap[RTerm, SparseVector[Boolean]] = new HashMap[RTerm, SparseVector[Boolean]]()
}
