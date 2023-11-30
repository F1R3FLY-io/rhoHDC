package io.f1r3fly.rhohdc.tinyrho

object Argonaut {
  import org.json4s.JsonDSL.WithBigDecimal._
  import org.json4s._
  import org.json4s.native.Serialization
  import org.json4s.native.Serialization.{read, write}

  implicit val formats: Formats = Serialization.formats(NoTypeHints)

  def toJSON[Q]( cc : HVWrapperT[Q] ) : String = {
    cc match {
      case hvW : HVWrapper[Q] => write( hvW )
      case hvExprW : HVExprWrapper[Q] => write( hvExprW )
      case _ => ???
    }
  }

  def toJSON( cc : RTerm ) : String = {
    write( cc )
  }

  // def fromJSON[T]( ser : String ) : T = {
  //   read[T](ser)
  // }
}
