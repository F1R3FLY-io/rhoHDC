package io.f1r3fly.rhohdc.tinyrho

import com.typesafe.scalalogging.Logger

import scala.jdk.CollectionConverters._
import java.io._

trait PrintOutConfigT
case object ConsoleAndLog extends PrintOutConfigT
case object ConsoleOnly   extends PrintOutConfigT
case object LogOnly       extends PrintOutConfigT

trait CompilerWorldStateT {
  def stdOutLogger: Logger
  def stdErrorLogger: Logger

  def printStdOut(s: String, cfg: PrintOutConfigT = ConsoleAndLog): Unit
  def printStdErr(s: String, cfg: PrintOutConfigT = ConsoleAndLog): Unit
}

object CompilerWorldState {
  val stdOutLogger                                                 = Logger("io.f1r3fly.rhohdc.stdout")
  val stdErrLogger                                                 = Logger("io.f1r3fly.rhohdc.stderr")

  def printStdOut(s: String, cfg: PrintOutConfigT = LogOnly): Unit =
    // BUGBUG: stupid implementation
    cfg match {
      case ConsoleAndLog => {
        scala.Console.println(s)
        stdOutLogger.debug(s)
      }
      case ConsoleOnly => {
        scala.Console.println(s)
      }
      case LogOnly => {
        stdOutLogger.debug(s)
      }
    }

  def printStdErr(s: String, cfg: PrintOutConfigT = LogOnly): Unit =
    // BUGBUG: stupid implementation
    cfg match {
      case ConsoleAndLog => {
        scala.Console.println(s)
        stdOutLogger.debug(s)
      }
      case ConsoleOnly => {
        scala.Console.println(s)
      }
      case LogOnly => {
        stdOutLogger.debug(s)
      }
    }
}
