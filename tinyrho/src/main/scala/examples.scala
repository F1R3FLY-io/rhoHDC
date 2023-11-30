package io.f1r3fly.rhohdc.tinyrho

object TinyRhoExamples {
  val nRZ = RQ( RZ )
  val firstListening: io.f1r3fly.rhohdc.tinyrho.RF = RF(RQ(RZ),RQ(RZ),RZ)
  val firstSending: io.f1r3fly.rhohdc.tinyrho.RS = RS(RQ(RZ),RZ)
  val firstComm = RP( firstListening, firstSending )
}
