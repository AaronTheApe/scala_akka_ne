package one.net

import akka.actor.ActorRef

object NetPrtcl {
  case class TopTpl(numIn: Integer, numHid: Integer, numOut: Integer, synTpls: Seq[SynTpl], actFn: (Seq[Double]) => Double)
  case class SynTpl(input: Integer, weight: Double, output: Integer)
  case class StimVec(values: Seq[Double])
  case class RespVec(values: Seq[Double])
  case class AddOutput(output: ActorRef)
  case class AddInput(input: ActorRef)
  case class SetInput(input: ActorRef)
  case class SetOutput(output: ActorRef)
  case class Stimulation(value: Double)
  case class SetWeight(weight: Double)
}
