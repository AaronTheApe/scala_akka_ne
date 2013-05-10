package one.net

import akka.actor.{Actor, ActorRef, FSM}
import scala.concurrent.duration._
import NetPrtcl._

object Synapse {
  sealed trait StateName
  case object StartState extends StateName
  case object HaveInputState extends StateName
  case object HaveWeightState extends StateName
  case object HaveOutputState extends StateName
  case object HaveInputAndWeightState extends StateName
  case object HaveInputAndOutputState extends StateName
  case object HaveWeightAndOutputState extends StateName
  case object ReadyState extends StateName

  sealed trait StateData
  case object StartData extends StateData
  case class HaveInputData(input: ActorRef) extends StateData
  case class HaveWeightData(weight: Double) extends StateData
  case class HaveOutputData(output: ActorRef) extends StateData
  case class HaveInputAndWeightData(input: ActorRef, weight: Double) extends StateData
  case class HaveInputAndOutputData(input: ActorRef, output: ActorRef) extends StateData
  case class HaveWeightAndOutputData(weight: Double, output: ActorRef) extends StateData
  case class ReadyData(input: ActorRef, weight: Double, output: ActorRef) extends StateData
}

import Synapse._
class Synapse extends Actor with FSM[StateName, StateData] {
  startWith(StartState, StartData)

  when(StartState) {
    case(Event(SetInput(input), StartData)) =>
      goto(HaveInputState) using HaveInputData(input)
    case(Event(SetWeight(weight), StartData)) =>
      goto(HaveWeightState) using HaveWeightData(weight)
    case(Event(SetOutput(output), StartData)) =>
      goto(HaveOutputState) using HaveOutputData(output)
  }

  when(HaveInputState) {
    case(Event(SetWeight(weight), HaveInputData(input))) =>
      goto(HaveInputAndWeightState) using HaveInputAndWeightData(input, weight)
    case(Event(SetOutput(output), HaveInputData(input))) =>
      goto(HaveInputAndOutputState) using HaveInputAndOutputData(input, output)
  }

  when(HaveWeightState) {
    case(Event(SetInput(input), HaveWeightData(weight))) =>
      goto(HaveInputAndWeightState) using HaveInputAndWeightData(input, weight)
    case(Event(SetOutput(output), HaveWeightData(weight))) =>
      goto(HaveWeightAndOutputState) using HaveWeightAndOutputData(weight, output)
  }

  when(HaveOutputState) {
    case(Event(SetWeight(weight), HaveOutputData(output))) =>
      goto(HaveWeightAndOutputState) using HaveWeightAndOutputData(weight, output)
    case(Event(SetInput(input), HaveOutputData(output))) =>
      goto(HaveInputAndOutputState) using HaveInputAndOutputData(input, output)
  }

  when(HaveInputAndWeightState) {
    case(Event(SetOutput(output), HaveInputAndWeightData(input, weight))) =>
      goto(ReadyState) using ReadyData(input, weight, output)
  }

  when(HaveInputAndOutputState) {
    case(Event(SetWeight(weight), HaveInputAndOutputData(input, output))) =>
      goto(ReadyState) using ReadyData(input, weight, output)
  }

  when(HaveWeightAndOutputState) {
    case(Event(SetInput(input), HaveWeightAndOutputData(weight, output))) =>
      goto(ReadyState) using ReadyData(input, weight, output)
  }

  when(ReadyState) {
    case(Event(Stimulation(value), ReadyData(input, weight, output))) => {
      output ! Stimulation(weight * value)
      stay using ReadyData(input, weight, output)
    }
  }
}
