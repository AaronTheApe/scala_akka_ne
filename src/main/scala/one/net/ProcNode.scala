package one.net

import akka.actor.{ Actor, ActorRef, FSM}
import scala.concurrent.duration._
import NetPrtcl._

object ProcNode {
  sealed trait State
  case object StartState extends State
  case object HasOutputsState extends State
  case object HasInputsState extends State
  case object ReadyState extends State
  case object ProcState extends State

  sealed trait Data
  case object StartData extends Data
  case class HasOutputsData(outputs: Set[ActorRef]) extends Data
  case class HasInputsData(inputs: Set[ActorRef]) extends Data
  case class ReadyData(inputs: Set[ActorRef], outputs: Set[ActorRef]) extends Data
  case class ProcData(inputs: Set[ActorRef], outputs: Set[ActorRef], remInputs: Set[ActorRef], inputValues: Map[ActorRef, Double]) extends Data
}

import ProcNode._
class ProcNode(activationFunc: Seq[Double] => Double) extends Actor with FSM[State, Data] {

  startWith(StartState, StartData)
 
  when(StartState) {
    case Event(AddOutput(output), StartData) =>
      goto(HasOutputsState) using HasOutputsData(Set(output))
    case Event(AddInput(input), StartData) =>
      goto(HasInputsState) using HasInputsData(Set(input))
  }

  when(HasInputsState) {
    case Event(AddOutput(output), HasInputsData(inputs)) =>
      goto(ReadyState) using ReadyData(inputs, Set(output))
    case Event(AddInput(input), HasInputsData(inputs)) =>
      stay using HasInputsData(inputs + input)
  }

  when(HasOutputsState) {
    case Event(AddInput(input), HasOutputsData(outputs)) =>
      goto(ReadyState) using ReadyData(Set(input), outputs)
    case Event(AddOutput(output), HasOutputsData(outputs)) =>
      stay using HasOutputsData(outputs + output)
  }

  when(ReadyState) {
    case Event(AddInput(newInput), ReadyData(inputs, outputs)) =>
      stay using ReadyData(inputs + newInput, outputs)
    case Event(AddOutput(newOutput), ReadyData(inputs, outputs)) =>
      stay using ReadyData(inputs, outputs + newOutput)
    case Event(Stimulation(value), ReadyData(inputs, outputs)) => {
      val newRemInputs = inputs - context.sender
      if(newRemInputs.isEmpty) {
        val outputStim = Stimulation(activationFunc(Seq(value)))
        outputs.map(o => o ! outputStim)
        stay using ReadyData(inputs, outputs)
      }
      else {
        goto(ProcState) using ProcData(inputs, outputs, inputs - context.sender, Map(context.sender -> value))
      }
    }
  }

  when(ProcState) {
    case Event(Stimulation(value), ProcData(inputs, outputs, remInputs, inputValues)) => {
      val newRemInputs = remInputs - context.sender
      if(newRemInputs.isEmpty) {
        val outputStim = Stimulation(activationFunc((inputValues + (context.sender -> value)).values.toSeq))
        outputs.map(o => o ! outputStim)
        goto(ReadyState) using ReadyData(inputs, outputs)
      } else {
        stay using ProcData(inputs, outputs, remInputs - context.sender, inputValues + (context.sender -> value))
      }
    }
  }

  initialize
}

