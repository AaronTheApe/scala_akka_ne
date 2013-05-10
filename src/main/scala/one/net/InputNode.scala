package one.net

import akka.actor.{ Actor, ActorRef, LoggingFSM}
import scala.concurrent.duration._
import NetPrtcl._

object InputNode {
  sealed trait StateName
  case object StartState extends StateName
  case object HasOutputs extends StateName
  case object HasInput extends StateName
  case object Ready extends StateName

  sealed trait StateData
  case object StartData extends StateData
  case class HasOutputsData(outputs: Set[ActorRef]) extends StateData
  case class HasInputData(input: ActorRef) extends StateData
  case class ReadyData(input: ActorRef, outputs: Set[ActorRef]) extends StateData
}

import InputNode._
class InputNode extends Actor with LoggingFSM[StateName, StateData] {
  startWith(StartState, StartData)
 
  when(StartState) {
    case Event(AddOutput(output), StartData) ⇒ {
      log.debug("Received AddOutput(" + output + ") from " + context.sender)
      goto(HasOutputs) using HasOutputsData(Set(output))
    }
    case Event(SetInput(input), StartData) =>
      log.debug("Received SetInput(" + input + ") from " + context.sender)
      goto(HasInput) using HasInputData(input)
  }

  when(HasOutputs) {
    case Event(SetInput(input), HasOutputsData(outputs)) => {
      log.debug("Received SetInput(" + input + ") from " + context.sender)
      goto(Ready) using ReadyData(input, outputs)
    }
  }

  when(HasInput) {
    case Event(AddOutput(output), HasInputData(input)) => {
      log.debug("Received AddOutput(" + output + ") from " + context.sender)
      goto(Ready) using ReadyData(input, Set(output))
    }
  }

  when(Ready) {
    case Event(AddOutput(newOutput), ReadyData(input, outputs)) => {
      log.debug("Received AddOutput(" + newOutput + ") from " + context.sender)
      stay using ReadyData(input, outputs + newOutput) 
    }
    case Event(Stimulation(value), ReadyData(input, outputs)) => {
      log.debug("Received Stimulation(" + value + ") from " + context.sender)
      outputs.map(o => o ! Stimulation(value))
      stay using ReadyData(input, outputs)
    }
  }
} 
