package one.net 

import akka.actor.{ Actor, ActorRef, LoggingFSM, Props} 
import scala.concurrent.duration._
import scala.collection.immutable.HashMap
import NetPrtcl._

object Network {
  sealed trait StateName
  case object StartState extends StateName
  case object Proc extends StateName

  sealed trait StateData
  case class StartData(nodes: Seq[ActorRef]) extends StateData
  case class ProcData(nodes: Seq[ActorRef], caller: ActorRef, remOutputs: Set[ActorRef], outputsToValues: Map[ActorRef, Double]) extends StateData
}

import Network._
class Network(topTpl: TopTpl) extends Actor with LoggingFSM[StateName, StateData] {
  startWith(StartState, transitionToStart)

  when(StartState) {
    case Event(StimVec(values), StartData(nodes)) => {
      log.debug("Received StimVec(" + values + ") from " + context.sender)
      values.zipWithIndex.map(vi => nodes(vi._2) ! Stimulation(vi._1))
      val remOutputs = (0 until topTpl.numOut).map(i => i + topTpl.numIn).map(o => nodes(o)).toSet
      goto(Proc) using ProcData(nodes, context.sender, remOutputs, Map())
    }
  }

  when(Proc) {
    case Event(Stimulation(value), ProcData(nodes, caller, remOutputs, outputsToValues)) =>
      log.debug("Received stim: " + value + " from " + context.sender)
      val newRemOutputs = remOutputs - context.sender
      val newOutputsToValues = outputsToValues + (context.sender -> value)
      if(newRemOutputs.isEmpty) {
        val response = RespVec((0 until topTpl.numOut).map(o => nodes(o + topTpl.numIn)).map(o => newOutputsToValues(o)))
        log.debug("Sending response: " + response)
        caller ! response
        goto(StartState) using StartData(nodes)
      }
      else {
        stay using ProcData(nodes, caller, newRemOutputs, newOutputsToValues)
      }
  }

  def transitionToStart:StartData = {
    val outputs = (0 until topTpl.numOut).map(_ => context.actorOf(Props(new ProcNode(topTpl.actFn)))).toSeq
    val hiddens = (0 until topTpl.numHid).map(_ => context.actorOf(Props(new ProcNode(topTpl.actFn)))).toSeq
    val inputs = (0 until topTpl.numIn).map(_ => context.actorOf(Props[InputNode])).toSeq

    (0 until topTpl.numOut).map(o => outputs(o) ! AddOutput(self))

    (0 until topTpl.numIn).map(i => inputs(i) ! SetInput(self)) 

    val nodes =  inputs ++ outputs ++ hiddens

    topTpl.synTpls.map(s => {
      val input = nodes(s.input)
      val output = nodes(s.output)
      val synapse = context.actorOf(Props[Synapse])
      output ! AddInput(synapse)
      synapse ! SetOutput(output)
      synapse ! SetWeight(s.weight)
      synapse ! SetInput(input)
      input ! AddOutput(synapse)
    })

    StartData(nodes)
  }

  initialize
}

