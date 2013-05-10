package one.net

import akka.actor.{Actor, ActorRef, LoggingFSM, Props}
import NetPrtcl._

object NeuroCon {
  sealed trait StateName
  object ReadyState extends StateName
  object ProcState extends StateName

  sealed trait StateData
  case class ReadyData(net: ActorRef) extends StateData
  case class ProcData[TaskEnv](net: ActorRef, task: ActorRef, env: TaskEnv) extends StateData
}

import NeuroCon._
class NeuroCon[TaskEnv, TaskAct](sensorArray: (TaskEnv) => StimVec, topTpl: TopTpl, actuatorArray: (TaskEnv, RespVec) => TaskAct) extends Actor with LoggingFSM[StateName, StateData] {

  startWith(ReadyState, ReadyData(context.actorOf(Props(new Network(topTpl)))))

  when(ReadyState) {
    case Event(env:TaskEnv, ReadyData(net)) =>
      net ! sensorArray(env)
      goto(ProcState) using ProcData[TaskEnv](net, context.sender, env)
  } 

  when(ProcState) {
    case Event(respVec:RespVec, procData:ProcData[TaskEnv]) =>
      procData.task ! actuatorArray(procData.env, respVec)
      goto(ReadyState) using ReadyData(procData.net)
  }
}
