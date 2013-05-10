package one.task

import akka.actor.{Actor, ActorRef, LoggingFSM}
import TttTaskUtil._
import TttTaskPrtcl._
import TttTaskPrtcl.SquareContents._
import TttTaskPrtcl.TttStatus._
import one.net.NetPrtcl._

object RandTttPlayer {
  sealed trait StateName
  case object Ready extends StateName

  sealed trait StateData
  case object ReadyData extends StateData
}

import RandTttPlayer._
class RandTttPlayer extends Actor with LoggingFSM[StateName, StateData] {
  startWith(Ready, ReadyData)

  when(Ready) {
    case Event(TttTaskEnv(squares), ReadyData) => {
      context.sender ! TttTaskAct(random.shuffle(squares.zipWithIndex.filter(_._1 == None)).head._2)
      stay using ReadyData
    }
  }
}
