package one.task

import akka.actor.{Actor, ActorRef, LoggingFSM}
import TttTaskUtil._
import TttTaskPrtcl._
import TttTaskPrtcl.SquareContents._
import TttTaskPrtcl.TttStatus._
import one.net.NetPrtcl._

object HumanTttPlayer {
  sealed trait StateName
  case object Ready extends StateName

  sealed trait StateData
  case object ReadyData extends StateData

  val topBottom = "\n   |   |   \n"
  val verticalSpacer = "\n-----------\n"

  def renderSquare(square: Option[SquareContents]) = {
    square match {
      case Some(X) => "X"
      case Some(O) => "O"
      case None => " "
    }
  }

  def renderRow(board:Seq[Option[SquareContents]], rowNum:Int) = {
    val (i0, i1, i2) = (0 + rowNum*3, 1 + rowNum*3, 2 + rowNum*3)
    val (a, b, c) = (board(i0), board(i1), board(i2))
    " " + renderSquare(a) + " | " +  renderSquare(b)  + " | " + renderSquare(c) + " "
  }

  def renderBoard(board:Seq[Option[SquareContents]]):String = {
    topBottom +
    renderRow(board, 0) +
    verticalSpacer +
    renderRow(board, 1) +
    verticalSpacer +
    renderRow(board, 2) +
    topBottom
  }
}

import HumanTttPlayer._
class HumanTttPlayer extends Actor with LoggingFSM[StateName, StateData] {
  startWith(Ready, ReadyData)

  when(Ready) {
    case Event(TttTaskEnv(squares), ReadyData) => {
      println(renderBoard(squares))
      println("Make a move(number for an empty square from 0 to 8 followed by <enter>):")
      context.sender ! TttTaskAct(readLine.toInt)
      stay using ReadyData
    }
  }
}
