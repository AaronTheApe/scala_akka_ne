package one.task

import akka.actor.{Actor, ActorRef, LoggingFSM}
import TttTaskUtil._
import TttTaskPrtcl._
import TttTaskPrtcl.SquareContents._
import TttTaskPrtcl.TttStatus._
import one.net.NetPrtcl._

object TttTask {
  sealed trait StateName
  object PXToMove extends StateName
  object POToMove extends StateName
  object GameOver extends StateName

  case class StateData(env: TttTaskEnv)

  val initialEnv = TttTaskEnv((0 until 9).map(_ => None).toSeq)
}

import TttTask._
class TttTask(px: ActorRef, po: ActorRef, eval: ActorRef) extends Actor with LoggingFSM[StateName, StateData] {
  startWith(PXToMove, StateData(initialEnv))

  override def preStart(): Unit = {
    px ! initialEnv
  }

  when(PXToMove) {
    case Event(TttTaskAct(move), StateData(TttTaskEnv(squares))) => {
      val newTttTaskEnv = TttTaskEnv(squares.updated(move, Some(X)))
      status(newTttTaskEnv) match {
        case XWon => {
          eval ! TttResult(XWon)
          goto(GameOver)
        }
        case Tie => {
          eval ! TttResult(Tie)
          goto(GameOver)
        }
        case NotDone => {
          po ! newTttTaskEnv
          goto(POToMove) using StateData(newTttTaskEnv)
        }
        case OWon => {
          throw new ImpossibleResultException("O won via X's move")
        }
      }
    }
  }

  when(POToMove) {
    case Event(TttTaskAct(move), StateData(TttTaskEnv(squares))) => {
      val newTttTaskEnv = TttTaskEnv(squares.updated(move, Some(O)))
      status(newTttTaskEnv) match {
        case XWon => {
          throw new ImpossibleResultException("X won via O's move")
        }
        case Tie => {
          throw new ImpossibleResultException("O tied via O's move")
        }
        case NotDone => {
          px ! newTttTaskEnv
          goto(PXToMove) using StateData(newTttTaskEnv)
        }
        case OWon => {
          eval ! TttResult(OWon)
          goto(GameOver)
        }
      }
    }
  }

  when(GameOver) {
    case Event(_, someState) => stay using someState
  }
}
