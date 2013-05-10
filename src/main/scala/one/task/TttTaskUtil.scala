package one.task

import TttTask._
import TttTaskPrtcl._
import TttTaskPrtcl.SquareContents._
import TttTaskPrtcl.TttStatus._
import one.net.NetPrtcl._
import akka.pattern.ask

object TttTaskUtil {
  val random = new scala.util.Random()

  def nextSquareContents : Stream[Option[SquareContents]] = 
    Stream.cons(random.nextInt(3) match {
      case 0 => Some(X)
      case 1 => Some(O)
      case 2 => None
    }, nextSquareContents)

  def status(env: TttTaskEnv):TttStatus = {
    env.squares match {
      case Seq(Some(X), Some(X), Some(X), _, _, _, _, _, _) => XWon
      case Seq(_, _, _, Some(X), Some(X), Some(X), _, _, _) => XWon
      case Seq(_, _, _, _, _, _, Some(X), Some(X), Some(X)) => XWon
      case Seq(Some(X), _, _, Some(X), _, _, Some(X), _, _) => XWon
      case Seq(_, Some(X), _, _, Some(X), _, _, Some(X), _) => XWon
      case Seq(_, _, Some(X), _, _, Some(X), _, _, Some(X)) => XWon
      case Seq(Some(X), _, _, _, Some(X), _, _, _, Some(X)) => XWon
      case Seq(_, _, Some(X), _, Some(X), _, Some(X), _, _) => XWon
      case Seq(Some(O), Some(O), Some(O), _, _, _, _, _, _) => OWon
      case Seq(_, _, _, Some(O), Some(O), Some(O), _, _, _) => OWon
      case Seq(_, _, _, _, _, _, Some(O), Some(O), Some(O)) => OWon
      case Seq(Some(O), _, _, Some(O), _, _, Some(O), _, _) => OWon
      case Seq(_, Some(O), _, _, Some(O), _, _, Some(O), _) => OWon
      case Seq(_, _, Some(O), _, _, Some(O), _, _, Some(O)) => OWon
      case Seq(Some(O), _, _, _, Some(O), _, _, _, Some(O)) => OWon
      case Seq(_, _, Some(O), _, Some(O), _, Some(O), _, _) => OWon
      case other => {
        if(other.contains(None)) {
          NotDone
        } else {
          Tie
        }
      }
    }
  }

  def tttSensorArray(env: TttTaskEnv): StimVec = {
    val unbiasedStim = StimVec(env.squares.map {
      case Some(c: SquareContents) => c match {
        case X => 1.0
        case O => -1.0
      }
      case None => 0.0
    })
    unbiasedStim.copy(values = unbiasedStim.values ++ Seq(1.0))
  }

  def tttActuatorArray(env: TttTaskEnv, rVec: RespVec): TttTaskAct = {
    TttTaskAct(rVec.values.zipWithIndex.sortBy(_._1).filter(x => env.squares(x._2) == None).head._2)
  }

  def nextTttTaskEnv():TttTaskEnv = {
    val squares = (0 until 9).map(_ => random.nextInt(3) match {
      case 0 => None
      case 1 => Some(SquareContents.X)
      case 2 => Some(SquareContents.O)
    }).toSeq
      TttTaskEnv(squares)
  }

  //TODO: Make the initial win match an exclusive or
  def nextXWin():(TttTaskEnv, TttTaskAct) = {
    val squareStream = nextSquareContents
    val boardIterator = squareStream.sliding(9)
    def nextXWinBoard():Seq[Option[SquareContents]] = {
      val board = boardIterator.next.force
      board match {
        case (Seq(Some(X), Some(X), Some(X), _, _, _, _, _, _)
           | Seq(_, _, _, Some(X), Some(X), Some(X), _, _, _)
           | Seq(_, _, _, _, _, _, Some(X), Some(X), Some(X))
           | Seq(Some(X), _, _, Some(X), _, _, Some(X), _, _)
           | Seq(_, Some(X), _, _, Some(X), _, _, Some(X), _)
           | Seq(_, _, Some(X), _, _, Some(X), _, _, Some(X))
           | Seq(Some(X), _, _, _, Some(X), _, _, _, Some(X))
           | Seq(_, _, Some(X), _, Some(X), _, Some(X), _, _)
        ) => {
          if(board.filter(_ == Some(O)).length + 1  == board.filter(_ == Some(X)).length) {
            board match {
              case (Seq(Some(O), Some(O), Some(O), _, _, _, _, _, _)
                 | Seq(_, _, _, Some(O), Some(O), Some(O), _, _, _)
                 | Seq(_, _, _, _, _, _, Some(O), Some(O), Some(O))
                 | Seq(Some(O), _, _, Some(O), _, _, Some(O), _, _)
                 | Seq(_, Some(O), _, _, Some(O), _, _, Some(O), _)
                 | Seq(_, _, Some(O), _, _, Some(O), _, _, Some(O))
                 | Seq(Some(O), _, _, _, Some(O), _, _, _, Some(O))
                 | Seq(_, _, Some(O), _, Some(O), _, Some(O), _, _)
              ) => nextXWinBoard
              case _ => board
            }
          }
          else
            nextXWinBoard
        }
        case _ => nextXWinBoard
      }
    }
    val winningBoard = nextXWinBoard()

    val winningMove = random.shuffle(winningBoard.zipWithIndex.filter(_._1 == Some(X))).head._2

    (TttTaskEnv(winningBoard.updated(winningMove, None)), TttTaskAct(winningMove))
  }


  def nextOWin():(TttTaskEnv, TttTaskAct) = {
    val squareStream = nextSquareContents
    val boardIterator = squareStream.sliding(9)
    def nextOWinBoard():Seq[Option[SquareContents]] = {
      val board = boardIterator.next.force
      board match {
        case (Seq(Some(O), Some(O), Some(O), _, _, _, _, _, _)
           | Seq(_, _, _, Some(O), Some(O), Some(O), _, _, _)
           | Seq(_, _, _, _, _, _, Some(O), Some(O), Some(O))
           | Seq(Some(O), _, _, Some(O), _, _, Some(O), _, _)
           | Seq(_, Some(O), _, _, Some(O), _, _, Some(O), _)
           | Seq(_, _, Some(O), _, _, Some(O), _, _, Some(O))
           | Seq(Some(O), _, _, _, Some(O), _, _, _, Some(O))
           | Seq(_, _, Some(O), _, Some(O), _, Some(O), _, _)
        ) => {
          if(board.filter(_ == Some(O)).length  == board.filter(_ == Some(X)).length) {
            board match {
              case (Seq(Some(X), Some(X), Some(X), _, _, _, _, _, _)
                 | Seq(_, _, _, Some(X), Some(X), Some(X), _, _, _)
                 | Seq(_, _, _, _, _, _, Some(X), Some(X), Some(X))
                 | Seq(Some(X), _, _, Some(X), _, _, Some(X), _, _)
                 | Seq(_, Some(X), _, _, Some(X), _, _, Some(X), _)
                 | Seq(_, _, Some(X), _, _, Some(X), _, _, Some(X))
                 | Seq(Some(X), _, _, _, Some(X), _, _, _, Some(X))
                 | Seq(_, _, Some(X), _, Some(X), _, Some(X), _, _)
              ) => nextOWinBoard
              case _ => board
            }
          }
          else
            nextOWinBoard
        }
        case _ => nextOWinBoard
      }
    }
    val winningBoard = nextOWinBoard()

    val winningMove = random.shuffle(winningBoard.zipWithIndex.filter(_._1 == Some(O))).head._2

    (TttTaskEnv(winningBoard.updated(winningMove, None)), TttTaskAct(winningMove))
  }

  def nextXTie():(TttTaskEnv, TttTaskAct) = {
    val squareStream = nextSquareContents
    val boardIterator = squareStream.sliding(9)
    def nextXTieBoard():Seq[Option[SquareContents]] = {
      val board = boardIterator.next.force
      board match {
        case (Seq(Some(O), Some(O), Some(O), _, _, _, _, _, _)
           | Seq(_, _, _, Some(O), Some(O), Some(O), _, _, _)
           | Seq(_, _, _, _, _, _, Some(O), Some(O), Some(O))
           | Seq(Some(O), _, _, Some(O), _, _, Some(O), _, _)
           | Seq(_, Some(O), _, _, Some(O), _, _, Some(O), _)
           | Seq(_, _, Some(O), _, _, Some(O), _, _, Some(O))
           | Seq(Some(O), _, _, _, Some(O), _, _, _, Some(O))
           | Seq(_, _, Some(O), _, Some(O), _, Some(O), _, _)
           | Seq(Some(X), Some(X), Some(X), _, _, _, _, _, _)
           | Seq(_, _, _, Some(X), Some(X), Some(X), _, _, _)
           | Seq(_, _, _, _, _, _, Some(X), Some(X), Some(X))
           | Seq(Some(X), _, _, Some(X), _, _, Some(X), _, _)
           | Seq(_, Some(X), _, _, Some(X), _, _, Some(X), _)
           | Seq(_, _, Some(X), _, _, Some(X), _, _, Some(X))
           | Seq(Some(X), _, _, _, Some(X), _, _, _, Some(X))
           | Seq(_, _, Some(X), _, Some(X), _, Some(X), _, _)              
        ) =>  nextXTieBoard
        case _ => {
          if((board.filter(_ == Some(O)).length + 1 == board.filter(_ == Some(X)).length) &&
            (board.filter(_ == None).length == 0)) {
            board
          }         
          else
            nextXTieBoard
        }
      }
    }
    val tyingBoard = nextXTieBoard()

    val tyingMove = random.shuffle(tyingBoard.zipWithIndex.filter(_._1 == Some(X))).head._2

    (TttTaskEnv(tyingBoard.updated(tyingMove, None)), TttTaskAct(tyingMove))
  }

  def nextXContMove():(TttTaskEnv, TttTaskAct) = {
    val squareStream = nextSquareContents
    val boardIterator = squareStream.sliding(9)
    def nextXContBoard():Seq[Option[SquareContents]] = {
      val board = boardIterator.next.force
      board match {
        case (Seq(Some(O), Some(O), Some(O), _, _, _, _, _, _)
           | Seq(_, _, _, Some(O), Some(O), Some(O), _, _, _)
           | Seq(_, _, _, _, _, _, Some(O), Some(O), Some(O))
           | Seq(Some(O), _, _, Some(O), _, _, Some(O), _, _)
           | Seq(_, Some(O), _, _, Some(O), _, _, Some(O), _)
           | Seq(_, _, Some(O), _, _, Some(O), _, _, Some(O))
           | Seq(Some(O), _, _, _, Some(O), _, _, _, Some(O))
           | Seq(_, _, Some(O), _, Some(O), _, Some(O), _, _)
           | Seq(Some(X), Some(X), Some(X), _, _, _, _, _, _)
           | Seq(_, _, _, Some(X), Some(X), Some(X), _, _, _)
           | Seq(_, _, _, _, _, _, Some(X), Some(X), Some(X))
           | Seq(Some(X), _, _, Some(X), _, _, Some(X), _, _)
           | Seq(_, Some(X), _, _, Some(X), _, _, Some(X), _)
           | Seq(_, _, Some(X), _, _, Some(X), _, _, Some(X))
           | Seq(Some(X), _, _, _, Some(X), _, _, _, Some(X))
           | Seq(_, _, Some(X), _, Some(X), _, Some(X), _, _)              
        ) =>  nextXContBoard
        case _ => {
          if((board.filter(_ == Some(O)).length + 1 == board.filter(_ == Some(X)).length) &&
            (board.filter(_ == None).length != 0)) {
            board
          }         
          else
            nextXContBoard
        }
      }
    }
    val contBoard = nextXContBoard()

    val contMove = random.shuffle(contBoard.zipWithIndex.filter(_._1 == Some(X))).head._2

    (TttTaskEnv(contBoard.updated(contMove, None)), TttTaskAct(contMove))
  }

  def nextOContMove():(TttTaskEnv, TttTaskAct) = {
    val squareStream = nextSquareContents
    val boardIterator = squareStream.sliding(9)
    def nextOContBoard():Seq[Option[SquareContents]] = {
      val board = boardIterator.next.force
      board match {
        case (Seq(Some(O), Some(O), Some(O), _, _, _, _, _, _)
           | Seq(_, _, _, Some(O), Some(O), Some(O), _, _, _)
           | Seq(_, _, _, _, _, _, Some(O), Some(O), Some(O))
           | Seq(Some(O), _, _, Some(O), _, _, Some(O), _, _)
           | Seq(_, Some(O), _, _, Some(O), _, _, Some(O), _)
           | Seq(_, _, Some(O), _, _, Some(O), _, _, Some(O))
           | Seq(Some(O), _, _, _, Some(O), _, _, _, Some(O))
           | Seq(_, _, Some(O), _, Some(O), _, Some(O), _, _)
           | Seq(Some(X), Some(X), Some(X), _, _, _, _, _, _)
           | Seq(_, _, _, Some(X), Some(X), Some(X), _, _, _)
           | Seq(_, _, _, _, _, _, Some(X), Some(X), Some(X))
           | Seq(Some(X), _, _, Some(X), _, _, Some(X), _, _)
           | Seq(_, Some(X), _, _, Some(X), _, _, Some(X), _)
           | Seq(_, _, Some(X), _, _, Some(X), _, _, Some(X))
           | Seq(Some(X), _, _, _, Some(X), _, _, _, Some(X))
           | Seq(_, _, Some(X), _, Some(X), _, Some(X), _, _)              
        ) =>  nextOContBoard
        case _ => {
          if((board.filter(_ == Some(O)).length == board.filter(_ == Some(X)).length) &&
            (board.filter(_ == None).length != 0)) {
            board
          }         
          else
            nextOContBoard
        }
      }
    }
    val contBoard = nextOContBoard()

    val contMove = random.shuffle(contBoard.zipWithIndex.filter(_._1 == Some(O))).head._2

    (TttTaskEnv(contBoard.updated(contMove, None)), TttTaskAct(contMove))
  }
}
