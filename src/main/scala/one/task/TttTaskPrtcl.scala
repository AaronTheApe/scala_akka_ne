package one.task

object TttTaskPrtcl {
  object SquareContents extends Enumeration {
    type SquareContents = Value
    val X = Value("X")
    val O = Value("O")
  }
  object TttStatus extends Enumeration {
    type TttStatus = Value
    val XWon = Value("XWon")
    val OWon = Value("OWon")
    val Tie = Value("Tie")
    val NotDone = Value("NotDone")
  }
  import SquareContents._
  case class TttTaskEnv(squares: Seq[Option[SquareContents]])
  case class TttTaskAct(square: Integer)
  case class TttResult(endStatus: TttStatus.Value)
  case class ImpossibleResultException(msg: String) extends Exception
}
