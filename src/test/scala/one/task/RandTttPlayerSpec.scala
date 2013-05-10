package one.task

import org.scalatest.{FeatureSpec, GivenWhenThen}
import akka.testkit.{TestFSMRef, TestProbe}
import akka.actor.{ActorSystem, ActorRef}
import TttTaskPrtcl._
import TttTaskPrtcl.TttStatus._
import TttTaskPrtcl.SquareContents._
import TttTaskUtil._
import scala.concurrent.duration._

class RandTttPlayerSpec extends FeatureSpec with GivenWhenThen {
  implicit val system = ActorSystem("TttSpec")

  val aFew = 3
  val lots = 1000
  val random = new scala.util.Random()

  import RandTttPlayer._

  feature("A RandTttPlayer begins in the Ready state with ReadyData") {
    scenario("RandTttPlayer is created") {
      Given("A TttTask is created")
      val testFSMRef = TestFSMRef(new RandTttPlayer)

      When("its state is queried")
      Then("its found to be in the Ready state")
      assert(testFSMRef.stateName == Ready)

      And("has ReadData")
      assert(testFSMRef.stateData == ReadyData)
    }
  }

  feature("When a TttPlayer receives a TttTaskEnv it responds by moving in one the unoccupied spaces") {
    scenario("A RandTttPlayer is given a board with at least one empty space") {
      Given("a RandTttPlayer and a TttTask")
      val tttTaskProbe = new TestProbe(system)
      val testFSMRef = TestFSMRef(new RandTttPlayer)

      When("it receives a TttTaskEnv with at least one empty space")
      def nextPlayableBoard():Seq[Option[SquareContents]] = {
        val squareStream = nextSquareContents
        val boardIterator = squareStream.sliding(9)
        val board = boardIterator.next.force
        if(board.filter(_ == None).length == 0)
          nextPlayableBoard
        else
          board
      }
      val board = nextPlayableBoard()
      tttTaskProbe.send(testFSMRef, TttTaskEnv(board))


      Then("the TttTask receives a valid TttTaskAct")
      assert(tttTaskProbe.expectMsgPF(0 millis){case act:TttTaskAct => board(act.square) == None})

      And("Magic Happens")
      //schedule a bunch of randoms and see who wins
    }
  }
}
