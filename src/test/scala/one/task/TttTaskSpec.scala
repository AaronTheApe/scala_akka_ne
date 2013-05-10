package one.task

import org.scalatest.{FeatureSpec, GivenWhenThen}
import akka.testkit.{TestFSMRef, TestProbe}
import akka.actor.{ActorSystem, ActorRef}
import TttTaskPrtcl._
import TttTaskPrtcl.TttStatus._
import TttTaskPrtcl.SquareContents._
import TttTaskUtil._

class TttTaskSpec extends FeatureSpec with GivenWhenThen {
  implicit val system = ActorSystem("TttSpec")

  val aFew = 3
  val lots = 1000
  val random = new scala.util.Random()

  import TttTask._

  feature("A TttTask in the PXToMove state transitions to the appropriate next state with respect to the move it receives from px") {
    scenario("TttTask in PXToMove receives a winning move") {
      Given("A TttTask is created")
      val px = TestProbe()
      val po = TestProbe()
      val eval = TestProbe()
      val testFSMRef = TestFSMRef(new TttTask(px.ref, po.ref, eval.ref))
      val (env, act) = nextXWin
      testFSMRef.setState(PXToMove, StateData(env))

      When("a winning move is submitted")
      px.send(testFSMRef, act)

      Then("eval receives TttResult(1,0)")
      eval.expectMsg(TttResult(XWon))
    }
    scenario("TttTask in PXToMove receives a tying move") {
      Given("A TttTask is created")
      val px = TestProbe()
      val po = TestProbe()
      val eval = TestProbe()
      val testFSMRef = TestFSMRef(new TttTask(px.ref, po.ref, eval.ref))
      val (env, act) = nextXTie()
      testFSMRef.setState(PXToMove, StateData(env))

      When("a winning move is submitted")
      po.send(testFSMRef, act)

      Then("eval receives TttResult(1,0)")
      eval.expectMsg(TttResult(Tie))
    }
    scenario("TttTask in PXToMove receives a continuing move") {
      Given("A TttTask is created")
      val px = TestProbe()
      val po = TestProbe()
      val eval = TestProbe()
      val testFSMRef = TestFSMRef(new TttTask(px.ref, po.ref, eval.ref))
      val (env, act) = nextXContMove()
      testFSMRef.setState(PXToMove, StateData(env))

      When("a winning move is submitted")
      po.send(testFSMRef, act)

      Then("eval receives TttResult(1,0)")
      eval.expectNoMsg
      assert(testFSMRef.stateName == POToMove)
      assert(testFSMRef.stateData == StateData(TttTaskEnv(env.squares.updated(act.square, Some(X)))))
    }

  }

  feature("A TttTask in the POToMove state transitions to the appropriate next state with respect to the move it receives from po") {
    scenario("TttTask in POToMove receives a winning move") {
      Given("A TttTask is created")
      val px = TestProbe()
      val po = TestProbe()
      val eval = TestProbe()
      val testFSMRef = TestFSMRef(new TttTask(px.ref, po.ref, eval.ref))
      val (env, act) = nextOWin
      testFSMRef.setState(POToMove, StateData(env))

      When("a winning move is submitted")
      po.send(testFSMRef, act)

      Then("eval receives TttResult(1,0)")
      eval.expectMsg(TttResult(OWon))
    }
    scenario("TttTask in POToMove receives a continuing move") {
      Given("A TttTask is created")
      val px = TestProbe()
      val po = TestProbe()
      val eval = TestProbe()
      val testFSMRef = TestFSMRef(new TttTask(px.ref, po.ref, eval.ref))
      val (env, act) = nextOContMove()
      testFSMRef.setState(POToMove, StateData(env))

      When("a winning move is submitted")
      po.send(testFSMRef, act)

      Then("eval receives TttResult(1,0)")
      eval.expectNoMsg
      assert(testFSMRef.stateName == PXToMove)
      assert(testFSMRef.stateData == StateData(TttTaskEnv(env.squares.updated(act.square, Some(O)))))
    }
  }
}
