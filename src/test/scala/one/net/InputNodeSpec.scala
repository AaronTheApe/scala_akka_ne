package one.net

import org.scalatest.FeatureSpec
import org.scalatest.GivenWhenThen
import akka.testkit.{TestFSMRef, TestProbe}
import akka.actor.{ActorRef, ActorSystem, Actor, FSM}
import scala.concurrent.duration._
import collection.breakOut
import NetPrtcl._

class InputNodeSpec extends FeatureSpec with GivenWhenThen {
  implicit val system = ActorSystem("InputUnitSpec")

  import one.net.InputNode._

  val random = new scala.util.Random()

  val aFew = 3
  val lots = 1000

  feature("An InputNode begins in StartState with StartData") {
    scenario("An InputNode is created and has its state queried") {
      Given("a freshly created InputNode")
      val testFSMRef = TestFSMRef(new InputNode)

      When("its state is queried")
      val stateName = testFSMRef.stateName
      val stateData = testFSMRef.stateData

      Then("its stateName is StartState")
      assert(stateName == StartState) 

      Then("its stateData is StartData")
      assert(stateData == StartData)
    }
  }

  feature("An InputNode in the StartState transitions to HasOutputs with HasOutputsData(Set(output))after it receives an AddOutput(output)") {
    scenario("An InputNode receives an AddOutput") {
      Given("an InputNode")
      val testFSMRef = TestFSMRef(new InputNode)

      When("it receives an AddOutput(someActorRef)")
      val someActorRef = new TestProbe(system).ref
      testFSMRef ! AddOutput(someActorRef)

      Then("it transitions to the HasOutputs") 
      assert(testFSMRef.stateName == HasOutputs)

      And("it has HasOutputs(Set(someActorRef))")
      assert(testFSMRef.stateData == HasOutputsData(Set(someActorRef)))
    }
  }

  feature("An InputNode in the StartState transitions to HasInput with HasInputData(input)after it receives a SetInput(input)") {
    scenario("An InputNode receives an SetInput") {
      Given("an InputNode in the StartState")
      val testFSMRef = TestFSMRef(new InputNode)

      When("it receives an SetInput(someActorRef)")
      val someActorRef = new TestProbe(system).ref
      testFSMRef ! SetInput(someActorRef)

      Then("it transitions to HasInput") 
      assert(testFSMRef.stateName == HasInput)

      And("it has HasInput(someActorRef)")
      assert(testFSMRef.stateData == HasInputData(someActorRef))
    }
  }

  feature("An InputNode in the HasInput state transitions to Ready with ReadyData(input, Set(output)) after it receives an AddOutput(output)") {
    scenario("An InputNode in the HasInput state receives an AddOutput") {
      Given("an InputNode in the HasInput state")
      val testFSMRef = TestFSMRef(new InputNode)
      val input = new TestProbe(system).ref
      testFSMRef.setState(HasInput, HasInputData(input))

      When("it receives an AddOutput(someActorRef)")
      val someActorRef = new TestProbe(system).ref
      testFSMRef ! AddOutput(someActorRef)

      Then("it transitions to Ready") 
      assert(testFSMRef.stateName == Ready)

      And("it has stateData Ready(input, Set(someActorRef))")
      assert(testFSMRef.stateData == ReadyData(input, Set(someActorRef)))
    }
  }

  feature("An InputNode in the HasOutputs state transitions to Ready with ReadyData(input, outputs) after it receives an AddInput(input)") {
    scenario("An InputNode in the HasOutputs state receives an AddInput") {
      Given("an InputNode in the HasOutputs state")
      val testFSMRef = TestFSMRef(new InputNode)
      val outputs = (0 until random.nextInt(aFew) + 1).map(_ => new TestProbe(system).ref).toSet
      testFSMRef.setState(HasOutputs, HasOutputsData(outputs))

      When("it receives an AddOutput(someActorRef)")
      val input = new TestProbe(system).ref
      testFSMRef ! SetInput(input)

      Then("it transitions to Ready") 
      assert(testFSMRef.stateName == Ready)

      And("it has stateData Ready(input, outputs)")
      assert(testFSMRef.stateData == ReadyData(input, outputs))
    }
  }

  feature("An InputNode stays in ReadyState with ReadyData(input, previousOutputs + newOutput) after it receives an AddOutput(newOutput)") {
    scenario("An InputNode in the ReadyState with ReadyData(someOutputs) receives an AddOutput(newOutput)") {
      Given("an InputNode in the ReadyState with ReadyData(input, someOutputs)")
      val testFSMRef = TestFSMRef(new InputNode)
      val someOutputs = (0 until aFew + 1).map(_ => new TestProbe(system).ref).toSet
      val input = new TestProbe(system).ref
      testFSMRef.setState(Ready, ReadyData(input, someOutputs))

      When("it receives an AddOutput(newOutput)")
      val newOutput = new TestProbe(system).ref
      testFSMRef ! AddOutput(newOutput)

      Then("it remains in the the Ready") 
      assert(testFSMRef.stateName == Ready)

      And("it has ReadyData(someOutputs + newOutput)")
      assert(testFSMRef.stateData == ReadyData(input, someOutputs + newOutput))
    }
  }

  feature("An InputNode in the Ready state forwards any Stimulation it receives to all of its outputs, and remains in the Ready state with the same ReadyData") {
    scenario("An InputNode in the ReadyState with ReadyData(input, someOutputs) receives a Stimulation(someValue)") {
      Given("an InputNode in the ReadyState with ReadyData(input, someOutputs)")
      val testFSMRef = TestFSMRef(new InputNode)
      val someOutputProbes = (0 until random.nextInt(lots) + 1).map(_ => new TestProbe(system))
      val someOutputRefs = someOutputProbes.map(o => o.ref).toSet
      val input = new TestProbe(system).ref
      testFSMRef.setState(Ready, ReadyData(input, someOutputRefs))

      When("it receives a Stimulation(someValue)")
      val someStimulation = Stimulation(random.nextDouble)
      testFSMRef ! someStimulation

      Then("all outputs receive the same Stimulation(someValue)")
      someOutputProbes.map(op => op.expectMsg(someStimulation))

      And("it remains in the the Ready state") 
      assert(testFSMRef.stateName == Ready)

      And("it has the same ReadyData(input, someOutputs)")
      assert(testFSMRef.stateData == ReadyData(input, someOutputRefs))
    }
  }
}
