package one.net

import org.scalatest.FeatureSpec
import org.scalatest.GivenWhenThen
import akka.testkit.{TestFSMRef, TestProbe}
import akka.actor.{ActorRef, ActorSystem, Actor, FSM}
import scala.concurrent.duration._
import collection.breakOut
import NetPrtcl._
import Synapse._

class SynapseSpec extends FeatureSpec with GivenWhenThen {
  implicit val system = ActorSystem("SynapseSpec")


  val random = new scala.util.Random()

  val aFew = 3
  val lots = 1000
 
  feature("A Synapse begins in the StartState state with StartData") {
    scenario("A Synapse is created and has its state queried") {
      Given("a freshly created Synapse")
      val testFSMRef = TestFSMRef(new Synapse)

      When("its state is queried")
      val stateName = testFSMRef.stateName
      val stateData = testFSMRef.stateData

      Then("its stateName is found to be StartState")
      assert(stateName == StartState)

      And("its stateData is found to be StartData")
      assert(stateData == StartData)
    }
  }

  feature("A Synapse in the StartState transitions to the HaveOutputState when it receives a SetOutput") {
    scenario("A Synapse in the StartState receives a SetOutput") {
      Given("A Synapse in the StartState")
      val testFSMRef = TestFSMRef(new Synapse)

      When("it receives a SetOutput(someOutput)")
      val output = new TestProbe(system).ref
      testFSMRef ! SetOutput(output)

      Then("it transitions to the HaveOutputState")
      assert(testFSMRef.stateName == HaveOutputState)

      And("its data is HaveOutputData(someOutput)")
      assert(testFSMRef.stateData == HaveOutputData(output))
    }
  }

  feature("A Synapse in the StartState transitions to the HaveWeightState when it receives a SetWeight") {
    scenario("A Synapse in the StartState receives a SetWeight") {
      Given("A Synapse in the StartState")
      val testFSMRef = TestFSMRef(new Synapse)

      When("it receives a SetWeight(someWeight)")
      val weight = random.nextDouble
      testFSMRef ! SetWeight(weight)

      Then("it transitions to the HaveWeightState")
      assert(testFSMRef.stateName == HaveWeightState)

      And("its data is HaveWeightData(someWeight)")
      assert(testFSMRef.stateData == HaveWeightData(weight))
    }
  }

  feature("A Synapse in the StartState transitions to the HaveInputState when it receives a SetInput") {
    scenario("A Synapse in the StartState receives a SetInput") {
      Given("A Synapse in the StartState")
      val testFSMRef = TestFSMRef(new Synapse)

      When("it receives a SetInput(someInput)")
      val input = new TestProbe(system).ref
      testFSMRef ! SetInput(input)

      Then("it transitions to the HaveInputState")
      assert(testFSMRef.stateName == HaveInputState)

      And("its data is HaveInputData(someInput)")
      assert(testFSMRef.stateData == HaveInputData(input))
    }
  }

  feature("A Synapse in the HaveInputState transitions to the HaveInputAndWeightState when it receives a SetWeight") {
    scenario("A Synapse in the HaveInputState receives a SetWeight") {
      Given("A Synapse in the HaveInputState")
      val testFSMRef = TestFSMRef(new Synapse)
      val input = new TestProbe(system).ref
      testFSMRef.setState(HaveInputState, HaveInputData(input))

      When("it receives a SetWeight(someWeight)")
      val weight = random.nextDouble
      testFSMRef ! SetWeight(weight)

      Then("it transitions to the HaveInputAndWeightState")
      assert(testFSMRef.stateName == HaveInputAndWeightState)

      And("its data is HaveInputAndWeight(someInput, someWeight)")
      assert(testFSMRef.stateData == HaveInputAndWeightData(input, weight))
    }
  }

  feature("A Synapse in the HaveInputState transitions to the HaveInputAndOutputState when it receives a SetOutput") {
    scenario("A Synapse in the HaveInputState receives a SetOutput") {
      Given("A Synapse in the HaveInputState")
      val testFSMRef = TestFSMRef(new Synapse)
      val input = new TestProbe(system).ref
      testFSMRef.setState(HaveInputState, HaveInputData(input))

      When("it receives a SetOutput(someOutput)")
      val output = new TestProbe(system).ref
      testFSMRef ! SetOutput(output)

      Then("it transitions to the HaveInputAndOutputState")
      assert(testFSMRef.stateName == HaveInputAndOutputState)

      And("its data is HaveInputAndOutput(someInput, someOutput)")
      assert(testFSMRef.stateData == HaveInputAndOutputData(input, output))
    }
  }

  feature("A Synapse in the HaveOutputState transitions to the HaveInputAndOutputState when it receives a SetInput") {
    scenario("A Synapse in the HaveOutputState receives a SetInput") {
      Given("A Synapse in the HaveOutputState")
      val testFSMRef = TestFSMRef(new Synapse)
      val output = new TestProbe(system).ref
      testFSMRef.setState(HaveOutputState, HaveOutputData(output))

      When("it receives a SetInput(someInput)")
      val input = new TestProbe(system).ref
      testFSMRef ! SetInput(input)

      Then("it transitions to the HaveInputAndOutputState")
      assert(testFSMRef.stateName == HaveInputAndOutputState)

      And("its data is HaveInputAndOutput(someInput, someOutput)")
      assert(testFSMRef.stateData == HaveInputAndOutputData(input, output))
    }
  }

  feature("A Synapse in the HaveWeightState transitions to the HaveInputAndWeightState when it receives a SetInput") {
    scenario("A Synapse in the HaveWeightState receives a SetInput") {
      Given("A Synapse in the HaveWeightState")
      val testFSMRef = TestFSMRef(new Synapse)
      val weight = random.nextDouble
      testFSMRef.setState(HaveWeightState, HaveWeightData(weight))

      When("it receives a SetInput(someInput)")
      val input = new TestProbe(system).ref
      testFSMRef ! SetInput(input)

      Then("it transitions to the HaveInputAndWeightState")
      assert(testFSMRef.stateName == HaveInputAndWeightState)

      And("its data is HaveInputAndWeight(someInput, someWeight)")
      assert(testFSMRef.stateData == HaveInputAndWeightData(input, weight))
    }
  }

  feature("A Synapse in the HaveWeightState transitions to the HaveWeightAndOutputState when it receives a SetOutput") {
    scenario("A Synapse in the HaveWeightState receives a SetOutput") {
      Given("A Synapse in the HaveWeightState")
      val testFSMRef = TestFSMRef(new Synapse)
      val weight = random.nextDouble
      testFSMRef.setState(HaveWeightState, HaveWeightData(weight))

      When("it receives a SetOutput(someOutput)")
      val output = new TestProbe(system).ref
      testFSMRef ! SetOutput(output)

      Then("it transitions to the HaveWeightAndOutputState")
      assert(testFSMRef.stateName == HaveWeightAndOutputState)

      And("its data is HaveWeightAndOutput(someWeight, someOutput)")
      assert(testFSMRef.stateData == HaveWeightAndOutputData(weight, output))
    }
  }

  feature("A Synapse in the HaveOutputState transitions to the HaveWeightAndOutputState when it receives a SetWeight") {
    scenario("A Synapse in the HaveOutputState receives a SetWeight") {
      Given("A Synapse in the HaveOutputState")
      val testFSMRef = TestFSMRef(new Synapse)
      val output = new TestProbe(system).ref
      testFSMRef.setState(HaveOutputState, HaveOutputData(output))

      When("it receives a SetWeight(someWeight)")
      val weight = random.nextDouble
      testFSMRef ! SetWeight(weight)

      Then("it transitions to the HaveWeightAndOutputState")
      assert(testFSMRef.stateName == HaveWeightAndOutputState)

      And("its data is HaveWeightAndOutput(someWeight, someOutput)")
      assert(testFSMRef.stateData == HaveWeightAndOutputData(weight, output))
    }
  }


  feature("A Synapse in the HaveInputAndWeightState transitions to the ReadyState when it receives a SetOutput") {
    scenario("A Synapse in the HaveInputAndWeightState receives a SetOutput") {
      Given("A Synapse in the HaveInputAndWeightState")
      val testFSMRef = TestFSMRef(new Synapse)
      val input = new TestProbe(system).ref
      val weight = random.nextDouble
      testFSMRef.setState(HaveInputAndWeightState, HaveInputAndWeightData(input, weight))

      When("it receives a SetWeight(someWeight)")
      val output = new TestProbe(system).ref
      testFSMRef ! SetOutput(output)

      Then("it transitions to the ReadyState")
      assert(testFSMRef.stateName == ReadyState)

      And("its data is ReadyData(someInput, someWeight, someOutput)")
      assert(testFSMRef.stateData == ReadyData(input, weight, output))
    }
  }

  feature("A Synapse in the HaveInputAndOutputState transitions to the ReadyState when it receives a SetWeight") {
    scenario("A Synapse in the HaveInputAndOutputState receives a SetWeight") {
      Given("A Synapse in the HaveInputAndOutputState")
      val testFSMRef = TestFSMRef(new Synapse)
      val input = new TestProbe(system).ref
      val output = new TestProbe(system).ref
      testFSMRef.setState(HaveInputAndOutputState, HaveInputAndOutputData(input, output))

      When("it receives a SetWeight(someWeight)")
      val weight = random.nextDouble
      testFSMRef ! SetWeight(weight)

      Then("it transitions to the ReadyState")
      assert(testFSMRef.stateName == ReadyState)

      And("its data is ReadyData(someInput, someWeight, someOutput)")
      assert(testFSMRef.stateData == ReadyData(input, weight, output))
    }
  }

  feature("A Synapse in the HaveWeightAndOutputState transitions to the ReadyState when it receives a SetInput") {
    scenario("A Synapse in the HaveWeightAndOutputState receives a SetInput") {
      Given("A Synapse in the HaveWeightAndOutputState")
      val testFSMRef = TestFSMRef(new Synapse)
      val weight = random.nextDouble
      val output = new TestProbe(system).ref
      testFSMRef.setState(HaveWeightAndOutputState, HaveWeightAndOutputData(weight, output))

      When("it receives a SetInput(someInput)")
      val input = new TestProbe(system).ref
      testFSMRef ! SetInput(input)

      Then("it transitions to the ReadyState")
      assert(testFSMRef.stateName == ReadyState)

      And("its data is ReadyData(someInput, someWeight, someOutput)")
      assert(testFSMRef.stateData == ReadyData(input, weight, output))
    }
  }

  feature("An Synapse in the ReadyState applies its weight to all Stimulations received from its input and forwards the result to its output") {
    scenario("A Synapse in the ReadyState receives a Stimulation from its input") {
      Given("A Synapse in the ReadyState")
      val testFSMRef = TestFSMRef(new Synapse)
      val inputProbe = new TestProbe(system)
      val weight = random.nextDouble
      val outputProbe = new TestProbe(system)
      testFSMRef.setState(ReadyState, ReadyData(inputProbe.ref, weight, outputProbe.ref))

      When("a Stimulation(someValue) is received from its input")
      val value = random.nextDouble
      testFSMRef ! Stimulation(value)

      Then("the its output subsequently receives Stimulation(someValue * weight)")
      outputProbe.expectMsg(Stimulation(value*weight))

      And("it remains in the ReadyState")
      assert(testFSMRef.stateName == ReadyState)
    }
  }
}


