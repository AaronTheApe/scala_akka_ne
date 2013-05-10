package one.net

import org.scalatest.FeatureSpec
import org.scalatest.GivenWhenThen
import akka.testkit.{TestFSMRef, TestProbe}
import akka.actor.{ActorRef, ActorSystem, Actor, FSM, Props}
import scala.concurrent.duration._
import collection.breakOut
import NetPrtcl._
import ProcNode._

class ProcNodeSpec extends FeatureSpec with GivenWhenThen { 
  implicit val system = ActorSystem("ProcUnitSpec")

  val random = new scala.util.Random()

  val aFew = 3
  val lots = 1000

  def SigmoidTheSum(inputs: Iterable[Double]): Double = {
    1 / (1 + math.exp(-inputs.sum))
  }

  feature("A ProcUnit begins in the StartSate with StartData") {
    Given("A ProcUnit is created with new ProcUnit(someActivationFunction: Seq[Double] -> Double)")
    val testFSMRef = TestFSMRef(new ProcNode(SigmoidTheSum))

    When("its state is queried")
    val stateName = testFSMRef.stateName
    val stateData = testFSMRef.stateData

    Then("its stateName is found to be StartState")
    assert(stateName == StartState)

    And("its stateData is found to be StartData")
    assert(stateData == StartData)
  }

  feature("A ProcUnit in the StartState transitions to the HasOutputsState when it receives an AddOutput") {
    Given("A ProcUnit in the StartState")
    val testFSMRef = TestFSMRef(new ProcNode(SigmoidTheSum))

    When("it receives an AddOutput")
    val output = new TestProbe(system).ref
    testFSMRef ! AddOutput(output)

    Then("it transitions to the HasOutputsState")
    assert(testFSMRef.stateName == HasOutputsState)

    And("it has data HasOutputsData(Set(output)")
    assert(testFSMRef.stateData == HasOutputsData(Set(output)))
  }

  feature("A ProcNode in the HasOutputsState transitions to Ready when it receives an AddInput") {
    scenario("A ProcNode in the HasOutputsState receives AddInput") {
      Given("A ProcNode in the HasOuputsState")
      val testFSMRef = TestFSMRef(new ProcNode(SigmoidTheSum))
      val output = new TestProbe(system).ref
      testFSMRef.setState(HasOutputsState, HasOutputsData(Set(output)))
      
      When("it receives AddInput(someInput)")
      val input = new TestProbe(system).ref
      testFSMRef ! AddInput(input)

      Then("it transitions to the ReadyState")
      assert(testFSMRef.stateName == ReadyState)
    }
  }

  feature("A ProcNode in the StartState transitions to the HasInputsState when it receives an AddInput") {
    scenario("A ProcNode in the StartState receives an AddInput") {
      Given("A ProcNode in the StartState")
      val testFSMRef = TestFSMRef(new ProcNode(SigmoidTheSum))
      
      When("a AddInput(newInput) is received")
      val newInput = new TestProbe(system).ref
      testFSMRef ! AddInput(newInput)

      Then("it transitions to the HasInputsState")
      assert(testFSMRef.stateName == HasInputsState)

      And("it has data of HasInputsData(Set(newInput))")
      assert(testFSMRef.stateData == HasInputsData(Set(newInput)))
    }
  }

  feature("A ProcNode in the HasInputsState transitions to the ReadyState when it receives an AddOutput") {
    scenario("A ProcNode in the HasInputsState receives an AddOutput") {
      Given("A ProcNode in the HasInputsState")
      val testFSMRef = TestFSMRef(new ProcNode(SigmoidTheSum))
      val inputs = (0 until aFew + 1).map(_ => new TestProbe(system).ref).toSet
      testFSMRef.setState(HasInputsState, HasInputsData(inputs))

      When("an AddOutput(newOutput) is received")
      val newOutput = new TestProbe(system).ref
      testFSMRef ! AddOutput(newOutput)

      Then("it transitions to the ReadyState")
      assert(testFSMRef.stateName == ReadyState)

      And("its stateData is ReadyData(inputs, outputs)")
      assert(testFSMRef.stateData == ReadyData(inputs, Set(newOutput)))
    }
  }

  feature("A ProcNode in the ReadyState stays in the ReadyState with the additional input when it receives an AddInput") {
    scenario("A ProcNode in the ReadyState receives an AddInput") {
      Given("A procNode in the ReadState")
      val testFSMRef = TestFSMRef(new ProcNode(SigmoidTheSum))
      val inputs = (0 until aFew + 1).map(_ => new TestProbe(system).ref).toSet
      val outputs = (0 until aFew + 1).map(_ => new TestProbe(system).ref).toSet
      testFSMRef.setState(ReadyState, ReadyData(inputs, outputs))

      When("an AddInput(newInput) is received")
      val newInput = new TestProbe(system).ref
      testFSMRef ! AddInput(newInput)

      Then("it stays in the ReadyState")
      assert(testFSMRef.stateName == ReadyState)

      And("has the additional input in its data")
      assert(testFSMRef.stateData == ReadyData(inputs + newInput, outputs))
    }
  }

  feature("A ProcNode in the ReadyState stays in the ReadyState with the additional output when it receivs an AddOutput") {
    scenario("A ProcNode in the ReadyState receives an AddOutput") {
      Given("A procNode in the ReadState")
      val testFSMRef = TestFSMRef(new ProcNode(SigmoidTheSum))
      val inputs = (0 until aFew + 1).map(_ => new TestProbe(system).ref).toSet
      val outputs = (0 until aFew + 1).map(_ => new TestProbe(system).ref).toSet
      testFSMRef.setState(ReadyState, ReadyData(inputs, outputs))

      When("an AddOutput(newOutput) is received")
      val newOutput = new TestProbe(system).ref
      testFSMRef ! AddOutput(newOutput)

      Then("it stays in the ReadyState")
      assert(testFSMRef.stateName == ReadyState)

      And("has the additional input in its data")
      assert(testFSMRef.stateData == ReadyData(inputs, outputs + newOutput))
    }
  }

  feature("A ProcNode with multiple inputs in the ReadyState transitions to the ProcState when it receives a Stimulation from one of its inputs") {
    scenario("A ProcNode in the ReadyState with multiple inputs receives a Stimulation") {
      Given("A ProcNode in the ReadyState with multiple inputs")
      val testFSMRef = TestFSMRef(new ProcNode(SigmoidTheSum))
      val inputProbes = (0 until random.nextInt(aFew) + 2).map(_ => new TestProbe(system)).toSet
      val inputs = inputProbes.map(ip => ip.ref).toSet
      val outputs = (0 until random.nextInt(aFew) + 1).map(_ => new TestProbe(system).ref).toSet
      testFSMRef.setState(ReadyState, ReadyData(inputs, outputs))

      When("a Stimulation(aValue) is received")
      val aValue = random.nextDouble
      val senderProbe = random.shuffle(inputProbes.toList).head
      val sender = senderProbe.ref
      senderProbe.send(testFSMRef, Stimulation(aValue))

      Then("it transitions to the ProcState")
      assert(testFSMRef.stateName == ProcState) 

      And("its stateData is ProcData(inputs, outputs, inputs - sender, Map(sender -> aValue))")
      assert(testFSMRef.stateData == ProcData(inputs, outputs, inputs - sender, Map(sender -> aValue))) 
    }
  }

  feature("A ProcNode with a single input in the ReadyState sends its processing output to all of its outputs and stays in the ReadyState when it receives a Stimulation") {
    scenario("A ProcNode in the ReadyState with a single inpup receives a Stimulation") {      
      Given("A ProcNode in the ReadyState with a single input")
      val testFSMRef = TestFSMRef(new ProcNode(SigmoidTheSum))
      val inputProbe = new TestProbe(system)
      val input = inputProbe.ref 
      val outputProbes = (0 until random.nextInt(aFew) + 1).map(_ => new TestProbe(system)).toSet
      val outputs = outputProbes.map(op => op.ref).toSet
      testFSMRef.setState(ReadyState, ReadyData(Set(input), outputs))

      When("a Stimulation(aValue) is received")
      val aValue = random.nextDouble
      inputProbe.send(testFSMRef, Stimulation(aValue))

      Then("it stays in the ReadyState")
      assert(testFSMRef.stateName == ReadyState) 

      And("all of its outputs receive the output of its activation function on its single input")
      val outputStim = Stimulation(SigmoidTheSum(Seq(aValue)))
      outputProbes.map(o => o.expectMsg(outputStim)) 
    }
  }

  feature("A ProcNode in the ProcState stays in the ProcState when it receives anything but its final Stimulation") {
    scenario("A ProcNode in the ProceState receives a Stimulation other than its last") {
      Given("A ProcNode in the ProcState with at least 3 inputs(one already received)")
      val testFSMRef = TestFSMRef(new ProcNode(SigmoidTheSum))
      //at least 3 input probes
      val inputProbes = (0 until random.nextInt(aFew) + 3).map(_ => new TestProbe(system)).toSet
      val inputs = inputProbes.map(ip => ip.ref)
      //at least 1 output probe
      val outputProbes = (0 until random.nextInt(aFew) + 1).map(_ => new TestProbe(system))
      val outputs = outputProbes.map(op => op.ref).toSet
      val alreadyReceivedInputProbe = random.shuffle(inputProbes.toList).head
      val remInputProbes = inputProbes.toSet - alreadyReceivedInputProbe
      val remInputs = remInputProbes.map(rip => rip.ref)
      val initialSenderValues = Map(alreadyReceivedInputProbe.ref -> random.nextDouble)
      testFSMRef.setState(ProcState, ProcData(inputs, outputs, remInputs, initialSenderValues))

      When("it receives Stimulations from just some of its remaining inputs")
      val nonsender = random.shuffle(remInputProbes.toList).head
      val senders = remInputProbes - nonsender
      val senderValues = senders.map(i => {
        val value = random.nextDouble
        i.send(testFSMRef, Stimulation(value))
        i.ref -> value
      }).toMap
      val newSenderValues = senderValues ++ initialSenderValues

      Then("it remains in the ProcState")
      assert(testFSMRef.stateName == ProcState)

      And("its stateData is ProcData(inputs, outputs, Set(nonsender), newSenderValues)")
      assert(testFSMRef.stateData == ProcData(inputs, outputs, Set(nonsender.ref), newSenderValues))  
    }
  }

  feature("A ProcNode in the ProcState sends its  processing output to all of its outputs and transitions to the ReadyState when it receives its last Stimulation") {
    scenario("A ProcNode in the ProcState receives its last Stimulation") {
      Given("A ProcNode in the ProcState with at least 3 inputs(one already received)")
      val testFSMRef = TestFSMRef(new ProcNode(SigmoidTheSum))
      //at least 3 input probes
      val inputProbes = (0 until random.nextInt(aFew) + 3).map(_ => new TestProbe(system)).toSet
      val inputs = inputProbes.map(ip => ip.ref)
      //at least 1 output probe
      val outputProbes = (0 until random.nextInt(aFew) + 1).map(_ => new TestProbe(system))
      val outputs = outputProbes.map(op => op.ref).toSet
      val alreadyReceivedInputProbe = random.shuffle(inputProbes.toList).head
      val remInputProbes = inputProbes.toSet - alreadyReceivedInputProbe
      val remInputs = remInputProbes.map(rip => rip.ref)
      val initialValue = random.nextDouble
      val initialSenderValues = Map(alreadyReceivedInputProbe.ref -> initialValue)
      testFSMRef.setState(ProcState, ProcData(inputs, outputs, remInputs, initialSenderValues))

      When("it receives Stimulations from all of its remaining inputs")
      val values = (remInputProbes.map(i => {
        val value = random.nextDouble
        i.send(testFSMRef, Stimulation(value))
        value
      }) + initialValue).toSeq

      Then("it remains transitions to the ReadyState with ReadyData(inputs, outputs)")
      assert(testFSMRef.stateName == ReadyState)
      assert(testFSMRef.stateData == ReadyData(inputs, outputs))

      And("and all of its outputs receive the activationFunction applied to the input values")
      val outputStim = Stimulation(SigmoidTheSum(values))
      outputProbes.map(op => op.expectMsg(outputStim))  
    }
  }
}
