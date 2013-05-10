package one.net

import org.scalatest.{FeatureSpec, GivenWhenThen}
import akka.actor.{ActorSystem, ActorRef}
import akka.testkit.{TestFSMRef, TestProbe}
import NetPrtcl._
import NeuroCon._
import one.task.TttTask._
import one.task.TttTaskPrtcl._
import one.task.TttTaskUtil._

class NeuroConSpec extends FeatureSpec with GivenWhenThen {
  implicit val system = ActorSystem("NeuroControllerSpec")

  val aFew = 3
  val lots = 1000
  val random = new scala.util.Random()

  def sigmoid(x: Double):Double = 1 / (1 + math.exp(-x))

  def stdActFn(inputs: Iterable[Double]):Double = sigmoid(inputs.sum)

  def nextTwoLayerTopTpl():TopTpl = {
    val numIn = random.nextInt(aFew) + 1
    val numHid = random.nextInt(aFew) + 1
    val numOut = random.nextInt(aFew) + 1
    val ins = (0 until numIn)
    val hids = (numIn+numOut until numIn+numOut+numHid)
    val outs = (numIn until numIn+numOut)
    val layerOneCons = ins.flatMap(i => hids.map(h => SynTpl(i, random.nextDouble, h)))
    val layerTwoCons = hids.flatMap(h => outs.map(o => SynTpl(h, random.nextDouble, o)))
    val synTpls = layerOneCons ++ layerTwoCons
    TopTpl(numIn, numHid, numOut, synTpls, stdActFn)
  }


  feature("A NeuroCon is constructed with a SensorArray, TopTpl, ActuatorArray and begins in the ReadyState with ReadyData") {
    scenario("A NeuroCon is created") {
      Given("A NeuroCon is created")      
      val topTpl = nextTwoLayerTopTpl()
      val testFSMRef = TestFSMRef(new NeuroCon[TttTaskEnv, TttTaskAct](tttSensorArray, topTpl, tttActuatorArray))

      When("its state is queried")
      val stateName = testFSMRef.stateName
      val stateData = testFSMRef.stateData

      Then("its stateName is ReadyStateName")
      assert(stateName == ReadyState)

      And("its stateData is ReadyData")
      assert(stateData.isInstanceOf[ReadyData])
    }
  }

  feature("A NeuroCon in the ReadyState that receives the TaskEnv forwards the result of its sensorArray to its NeuralNetwork") {
    scenario("A NeuroCon in the ReadyState receives a TaskEnv") {
      Given("a NeuroCon in the ReadyState with a given network")
      val topTpl = nextTwoLayerTopTpl()
      val testFSMRef = TestFSMRef(new NeuroCon[TttTaskEnv, TttTaskAct](tttSensorArray, topTpl, tttActuatorArray))
      val taskTestProbe = TestProbe()
      val netTestProbe = TestProbe()
      testFSMRef.setState(ReadyState, ReadyData(netTestProbe.ref))

      When("it receives an TaskEnv")
      val env = nextTttTaskEnv()
      taskTestProbe.send(testFSMRef, env)

      Then("its net receives the corresponding input vector")
      netTestProbe.expectMsg(tttSensorArray(env))

      And("it transitions to the ProcState with ProcData(net, context.sender)")
      assert(testFSMRef.stateName == ProcState)
      assert(testFSMRef.stateData == ProcData[TttTaskEnv](netTestProbe.ref, taskTestProbe.ref, env))
    }
  }

  feature("A NeuroCon in the ProcState that receives a RespVec, forwards the result of its actuatorArray to its task, and returns to the ReadyState") {
    scenario("A NeuroCon in the ProcState receives a RespVec") {
      Given("a NeuroCon in the ProcState with a given network")
      val topTpl = nextTwoLayerTopTpl()
      val testFSMRef = TestFSMRef(new NeuroCon[TttTaskEnv, TttTaskAct](tttSensorArray, topTpl, tttActuatorArray))
      val taskTestProbe = TestProbe()
      val netTestProbe = TestProbe()
      val env = nextTttTaskEnv()
      testFSMRef.setState(ProcState, ProcData[TttTaskEnv](netTestProbe.ref, taskTestProbe.ref, env))

      When("it receives a RespVec")
      val respVec = RespVec((0 until 9).map(_ => random.nextDouble * 2 - 1).toSeq)
      netTestProbe.send(testFSMRef, respVec)

      Then("it forwards the result of its actuatorArray to its task")
      taskTestProbe.expectMsg(tttActuatorArray(env, respVec))
      
      And("its state returns to ReadyState with ReadyData(netTestProbe.ref)")
      assert(testFSMRef.stateName == ReadyState)
      assert(testFSMRef.stateData == ReadyData(netTestProbe.ref))
    }
  }
}
