package one.net

import org.scalatest.FeatureSpec
import org.scalatest.GivenWhenThen
import akka.testkit.{TestFSMRef, TestProbe}
import akka.actor.{ActorRef, ActorSystem, Actor, FSM}
import NetPrtcl._

class NetworkSpec extends FeatureSpec with GivenWhenThen {
  implicit val system = ActorSystem("NetworkSpec")

  val random = new scala.util.Random()

  val aFew = 3
  val lots = 1000

  def sigmoid(x: Double):Double = 1 / (1 + math.exp(-x))

  def stdAct(inputs: Iterable[Double]):Double = sigmoid(inputs.sum)

  feature("A standard two-layer Network can be created and repeatedly used for processing") {
    scenario("A standard two-layer Network is created and repeatedly used for processing") {
      Given("a Network is created with some inputs and some outputs")
      val numIn = random.nextInt(aFew) + 1
      val numHid = random.nextInt(aFew) + 1
      val numOut = random.nextInt(aFew) + 1
      val ins = (0 until numIn)
      val hids = (numIn+numOut until numIn+numOut+numHid)
      val outs = (numIn until numIn+numOut)
      val layerOneCons = ins.flatMap(i => hids.map(h => SynTpl(i, random.nextDouble, h)))
      val layerTwoCons = hids.flatMap(h => outs.map(o => SynTpl(h, random.nextDouble, o)))
      val synTpls = layerOneCons ++ layerTwoCons
      val topTpl = TopTpl(numIn, numHid, numOut, synTpls, stdAct)
      val testFSMRef = TestFSMRef(new Network(topTpl))

      When("it receives some StimulusVectors")
      Then("it responds to the sender of each StimulusVector with a ResponseVector")
      val inputs = ins.map(i => random.nextDouble).toArray
      val hidSigs = hids.map(h => sigmoid(layerOneCons.filter(_.output == h).map(s => inputs(s.input)*s.weight).sum)).toArray
      val outputs = outs.map(o => sigmoid(layerTwoCons.filter(_.output == o).map(s => hidSigs(s.input-(numIn+numOut))*s.weight).sum))
      val probe = new TestProbe(system)
      probe.send(testFSMRef, StimVec(inputs))
      probe.expectMsg(RespVec(outputs))
    }
  }
}

//       val numInputs = random.nextInt(aFew) + 1
//       val numOutputs = random.nextInt(aFew) + 1
//       val numHidden = random.nextInt(aFew) + 1
//       val testFSMRef = TestFSMRef(new Network(numInputs, numOutputs, numHidden))

//       When("the state of the Network is queried")
//       val stateName = testFSMRef.stateName
//       //val nodes = testFSMRef.stateData.nodes

//       Then("the stateName is Ontogeny")
//       assert(stateName == Ontogeny)

//       And("the nodes array is of X+Y size")
//       //assert(nodes.size == numInputs + numOutputs + numHidden)
//     }
//   }



//   //cybenko satisfaction
//   feature("a network performs a layered linear combination of sigmoids") {
//     scenario("a two layer network is constructed and forwarded random inputs") {
//       Given("a two layer network")
//       val numInputs = random.nextInt(aFew) + 1
//       val numOutputs = random.nextInt(aFew) + 1
//       val numHidden = random.nextInt(aFew) + 1

//       val testFSMRef = TestFSMRef(new Network(numInputs, numOutputs, numHidden))

//       val firstLayerWeights = (0 until numInputs).flatMap(i => (numInputs + numOutputs until numHidden+numOutputs+numInputs).map(h => Connection(i, h, random.nextDouble))) 
//       val secondLayerWeights = (numInputs + numOutputs until numHidden + numOutputs + numInputs).flatMap(h => (numInputs until numInputs + numOutputs).map(o => Connection(h, o, random.nextDouble))) 

//       testFSMRef ! AddConnections((firstLayerWeights ++ secondLayerWeights).toList)

//       val actuatorArray = new TestProbe(system)
//       testFSMRef ! SetActuatorArray(actuatorArray.ref)

//       When("the network is fed random inputs")
//       val randomInputs = (0 until numInputs).map(_ => random.nextDouble)

//       testFSMRef ! StimulusVector(randomInputs)

//       Then("the nework transforms these inputs into a two layer linear combination of the sigmoids sent to outputs")
//       def sigmoid(x:Double):Double = 1 / (1 + math.exp(-x))

//       val outputNodes = (0 until numOutputs).map(x => numInputs + x).toArray
//       val hiddenNodes = (0 until numHidden).map(x => numInputs + numOutputs + x).toArray
//       //val hiddenOuts = hiddenNodes.map(h => firstLayerWeights)
//       val hiddenOuts = hiddenNodes.map(h => firstLayerWeights.filter(c => c.output == h).map(c => c.weight).zip(randomInputs).map(f => f._1 * f._2).sum)
//       val outs = outputNodes.map(o => secondLayerWeights.filter(c => c.output == o).map(c => c.weight).zip(hiddenOuts).map(f => f._1 * f._2).sum)


//       hiddenOuts.map(h => System.out.println("zhidden: " + h))
//       outs.map(o => System.out.println("zouts: " + o))
//       actuatorArray.expectMsg(ResponseVector(outs))
//     }
//   }
// }
