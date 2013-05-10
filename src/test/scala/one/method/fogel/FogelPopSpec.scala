package one.method.fogel

import org.scalatest.FeatureSpec
import org.scalatest.GivenWhenThen
import akka.testkit.{TestFSMRef, TestProbe}
import akka.actor.{ActorRef, ActorSystem, Actor, FSM}
import scala.concurrent.duration._
import collection.breakOut
import FogelPrtcl._
import akka.pattern.ask
import akka.util.Timeout
import scala.concurrent.duration._
import scala.concurrent.Await

class FogelPopSpec extends FeatureSpec with GivenWhenThen {
  implicit val system = ActorSystem("FogelMethodSpec")

  import one.method.fogel.FogelPop._

  val random = new scala.util.Random()

  val aFew = 3
  val lots = 1000

  feature("A FogelPop(popSize) begins in ReadyState with ReadyData") {
    scenario("An A FogelPop is created and has its state queried") {
      Given("a freshly created FogelPop")
      val popSize = random.nextInt(50) * 2
      val numIn = 10
      val numOut = 9
      val minHid = 1
      val maxHid = 10
      val testFSMRef = TestFSMRef(new FogelPop(popSize, numIn, numOut, minHid, maxHid))

      When("its state is queried")
      Then("its stateName is Ready")
      assert(testFSMRef.stateName == Ready) 

      And("its stateData is appropriate ReadyData")
      testFSMRef.stateData match {
        case ReadyData(0, popSize, members) => {
          assert(members.size == popSize)
          members.values.map {m => 
            assert(m.interNetFit == 0)
            assert(m.taskFit == 0)
            assert(m.chromo.numIn == 10)
            assert(m.chromo.numOut == 9)
            assert(m.chromo.numHid >= 1)
            assert(m.chromo.numHid <= 10)
          }
        }
        case _ => fail
      }
    }
  }

  feature("A FogelPop can be asked for its Champion when Ready") {
    scenario("A FogelPop is created with some random members and queried for its Champion") {
      Given("a FogelPop with some random taskFits")
      val popSize = random.nextInt(50) * 2
      val numIn = 9
      val numOut = 9
      val minHid = 1
      val maxHid = 10
      val testFSMRef = TestFSMRef(new FogelPop(popSize, numIn, numOut, minHid, maxHid))
      val membersWithTaskFits = testFSMRef.stateData.asInstanceOf[ReadyData].members.map{ case (i,m) => (i, m.copy(taskFit = random.nextInt(100)))}
      testFSMRef.setState(Ready, ReadyData(0, popSize, membersWithTaskFits)) 

      When("it receives a GetChampion")
      implicit val timeout = Timeout(5 seconds)
      val fChamp = testFSMRef ? GetChampion

      Then("it responds with the current Member with the highest taskFitness")
      val champion = membersWithTaskFits.values maxBy {_.taskFit}
      val result = Await.result(fChamp, timeout.duration).asInstanceOf[Member]
      assert(result == champion)
    }
  }

  feature("A FogelPop can be asked to PerfGen") {
    scenario("A FogelPop and asked to PerfGen") {
      Given("a FogelPop")
      val popSize = 50
      val numIn = 9
      val numOut = 9
      val minHid = 1
      val maxHid = 2
      val testFSMRef = TestFSMRef(new FogelPop(popSize, numIn, numOut, minHid, maxHid))
      val membersWithTaskFits = testFSMRef.stateData.asInstanceOf[ReadyData].members.map{case (i, m) => (i, m.copy(taskFit = 0))}
      testFSMRef.setState(Ready, ReadyData(0, popSize, membersWithTaskFits))


      When("it receives a PerfGen")

      Then("it responds with the next generation's champion member")
      (0 until 100).map(_ => {
        implicit val timeout = Timeout(30 seconds)
        val fChamp = testFSMRef ? PerfGen
        val result = Await.result(fChamp, timeout.duration).asInstanceOf[Member]
        println(result.taskFit)
      }
      )
    }
  }
}
