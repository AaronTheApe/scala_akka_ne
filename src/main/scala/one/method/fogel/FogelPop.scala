package one.method.fogel

import akka.actor.{ Actor, ActorRef, LoggingFSM, Props} 
import scala.concurrent.duration._
import scala.collection.immutable.HashMap
import FogelPrtcl._
import one.net.NetPrtcl._
import one.task.RandTttPlayer
import one.net.Network
import one.task.TttTask
import one.net.NeuroCon
import one.task.TttTaskPrtcl._
import one.task.TttTaskUtil._ 
import one.task.TttTaskPrtcl.TttStatus._

object FogelPop {
  sealed trait StateName
  case object Ready extends StateName
  case object Selection extends StateName

  sealed trait StateData
  case class ReadyData(gen:Int, nextId:Int, members: Map[Int,Member]) extends StateData
  case class SelectionData(gen:Int, nextId:Int, members: Map[Int,Member], tasksToIds:Map[ActorRef, (Int, Seq[ActorRef])], obs:ActorRef) extends StateData

  val numTrials = 30
}

import FogelPop._

class FogelPop(popSize: Int, numIn:Int, numOut:Int, minHid:Int, maxHid:Int) extends Actor with LoggingFSM[StateName, StateData] {
  def nextTopTpl():TopTpl = {
    val random = new scala.util.Random
    def nextWeight():Double = random.nextDouble - 0.5
    val numHid = random.nextInt(maxHid - minHid) + minHid
    val l1SynTpls = (0 until numIn).flatMap{
      i => (numIn+numOut until numIn+numOut+numHid).map {
        h => SynTpl(i, nextWeight, h)
      }
    }.toSeq
    val l2SynTpls = (numIn+numOut until numIn+numOut+numHid).flatMap{
      h => (numIn until numIn + numOut).map {
        o => SynTpl(h, nextWeight, o)
      }
    }.toSeq
    def sigmoidSumOf(vals: Seq[Double]) = 1 / (1 + math.exp(-vals.sum))
    TopTpl(numIn, numHid, numOut, l1SynTpls ++ l2SynTpls, sigmoidSumOf)
  }

  def initialMembers(popSize:Int):Map[Int,Member] = {
    (0 until popSize).map(x => (x -> Member(0, 0, nextTopTpl))).toMap
  }

  startWith(Ready, ReadyData(0, popSize, initialMembers(popSize)))

  when(Ready) {
    case Event(GetChampion, ReadyData(gen, nextId, members)) => {
      context.sender ! members.values.maxBy {_.taskFit}
      stay using ReadyData(gen, nextId, members)
    }
    case Event(PerfGen, ReadyData(gen, nextId, members)) => {
      val tasksToIds = members.flatMap{ case (i,m) => (0 until numTrials).map(_ => {
        val mPlayer = context.actorOf(Props(classOf[NeuroCon[TttTaskEnv, TttTaskAct]], tttSensorArray _, m.chromo, tttActuatorArray _))
        val rPlayer = context.actorOf(Props[RandTttPlayer])
        val tttTask = context.actorOf(Props(classOf[TttTask], mPlayer, rPlayer, self))
        (tttTask -> (i, Seq(mPlayer, rPlayer, tttTask)))
      })}.toMap
      val resetMembers = members.map{case (i,m) => (i -> m.copy(taskFit = 0))}
      goto(Selection) using SelectionData(gen, nextId, resetMembers, tasksToIds, sender)
    } 
  }

  when(Selection) {
    case Event(TttResult(result), SelectionData(gen, nextId, members,tasksToIds, obs)) => {
      val (id, toStop) = tasksToIds(sender)
      toStop.map(x => context.stop(x))
      val newTasksToIds = tasksToIds - sender
      val member = members(id)
      val taskFitUpdatedMembers = result match {
        case XWon => {
          //log.info("XWon before: " + member.taskFit)
          val newMember = member.copy(taskFit = member.taskFit + 1)
          //log.info("XWon after: " + newMember.taskFit)
          members.updated(id, newMember)
        }
        case OWon => {
          members.updated(id, member.copy(taskFit = member.taskFit - 10))
        }
        case Tie => {
          members
        }
      }
      if(newTasksToIds.isEmpty) {
        val interNetFitUpdatedMembers = taskFitUpdatedMembers.map{case (i, m) => { 
          (i -> m.copy(interNetFit =
            random.shuffle(taskFitUpdatedMembers.values).take(10).map(om => {
              if(m.taskFit > om.taskFit)
                1
              else
                0
            }).sum))
        }}
        //println("max before cull: " +  interNetFitUpdatedMembers.maxBy{_._2.taskFit}._2.taskFit)
        val survivors = interNetFitUpdatedMembers.toIndexedSeq.sortBy{_._2.interNetFit}.takeRight(popSize/2).toMap
       // println("max after cull: " +  survivors.maxBy{_._2.taskFit}._2.taskFit)
        val unmutatedOffspring = survivors.map{case (i, m) => ((nextId + i) -> m)}

        def mutateSynapse(s:SynTpl):SynTpl = {
          s.copy(weight = s.weight + random.nextGaussian)
        }
        def mutateSynapses(xs:Seq[SynTpl]):Seq[SynTpl] = {
          xs.map(s => mutateSynapse(s))
        }
        def mutateChromo(c:TopTpl):TopTpl = {
          c.copy(synTpls = mutateSynapses(c.synTpls))
        }
        def mutateMember(m:Member):Member = {
          m.copy(chromo = mutateChromo(m.chromo))
        }
        val mutatedOffspring = unmutatedOffspring.map(io => (io._1 -> mutateMember(io._2)))
        val newGenMembers = survivors ++ mutatedOffspring
        obs ! members.values.maxBy {_.taskFit}
        goto(Ready) using ReadyData(gen+1, nextId+(popSize/2), newGenMembers)
      }
      else {
        stay using SelectionData(gen, nextId, taskFitUpdatedMembers, newTasksToIds, obs)
      }
    }
  }
}
