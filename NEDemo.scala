object NEDemo {
  def runDemo():Unit = {
    import akka.actor.{ActorRef, ActorSystem, Actor, FSM, Props}
    import scala.concurrent.duration._
    import collection.breakOut
    import one.method.fogel.FogelPrtcl._
    import one.method.fogel.FogelPop
    import one.method.fogel.FogelPop._
    import akka.pattern.ask
    import akka.util.Timeout
    import scala.concurrent.duration._
    import scala.concurrent.Await
    import one.net.NeuroCon
    import one.task.TttTask
    import one.task.TttTaskPrtcl._
    import one.task.TttTaskUtil._
    import one.task.HumanTttPlayer
    implicit val system = ActorSystem("FogelMethodDemo")


    val random = new scala.util.Random()
    val popSize = 30
    val numIn = 10
    val numOut = 9
    val minHid = 1
    val maxHid = 3
    val pop = system.actorOf(Props(classOf[FogelPop],popSize, numIn, numOut, minHid, maxHid))    
    (0 until 10).map(g => {
      implicit val timeout = Timeout(30 seconds)
      val fChamp = pop ? PerfGen
      val result = Await.result(fChamp, timeout.duration).asInstanceOf[Member]
      println("g"+g+ ": " + result.taskFit)
    })
    implicit val timeout = Timeout(30 seconds)
    val fChamp = pop ? PerfGen
    val result = Await.result(fChamp, timeout.duration).asInstanceOf[Member]

    println("After one final practice for the big game: " + result.taskFit)
    val mPlayer = system.actorOf(Props(classOf[NeuroCon[TttTaskEnv, TttTaskAct]], tttSensorArray _, result.chromo, tttActuatorArray _))
    val hPlayer = system.actorOf(Props[HumanTttPlayer])
    val tttTask = system.actorOf(Props(classOf[TttTask], mPlayer, hPlayer, hPlayer))

    Thread.sleep(100000000)
  }
}
