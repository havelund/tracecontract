/*
  © [2011]. California Institute of Technology.
  ALL RIGHTS RESERVED. U.S. Government sponsorship acknowledged.
  Any commercial use must be negotiated with the Office of Technology
  Transfer at the California Institute of Technology. The technical data in
  this document is controlled under the U.S. Export Regulations, release to
  foreign persons may require an export authorization.
*/

package tracecontract

import java.util.concurrent.TimeUnit
import akka.actor.{ActorSystem, Scheduler}
import scala.concurrent.duration.FiniteDuration
import scala.concurrent.ExecutionContext.Implicits.global

private case class Timer(time: Int)(code: => Unit) {
  private val myRunnable = new Runnable {
    override def run(): Unit = {
      code
      actorSystem.terminate()
    }
  }

  private val actorSystem = ActorSystem.create("timer")
  private val timer = actorSystem.scheduler.scheduleOnce(FiniteDuration(time, TimeUnit.SECONDS), myRunnable)

  def stop() {
    timer.cancel()
    actorSystem.terminate()
  }
}

/*

Old Scala actor version:

private case class Timer(time : Int)(code : => Unit) {
  object DONE

  val timer =
    actor {
      reactWithin(time) {
        case TIMEOUT => code
        case DONE => // exit the actor
      }
    }

  def stop() {
    timer ! DONE
  }
}

 */