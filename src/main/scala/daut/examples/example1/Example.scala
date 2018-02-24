package daut.examples.example1

import daut._
import daut.Monitor

trait LockEvent
case class acquire(thread: Int, lock: Int) extends LockEvent
case class release(thread: Int, lock: Int) extends LockEvent

/**
  * A task acquiring a lock should eventually release it. At most one task
  * can acquire a lock at a time. A task cannot release a lock it has not acquire.

  * This monitor illustrates always states, hot states, and the use of a
  * fact (Locked) to record history. This is effectively in part a past time property.
  */

class TestMonitor extends Monitor[LockEvent] {
  case class Locked(thread: Int, lock: Int) extends state {
    hot {
      case acquire(_, `lock`) => error
      case release(`thread`, `lock`) => ok
    }
  }

  always {
    case acquire(t, l)                  => Locked(t, l)
    case release(t, l) if !Locked(t, l) => error
  }
}

object Main {
  def main(args: Array[String]) {
    val m = new TestMonitor
    m.verify(acquire(1, 10))
    m.verify(acquire(2, 10))
  }
}
