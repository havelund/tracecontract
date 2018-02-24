package daut.examples.example5

import daut._
import daut.Monitor

trait LockEvent
case class acquire(thread: Int, lock: Int) extends LockEvent
case class release(thread: Int, lock: Int) extends LockEvent

/**
  * A task acquiring a lock should eventually release it.

  * This monitor illustrates simple response properties.
  */

class TestMonitor extends Monitor[LockEvent] {
  always {
    case acquire(t, l) =>
      hot {
        case release(`t`, `l`) => ok
      }
  }
}

object Main {
  def main(args: Array[String]) {
    val m = new TestMonitor
    m.verify(acquire(1, 10))
    m.verify(release(2, 10))
    m.end()
  }
}



