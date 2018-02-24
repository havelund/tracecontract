package daut.examples.example3

import daut._
import daut.Monitor

trait LockEvent
case class acquire(thread: Int, lock: Int) extends LockEvent
case class release(thread: Int, lock: Int) extends LockEvent

/**
  * An monitor acquired by a task t should not be released by any other task while
  * acquired by t, that is, until it is released by t.
  *
  * This monitor illustrates until states.
  */

class TestMonitor extends Monitor[LockEvent] {
  always {
    case acquire(t, l) =>
      until {
        case release(`t`, `l`) => ok
      } watch {
        case release(_, `l`) => error
      }
  }
}

object Main {
  def main(args: Array[String]) {
    val m = new TestMonitor
    m.verify(acquire(1, 10))
    m.verify(release(2, 10))
  }
}


