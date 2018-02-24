package daut.examples.example2

import daut._
import daut.Monitor

trait TaskEvent
case class start(task: Int) extends TaskEvent
case class stop(task: Int) extends TaskEvent

/**
  *
  * Tasks should be executed (started and stopped) in increasing order according
  * to task numbers, staring from task 0: 0, 1, 2, ...
  *
  * This monitor illustrates next-states (as in Finite State Machines) and
  * state machines.
  */

class TestMonitor extends Monitor[TaskEvent] {

  case class Start(task: Int) extends state {
    wnext {
      case start(`task`) => Stop(task)
    }
  }

  case class Stop(task: Int) extends state {
    next {
      case stop(`task`) => Start(task + 1)
    }
  }

  Start(0)
}

object Main {
  def main(args: Array[String]) {
    val m = new TestMonitor
    m.PRINT = true
    m.verify(start(0))
    m.verify(stop(0))
    m.verify(start(1))
    m.verify(stop(1))
    m.verify(start(2))
    m.verify(stop(2))
    m.end()
  }
}
