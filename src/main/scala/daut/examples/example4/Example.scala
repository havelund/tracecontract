package daut.examples.example4

import daut._
import daut.Monitor

trait LockEvent
case class acquire(thread: Int, lock: Int) extends LockEvent
case class release(thread: Int, lock: Int) extends LockEvent

/**
  * At most one task can acquire a lock at a time. A task cannot release a lock it has not acquired.
  *
  * This monitor illustrates the find function, which looks for stored facts matching a pattern,
  * and the ensure function, which checks a condition (an assert). This function here in this
  * example tests for the presence of a Locked fact.
  */

class TestMonitor1 extends Monitor[LockEvent] {
  case class Locked(thread: Int, lock: Int) extends state {
    watch {
      case release(thread, lock) => ok
    }
  }

  always {
    case acquire(t, l) => {
      find {
        case Locked(_,`l`) => error("allocated more than once")
      } orelse {
        Locked(t,l)
      }
    }
    case release(t, l) => ensure(Locked(t,l))
  }
}

/**
  * When a task t is acquiring a lock that some other task holds,
  * and t therefore cannot get it, then t is not allowed to hold
  * any other locks (to prevent deadlocks).
  *
  * This monitor illustrates nested find calls, simulating a rule-based
  * style of writing monitors.
  */

class TestMonitor2 extends Monitor[LockEvent] {
  case class Locked(thread: Int, lock: Int) extends state {
    watch {
      case release(thread, lock) => ok
    }
  }

  always {
    case acquire(t, l) => {
      find {
        case Locked(_,`l`) =>
          find {
            case Locked(`t`,x) if l != x => error
          } orelse {
            println("Can't lock but is not holding any other lock, so it's ok")
          }
      } orelse {
        Locked(t,l)
      }
    }
  }
}

/**
  * This monitor illustrates how monitors can be composed in a hierarchy.
  */

class TestMonitor extends Monitor[LockEvent] {
  monitor(
    new TestMonitor1,
    new TestMonitor2
  )
}

object Main {
  def main(args: Array[String]) {
    val m = new TestMonitor
    m.verify(acquire(2,  1))
    m.verify(acquire(2,  5))
    m.verify(acquire(1, 10))
    m.verify(acquire(2, 10))
  }
}