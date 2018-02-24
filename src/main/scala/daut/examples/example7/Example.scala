package daut.examples.example7

import daut._
import daut.Monitor

trait Event
case class enter(thread: Int) extends Event
case class exit(thread:Int) extends Event
case class abort(thread:Int) extends Event


/**
  * A task acquiring a lock should eventually release it.

  * This monitor illustrates during predicate and invariants.
  */

class TestMonitor extends Monitor[Event] {
  val critical1 = during(enter(1))(exit(1), abort(1))
  val critical2 = during(enter(2))(exit(2), abort(1))

  invariant {
    !(critical1 && critical2)
  }
}

object Main {
  def main(args: Array[String]) {
    val m = new TestMonitor
    m.PRINT = true

    m.verify(enter(1))
    m.verify(enter(2))
    m.verify(exit(1))
    m.verify(abort(2))
    m.end()
  }
}