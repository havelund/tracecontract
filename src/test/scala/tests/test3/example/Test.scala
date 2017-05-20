package tests.test3.example

import tracecontract._
import tests._
import org.junit.Test

// Scala Days 2011 paper: Checking Flight Rules with TraceContract: Application of a Scala DSL for Trace Analysis  
// H. Barringer, K. Havelund and R. Morris                                                             

// initial example

// =======
// Events:
// =======

abstract class Event
case class COMMAND(name: String) extends Event
case class SUCCESS(name: String) extends Event
case class FAIL(name: String) extends Event

// ====================
// CommandRequirements:
// ====================

class CommandRequirements extends Monitor[Event] {
  property('R1) {
    always {
      case COMMAND(name) =>
        hot {
          case FAIL(`name`) => error
          case SUCCESS(`name`) => ok
        }
    }
  }

  property('R2) {
    always {
      case SUCCESS(name) =>
        state {
          case SUCCESS(`name`) => error
          case COMMAND(`name`) => ok
        }
    }
  }
}

// Use monitor:

object TraceAnalysis {
  def main(args: Array[String]) {
    val trace: List[Event] =
      List(
        COMMAND("STOP_DRIVING"),
        COMMAND("TAKE_PICTURE"),
        SUCCESS("STOP_DRIVING"),
        SUCCESS("STOP_DRIVING"))
    val monitor = new CommandRequirements
    monitor.verify(trace)
  }
}

class Test_1 extends Contract[Event] {
  @Test def test() {
    test(new CommandRequirements)(
      COMMAND("STOP_DRIVING"),
      COMMAND("TAKE_PICTURE"),
      SUCCESS("STOP_DRIVING"),
      SUCCESS("TAKE_PICTURE"))()
  }
}

class Test_2 extends Contract[Event] {
  @Test def test() {
    test(new CommandRequirements)(
      COMMAND("STOP_DRIVING"), // 1
      COMMAND("TAKE_PICTURE"), // 2
      FAIL("STOP_DRIVING"), // 3
      SUCCESS("TAKE_PICTURE"), // 4
      SUCCESS("TAKE_PICTURE") // 5
      )(
        safe("CommandRequirements", "'R1", (1, 3)),
        safe("CommandRequirements", "'R2", (4, 5)))
  }
}
