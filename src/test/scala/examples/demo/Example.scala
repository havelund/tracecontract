package examples.demo

import tracecontract._

/**
 * Requirements:
 * - CommandsMustSucceed:
 * A command must eventually succeed without a failure before.
 *
 * - OnlyOneSuccess:
 * A command must not succeed more than once.
 *
 * - Alternation:
 * Commands and successes should alternate.
 */

// ====================
// === Event kinds: ===
// ====================

abstract class Event

case class COMMAND(name: String, nr: Int) extends Event

case class SUCCESS(name: String, nr: Int) extends Event

case class FAIL(name: String, nr: Int) extends Event

// ====================================================
// === Properties (implementation of requirements): ===
// ====================================================

class CommandsMustSucceed extends Monitor[Event] {
  require {
    case COMMAND(name, number) =>
      hot {
        case FAIL(`name`, `number`) => error
        case SUCCESS(`name`, `number`) => ok
      }
  }
}

class OnlyOneSuccess extends Monitor[Event](Severity.WARNING) {
  require {
    case SUCCESS(_, number) =>
      state {
        case SUCCESS(_, `number`) => {
          //F()
          error
        }
      }
  }
}

case object SUPERBAD extends Severity {
  val level = 1
}

class Alternation extends Monitor[Event](SUPERBAD) {
  property(S1)

  def S1: Formula =
    weak {
      case COMMAND(name, number) => S2(name, number)
    }

  def S2(name: String, number: Int): Formula =
    weak {
      case SUCCESS(`name`, `number`) => S1
    }
}

// ==============================================
// === Combining properties into one monitor: ===
// ==============================================

class Requirements1 extends Monitor[Event] {
  monitor(
    new CommandsMustSucceed,
    new OnlyOneSuccess)
}

class Requirements2 extends Monitor[Event] {
  monitor(
    new Alternation)
}

class Requirements extends Monitor[Event] {
  monitor(new Requirements1, new Requirements2)
}

// ====================================
// === Performing a trace analysis: ===
// ====================================

object TraceAnalysis1 {

  def main(args: Array[String]) {
    def trace: List[Event] =
      List(
        COMMAND("STOP_DRIVING", 1), // event #1
        COMMAND("TAKE_PICTURE", 2), // event #2
        COMMAND("START_DRIVING", 3), // event #3
        SUCCESS("TAKE_PICTURE", 2), // event #4
        SUCCESS("STOP_DRIVING", 1), // event #5
        SUCCESS("TAKE_PICTURE", 2), // event #6
        COMMAND("STOP_THE_DRIVING", 10), // event #7
        COMMAND("TAKE_THE_PICTURE", 20), // event #8
        COMMAND("START_THE_DRIVING", 30), // event #9
        SUCCESS("TAKE_THE_PICTURE", 20), // event #10
        SUCCESS("STOP_THE_DRIVING", 10), // event #11
        SUCCESS("TAKE_THE_PICTURE", 20)) // event #12

    val monitor = new Requirements
    monitor.verify(trace)
  }
}

