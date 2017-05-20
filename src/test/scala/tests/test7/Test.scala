
package tests.test7

import tracecontract._
import tests._

// Demo examples

// -----------------------------------------------------
// Events:
// -----------------------------------------------------

abstract class Event

case class COMMAND(name: String, nr: Int) extends Event
case class SUCCESS(name: String, nr: Int) extends Event
case class FAIL(name: String, nr: Int)    extends Event
case class EVR(message: String)           extends Event

// -----------------------------------------------------
// A trace:
// -----------------------------------------------------

object Trace {
  val trace: List[Event] =
    List(
      COMMAND("STOP_DRIVING", 1),
      COMMAND("TAKE_PICTURE", 2),
      SUCCESS("TAKE_PICTURE", 2),
      SUCCESS("TAKE_PICTURE", 2))
}

import Trace._

// -----------------------------------------------------
// LogScope versus TraceContract:
// -----------------------------------------------------

/*
monitor CommandMustSucceed {
  always {
    COMMAND(n,x) => RequireSuccess(n,x)
  }

  hot RequireSuccess(name,number) {
    FAIL(name,number) => error
    SUCCESS(name,number) => ok
  }
}
*/

class CommandMustSucceed extends Monitor[Event] {
  require {
    case COMMAND(n,x) => RequireSuccess(n,x)
  }

  def RequireSuccess(name: String, number: Int) =
    hot {
      case FAIL(`name`, `number`) => error
      case SUCCESS(`name`, `number`) => ok
    }
}


// -----------------------------------------------------
// Inlining the "RequireSuccess" state:
// -----------------------------------------------------

class CommandMustSucceed2 extends Monitor[Event] {
  require {
    case COMMAND(n, x) => 
      hot {
        case FAIL(`n`, `x`) => error
        case SUCCESS(`n`, `x`) => ok
      }
  }
}


// -----------------------------------------------------
// Linear Temporal Logic:
// -----------------------------------------------------

class CommandMustSucceed3 extends Monitor[Event] {
  require {
    case COMMAND(n, x) => 
        not(FAIL(n, x)) until (SUCCESS(n, x))
  }
}


// -----------------------------------------------------
// Linear Temporal Logic and side effects:
// -----------------------------------------------------

class CommandMustSucceed4 extends Monitor[Event] {
  var count = 0
  require {
    case COMMAND(n, x) if count < 10 => 
        count += 1
        not(FAIL(n, x)) until (SUCCESS(n, x))
  }
}


// -----------------------------------------------------
// Applying:
// -----------------------------------------------------

class IncreasingCommandNumbers extends Monitor[Event] {}

class Requirements extends Monitor[Event] {
  monitor(
    new CommandMustSucceed, 
    new MaxOneSuccess, 
    new IncreasingCommandNumbers
  )
}

object Apply {
  def readLog(): List[Event] = {trace}

  def main(args: Array[String]) {
    val monitor = new Requirements
    val log = readLog()
    monitor.verify(log)
  }
}


// -----------------------------------------------------
// MaxOneSuccess:
// -----------------------------------------------------

class MaxOneSuccess extends Monitor[Event] {
  always {
    case SUCCESS(_, number) =>
      state {
        case SUCCESS(_, `number`) => error
      }
  }
}

