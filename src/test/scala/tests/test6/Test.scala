
package tests.test6

import tracecontract._
import tests._
import org.junit.Test
import scala.language.postfixOps

// FM 2011 paper: TraceContract, A Scala DSL for Trace Analysis
// Howard Barringer and Klaus Havelund

// =======
// Events:
// =======

abstract class Event
case class COMMAND(name: String, nr: Int) extends Event
case class SUCCESS(name: String, nr: Int) extends Event
case class FAIL(name: String, nr: Int) extends Event

// ==============
// Example Trace:
// ==============

object ExampleTrace {
  val trace: List[Event] = List(
    COMMAND("STOP_DRIVING", 1), SUCCESS("STOP_DRIVING", 1),
    COMMAND("TAKE_PICTURE", 2), FAIL("TAKE_PICTURE", 2))
}

// ====================
// CommandRequirements:
// ====================

class CommandRequirements extends Monitor[Event] {
  property('R1) {
    always {
      case COMMAND(name, number) =>
        hot {
          case FAIL(`name`, `number`) => error
          case SUCCESS(`name`, `number`) => ok
        }
    }
  }

  property('R2) {
    always {
      case SUCCESS(_, number) =>
        state {
          case SUCCESS(_, `number`) => error
        }
    }
  }

  def invariant(name: Symbol)(block: Block) = property(name) { always { block } }

  var commands: Set[String] = Set()

  invariant('R3) {
    case COMMAND(name, number) =>
      commands += name
      increaseCmdNumber(number) and holdCmd(name, number)
  }

  def increaseCmdNumber(number: Int) =
    state {
      case COMMAND(_, number2) => number2 == number + 1
    }

  def holdCmd(name: String, number: Int) =
    state {
      case COMMAND(`name`, number2) if number2 != number => error
      case SUCCESS(`name`, `number`) => ok
    }

  invariant('R4) {
    case COMMAND(name, number) =>
      not(FAIL(name, number)) until SUCCESS(name, number)
  }

  case class Commanded(name: String, number: Int) extends Fact

  invariant('R5) {
    case COMMAND(name, number) => Commanded(name, number) +
    case SUCCESS(name, number) => Commanded(name, number) -
    case FAIL(name, number) if Commanded(name, number) ~ => error
  }
}

class Test_1 extends Contract[Event] {
  @Test def test() {
    test(new CommandRequirements)(
      COMMAND("A", 1),
      COMMAND("B", 2),
      SUCCESS("A", 1),
      SUCCESS("B", 2))()
  }
}

class Test_2 extends Contract[Event] {
  @Test def test() {
    test(new CommandRequirements)(
      COMMAND("A", 1), // 1
      COMMAND("B", 2), // 2
      FAIL("A", 1), // 3 
      SUCCESS("B", 2), // 4
      SUCCESS("B", 2), // 5
      FAIL("C", 3), // 6
      COMMAND("D", 5) // 7   
      )(
        safe("CommandRequirements", "'R1", (1, 3)),
        live("CommandRequirements", "'R1", (7)),
        safe("CommandRequirements", "'R2", (4, 5)),
        safe("CommandRequirements", "'R3", (2, 4, 7)),
        safe("CommandRequirements", "'R4", (1, 3)),
        live("CommandRequirements", "'R4", (7)),
        safe("CommandRequirements", "'R5", (6)))
  }
}


