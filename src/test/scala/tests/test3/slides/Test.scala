package tests.test3.slides

import tracecontract._
import tests._
import scala.language.postfixOps

// =======
// Events:
// =======

abstract class Event
case class COMMAND(name: String, nr: Int) extends Event
case class SUCCESS(name: String, nr: Int) extends Event
case class FAIL   (name: String, nr: Int) extends Event

class CommandMustSucceed1 extends Monitor[Event] {
  require {
    case COMMAND(n,x) => RequireSuccess(n,x)
  }

  def RequireSuccess(name: String, number: Int) =
    hot {
      case FAIL(`name`, `number`) => error
      case SUCCESS(`name`, `number`) => ok
    }
}

class CommandMustSucceed2 extends Monitor[Event] {
  require {
    case COMMAND(n, x) =>
      hot {
        case FAIL(`n`, `x`) => error
        case SUCCESS(`n`, `x`) => ok
      } 
  }
}

class CommandMustSucceed3 extends Monitor[Event] {
  require {
    case COMMAND(n, x) =>
      not(FAIL(n, x)) until (SUCCESS(n, x))
  }
}

class CommandMustSucceed4 extends Monitor[Event] {
  var count = 0
  require {
    case COMMAND(n, x) if count < 10 =>
      count += 1
      not(FAIL(n, x)) until (SUCCESS(n, x))
  }
}

class LTL extends Monitor[Event] {
  property {
    globally(
      COMMAND("A",42) implies 
        not(FAIL("A", 42)) until (SUCCESS("A", 42))
    )
  }  
}

class SuccessHasAReason extends Monitor[Event] {
  case class Commanded(name: String, nr: Int) extends Fact

  require {
    case COMMAND(n,x) => Commanded(n,x) +
    case SUCCESS(n,x) => Commanded(n,x) ?-
  }
}
