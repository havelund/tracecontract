
package tests.test0

import tracecontract._
import tests._
import org.junit.Test

// Examples from the user manual

// =======
// Events:
// =======

abstract class Event
case class COMMAND(name: String, nr: Int) extends Event
case class SUCCESS(name: String, nr: Int) extends Event
case class FAIL(name: String, nr: Int) extends Event

// ======
// R1_R2:
// ======

class R1_R2 extends Monitor[Event] {
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
}

// ==================
// Using the Monitor:
// ==================

object TraceAnalysis {
  def main(args: Array[String]) {
    val trace: List[Event] =
      List(
        COMMAND("STOP_DRIVING", 1),
        COMMAND("TAKE_PICTURE", 2),
        SUCCESS("TAKE_PICTURE", 2),
        SUCCESS("TAKE_PICTURE", 2))

    val monitor = new R1_R2
    monitor.verify(trace)
  }
}

// ===
// R3:
// ===

class R3 extends Monitor[Event] {
  property('R3) {
    always {
      case COMMAND(x, y) =>
        state {
          case SUCCESS(`x`, `y`) =>
            step {
              case SUCCESS(`x`, `y`) =>
                // ----------------------------------------
                strong {
                  case FAIL(`x`, `y`) => ok
                }
            }
        }
    }
  }
}

// ====
// R3_:
// ====

class R3_ extends Monitor[Event] {
  require {
    case COMMAND(x, y) =>
      state {
        case SUCCESS(`x`, `y`) =>
          step {
            case SUCCESS(`x`, `y`) =>
              // ----------------------------------------
              strong {
                case FAIL(`x`, `y`) => ok
              }
          }
      }
  }
}

// ===
// R1:
// ===

class R1 extends Monitor[Event] {
  require {
    case COMMAND(name, number) => ExpectSuccess(name, number)
  }

  def ExpectSuccess(name: String, number: Int): Formula =
    hot {
      case FAIL(`name`, `number`) => error
      case SUCCESS(`name`, `number`) => ok
    }
}

// ===
// R4:
// ===

class R4 extends Monitor[Event] {
  property { S1 }

  def S1: Formula =
    state {
      case COMMAND(name, number) => S2(name, number)
      case _ => error
    }

  def S2(name: String, number: Int): Formula =
    state {
      case SUCCESS(`name`, `number`) => S1
      case _ => error
    }
}

// ===
// R5:
// ===

class R5 extends Monitor[Event] {
  var commands: Set[String] = Set()

  require {
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

  override def finish() {
    println("commands issued: " + commands)
  }
}

// =============
// CollectNames:
// =============

class CollectNames extends Monitor[Event] {
  var commands: Set[String] = Set()

  require {
    case COMMAND(x, _) => commands += x
  }

  override def finish() {
    println("commands issued: " + (commands.mkString("{", ",", "}")))
  }
}

// ===============
// TraceAnalysis2:
// ===============

class Requirements extends Monitor[Event] {
  monitor(new R1_R2, new R3, new R4)
}

class TraceAnalysis2 {
  def readTrace(): List[Event] =
    List(
      COMMAND("STOP_DRIVING", 1),
      COMMAND("TAKE_PICTURE", 2),
      SUCCESS("TAKE_PICTURE", 2),
      SUCCESS("TAKE_PICTURE", 2))

  def main(args: Array[String]) {
    val monitor = new Requirements
    val trace = readTrace()
    monitor.verify(trace)
  }
}

// ===============
// TraceAnalysis3:
// ===============

class TraceAnalysis3 {
  def readTrace(): List[Event] =
    List(
      COMMAND("STOP_DRIVING", 1),
      COMMAND("TAKE_PICTURE", 2),
      SUCCESS("TAKE_PICTURE", 2),
      SUCCESS("TAKE_PICTURE", 2))

  def main(args: Array[String]) {
    val monitor = new Requirements
    val trace = readTrace()
    for (event <- trace) monitor.verify(event)
    monitor.end()
  }
}

// ======
// R6_R7:
// ======

class R6_R7 extends Monitor[Event] {
  property('R6) {
    globally {
      COMMAND("STOP_DRIVING", 10) implies
        (not(FAIL("STOP_DRIVING", 10)) until SUCCESS("STOP_DRIVING", 10))
    }
  }

  property('R7) {
    globally {
      SUCCESS("STOP_DRIVING", 10) implies {
        weaknext {
          never { SUCCESS("STOP_DRIVING", 10) }
        } and
          eventually(COMMAND("TAKE_PICTURE", 11))
      }
    }
  }
}

// ===
// R8:
// ===

class R8 extends Monitor[Event] {
  property {
    globally {
      commandStop implies (not(failStop) until successStop)
    }
  }

  def commandStop = matches {
    case COMMAND("STOP_DRIVING", _) => true
  }

  def failStop = matches {
    case FAIL("STOP_DRIVING", _) => true
  }

  def successStop = matches {
    case SUCCESS("STOP_DRIVING", _) => true
  }
}

// ======
// R1_SL:
// ======

class R1_SL extends Monitor[Event] {
  require {
    case COMMAND(name, number) =>
      hot {
        case FAIL(`name`, `number`) => error
        case SUCCESS(`name`, `number`) => ok
      }
  }
}

// =======
// R1_LTL:
// =======

class R1_LTL extends Monitor[Event] {
  require {
    case COMMAND(name, number) =>
      not(FAIL(name, number)) until SUCCESS(name, number)
  }
}

// ===
// R9:
// ===

class R9 extends Monitor[Event] {
  case class Commanded(name: String, number: Int) extends Fact

  require {
    case COMMAND(name, number) => Commanded(name, number) +
    case SUCCESS(name, number) => Commanded(name, number) -
    case FAIL(name, number) if Commanded(name, number) ~ => error
  }
}

// ========
// R10_R11:
// ========

class R10_R11 extends Monitor[Event] {
  requirement('R10) {
    case COMMAND(name, number) => within(10 seconds) { eventually { SUCCESS(name, number) } }
  }

  requirement('R11) {
    case COMMAND(name, number) => within(10 seconds) { never { FAIL(name, number) } }
  }
}

// =============
// Timed Events:
// =============

case class tCOMMAND(name: String, nr: Int, time: Int) extends Event
case class tSUCCESS(name: String, nr: Int, time: Int) extends Event
case class tFAIL(name: String, nr: Int, time: Int) extends Event

// =======
// R10_v1:
// =======

class R10_v1 extends Monitor[Event] {
  require {
    case tCOMMAND(name, number, time1) =>
      hot {
        case tSUCCESS(`name`, `number`, time2) if (time1, time2) within (10 seconds) => ok
      }
  }
}

// =======
// R10_v2:
// =======

class R10_v2 extends Monitor[Event] {
  require {
    case tCOMMAND(name, number, time1) =>
      hot {
        case tSUCCESS(`name`, `number`, time2) => (time1, time2) within (10 seconds)
      }
  }
}

// =============
// Timed Events:
// =============

abstract class TimedEvent { val time: Int }
case class teCOMMAND(name: String, nr: Int, time: Int) extends TimedEvent
case class teSUCCESS(name: String, nr: Int, time: Int) extends TimedEvent
case class teFAIL(name: String, nr: Int, time: Int) extends TimedEvent

// =======
// R10_v3:
// =======

class R10_v3 extends Monitor[TimedEvent] {
  require {
    case teCOMMAND(name, number, time) =>
      hot {
        case e if (time, e.time) beyond (10 seconds) => error
        case teSUCCESS(`name`, `number`, _) => ok
      }
  }
}

// ======
// Tests:
// ======

class Rs1 extends Monitor[Event] {
  monitor(
    new R1_R2,
    new R3,
    new R3_,
    new R1,
    new R4,
    new R5,
    new R6_R7,
    new R8,
    new R1_SL,
    new R1_LTL,
    new R9,
    new R10_R11,
    new R10_v1,
    new R10_v2)
}

// --------------------------------
// R1_R2   : c -> !f u s | s -> !s
// R3      : c s;s -> Of
// R3_     : c s;s -> Of (same)
// R1      : c -> !f u s
// R4      : (c;s)*   
// R5      : c -> c+1 & !c=/= u s
// R6_R7   : c(STOP_DRIVING,10) -> !f u S 
//         & s(STOP_DRIVING,10) -> Ow[]!s & <>c(TAKE_PICTURE,11)
// R8      : c(STOP_DRIVING,_) -> !f u s 
// R1_SL   : c -> !f u s
// R1_LTL  : c -> !f u s 
// R9      : f -> !s since c
// R10_R11 : c -> <10>s
//         & c -> [10]!f
// --------------------------------

class Test1_1 extends Contract[Event] {
  @Test
  def test1() {
    test(new Rs1)(
      COMMAND("A", 1),
      COMMAND("B", 2),
      SUCCESS("A", 1),
      COMMAND("C", 3),
      SUCCESS("C", 3),
      SUCCESS("B", 2))(
        safe("R4", "", (1, 2)))
  }
}

class Test1_2 extends Contract[Event] {
  @Test
  def test2() {
    test(new Rs1)(
      COMMAND("A", 1),
      SUCCESS("A", 1),
      COMMAND("B", 2),
      SUCCESS("B", 2),
      COMMAND("C", 3),
      SUCCESS("C", 3))()
  }
}

class Test1_3 extends Contract[Event] {
  @Test
  def test3() {
    test(new Rs1)(
      COMMAND("A", 1), // 1
      SUCCESS("A", 1), // 2
      SUCCESS("A", 1), // 3
      COMMAND("B", 2), // 4
      FAIL("B", 2), // 5
      COMMAND("C", 3), // 6
      FAIL("A", 1), // 7
      COMMAND("STOP_DRIVING", 10), // 8
      SUCCESS("STOP_DRIVING", 10), // 9
      SUCCESS("STOP_DRIVING", 10) // 10
      )(
        safe("R1_R2", "'R1", (4, 5)),
        live("R1_R2", "'R1", (6)),
        safe("R1_R2", "'R2", (2, 3)),
        safe("R1_R2", "'R2", (9, 10)),
        safe("R3", "'R3", (1, 2, 3, 4)),
        live("R3", "'R3", (8, 9, 10)),
        safe("R3_", "", (1, 2, 3, 4)),
        live("R3_", "", (8, 9, 10)),
        safe("R1", "", (4, 5)),
        live("R1", "", (6)),
        safe("R4", "", (1, 2, 3)),
        safe("R5", "", (6, 8)),
        safe("R6_R7", "'R7", (9, 10)),
        live("R6_R7", "'R7", (10)),
        safe("R1_SL", "", (4, 5)),
        live("R1_SL", "", (6)),
        safe("R1_LTL", "", (4, 5)),
        live("R1_LTL", "", (6)),
        safe("R9", "", (7)),
        live("R10_R11", "'R10", (4)),
        live("R10_R11", "'R10", (6)),
        safe("R10_R11", "'R11", (4, 5)),
        safe("R10_R11", "'R11", (1, 7)))
  }
}




