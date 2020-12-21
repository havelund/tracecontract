
package tests.test2

import tracecontract._
import tests._
import org.junit.Test
import scala.language.postfixOps

// ---------------
// Events:
// ---------------

abstract class Event

case class C(name: String) extends Event

case class D(name: String) extends Event

case class S(name: String) extends Event

case class F(name: String) extends Event

case class E(mess: String) extends Event

// ---
// R1:
// ---

class R1 extends Monitor[Event] {
  property('P1) {
    globally {
      (C("A") implies (not(F("A")) until S("A"))) and
        (C("B") implies (not(F("B")) until S("B")))
    }
  }
  property('P2) {
    always {
      case C(x) => (not(F(x)) until S(x))
    }
  }
  property('P3) {
    always {
      case C(x) =>
        hot {
          case F(`x`) => error
          case S(`x`) => ok
        }
    }
  }
}

class Test1_1 extends Contract[Event] {
  @Test def test() {
    test(new R1)(
      C("A"), S("A"),
      C("B"), S("B"),
      C("C"), S("C"))()
  }
}

class Test1_2 extends Contract[Event] {
  @Test def test() {
    test(new R1)(
      C("A"), F("A"),
      C("B"),
      C("C"),
      C("D"), S("D"))(
        safe("R1", "'P1", (1, 2)),
        live("R1", "'P1", (3)),
        safe("R1", "'P2", (1, 2)),
        live("R1", "'P2", (3)),
        live("R1", "'P2", (4)),
        safe("R1", "'P3", (1, 2)),
        live("R1", "'P3", (3)),
        live("R1", "'P3", (4)))
  }
}

// ---
// R2:
// ---

class R2(symbol: String) extends Monitor[Event] {
  var failText1: String = "nothing"
  var failText2: String = "nothing"
  var failText3: String = "nothing"

  requirement('P1) {
    case C(x) => RequireSuccess(x)
  }

  def RequireSuccess(cmdName: String) =
    hot {
      case e @ F(`cmdName`) =>
        failText1 = cmdName
        error(e.toString)
      case S(`cmdName`) => ok
    } onfail {
      failText2 = cmdName
      println(cmdName + " command did not succeed\n")
    }

  requirement('P2) {
    case C(x) =>
      eventually(S(x)) onfail {
        failText3 = x
        println("indeed, command " + x + " did not succeed\n")
      }
  }

  def printState(symb: String) {
    println(symb * 80)
    println("--- failText1 = " + failText1)
    println("--- failText2 = " + failText2)
    println("--- failText3 = " + failText3)
    println(symb * 80)
  }

  override def finish() {
    printState(symbol)
    if (symbol == "2") {
      assert(failText1 == "A")
      assert(failText2 == "B")
      assert(failText3 == "B")
    }
  }
}

class Test2_1 extends Contract[Event] {
  @Test def test() {
    test(new R2("1"))(
      C("A"), S("A"),
      C("B"), S("B"))()
  }
}

class Test2_2 extends Contract[Event] {
  @Test def test() {
    val r2 = new R2("2")
    test(r2)(
      C("A"), F("A"),
      C("B"),
      C("C"), S("C"))(
        safe("R2", "'P1", (1, 2)),
        live("R2", "'P1", (3)),
        live("R2", "'P2", (1)),
        live("R2", "'P2", (3)))
  }
}

class R3 extends Monitor[Event] {
  require {
    case C(x) =>
      eventually(F(x)) or (eventually(D(x)) and eventually(S(x)))
  }

  require {
    case S(x) => E(x + "!") within 2
  }
}

class Test3_1 extends Contract[Event] {
  @Test def test() {
    test(new R3)(
      C("A"), D("A"), S("A"), E("A!"),
      C("B"), F("B"),
      C("C"), S("C"), D("C"), E("C!"))()
  }
}

class Test3_2 extends Contract[Event] {
  @Test def test() {
    test(new R3)(
      C("A"), D("A"), S("A"), E(".."), E(".."), E("A!"),
      C("B"), S("B"),
      C("C"), S("C"), D("C"), E("C!"))(
        live("R3", "", (7, 8)),
        safe("R3", "", (3, 4, 5, 6)),
        safe("R3", "", (8, 9, 10, 11)))
  }
}

class Test3_3 extends Contract[Event] {
  @Test def test() {
    test(new R3)(
      C("A"), D("A"), S("A"), E(".."), E("A!"))()
  }
}

// ===
// R4:
// ===

class R4 extends Monitor[Event] {
  require {
    case C(x) =>
      always {
        case F(`x`) => eventually(C(`x`))
      }
  }

}

class Test4_1 extends Contract[Event] {
  @Test def test() {
    test(new R4)(
      C("A"), F("A"), E(".."), C("A"), F("A"), E(".."), C("A"),
      C("B"),
      C("C"), S("C"))()
  }
}

class Test4_2 extends Contract[Event] {
  @Test def test() {
    test(new R4)(
      C("A"), F("A"), E(".."), C("A"), F("A"), E(".."), C("A"), F("A"),
      C("B"), F("B"), C("B"), F("B"),
      C("C"), S("C"))(
        live("R4", "", (8)),
        live("R4", "", (8)),
        live("R4", "", (8)),
        live("R4", "", (12)),
        live("R4", "", (12)))
  }
}

// ---
// R5:
// ---

class R5 extends Monitor[Event] {
  def S1: Formula =
    state {
      case C(x) => S2(x)
    }

  def S2(x: String): Formula =
    hot {
      case F(`x`) => error
      case C(_) => error
      case S(`x`) => S3(x)
    }

  def S3(x: String) =
    state {
      case E("retry") => S2(x)
      case E("done") => S1
    }

  property(S1)
}

class Test5_1 extends Contract[Event] {
  @Test def test() {
    test(new R5)(
      C("A"), S("A"), E("done"),
      C("B"), S("B"), E("retry"), S("B"), E("done"),
      C("C"), S("C"), E("retry"), S("C"),
      C("D"), S("D"))()
  }
}

class Test5_2 extends Contract[Event] {
  @Test def test() {
    test(new R5)(
      C("A"), E("done"))(
        live("R5", "", (1)))
  }
}

class Test5_3 extends Contract[Event] {
  @Test def test() {
    test(new R5)(
      C("B"), E(".."), C("B"))(
        safe("R5", "", (1, 3)))
  }
}

class Test5_4 extends Contract[Event] {
  @Test def test() {
    test(new R5)(
      C("B"), E(".."), S("B"), E(".."), E("retry"), F("B"))(
        safe("R5", "", (1, 3, 5, 6)))
  }
}

class Test5_5 extends Contract[Event] {
  @Test def test() {
    test(new R5)(
      C("B"), E(".."), S("B"), E(".."), E("retry"), E("done"))(
        live("R5", "", (1, 3, 5)))
  }
}

// ---
// R6:
// ---

class R6 extends Monitor[Event] {
  def P: Formula =
    weak {
      case C(x) => strong {
        case D(`x`) => strong {
          case S(`x`) => P
        }
      }
    }

  property(P)
}

class Test6_1 extends Contract[Event] {
  @Test def test() {
    test(new R6)(
      C("A"), D("A"), S("A"),
      C("B"), D("B"), S("B"),
      C("C"), D("C"), S("C"),
      C("D"), D("D"), S("D"))()
  }
}

class Test6_2 extends Contract[Event] {
  @Test def test() {
    test(new R6)(
      E(".."), C("A"), D("A"), S("A"))(
        safe("R6", "", (1)))
  }
}

class Test6_3 extends Contract[Event] {
  @Test def test() {
    test(new R6)(
      C("A"), E(".."), D("A"), S("A"))(
        safe("R6", "", (1, 2)))
  }
}

class Test6_4 extends Contract[Event] {
  @Test def test() {
    test(new R6)(
      C("A"), D("A"),
      C("B"), D("B"), S("B"))(
        safe("R6", "", (1, 2, 3)))
  }
}

class Test6_5 extends Contract[Event] {
  @Test def test() {
    test(new R6)(
      C("A"), D("A"), S("A"),
      D("B"))(
        safe("R6", "", (1, 2, 3, 4)))
  }
}

// ---
// R7:
// ---

class R7(n: Int) extends Monitor[Event] {
  var counter: Int = 0

  require {
    case C(x) if counter < n => {
      counter += 1
      eventually(S(x))
    }
  }

  override def finish() {
    assert(counter == n)
  }
}

class Test7_1 extends Contract[Event] {
  @Test def test() {
    test(new R7(3))(
      C("A"), D("A"), S("A"),
      C("B"), D("B"), S("B"),
      C("C"), D("C"), S("C"),
      C("D"), D("D"))()
  }
}

class Test7_2 extends Contract[Event] {
  @Test def test() {
    test(new R7(3))(
      C("A"), D("A"), S("A"),
      C("B"), D("B"), S("B"),
      C("C"), D("C"))(
        live("R7", "", (7)))
  }
}

// ---
// R8:
// ---

class R8 extends Monitor[Event] {
  requirement('P1) {
    case C(x) => within(1 second) {
      eventually(S(x))
    }
  }

  requirement('P2) {
    case C(x) =>
      within(1000) {
        hot {
          case S(`x`) => ok
        }
      }
  }

  requirement('P3) {
    case C(x) => within(1 second) {
      never(F(x))
    }
  }

  requirement('P4) {
    case C(x) =>
      within(1000) {
        state {
          case F(`x`) => error
        }
      }
  }
}

// That is:
// 'P12 : C(x) -1000-> C(x)
// 'p34 : C(x) -1000-> !F(x)

class Test8_1 extends Contract[Event] {
  val m = new R8
  m.verify(C("A"))
  Thread.sleep(500)
  m.verify(S("A"))
  m.end()
  val r = m.getMonitorResult
  mustSatisfy(testid, r)
}

class Test8_2 extends Contract[Event] {
  val m = new R8
  m.verify(C("A"))
  m.verify(C("B"))
  Thread.sleep(500)
  m.verify(S("B"))
  m.verify(S("A"))
  m.end()
  val r = m.getMonitorResult
  mustSatisfy(testid, r)
}

class Test8_3 extends Contract[Event] {
  val m = new R8
  m.verify(C("A")) // 1
  m.verify(C("B")) // 2
  m.verify(C("C")) // 3
  Thread.sleep(500)
  m.verify(S("C")) // 4
  m.verify(F("B")) // 5
  Thread.sleep(600)
  // --- deadline passed ---
  m.verify(S("A")) // 6
  m.end()
  val r = m.getMonitorResult
  mustSatisfy(testid, r,
    safe("R8", "'P1", (1, 6)),
    safe("R8", "'P1", (2, 6)),
    safe("R8", "'P2", (1, 6)),
    safe("R8", "'P2", (2, 6)),
    safe("R8", "'P3", (2, 5)),
    safe("R8", "'P4", (2, 5)))
}

// ---
// R9:
// ---

class R9 extends Monitor[Event] {
  require {
    case C(x) =>
      state {
        case S(`x`) =>
          step {
            case S(`x`) =>
              strong {
                case F(`x`) => ok
              }
          }
      }
  }
}

// --- C(x)? --- S(x)? S(x)? F(x)! ---

class Test9_1 extends Contract[Event] {
  @Test def test() {
    test(new R9)(
      C("A"), E("."), S("A"),
      C("B"), E("."), S("B"), E("."), S("B"),
      C("C"), E("."), S("C"), S("C"), F("C"))()
  }
}

class Test9_2 extends Contract[Event] {
  @Test def test() {
    test(new R9)(
      C("A"), E("."), S("A"), S("A"))(
        live("R9", "", (1, 3, 4)))
  }
}

class Test9_3 extends Contract[Event] {
  @Test def test() {
    test(new R9)(
      C("A"), E("."), S("A"), S("A"), E("."))(
        safe("R9", "", (1, 3, 4, 5)))
  }
}

// ----
// R10:
// ----

class R10 extends Monitor[Event] {
  select {
    case C(_) => true
    case S(_) => true
    case F(_) => true
  }

  def S1: Formula =
    weak {
      case C(n) => S2(n)
    }

  def S2(name: String): Formula =
    weak {
      case S(`name`) => S1
    }

  property {
    S1
  }
}

class Test10_1 extends Contract[Event] {
  @Test def test() {
    test(new R10)(
      E("."), C("A"), E("."), S("A"),
      E("."), C("B"), E("."), S("B"),
      E("."), C("C"), E("."), S("C"))()
  }
}

class Test10_2 extends Contract[Event] {
  @Test def test() {
    test(new R10)(
      E("."), C("A"), E("."), C("B"),
      E("."), S("A"), E("."), S("B"))(
        safe("R10", "", (2, 4)))
  }
}

class Test10_3 extends Contract[Event] {
  @Test def test() {
    test(new R10)(
      S("?"), C("A"), E("."), S("A"),
      E("."), C("B"), E("."), S("B"))(
        safe("R10", "", (1)))
  }
}

// ----
// R11:
// ----

class R11 extends Monitor[Event] {
  def S1: Formula =
    state {
      case C(n) => S2(n)
      case F(_) | S(_) => error
    }

  def S2(name: String): Formula =
    state {
      case S(`name`) => S1
      case F(_) | C(_) => error
    }

  property {
    S1
  }
}

class Test11_1 extends Contract[Event] {
  @Test def test() {
    test(new R11)(
      E("."), C("A"), E("."), S("A"),
      E("."), C("B"), E("."), S("B"),
      E("."), C("C"), E("."), S("C"))()
  }
}

class Test11_2 extends Contract[Event] {
  @Test def test() {
    test(new R11)(
      E("."), C("A"), E("."), C("B"),
      E("."), S("A"), E("."), S("B"))(
        safe("R11", "", (2, 4)))
  }
}

class Test11_3 extends Contract[Event] {
  @Test def test() {
    test(new R11)(
      S("?"), C("A"), E("."), S("A"),
      E("."), C("B"), E("."), S("B"))(
        safe("R11", "", (1)))
  }
}

// ----
// R12:
// ----

class R12 extends Monitor[Event] {
  requirement('Eq) {
    case C(n) => eventuallyEq(3) {
      S(n)
    }
  }
  requirement('Le) {
    case C(n) => eventuallyLe(3) {
      S(n)
    }
  }
  requirement('Lt) {
    case C(n) => eventuallyLt(3) {
      S(n)
    }
  }
  requirement('Ge) {
    case C(n) => eventuallyGe(3) {
      S(n)
    }
  }
  requirement('Gt) {
    case C(n) => eventuallyGt(3) {
      S(n)
    }
  }
  requirement('Bw) {
    case C(n) => eventuallyBw(3, 5) {
      S(n)
    }
  }
}

class Test12_1 extends Contract[Event] {
  @Test def test() {
    test(new R12)(
      //   1       2       3     4(3)
      C("A"), E("."), E("."), S("A"), // violates: Lt[1], Gt[2] 
      //   5       6     7(2)      8
      C("B"), E("."), S("B"), E("."), // violates: Eq[3], Ge[4], Gt[5], Bw[6]
      //   9      10      11      12     13(4)
      C("C"), E("."), E("."), E("."), S("C"), // violates: Eq[7], Le[8], Lt[9]
      //  14      15      16      17      18     19(5)
      C("D"), E("."), E("."), E("."), E("."), S("D"), // violates: Eq[10], Le[11], Lt[12]
      //  20      21      22      23      24      25     26(6)
      C("E"), E("."), E("."), E("."), E("."), E("."), S("E"), // violates Eq[13], Le[14], Lt[15], Bw[16]
      //  27      28
      C("F"), E(".") // violates: Eq[17], Le[18], Lt[19], Ge[20], Gt[21], Bw[22]
      )(
        safe("R12", "'Eq", (5, 6, 7)), // 3
        safe("R12", "'Eq", (9, 10, 11, 12)), // 7
        safe("R12", "'Eq", (14, 15, 16, 17)), // 10
        safe("R12", "'Eq", (20, 21, 22, 23)), // 13
        live("R12", "'Eq", (27, 28)), // 17
        safe("R12", "'Le", (9, 10, 11, 12)), // 8
        safe("R12", "'Le", (14, 15, 16, 17)), // 11
        safe("R12", "'Le", (20, 21, 22, 23)), // 14
        live("R12", "'Le", (27, 28)), // 18
        safe("R12", "'Lt", (1, 2, 3)), // 1
        safe("R12", "'Lt", (9, 10, 11)), // 9
        safe("R12", "'Lt", (14, 15, 16)), // 12
        safe("R12", "'Lt", (20, 21, 22)), // 15
        live("R12", "'Lt", (27, 28)), // 19
        safe("R12", "'Ge", (5, 6, 7)), // 4
        live("R12", "'Ge", (27, 28)), // 20
        safe("R12", "'Gt", (1, 2, 3, 4)), // 2
        safe("R12", "'Gt", (5, 6, 7)), // 5
        live("R12", "'Gt", (27, 28)), // 21
        safe("R12", "'Bw", (5, 6, 7)), // 6
        safe("R12", "'Bw", (20, 21, 22, 23, 24, 25)), // 16
        live("R12", "'Bw", (27, 28)) // 22
        )
  }
}

// ----
// R13:
// ----

case class tC(name: String, time: Int) extends Event

case class tS(name: String, time: Int) extends Event

case class tF(name: String, time: Int) extends Event

class R13 extends Monitor[Event] {
  require {
    case tC(n, t1) =>
      hot {
        case tS(`n`, t2) => (t1, t2) within (10 seconds)
      }
  }
}

class Test13_1 extends Contract[Event] {
  @Test def test() {
    test(new R13)(
      tC("A", 10000), E("."), tS("A", 15000),
      tC("B", 20000), E("."), tS("B", 25000))()
  }
}

class Test13_2 extends Contract[Event] {
  @Test def test() {
    test(new R13)(
      tC("A", 10000), E("."), tS("A", 100000),
      tC("B", 20000), E("."), tS("B", 200000))(
        safe("R13", "", (1, 3)),
        safe("R13", "", (4, 6)))
  }
}

// ----
// R14:
// ---

class R14 extends Monitor[Event] {
  property {
    globally {
      CA implies (not(FA) until SA)
    }
  }

  def CA = matches {
    case C("A") => true
  }

  def FA = matches {
    case F("A") => true
  }

  def SA = matches {
    case S("A") => true
  }
}

class Test14_1 extends Contract[Event] {
  @Test def test() {
    test(new R14)(
      C("A"), E("."), S("A"),
      C("B"), E("."),
      C("A"), E("."), C("A"), S("A"))()
  }
}

class Test14_2 extends Contract[Event] {
  @Test def test() {
    test(new R14)(
      C("A"), E("."), F("A"),
      C("B"), E("."),
      C("A"), E("."), C("A"))(
        safe("R14", "", (1, 3)),
        live("R14", "", (6)),
        live("R14", "", (8)))
  }
}

// ----
// R15:
// ----

class R15 extends Monitor[Event] {

  case class Commanded(name: String) extends Fact

  require {
    case C(n) => Commanded(n) +
    case S(n) => Commanded(n) -
    case F(n) if Commanded(n) ~ => error
  }
}

class Test15_1 extends Contract[Event] {
  @Test def test() {
    test(new R15)(
      C("A"), E("."), F("A"), S("A"),
      C("B"), E("."), S("B"),
      C("C"), E("."), F("C"), E("."))()
  }
}

class Test15_2 extends Contract[Event] {
  @Test def test() {
    test(new R15)(
      C("A"), E("."), S("A"), F("D"),
      C("B"), E("."), F("B"),
      C("C"), E("."), S("C"), F("C"))(
        safe("R15", "", (4)),
        safe("R15", "", (11)))
  }
}

// ----
// R16:
// ----

class R16 extends Monitor[Event] {

  case class Commanded(name: String) extends Fact

  require {
    case C(n) => Commanded(n) +
    case S(n) => Commanded(n) -
    case F(n) if Commanded(n) ? => error
  }
}

class Test16_1 extends Contract[Event] {
  @Test def test() {
    test(new R16)(
      C("A"), E("."), S("A"), F("A"),
      F("B"),
      C("C"), E("."), S("C"), F("C"))()
  }
}

class Test16_2 extends Contract[Event] {
  @Test def test() {
    test(new R16)(
      C("A"), E("."), F("A"), S("A"),
      F("B"),
      C("C"), E("."), F("C"), S("C"), F("C"), F("D"))(
        safe("R16", "", (3)),
        safe("R16", "", (8)))
  }
}

// ----
// R17:
// ----

class R17 extends Monitor[Event] {

  case class Commanded(name: String) extends Fact

  requirement('P1) {
    case C(n) => Commanded(n) +
    case S(n) => Commanded(n) ?-
  }

  requirement('P2) {
    case C(n) => Commanded(n) +
    case S(n) if Commanded(n) ~ => error
    case S(n) if Commanded(n) ? => Commanded(n) -
  }
}

class Test17_1 extends Contract[Event] {
  @Test def test() {
    test(new R17)(
      C("A"), E("."), S("A"),
      C("B"), E("."), S("B"))()
  }
}

class Test17_2 extends Contract[Event] {
  @Test def test() {
    test(new R17)(
      C("A"), E("."), S("A"), S("D"),
      C("B"), E("."), S("C"), S("C"))(
        safe("R17", "'P1", (4)),
        safe("R17", "'P1", (7)),
        safe("R17", "'P1", (8)),
        safe("R17", "'P2", (4)),
        safe("R17", "'P2", (7)),
        safe("R17", "'P2", (8)))
  }
}

// ----
// R18:
// ----

class R18 extends Monitor[Event] {

  case class Commanded(name: String) extends Fact

  require {
    case C(n) => Commanded(n) ~+
    case S(n) => Commanded(n) -
  }
}

class Test18_1 extends Contract[Event] {
  @Test def test() {
    test(new R18)(
      C("A"), E("."), S("A"),
      C("B"), E("."), S("C"))()
  }
}

class Test18_2 extends Contract[Event] {
  @Test def test() {
    test(new R18)(
      C("A"), E("."), C("A"), S("A"),
      C("B"), E("."), S("B"),
      C("C"), C("C"), S("C"), C("C"), S("C"))(
        safe("R18", "", (3)),
        safe("R18", "", (9)))
  }
}

