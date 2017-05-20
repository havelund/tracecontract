
package tests.test1

import tracecontract._
import tests._
import org.junit.Test

// Examples corresponding to LogScope specifications

// ---------------
// Events:
// ---------------

abstract class Event
case class COMMAND(Type: String, Stem: String, Number: Int) extends Event
case class SUCCESS(Stem: String, Number: Int) extends Event
case class DISPATCH(Stem: String, Number: Int) extends Event
case class DISPATCHFAILURE(Stem: String, Number: Int) extends Event
case class FAILURE(Stem: String, Number: Int) extends Event
case class CHANNEL(Status1: Int, Status8: Int) extends Event
case class PRODUCT(ImageSize: Int) extends Event
case class EVR(message: String) extends Event
case class tCOMMAND(Type: String, Stem: String, Number: Int, Time: Int) extends Event
case class tSUCCESS(Stem: String, Number: Int, Time: Int) extends Event

// ===============
// Util for tests:
// ===============

// making writing tests easier

object UtilForTests {
  def C(Type: String, Stem: String, Number: Int) = COMMAND(Type, Stem, Number)
  def S(Stem: String, Number: Int) = SUCCESS(Stem, Number)
  def D(Stem: String, Number: Int) = DISPATCH(Stem, Number)
  def DF(Stem: String, Number: Int) = DISPATCHFAILURE(Stem, Number)
  def F(Stem: String, Number: Int) = FAILURE(Stem, Number)
  def CH(Status1: Int, Status8: Int) = CHANNEL(Status1, Status8)
  def P(ImageSize: Int) = PRODUCT(ImageSize)
  def E(message: String) = EVR(message)
  def tC(Type: String, Stem: String, Number: Int, Time: Int) = tCOMMAND(Type, Stem, Number, Time)
  def tS(Stem: String, Number: Int, Time: Int) = tSUCCESS(Stem, Number, Time)
}
import UtilForTests._

// ---
// M1:
// ---

// --------------------------------------------------
// LogScope spec: 
// --------------------------------------------------
// pattern P1:
//   COMMAND{Type:"FSW",Stem:x,Number:y} =>
//     EVR{Success:x,Number:y}
// --------------------------------------------------

// --------------------------------------------------
// LogScope spec: 
// --------------------------------------------------
// pattern P2:
//   COMMAND{Type:"FSW",Stem:x,Number:y} =>
//     !EVR{Failure:x,Number:y}
// --------------------------------------------------

class M1 extends Monitor[Event] {
  def P1 = always { case COMMAND("FSW", x, y) => eventually(SUCCESS(x, y)) }
  def P2 = always { case COMMAND("FSW", x, y) => never(FAILURE(x, y)) }
  property('P1) { P1 }
  property('P2) { P2 }
}

class Test1_1 extends Contract[Event] {
  @Test
  def test() {
    test(new M1)(
      C("FSW", "A", 1), S("A", 1),
      C("FSW", "B", 2), S("B", 2))()
  }
}

class Test1_2 extends Contract[Event] {
  @Test
  def test() {
    test(new M1)(
      C("FSW", "A", 1), F("A", 1), S("A", 1),
      C("FSW", "B", 2), D("B", 2), S("B", 2), S("B", 2),
      C("FSW", "C", 3))(
        live("M1", "'P1", (8)),
        safe("M1", "'P2", (1, 2)))
  }
}

// ---
// M2:
// ---

// --------------------------------------------------
// LogScope spec: 
// --------------------------------------------------
// pattern P3 :
//   COMMAND{Type:"FSW",Stem:x,Number:y} =>
//     [
//       !EVR{DispatchFailure:x,Number:y},
//        EVR{Dispatch:x,Number:y},
//       !EVR{Failure:x,Number:y},
//        EVR{Success:x,Number:y},
//       !EVR{Success:x,Number:y}
// ]
// --------------------------------------------------

class M2 extends Monitor[Event] {
  def P3 =
    always {
      case COMMAND("FSW", x, y) =>
        hot {
          case DISPATCHFAILURE(`x`, `y`) => error
          case DISPATCH(`x`, `y`) =>
            hot {
              case FAILURE(`x`, `y`) => error
              case SUCCESS(`x`, `y`) =>
                state {
                  case SUCCESS(`x`, `y`) => error
                }
            }
        }
    }
  property('P3) { P3 }
}

class Test2_1 extends Contract[Event] {
  @Test
  def test() {
    test(new M2)(
      C("FSW", "A", 1), D("A", 1), S("A", 1),
      C("FSW", "B", 2), D("B", 2), S("B", 2))()
  }
}

class Test2_2 extends Contract[Event] {
  @Test
  def test() {
    test(new M2)(
      C("FSW", "A", 1), DF("A", 1),
      C("FSW", "B", 2), S("B", 2),
      C("FSW", "C", 3), D("C", 3), F("C", 3),
      C("FSW", "D", 4), D("D", 4), S("D", 4), S("D", 4),
      C("FSW", "E", 5), D("E", 5), S("E", 5))(
        safe("M2", "'P3", (1, 2)),
        safe("M2", "'P3", (5, 6, 7)),
        safe("M2", "'P3", (8, 9, 10, 11)),
        live("M2", "'P3", (3)))
  }
}

// ---
// M3:
// ---

// --------------------------------------------------
// LogScope spec: 
// --------------------------------------------------
// pattern P4 :
//   COMMAND{Type:"FSW",Stem:x,Number:y} =>
//     {
//       EVR{Dispatch:x,Number:y},
//       [
//         EVR{Success:x,Number:y},
//        !EVR{Success:x,Number:y}
//       ],
//      !EVR{DispatchFailure:x,Number:y},
//      !EVR{Failure:x,Number:y}
//   }
// --------------------------------------------------

class M3 extends Monitor[Event] {
  def P4 =
    always {
      case COMMAND("FSW", x, y) =>
        eventually(DISPATCH(x, y)) and
          hot {
            case SUCCESS(`x`, `y`) =>
              state {
                case SUCCESS(`x`, `y`) => error
              }
          } and
          never(DISPATCHFAILURE(x, y)) and
          never(FAILURE(x, y))
    }
  property('P4) { P4 }
}

class Test3_1 extends Contract[Event] {
  @Test
  def test() {
    test(new M3)(
      C("FSW", "A", 1), D("A", 1), S("A", 1),
      C("FSW", "B", 2), D("B", 2), S("B", 2))()
  }
}

class Test3_2 extends Contract[Event] {
  @Test
  def test() {
    test(new M3)(
      C("FSW", "A", 1), DF("A", 1),
      C("FSW", "B", 2), S("B", 2),
      C("FSW", "C", 3), D("C", 3), F("C", 3),
      C("FSW", "D", 4), D("D", 4), S("D", 4), S("D", 4),
      C("FSW", "E", 5), D("E", 5), S("E", 5))(
        safe("M3", "'P4", (1, 2)),
        safe("M3", "'P4", (5, 6, 7)),
        safe("M3", "'P4", (8, 9, 10, 11)),
        live("M3", "'P4", (3, 4)))
  }
}

// ---
// M4:
// ---

// --------------------------------------------------
// LogScope spec: 
// --------------------------------------------------
// pattern P5 :
//   EVR{Success:_,Number:y} =>
//     !EVR{Success:_,Number:z} where {: z <= y :} 
// --------------------------------------------------

class M4 extends Monitor[Event] {
  def P5 =
    always {
      case SUCCESS(_, y) =>
        state {
          case SUCCESS(_, z) if z <= y => error
        }
    }
  property('P5) { P5 }
}

class Test4_1 extends Contract[Event] {
  @Test
  def test() {
    test(new M4)(
      C("FSW", "A", 1), S("A", 1),
      C("FSW", "B", 2), S("B", 2))()
  }
}

class Test4_2 extends Contract[Event] {
  @Test
  def test() {
    test(new M4)(
      C("FSW", "A", 1),
      C("FSW", "B", 2), S("B", 2),
      S("A", 1))(
        safe("M4", "'P5", (3, 4)))
  }
}

// ---
// M5:
// ---

// --------------------------------------------------
// LogScope spec: 
// --------------------------------------------------
// {:
// def within(t1,t2,max):
//   return (t2-t1) <= max
// :}
//
// pattern P6:
//   COMMAND{Type:"FSW",Stem:x,Number:y,Time:t1}
//     where {:x.startswith("PWR"):} =>
//       EVR{Success:x,Number:y,Time:t2}
//         where within(t1,t2,10000)
// --------------------------------------------------

class M5 extends Monitor[Event] {
  def within(t1: Int, t2: Int, max: Int) = (t2 - t1) <= max

  def P6 =
    always {
      case tCOMMAND("FSW", x, y, t1) if x startsWith "PWR" =>
        hot {
          case tSUCCESS(`x`, `y`, t2) if within(t1, t2, 10000) => ok
        }
    }
  property('P6) { P6 }
}

class Test5_1 extends Contract[Event] {
  @Test
  def test() {
    test(new M5)(
      tC("FSW", "PWR_A", 1, 1000), tS("PWR_A", 1, 3000),
      tC("FSW", "PWR_B", 2, 4000), tS("PWR_B", 2, 8000))()
  }
}

class Test5_2 extends Contract[Event] {
  @Test
  def test() {
    test(new M5)(
      tC("FSW", "PWR_A", 1, 1000), tS("PWR_A", 1, 12000),
      tC("FSW", "PWR_B", 2, 14000), tS("PWR_B", 2, 25000),
      tC("FSW", "PWR_B", 2, 30000), tS("PWR_B", 2, 35000))(
        live("M5", "'P6", (1)),
        live("M5", "'P6", (3)))
  }
}

// ---
// M6:
// ---

// --------------------------------------------------
// LogScope spec: 
// --------------------------------------------------
// pattern P7 :
//   COMMAND{Type:"FSW",Stem:"PICT"} =>
//     [
//       CHANNEL{Status1:{0 : 1, 4 : x}},
//       CHANNEL{Status8:{2 : x}},
//       PRODUCT{ImageSize:[1000,2000]}
//     ]  
// -------------------------------------------------- 

class M6 extends Monitor[Event] {
  def P7 =
    always {
      case COMMAND("FSW", "PICT", _) =>
        hot {
          case CHANNEL(status1, _) if (status1 bit 0) == 1 =>
            val x = status1 bit 2
            hot {
              case CHANNEL(_, status8) if (status8 bit 0) == x =>
                hot {
                  case PRODUCT(imagesize) if imagesize inrange (1000, 2000) => ok
                }
            }
        }
    }
  property('P7) { P7 }
}

class Test6_1 extends Contract[Event] {
  @Test
  def test() {
    test(new M6)(
      C("FSW", "PICT", 1), CH(5, 0), CH(0, 5), P(1500))()
  }
}

class Test6_2 extends Contract[Event] {
  @Test
  def test() {
    test(new M6)(
      C("FSW", "PICT", 1), CH(5, 0), CH(0, 2), P(1500))(
        live("M6", "'P7", (1, 2)))
  }
}

// ---
// M7:
// ---

// --------------------------------------------------
// LogScope spec: 
// -------------------------------------------------- 
// {:
//   counter = 0
//
//   def count():
//     global counter
//     counter = counter + 1
//
//   def amongstfirst(limit):
//     return counter < limit
// :}
//
// pattern P8 :
//   COMMAND{Stem:x} where amongstfirst(1) do count() =>
//     EVR{Success:x} do {: print x + " succeeded" :}  
// -------------------------------------------------- 

class M7 extends Monitor[Event] {
  var counter = 0
  def count() { counter += 1 }
  def amongstfirst(limit: Int) = counter < limit

  def P8 =
    always {
      case COMMAND(_, x, _) if amongstfirst(2) => {
        count()
        hot {
          case SUCCESS(`x`, _) => println(x + " succeeded")
        }
      }
    }
  property('P8) { P8 }
}

class Test7_1 extends Contract[Event] {
  @Test
  def test() {
    test(new M7)(
      C("FSW", "A", 1), S("A", 1),
      C("FSW", "B", 2), S("B", 2),
      C("FSW", "C", 3))()
  }
}

class Test7_2 extends Contract[Event] {
  @Test
  def test() {
    test(new M7)(
      C("FSW", "A", 1), S("A", 1),
      C("FSW", "B", 2),
      C("FSW", "C", 3))(
        live("M7", "'P8", (3)))
  }
}

// ---
// M8:
// ---

// --------------------------------------------------
// LogScope spec: 
// -------------------------------------------------- 
// pattern P9 :
//   COMMAND{Type:"FSW",Stem:x,Number:y} =>
//     {
//       EVR{Dispatch:x,Number:y},
//       [
//         EVR{Success:x,Number:y},
//        !EVR{Success:x,Number:y}
//       ],
//       !EVR{DispatchFailure:x,Number:y},
//       !EVR{Failure:x,Number:y}
//     }
//     upto COMMAND{Type:"FSW"} 
// -------------------------------------------------- 

class M8 extends Monitor[Event] {
  def P9 =
    always {
      case COMMAND("FSW", x, y) =>
        eventually(DISPATCH(x, y)) and
          hot {
            case SUCCESS(`x`, `y`) =>
              state {
                case SUCCESS(`x`, `y`) => error
              }
          } and
          never(DISPATCHFAILURE(x, y)) and
          never(FAILURE(x, y))
    } upto {
      case COMMAND("FSW", "C", _) => true
    }

  property('P9) { P9 }
}

class Test8_1 extends Contract[Event] {
  @Test
  def test() {
    test(new M8)(
      C("FSW", "A", 1), D("A", 1), S("A", 1),
      C("FSW", "B", 2), D("B", 2), S("B", 2),
      C("FSW", "C", 3), DF("B", 2),
      S("A", 1))()
  }
}

class Test8_2 extends Contract[Event] {
  @Test
  def test() {
    test(new M8)(
      C("FSW", "A", 1), D("A", 1), S("A", 1),
      C("FSW", "B", 2),
      C("FSW", "C", 3),
      DF("B", 2))(
        safe("M8", "'P9", (1, 2, 3, 4, 5)))
  }
}

class Test8_3 extends Contract[Event] {
  @Test
  def test() {
    test(new M8)(
      C("FSW", "A", 1), F("A", 1),
      C("FSW", "B", 2),
      C("FSW", "C", 3),
      F("B", 2), S("C", 3))(
        safe("M8", "'P9", (1, 2)))
  }
}

// observation: due to the fact that always is not at the
// outermost level, as soon as an error is detected,
// the monitor goes down.

// ---
// M9:
// ---

// --------------------------------------------------
// LogScope spec: 
// -------------------------------------------------- 
// automaton A_P3 {
//   always S1 {
//     COMMAND{Type:"FSW",Stem:x,Number:y} => 
//       S2(x,y)
//   }
//
//   hot state S2(x,y) {
//     EVR{DispatchFailure:x,Number:y} => error
//     EVR{Dispatch:x,Number:y} => S3(x,y)
//   }
//
//   hot state S3(x,y) {
//     EVR{Failure:x,Number:y} => error
//     EVR{Success:x,Number:y} => S4(x,y)
//   }
//
//   state{} S4(x,y) {
//     EVR{Success:x,Number:y} => error
//   }
// }
// -------------------------------------------------- 

class M9 extends Monitor[Event] {
  require {
    case COMMAND("FSW", x, y) => S2(x, y)
  }

  def S2(x: String, y: Int) =
    hot {
      case DISPATCHFAILURE(`x`, `y`) => error
      case DISPATCH(`x`, `y`) => S3(x, y)
    }

  def S3(x: String, y: Int) =
    hot {
      case FAILURE(`x`, `y`) => error
      case SUCCESS(`x`, `y`) => S4(x, y)
    }

  def S4(x: String, y: Int) =
    state {
      case SUCCESS(`x`, `y`) => error
    }
}

class Test9_1 extends Contract[Event] {
  @Test
  def test() {
    test(new M9)(
      C("FSW", "A", 1), D("A", 1), S("A", 1),
      C("FSW", "B", 2), D("B", 2), S("B", 2))()
  }
}

class Test9_2 extends Contract[Event] {
  @Test
  def test() {
    test(new M9)(
      C("FSW", "A", 1), DF("A", 1),
      C("FSW", "B", 2), S("B", 2),
      C("FSW", "C", 3), D("C", 3), F("C", 3),
      C("FSW", "D", 4), D("D", 4), S("D", 4), S("D", 4),
      C("FSW", "E", 5), D("E", 5), S("E", 5))(
        safe("M9", "", (1, 2)),
        safe("M9", "", (5, 6, 7)),
        safe("M9", "", (8, 9, 10, 11)),
        live("M9", "", (3)))
  }
}

// ----
// M10:
// ----

// --------------------------------------------------
// LogScope spec: 
// -------------------------------------------------- 
// automaton A_P4 {
//   always S1 {
//     COMMAND{Type:"FSW",Stem:x,Number:y} => 
//       S2(x,y), S3(x,y), S4(x,y), S5(x,y)
//   }
//
//   hot state S2(x,y) {
//     EVR{Dispatch:x,Number:y} => done
//   }
//
//   hot state S3(x,y) {
//     EVR{Success:x,Number:y} => S6(x,y)
//   }
//
//   state S4(x,y) {
//     EVR{DispatchFailure:x,Number:y} => error
//   }
//
//   state S5(x,y) {
//     EVR{Failure:x,Number:y} => error
//   }
//  
//   state S6(x,y) {
//     EVR{Success:x,Number:y} => error
//   }
// }
// -------------------------------------------------- 

class M10 extends Monitor[Event] {
  require {
    case COMMAND("FSW", x, y) => S2(x, y) and S3(x, y) and S4(x, y) and S5(x, y)
  }

  def S2(x: String, y: Int) =
    hot {
      case DISPATCH(`x`, `y`) => ok
    }

  def S3(x: String, y: Int) =
    hot {
      case SUCCESS(`x`, `y`) => S6(x, y)
    }

  def S4(x: String, y: Int) =
    state {
      case DISPATCHFAILURE(`x`, `y`) => error
    }

  def S5(x: String, y: Int) =
    state {
      case FAILURE(`x`, `y`) => error
    }

  def S6(x: String, y: Int) =
    state {
      case SUCCESS(`x`, `y`) => error
    }
}

class Test10_1 extends Contract[Event] {
  @Test
  def test() {
    test(new M10)(
      C("FSW", "A", 1), D("A", 1), S("A", 1),
      C("FSW", "B", 2), D("B", 2), S("B", 2))()
  }
}

class Test10_2 extends Contract[Event] {
  @Test
  def test() {
    test(new M10)(
      C("FSW", "A", 1), DF("A", 1),
      C("FSW", "B", 2), S("B", 2),
      C("FSW", "C", 3), D("C", 3), F("C", 3),
      C("FSW", "D", 4), D("D", 4), S("D", 4), S("D", 4),
      C("FSW", "E", 5), D("E", 5), S("E", 5))(
        safe("M10", "", (1, 2)),
        safe("M10", "", (5, 6, 7)),
        safe("M10", "", (8, 9, 10, 11)),
        live("M10", "", (3, 4)))
  }
}

