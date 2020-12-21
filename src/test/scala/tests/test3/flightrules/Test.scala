
package tests.test3.flightrules

import tracecontract._
import tests._
import org.junit.Test
import scala.language.postfixOps

// Scala Days 2011 paper: Checking Flight Rules with TraceContract: Application of a Scala DSL for Trace Analysis 
// H. Barringer, K. Havelund and R. Morris 

// flight rules

// --------------
// --- Events ---
// --------------

abstract class Event {
  val time: Int
}

case class COMMAND(
  name: String,
  value: Any,
  time: Int,
  deadline: Int) extends Event

case class STATUS(
  name: String,
  which: String,
  value: Int,
  time: Int) extends Event

// --- 1 ----------------------------------------------------------------------------------
// Command Rate:
// Operations shall limit ATS and RTS commands to no more than 5 commands per second.
// ----------------------------------------------------------------------------------------

class R1 extends Monitor[Event] {
  require {
    case COMMAND(name, _, time, _) if name startsWith "ATS" => count(time)
  }

  def count(time: Int, nr: Int = 1): Formula =
    state {
      case COMMAND(name, _, time2, _) if name startsWith "ATS" =>
        if ((time, time2) beyond (1 second))
          ok
        else if (nr == 5)
          error
        else
          count(time, nr + 1)
    }
}

class Test1_1 extends Contract[Event] {
  @Test def test() {
    test(new R1)(
      COMMAND("ATS_A", null, 1000, 500000),
      COMMAND("ATS_B", null, 2000, 500000),
      COMMAND("ATS_C", null, 3000, 500000),
      COMMAND("ATS_D", null, 4000, 500000),
      COMMAND("ATS_E", null, 5000, 500000),
      COMMAND("ATS_F", null, 6000, 500000),
      COMMAND("ATS_G", null, 7000, 500000),
      COMMAND("ATS_H", null, 8000, 500000),
      COMMAND("ATS_I", null, 9000, 500000),
      COMMAND("ATS_J", null, 10000, 500000),
      COMMAND("ATS_K", null, 11000, 500000),
      COMMAND("ATS_L", null, 12000, 500000),
      COMMAND("ATS_M", null, 13000, 500000),
      COMMAND("ATS_N", null, 14000, 500000),
      COMMAND("ATS_O", null, 15000, 500000),
      COMMAND("ATS_P", null, 16000, 500000),
      COMMAND("ATS_Q", null, 17000, 500000),
      COMMAND("ATS_R", null, 18000, 500000),
      COMMAND("ATS_S", null, 19000, 500000),
      COMMAND("ATS_T", null, 20000, 500000))()
  }
}

class Test1_2 extends Contract[Event] {
  @Test def test() {
    test(new R1)(
      COMMAND("ATS_A", null, 1000, 500000), // 1
      COMMAND("ATS_B", null, 2000, 500000), // 2
      COMMAND("ATS_C", null, 3000, 500000), // 3
      COMMAND("ATS_D", null, 4000, 500000), // 4  !
      COMMAND("ATS_E", null, 4100, 500000), // 5  !
      COMMAND("ATS_F", null, 4200, 500000), // 6  !
      COMMAND("ATS_G", null, 4300, 500000), // 7  !
      COMMAND("ATS_H", null, 4400, 500000), // 8  !
      COMMAND("ATS_I", null, 4500, 500000), // 9  !
      COMMAND("ATS_J", null, 10000, 500000), // 10
      COMMAND("ATS_K", null, 11000, 500000), // 11
      COMMAND("ATS_L", null, 12000, 500000), // 12
      COMMAND("ATS_M", null, 13000, 500000), // 13
      COMMAND("ATS_N", null, 14000, 500000), // 14 !
      COMMAND("ATS_O", null, 14100, 500000), // 15 !
      COMMAND("ATS_P", null, 14200, 500000), // 16 !
      COMMAND("ATS_Q", null, 14300, 500000), // 17 !
      COMMAND("ATS_R", null, 14400, 500000), // 18 !
      COMMAND("ATS_S", null, 14500, 500000), // 19 !
      COMMAND("ATS_T", null, 20000, 500000) // 20  
      )(
        safe("R1", "", (4, 5, 6, 7, 8, 9)),
        safe("R1", "", (14, 15, 16, 17, 18, 19)))
  }
}

// --- 2 ----------------------------------------------------------------------------------
// Command Granularity:
// No Stored Command Sequence shall include commands or command sequences whose successful 
// execution depends on command time granularity of less than one second.
// ----------------------------------------------------------------------------------------

class R2 extends Monitor[Event] {
  require {
    case COMMAND(_, _, time, deadline) =>
      (time, deadline) beyond (1 second)
  }
}

class Test2_1 extends Contract[Event] {
  @Test def test() {
    test(new R2)(
      COMMAND("A", null, 1000, 3000), // 1
      COMMAND("B", null, 2000, 4000), // 2
      COMMAND("C", null, 3000, 5000), // 3
      COMMAND("D", null, 4000, 6000), // 4  
      COMMAND("E", null, 5000, 7000) // 5  
      )()
  }
}

class Test2_2 extends Contract[Event] {
  @Test def test() {
    test(new R2)(
      COMMAND("A", null, 1000, 3000), // 1
      COMMAND("B", null, 2000, 2500), // 2 !
      COMMAND("C", null, 3000, 5000), // 3
      COMMAND("D", null, 4000, 4500), // 4 ! 
      COMMAND("E", null, 5000, 7000) // 5  
      )(
        safe("R2", "", (2)),
        safe("R2", "", (4)))
  }
}

// --- 3 ----------------------------------------------------------------------------------
// Command Precondition:
// Instrument shall not be powered on when any temperature sensor reads less than -20 
// degrees C. 
// ----------------------------------------------------------------------------------------

class R3 extends Monitor[Event] {
  require {
    case STATUS("TEMP", sensor, value, _) if value < -20 =>
      state {
        case COMMAND("POWERON", _, _, _) => error
        case STATUS("TEMP", `sensor`, value2, _) if value2 >= -20 => ok
      }
  }
}

class Test3_1 extends Contract[Event] {
  @Test def test() {
    test(new R3)(
      COMMAND("A", null, 1000, 3000), // 1
      STATUS("TEMP", "two", -30, 2000), // 2
      COMMAND("C", null, 3000, 5000), // 3
      STATUS("TEMP", "two", 10, 4000), // 4
      COMMAND("E", null, 5000, 7000), // 5  
      COMMAND("POWERON", null, 6000, 8000) // 6  
      )()
  }
}

class Test3_2 extends Contract[Event] {
  @Test def test() {
    test(new R3)(
      COMMAND("A", null, 1000, 3000), // 1
      STATUS("TEMP", "two", -30, 2000), // 2 !
      COMMAND("C", null, 3000, 5000), // 3
      STATUS("TEMP", "two", -25, 4000), // 4 !
      COMMAND("E", null, 5000, 7000), // 5  
      COMMAND("POWERON", null, 6000, 8000) // 6 ! 
      )(
        safe("R3", "", (2, 6)),
        safe("R3", "", (4, 6)))
  }
}

// --- 4 ----------------------------------------------------------------------------------
// Command Minimum/Maximum Duration:
// The spacecraft shall acquire and maintain a sun-pointing attitude from an arbitrary 
// attitude in no more than 30 minutes.
// ----------------------------------------------------------------------------------------

class R4_1 extends Monitor[Event] {
  require {
    case COMMAND("SUN_POINTING", _, time, _) =>
      hot {
        case e: Event if (time, e.time) beyond (30 minutes) => error
        case STATUS("SUN_POINTING", _, _, _) => ok
      }
  }
}

class R4_2 extends Monitor[Event] {
  require {
    case COMMAND("SUN_POINTING", _, time1, _) =>
      hot {
        case STATUS("SUN_POINTING", _, _, time2) if (time1, time2) within (30 minutes) => ok
      }
  }
}

class R4 extends Monitor[Event] {
  monitor(new R4_1, new R4_2)
}

class Test4_1 extends Contract[Event] {
  @Test def test() {
    test(new R4)(
      COMMAND("A", null, 10000, 13000), // 1
      COMMAND("SUN_POINTING", null, 20000, 23000), // 2
      COMMAND("B", null, 30000, 33000), // 3
      STATUS("SUN_POINTING", "one", -25, 40000), // 4 
      COMMAND("C", null, 50000, 53000), // 5
      COMMAND("SUN_POINTING", null, 60000, 63000), // 6  
      COMMAND("D", null, 70000, 73000), // 7  
      STATUS("SUN_POINTING", "two", -25, 80000), // 8 
      COMMAND("E", null, 90000, 93000) // 9  
      )()
  }
}

class Test4_2 extends Contract[Event] {
  @Test def test() {
    test(new R4)(
      COMMAND("A", null, 10000, 13000), // 1
      COMMAND("SUN_POINTING", null, 20000, 23000), // 2
      COMMAND("B", null, 30000, 33000), // 3
      STATUS("SUN_POINTING", "one", -25, 40000), // 4 
      COMMAND("C", null, 50000, 53000), // 5
      COMMAND("SUN_POINTING", null, 60000, 63000), // 6 !  
      COMMAND("D", null, 70000, 73000), // 7  
      STATUS("SUN_POINTING", "two", -25, 2000000), // 8 !
      COMMAND("E", null, 20003000, 20006000) // 9  
      )(
        safe("R4_1", "", (6, 8)),
        live("R4_2", "", (6)))
  }
}

class Test4_3 extends Contract[Event] {
  @Test def test() {
    test(new R4)(
      COMMAND("A", null, 10000, 13000), // 1
      COMMAND("SUN_POINTING", null, 20000, 23000), // 2
      COMMAND("B", null, 30000, 33000), // 3
      STATUS("SUN_POINTING", "one", -25, 40000), // 4 
      COMMAND("C", null, 50000, 53000), // 5
      COMMAND("SUN_POINTING", null, 60000, 63000), // 6 !  
      COMMAND("D", null, 70000, 73000), // 7  
      COMMAND("E", null, 20003000, 20006000) // 8  
      )(
        safe("R4_1", "", (6, 8)),
        live("R4_2", "", (6)))
  }
}

// --- 5 ----------------------------------------------------------------------------------
// Duration-wait:
// ACS commands shall not be issued within 1 second of an ACS mode command.
// ----------------------------------------------------------------------------------------

// early detection:

class R5_1 extends Monitor[Event] {
  require {
    case COMMAND("ACS_MODE", _, time, _) =>
      state {
        case e: Event if (time, e.time) beyond (1 second) => ok
        case COMMAND("ACS", _, _, _) => error
      }
  }
}

// eventually detection:

class R5_2 extends Monitor[Event] {
  require {
    case COMMAND("ACS_MODE", _, time1, _) =>
      state {
        case COMMAND("ACS", _, time2, _) if (time1, time2) within (1 second) => error
      }
  }
}

// using until:

class R5_3 extends Monitor[Event] {
  var counter: Int = 0
  require {
    case COMMAND("ACS_MODE", _, time, _) =>
      until {
        case e: Event if (time, e.time) beyond (1 second) => ok
        case COMMAND("ACS", _, _, _) =>
          counter += 1
          error
      }
  }

  override def finish() {
    if (counter > 0) assert(counter == 1)
  }
}

class R5 extends Monitor[Event] {
  monitor(new R5_1, new R5_2, new R5_3)
}

class Test5_1 extends Contract[Event] {
  @Test def test() {
    test(new R5)(
      COMMAND("A", null, 1000, 3000), // 1
      COMMAND("ACS_MODE", null, 2000, 4000), // 2 !
      COMMAND("B", null, 3000, 5000), // 3
      COMMAND("ACS", null, 4000, 6000), // 4 ! 
      COMMAND("C", null, 6000, 8000) // 5  
      )()
  }
}

class Test5_2 extends Contract[Event] {
  @Test def test() {
    test(new R5)(
      COMMAND("A", null, 1000, 3000), // 1
      COMMAND("ACS_MODE", null, 2000, 4000), // 2 !
      COMMAND("B", null, 2100, 5000), // 3
      COMMAND("ACS", null, 2200, 6000), // 4 ! 
      COMMAND("C", null, 6000, 8000) // 5  
      )(
        safe("R5_1", "", (2, 4)),
        safe("R5_2", "", (2, 4)))
  }
}

// --- 6 ----------------------------------------------------------------------------------
// Command Order timed sequence:
// The TWTA shall be turned on 300 seconds before turning on the Ka modulator.
// ----------------------------------------------------------------------------------------

// --- solution 1: a state machine

class R6_1 extends Monitor[Event] {
  property { Init }

  def Init: Formula =
    state {
      case COMMAND("TURNON", "TWTA", time, _) =>
        On(time)
      case COMMAND("TURNON", "KA", _, _) => error
    }

  def On(twtaTime: Int): Formula =
    state {
      case COMMAND("TURNOFF", "TWTA", _, _) => Init
      case COMMAND("TURNON", "KA", kaTime, _) if (twtaTime, kaTime) within (300 seconds) =>
        error
    }
}

class Test6_1 extends Contract[Event] {
  @Test def test() {
    test(new R6_1)(
      COMMAND("TURNON", "TWTA", 1000, 1100),
      COMMAND("TURNOFF", "TWTA", 2000, 2100),
      COMMAND("TURNON", "TWTA", 1000, 1100),
      COMMAND("TURNON", "KA", 320000, 320100),
      COMMAND("TURNON", "TWTA", 400000, 400100),
      COMMAND("TURNON", "KA", 800000, 800100))()
  }
}

class Test6_2 extends Contract[Event] {
  @Test def test() {
    test(new R6_1)(
      COMMAND("TURNON", "TWTA", 1000, 1100),
      COMMAND("TURNOFF", "TWTA", 2000, 2100),
      COMMAND("TURNON", "TWTA", 1000, 1100),
      COMMAND("TURNON", "KA", 300000, 320100),
      COMMAND("TURNON", "TWTA", 400000, 400100),
      COMMAND("TURNON", "KA", 800000, 800100))(
        safe("R6_1", "", (1, 2, 3, 4)))
  }
}

// --- solution 2: a local time variable

class R6_2 extends Monitor[Event] {
  var timeOn: Option[Int] = None

  require {
    case COMMAND("TURNON", "TWTA", time, _) => timeOn = Some(time)
    case COMMAND("TURNOFF", "TWTA", _, _) => timeOn = None
    case COMMAND("TURNON", "KA", kaTime, _) =>
      timeOn match {
        case None => false
        case Some(twtaTime) => (twtaTime, kaTime) beyond (300 seconds)
      }
  }
}

class Test6_3 extends Contract[Event] {
  @Test def test() {
    test(new R6_2)(
      COMMAND("TURNON", "TWTA", 1000, 1100),
      COMMAND("TURNOFF", "TWTA", 2000, 2100),
      COMMAND("TURNON", "TWTA", 1000, 1100),
      COMMAND("TURNON", "KA", 320000, 320100),
      COMMAND("TURNON", "TWTA", 400000, 400100),
      COMMAND("TURNON", "KA", 800000, 800100))()
  }
}

class Test6_4 extends Contract[Event] {
  @Test def test() {
    test(new R6_2)(
      COMMAND("TURNON", "TWTA", 1000, 1100),
      COMMAND("TURNOFF", "TWTA", 2000, 2100),
      COMMAND("TURNON", "TWTA", 1000, 1100),
      COMMAND("TURNON", "KA", 300000, 320100),
      COMMAND("TURNON", "TWTA", 400000, 400100),
      COMMAND("TURNON", "KA", 800000, 800100))(
        safe("R6_2", "", (4)))
  }
}

// --- 7 ----------------------------------------------------------------------------------
// Command Order concurrency/exclusion:
// No firing of main thrusters while spacecraft is in fine point mode.
// ----------------------------------------------------------------------------------------

// --- solution 1: temporal logic

class R7_1 extends Monitor[Event] {
  require {
    case COMMAND("SET_MODE", 2, _, _) =>
      state {
        case COMMAND("SET_MODE", x, _, _) if x != 2 => ok
        case COMMAND("FIRE_MAIN_THRUSTER", _, _, _) => error
      }
  }
}

class Test7_1 extends Contract[Event] {
  @Test def test() {
    test(new R7_1)(
      COMMAND("SET_MODE", 2, 1000, 1100),
      COMMAND("A", null, 2000, 2100),
      COMMAND("SET_MODE", 3, 3000, 3100),
      COMMAND("FIRE_MAIN_THRUSTER", null, 4000, 4100),
      COMMAND("SET_MODE", 2, 5000, 5100),
      COMMAND("A", null, 6000, 6100),
      COMMAND("SET_MODE", 4, 7000, 7100),
      COMMAND("FIRE_MAIN_THRUSTER", null, 8000, 8100))()
  }
}

class Test7_2 extends Contract[Event] {
  @Test def test() {
    test(new R7_1)(
      COMMAND("SET_MODE", 2, 1000, 1100),
      COMMAND("A", null, 2000, 2100),
      COMMAND("SET_MODE", 3, 3000, 3100),
      COMMAND("FIRE_MAIN_THRUSTER", null, 4000, 4100),
      COMMAND("SET_MODE", 2, 5000, 5100),
      COMMAND("A", null, 6000, 6100),
      COMMAND("FIRE_MAIN_THRUSTER", null, 8000, 8100))(
        safe("R7_1", "", (5, 7)))
  }
}

// --- solution 2: a local mode variable

class R7_2 extends Monitor[Event] {
  var mode: Int = 0

  require {
    case COMMAND("SET_MODE", x, _, _) => mode = x.asInstanceOf[Int]
    case COMMAND("FIRE_MAIN_THRUSTER", _, _, _) if mode == 2 => error
  }
}

class Test7_3 extends Contract[Event] {
  @Test def test() {
    test(new R7_2)(
      COMMAND("SET_MODE", 2, 1000, 1100),
      COMMAND("A", null, 2000, 2100),
      COMMAND("SET_MODE", 3, 3000, 3100),
      COMMAND("FIRE_MAIN_THRUSTER", null, 4000, 4100),
      COMMAND("SET_MODE", 2, 5000, 5100),
      COMMAND("A", null, 6000, 6100),
      COMMAND("SET_MODE", 4, 7000, 7100),
      COMMAND("FIRE_MAIN_THRUSTER", null, 8000, 8100))()
  }
}

class Test7_4 extends Contract[Event] {
  @Test def test() {
    test(new R7_2)(
      COMMAND("SET_MODE", 2, 1000, 1100),
      COMMAND("A", null, 2000, 2100),
      COMMAND("SET_MODE", 3, 3000, 3100),
      COMMAND("FIRE_MAIN_THRUSTER", null, 4000, 4100),
      COMMAND("SET_MODE", 2, 5000, 5100),
      COMMAND("A", null, 6000, 6100),
      COMMAND("FIRE_MAIN_THRUSTER", null, 8000, 8100))(
        safe("R7_2", "", (7)))
  }
}

// --- 8 ----------------------------------------------------------------------------------
// Mode	Transition:
// Mode and submode transitions shall be restricted to the set represented in the below  
// state transition diagram under nominal flight operations.
// ----------------------------------------------------------------------------------------

// without select:

class R8_1 extends Monitor[Event] {

  property { SPM }

  def SPM: Formula =
    state {
      case COMMAND("MOVETO", "DB3", _, _) => DB3
      case COMMAND("MOVETO", _, _, _) => error
    }

  def DB1: Formula =
    state {
      case COMMAND("MOVETO", "SLM", _, _) => SLM
      case COMMAND("MOVETO", "DB2", _, _) => DB2
      case COMMAND("MOVETO", "DB3", _, _) => DB3
      case COMMAND("MOVETO", "DVM", _, _) => DVM
      case COMMAND("MOVETO", _, _, _) => error
    }

  def DB2: Formula =
    state {
      case COMMAND("MOVETO", "SLM", _, _) => SLM
      case COMMAND("MOVETO", "DB1", _, _) => DB1
      case COMMAND("MOVETO", "DB3", _, _) => DB3
      case COMMAND("MOVETO", _, _, _) => error
    }

  def DB3: Formula =
    state {
      case COMMAND("MOVETO", "DB2", _, _) => DB2
      case COMMAND("MOVETO", _, _, _) => error
    }

  def SLM: Formula =
    state {
      case COMMAND("MOVETO", "DB2", _, _) => DB2
      case COMMAND("MOVETO", "DB3", _, _) => DB3
      case COMMAND("MOVETO", _, _, _) => error
    }

  def DVM: Formula =
    state {
      case COMMAND("MOVETO", "DB3", _, _) => DB3
      case COMMAND("MOVETO", _, _, _) => error
    }
}

class Test8_1 extends Contract[Event] {
  @Test def test() {
    test(new R8_1)(
      COMMAND("MOVETO", "DB3", 1000, 1100),
      COMMAND("MOVETO", "DB2", 2000, 2100),
      COMMAND("MOVETO", "DB1", 3000, 3100),
      COMMAND("MOVETO", "SLM", 4000, 4100),
      COMMAND("MOVETO", "DB2", 5000, 5100),
      COMMAND("MOVETO", "DB1", 6000, 6100),
      COMMAND("MOVETO", "DVM", 7000, 7100),
      COMMAND("MOVETO", "DB3", 8000, 8100),
      COMMAND("MOVETO", "DB2", 9000, 9100),
      COMMAND("MOVETO", "SLM", 10000, 10100))()
  }
}

class Test8_2 extends Contract[Event] {
  @Test def test() {
    test(new R8_1)(
      COMMAND("MOVETO", "DB3", 1000, 1100),
      COMMAND("MOVETO", "DB2", 2000, 2100),
      COMMAND("MOVETO", "DB1", 3000, 3100),
      COMMAND("MOVETO", "SLM", 4000, 4100),
      COMMAND("MOVETO", "DB1", 5000, 5100), // bad move
      COMMAND("MOVETO", "DB1", 6000, 6100),
      COMMAND("MOVETO", "DVM", 7000, 7100),
      COMMAND("MOVETO", "DB3", 8000, 8100),
      COMMAND("MOVETO", "DB2", 9000, 9100),
      COMMAND("MOVETO", "SLM", 10000, 10100))(
        safe("R8_1", "", (1, 2, 3, 4, 5)))
  }
}

// with select:

class R8_2 extends Monitor[Event] {
  select { case COMMAND("MOVETO", _, _, _) => true }

  property { SPM }

  def SPM: Formula =
    weak {
      case COMMAND("MOVETO", "DB3", _, _) => DB3
    }

  def DB1: Formula =
    weak {
      case COMMAND("MOVETO", "SLM", _, _) => SLM
      case COMMAND("MOVETO", "DB2", _, _) => DB2
      case COMMAND("MOVETO", "DB3", _, _) => DB3
      case COMMAND("MOVETO", "DVM", _, _) => DVM
    }

  def DB2: Formula =
    weak {
      case COMMAND("MOVETO", "SLM", _, _) => SLM
      case COMMAND("MOVETO", "DB1", _, _) => DB1
      case COMMAND("MOVETO", "DB3", _, _) => DB3
    }

  def DB3: Formula =
    weak {
      case COMMAND("MOVETO", "DB2", _, _) => DB2
    }

  def SLM: Formula =
    weak {
      case COMMAND("MOVETO", "DB2", _, _) => DB2
      case COMMAND("MOVETO", "DB3", _, _) => DB3
    }

  def DVM: Formula =
    weak {
      case COMMAND("MOVETO", "DB3", _, _) => DB3
    }
}

class Test8_3 extends Contract[Event] {
  @Test def test() {
    test(new R8_2)(
      COMMAND("MOVETO", "DB3", 1000, 1100),
      COMMAND("MOVETO", "DB2", 2000, 2100),
      COMMAND("MOVETO", "DB1", 3000, 3100),
      COMMAND("MOVETO", "SLM", 4000, 4100),
      COMMAND("MOVETO", "DB2", 5000, 5100),
      COMMAND("MOVETO", "DB1", 6000, 6100),
      COMMAND("MOVETO", "DVM", 7000, 7100),
      COMMAND("MOVETO", "DB3", 8000, 8100),
      COMMAND("MOVETO", "DB2", 9000, 9100),
      COMMAND("MOVETO", "SLM", 10000, 10100))()
  }
}

class Test8_4 extends Contract[Event] {
  @Test def test() {
    test(new R8_2)(
      COMMAND("MOVETO", "DB3", 1000, 1100),
      COMMAND("MOVETO", "DB2", 2000, 2100),
      COMMAND("MOVETO", "DB1", 3000, 3100),
      COMMAND("MOVETO", "SLM", 4000, 4100),
      COMMAND("MOVETO", "DB1", 5000, 5100), // bad move
      COMMAND("MOVETO", "DB1", 6000, 6100),
      COMMAND("MOVETO", "DVM", 7000, 7100),
      COMMAND("MOVETO", "DB3", 8000, 8100),
      COMMAND("MOVETO", "DB2", 9000, 9100),
      COMMAND("MOVETO", "SLM", 10000, 10100))(
        safe("R8_2", "", (1, 2, 3, 4, 5)))
  }
}

// --- 9 ----------------------------------------------------------------------------------
// Parameter Format/Value Checking:
// Commanded quaternions shall take the following form: 
// a. Normalized to 1.0 
// b. Scalar/real part given by fourth component
// ----------------------------------------------------------------------------------------

// Not formalized.

