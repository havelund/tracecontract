
package tests.test8

import tracecontract._
import tests._
import org.junit.Test

// Examples that required testing after code modifications.

abstract class Event

case class Command(name: String, nr: Int) extends Event
case class Success(name: String, nr: Int) extends Event
case class Fail(name: String, nr: Int) extends Event

// =====================================
// Testing clearly false specifications:
// =====================================

class GloballyFalse extends Monitor[Event] {
  property {
    globally(False)
  }
}

class GloballyEventually extends Monitor[Event] {
  property {
    globally(eventually(Success("THIS_WILL_NEVER_HAPPEN", 999)))
  }
}

class TrueCausesFalse extends Monitor[Event] {
  property {
    True causes False
  }
}

class IfThenElseYes extends Monitor[Event] {
  property {
    If(eventually(Command("TAKE_PICTURE", 2))) Then
      eventually(Success("TAKE_PICTURE", 2)) Else
      never(Command("STOP_DRIVING", 1))
  }
}

class IfThenElseNo extends Monitor[Event] {
  property {
    If(eventually(Command("TAKE_PICTURES", 2))) Then
      eventually(Success("TAKE_PICTURE", 2)) Else
      never(Command("STOP_DRIVING", 1))
  }
}

class Requirements1 extends Monitor[Event] {
  monitor(
    new GloballyFalse,
    new GloballyEventually,
    new TrueCausesFalse,
    new IfThenElseYes,
    new IfThenElseNo)
}

class Test1 extends Contract[Event] {
  @Test def test() {
    test(new Requirements1)(
      Command("STOP_DRIVING", 1), // 1
      Command("TAKE_PICTURE", 2), // 2
      Fail("STOP_DRIVING", 1), // 3
      Success("TAKE_PICTURE", 2), // 4
      Success("SEND_TELEMETRY", 42) // 5
      )(
        safe("GloballyFalse", "", (1)),
        safe("GloballyFalse", "", (2)),
        safe("GloballyFalse", "", (3)),
        safe("GloballyFalse", "", (4)),
        safe("GloballyFalse", "", (5)),
        live("GloballyEventually", "", (1)),
        live("GloballyEventually", "", (2)),
        live("GloballyEventually", "", (3)),
        live("GloballyEventually", "", (4)),
        live("GloballyEventually", "", (5)),
        safe("TrueCausesFalse", "", (1)),
        live("IfThenElseNo", "", (1, 4)))
  }
}

// =============================
// Testing conditional formulas:
// =============================

class CountWithImplies extends Monitor[Event] {
  var counter = 0

  property {
    globally {
      // Due to conditional evaluation of 'implies' the counter will only
      // be counted up 3 times.
      Command("STOP_DRIVING", 1) implies exec {
        counter += 1
      }
    }
  }

  override def finish() {
    if (counter != 3) assert(false, "counter=" + counter)
  }
}

class CountWithOr extends Monitor[Event] {
  var counter = 0

  property {
    globally {
      // Due to conditional evaluation of 'or' the counter will only
      // be counted up 3 times.
      not(Command("STOP_DRIVING", 1)) or exec {
        counter += 1
      }
    }
  }

  override def finish() {
    if (counter != 3) assert(false, "counter=" + counter)
  }
}

class AvoidSecondAndArg extends Monitor[Event] {
  property {
    globally {
      // Due to conditional evaluation of 'and' the assert should never
      // be executed since there are no Command("DESTROY_ROCK",42) events.
      not(Command("DESTROY_ROCK", 42) and exec { assert(false) })
    }
  }
}

class Requirements2 extends Monitor[Event] {
  monitor(
    new CountWithImplies,
    new CountWithOr,
    new AvoidSecondAndArg)
}

class Test2 extends Contract[Event] {
  @Test def test() {
    test(new Requirements2)(
      Command("STOP_DRIVING", 1), // 1
      Command("TAKE_PICTURE", 2), // 2
      Fail("STOP_DRIVING", 1), // 3
      Command("STOP_DRIVING", 1), // 4
      Success("TAKE_PICTURE", 2), // 5
      Command("STOP_DRIVING", 1), // 6
      Success("SEND_TELEMETRY", 42) // 7
      )()
  }
}

// ========================
// Testing and on Booleans:
// ========================

case class FRCCommand(name: String, number: Int, params: Map[String, String]) extends Event

class DurationAnd extends Monitor[Event] {
  require {
    case FRCCommand("acs_set_rcs_off_pulse_dur", _, params) => {
      (params.get("dur").isDefined) and
        (params.get("dur").get.toInt >= 100) and
        (params.get("dur").get.toInt % 100 == 0)
    }
  }
}

class Test3 extends Contract[Event] {
  @Test def test() {
    test(new DurationAnd)(
      FRCCommand("acs_set_rcs_off_pulse_dur", 42, Map("dur" -> "100")), // 1
      FRCCommand("acs_set_rcs_off_pulse_dur", 42, Map("dur" -> "401")), // 2 safe
      FRCCommand("acs_set_rcs_off_pulse_dur", 42, Map("dur" -> "200")), // 3
      FRCCommand("acs_set_rcs_off_pulse_dur", 42, Map("dur" -> "0")), // 4 safe
      FRCCommand("acs_set_rcs_off_pulse_dur", 42, Map("dur" -> "300")), // 5
      FRCCommand("acs_set_rcs_off_pulse_dur", 42, Map("durs" -> "400")), // 6 safe
      FRCCommand("acs_set_rcs_off_pulse_dur", 42, Map("dur" -> "400")) // 7
      )(
        safe("DurationAnd", "", (2)),
        safe("DurationAnd", "", (4)),
        safe("DurationAnd", "", (6)))
  }
}

// ========================
// Testing or on Booleans:
// ========================

class DurationOr extends Monitor[Event] {
  require {
    case FRCCommand("acs_set_rcs_off_pulse_dur", _, params) => {
      !(params.get("dur").isDefined) or
        ((params.get("dur").get.toInt >= 100) and
          (params.get("dur").get.toInt % 100 == 0))
    }
  }
}

class Test4 extends Contract[Event] {
  @Test def test() {
    test(new DurationOr)(
      FRCCommand("acs_set_rcs_off_pulse_dur", 42, Map("dur" -> "100")), // 1
      FRCCommand("acs_set_rcs_off_pulse_dur", 42, Map("dur" -> "401")), // 2 safe
      FRCCommand("acs_set_rcs_off_pulse_dur", 42, Map("dur" -> "200")), // 3
      FRCCommand("acs_set_rcs_off_pulse_dur", 42, Map("dur" -> "0")), // 4 safe
      FRCCommand("acs_set_rcs_off_pulse_dur", 42, Map("dur" -> "300")), // 5
      FRCCommand("acs_set_rcs_off_pulse_dur", 42, Map("durs" -> "400")), // 6 
      FRCCommand("acs_set_rcs_off_pulse_dur", 42, Map("dur" -> "400")) // 7
      )(
        safe("DurationOr", "", (2)),
        safe("DurationOr", "", (4)))
  }
}

// ============================
// Testing implies on Booleans:
// ============================

class DurationImplies extends Monitor[Event] {
  require {
    case FRCCommand("acs_set_rcs_off_pulse_dur", _, params) => {
      (params.get("dur").isDefined) implies
        ((params.get("dur").get.toInt >= 100) and
          (params.get("dur").get.toInt % 100 == 0))
    }
  }
}

class Test5 extends Contract[Event] {
  @Test def test() {
    test(new DurationImplies)(
      FRCCommand("acs_set_rcs_off_pulse_dur", 42, Map("dur" -> "100")), // 1
      FRCCommand("acs_set_rcs_off_pulse_dur", 42, Map("dur" -> "401")), // 2 safe
      FRCCommand("acs_set_rcs_off_pulse_dur", 42, Map("dur" -> "200")), // 3
      FRCCommand("acs_set_rcs_off_pulse_dur", 42, Map("dur" -> "0")), // 4 safe
      FRCCommand("acs_set_rcs_off_pulse_dur", 42, Map("dur" -> "300")), // 5
      FRCCommand("acs_set_rcs_off_pulse_dur", 42, Map("durs" -> "400")), // 6 
      FRCCommand("acs_set_rcs_off_pulse_dur", 42, Map("dur" -> "400")) // 7
      )(
        safe("DurationImplies", "", (2)),
        safe("DurationImplies", "", (4)))
  }
}
