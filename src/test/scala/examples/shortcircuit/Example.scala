package examples.shortcircuit

import tracecontract._

trait Event
case class FRCCommand(name: String, number: Int, params: Map[String, String]) extends Event

class RCSBurnDurationGranularity extends Monitor[Event] {
  require {
    case FRCCommand("acs_set_rcs_off_pulse_dur", _, params) => {
      (params.get("dur").isDefined) and
        (params.get("dur").get.toInt >= 100) and
        (params.get("dur").get.toInt % 100 == 0)
    }
  }
}

object Test {
  def main(args: Array[String]) {
    val monitor = new RCSBurnDurationGranularity
    val trace: List[Event] = List(
      FRCCommand("acs_set_rcs_off_pulse_dur", 42, Map("durs" -> "400")))
    monitor.verify(trace)
  }
}
