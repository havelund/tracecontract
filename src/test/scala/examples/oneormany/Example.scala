package examples.oneormany

import tracecontract._

abstract class Event

case class FlightData(flight: String, time: Int, location: (Int,Int)) extends Event


class FlightDataWithin5 extends Monitor[Event] {
  require {
    case FlightData(flight, time1, _) =>
      state {
        case FlightData(`flight`, time2, _) => time2 - time1 <= 5
        case FlightData(_, time2, _) if time2 - time1 > 5 => error
      }
  }
}

object TraceAnalysis {

  def main(args: Array[String]) {
    def trace: List[Event] =
      List( 
        FlightData("UA", 101, (1,2)), 
        FlightData("AA", 102, (1,2)),
        FlightData("SW", 103, (1,2)),
        FlightData("UA", 104, (1,2)),
        FlightData("UA", 105, (1,2)),
        FlightData("AA", 106, (1,2)),
        FlightData("SW", 107, (1,2)),
        FlightData("SW", 108, (1,2)),
        FlightData("UA", 119, ((1,2))))

    val monitor = new FlightDataWithin5
    monitor.verify(trace)
  }
}

