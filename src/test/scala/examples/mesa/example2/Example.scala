package examples.mesa.example2

import tracecontract._

trait Event

case class Waypoint(num: Long) extends Event

case object Other extends Event

object Event {
  val W1 = 1
  val W2 = 2
  val W3 = 3
}

import examples.mesa.example2.Event._

class WaypointMonitor extends Monitor[Event] {
  property('waypoints_ordering_statelogic) {
    state {
      case Waypoint(W2) | Waypoint(W3) => error
      case Waypoint(W1) =>
        state {
          case Waypoint(W1) | Waypoint(W3) => error
          case Waypoint(W2) =>
            state {
              case Waypoint(W1) | Waypoint(W2) => error
              case Waypoint(W3) => ok("Property waypoints_ordering is satisfied")
            }
        }
    }
  }
}

class StrongWaypointMonitor1 extends Monitor[Event] {
  property('waypoints_ordering_statelogic) {
    state {
      case Waypoint(W2) | Waypoint(W3) => error
      case Waypoint(W1) =>
        strong {
          case Waypoint(W2) =>
            strong {
              case Waypoint(W3) => ok("Property waypoints_ordering is satisfied")
            }
        }
    }
  }
}

class StrongWaypointMonitor2 extends Monitor[Event] {
  select { case Waypoint(_) => true }

  property('waypoints_ordering_statelogic) {
    strong {
      case Waypoint(W1) =>
        strong {
          case Waypoint(W2) =>
            strong {
              case Waypoint(W3) => ok("Property waypoints_ordering is satisfied")
            }
        }
    }
  }
}

object TraceAnalysis {
  def main(args: Array[String]) {
    def trace: List[Event] =
      List(
        Other,
        Waypoint(W1),
        Other,
        Waypoint(W2),
        Other,
        Waypoint(W3),
        Other
      )

    val monitor = new StrongWaypointMonitor2
    monitor.verify(trace)
  }
}
