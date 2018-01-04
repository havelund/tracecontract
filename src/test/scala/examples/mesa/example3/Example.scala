package examples.mesa.example3

import tracecontract._

// ----------------------------
// My tricks to make this work:
// ----------------------------

trait Event

case class Waypoint(wp: Int) extends Event

case object Other extends Event

class Config(wps: Int*) {
  def getLongList(name: String): List[Long] = (wps.toList map { case i => i.toLong })
}

class MesaMonitor(config: Config) extends Monitor[Event] {
  implicit def liftList(l: List[Long]) = new {
    def get(i: Int): Long = l(i)
  }
}


// -------------------------
// The original formulation:
// -------------------------

class WaypointsOrderMonitor_4(config: Config) extends MesaMonitor(config) {
  property {
    waypointsOrderFormula
  }

  val W = config.getLongList("waypoints")

  var i = 0

  def waypointsOrderFormula: Formula =
    state {
      case Waypoint(p) if W.contains(p) && W.get(i) != p =>
        error
      case Waypoint(p) if W.contains(p) && W.get(i) == p && i < W.size - 1 =>
        i = i + 1
        waypointsOrderFormula
      case Waypoint(p) if W.contains(p) && W.get(i) == p && i == W.size - 1 =>
        ok("Property waypoints_ordering_variable_size is satisfied")
    }
}


// -------------------------------------------------------------------------
// Pulling the check on size out into an if-statement (one less transition):
// -------------------------------------------------------------------------

class WaypointsOrderMonitor_5(config: Config) extends MesaMonitor(config) {
  property {
    waypointsOrderFormula
  }

  val W = config.getLongList("waypoints")

  var i = 0

  def waypointsOrderFormula: Formula =
    state {
      case Waypoint(p) if W.contains(p) && W.get(i) != p =>
        error
      case Waypoint(p) if W.contains(p) && W.get(i) == p =>
        if (i < W.size - 1) {
          i = i + 1
          waypointsOrderFormula
        } else {
          ok("Property waypoints_ordering_variable_size is satisfied")
        }
    }
}


// ----------------------------------------------------------------------------
// Pulling also check on index out into if-statement, one transition remaining:
// ----------------------------------------------------------------------------

class WaypointsOrderMonitor_6(config: Config) extends MesaMonitor(config) {
  property {
    waypointsOrderFormula
  }

  val W = config.getLongList("waypoints")

  var i = 0

  def waypointsOrderFormula: Formula =
    state {
      case Waypoint(p) if W.contains(p) =>
        if (W.get(i) != p)
          error
        else if (i == W.size - 1)
          ok("Property waypoints_ordering_variable_size is satisfied")
        else {
          i = i + 1
          waypointsOrderFormula
        }
    }
}

// ----------------------------------------------------------------------
// Turning variable i into a parameter to the formula (functional style):
// ----------------------------------------------------------------------

class WaypointsOrderMonitor_7(config: Config) extends MesaMonitor(config) {
  property {
    waypointsOrderFormula(0)
  }

  val W = config.getLongList("waypoints")

  def waypointsOrderFormula(i: Int): Formula =
    state {
      case Waypoint(p) if W.contains(p) =>
        if (W.get(i) != p)
          error
        else if (i == W.size - 1)
          ok("Property waypoints_ordering_variable_size is satisfied")
        else
          waypointsOrderFormula(i + 1)
    }
}

// ---------------------------------------
// Passing the list itself to the formula:
// ---------------------------------------

class WaypointsOrderMonitor_8(config: Config) extends MesaMonitor(config) {
  val W = config.getLongList("waypoints")

  property {
    waypointsOrderFormula(W)
  }

  def waypointsOrderFormula(wps: List[Long]): Formula =
    state {
      case Waypoint(p) if wps.contains(p) =>
        if (wps.head != p)
          error
        else if (wps.size == 1)
          ok("Property waypoints_ordering_variable_size is satisfied")
        else
          waypointsOrderFormula(wps.tail)
    }
}


// -----------------
// Testing monitors:
// -----------------

object TraceAnalysis {
  def main(args: Array[String]) {
    val config = new Config(1, 2, 3)

    def trace: List[Event] =
      List(
        Other,
        Waypoint(1),
        Other,
        Waypoint(3),
        Other,
        Waypoint(2),
        Other
      )

    println("START")
    val monitor = new WaypointsOrderMonitor_8(config)
    monitor.setDebug(false)
    monitor.verify(trace)
  }
}
