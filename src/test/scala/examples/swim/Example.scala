package examples.swim

import tracecontract._


// ====================
// === Event kinds: ===
// ====================

abstract class MesaEvent

case class Enqueue(message: Any) extends MesaEvent

case class Dequeue(message: Any) extends MesaEvent

// ========================
// === Data structures: ===
// ========================

case class Date(time: Int) {
  def isAfter(other: Date): Boolean = {
    time > other.time
  }
}

case class FlightPos(id: String, cs: String, pos: Int, alt: Int, speed: Int, heading: Int, date: Date)

// ===================
// === Properties: ===
// ===================

trait MesaMonitor extends Monitor[MesaEvent] {
  def seen(cs: String): Boolean = false // your logic here
}

/**
  * Property formulated using 'require' keyword, one property per class.
  * I prefer this style. The class name becomes the name of the property
  * and it reduces what one has to write - saves 2 lines, and saves giving
  * name to the property *and* the class.
  */

class FlightSeqOrder extends MesaMonitor {
  require {
    case Dequeue(FlightPos(_, cs, _, _, _, _, date1)) => state {
      case Dequeue(FlightPos(_, `cs`, _, _, _, _, date2)) =>
        date2.isAfter(date1)
    }
  }
}

/**
  * Property formulated using 'property' keyword. This style is also ok, no problem
  * for the paper.
  */

class FlightSeqOrderProperty extends MesaMonitor {
  property('flight_seq_order) {
    always {
      case Dequeue(FlightPos(_, cs, _, _, _, _, date1)) => state {
        case Dequeue(FlightPos(_, `cs`, _, _, _, _, date2)) =>
          date2.isAfter(date1)
      }
    }
  }
}

class Properties extends MesaMonitor {
  monitor(
    new FlightSeqOrder
  )
}

// =======================
// === Trace analysis: ===
// =======================

object TraceAnalysis1 {

  def main(args: Array[String]) {
    def trace: List[MesaEvent] =
      List(
        Dequeue(FlightPos("A", "A1", 99, 99, 99, 99, Date(10))),
        Dequeue(FlightPos("A", "A2", 99, 99, 99, 99, Date(20))),
        Dequeue(FlightPos("A", "A1", 99, 99, 99, 99, Date(30))),
        Dequeue(FlightPos("A", "A2", 99, 99, 99, 99, Date(15))) // This is wrong and should be caught
      )

    val monitor = new Properties
    monitor.verify(trace)
    /*
    // Testing the performance - showing that it does not "explode".
    for (i <- 1 to 10000000) {
      if (i % 10000 == 0) println(i / 10000)
      monitor.verify(Dequeue(FlightPos("A", "A2", 99, 99, 99, 99, Date(i))))
    }
    */
  }
}


