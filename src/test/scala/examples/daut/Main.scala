package examples.daut

import tracecontract._
import scala.math._

// --------------------------------------------------------------------------------
abstract class Event {
  val t: Int
}

case class Time(t: Int) extends Event
case class Value(t: Int, name: String, x: Int) extends Event
case class Situation(t: Int, name: String, dt: Int) extends Event

// --------------------------------------------------------------------------------
abstract class Abstraction( var eventStream: List[Event] ) extends Monitor[Event] {

  def newSituation(t: Int, situationName: String, dt: Int) {
    var i_t = eventStream.indexWhere(_.t == t)
    eventStream = insert[Event]( eventStream, i_t + 1, new Situation( t, situationName, dt ) )
  }

  def insert[T](list: List[T], i: Int, value: T) = {
    val (front, back) = list.splitAt(i)
    front ++ List(value) ++ back
  }
}

// --------------------------------------------------------------------------------
class PropValue(
                 eventStream: List[Event],
                 newSituationName: String,
                 var t0: Int,
                 dt: Int,
                 p: (Int) => Boolean,
                 observedValueName: String) extends Abstraction ( eventStream ) {

  require {
    case Value(t, name, x) if (observedValueName==name && !p(x)) =>
      state {
        case Value(t_,name_, x_) if (observedValueName==name_ && p(x_)) =>
          println("*** A ***")
          t0 = t_
      }
    case Value(t, name, x) if (observedValueName==name && p(x) && (t - t0) >= dt) =>
      println("*** B ***")
      newSituation(t, newSituationName, (t - t0))
  }
}

// --------------------------------------------------------------------------------
object Main extends App {

  var eventStream = List[Event](
    Time (0),
    Value (0, "x", 1), // in range
    Time (1),
    Value (1, "x", 0), // in range
    Time (2),
    Value (2, "x", 50), // in range
    Time (3),
    Value (3, "x", 30), // in range
    Time (4),
    Value (4, "x", 100), // in range
    Time (5),
    Value (5, "x", 120), // NOT in range
    Time (6),
    Value (6, "x", 130), // NOT in range
    Time (7),
    Value (7, "x", 80), // in range AGAIN
    Time (8),
    Value (8, "x", 101) // NOT in range
  )

  var abstractions = List[Abstraction](
    new PropValue(eventStream, "xInRange", 0, 0, (x) => 0 <= x && x <= 100, "x")
  )

  var i = 0
  var ready: Boolean = false
  while (!ready) {
    //
    // Next event
    //
    var event = eventStream(i)
    println()
    println(s"---------- event ${i}: ${event}")
    //
    // Abstraction
    //
    var insertions = 0
    for ( a <- abstractions) {
      a.eventStream = eventStream
      a.verify(event)
      if (a.eventStream.length != eventStream.length) insertions += a.eventStream.length - eventStream.length
      eventStream = a.eventStream
    }
    //
    // update event index
    //
    i += (insertions + 1)
    ready = i == eventStream.length || i > 100
  }
  //
  // log Event Stream
  //
  eventStream.foreach( println )
  //
  // log situations
  //
  eventStream.filter( (e) =>
    e match {
      case x: Situation => true
      case _ => false
    }
  ).foreach( println )
}
