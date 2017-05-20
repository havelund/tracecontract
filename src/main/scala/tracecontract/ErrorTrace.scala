/*
  © [2011]. California Institute of Technology. 
  ALL RIGHTS RESERVED. U.S. Government sponsorship acknowledged.  
  Any commercial use must be negotiated with the Office of Technology 
  Transfer at the California Institute of Technology. The technical data in 
  this document is controlled under the U.S. Export Regulations, release to 
  foreign persons may require an export authorization.
*/

package tracecontract

/**
 * An error (or success) trace represents the sequence of events that has lead to an error (success).
 * 
 * The trace consists of those events that have caused a property
 * to fail (succeed).
 *
 */
class ErrorTrace[Event]() {
  private var trace: List[(Int, Event)] = List()

  private[tracecontract] def addEvent(count: Int, event: Event) {
    trace ::= (count, event)
  }

  private[tracecontract] def getLastIndex : Int = trace(0)._1
  
  private[tracecontract] def getLastEvent : Event = trace(0)._2

  /**
   * Returns the error (success) trace.
   * 
   * @return the error (success) trace represented as a list of pairs, each pair 
   * ''(number,event)'' consisting of an event number (counting all events) 
   * and an event. The rightmost pair represents the latest event received.
   */
  def getTrace: List[(Int, Event)] = trace.reverse
  
  override def toString: String = {
    if (trace.isEmpty)
      "No trace."
    else {
      var result = "Trace:\n"
      for ((count, event) <- getTrace) { // getTrace reverses the trace
        result += "  " + count + "=" + event + "\n"
      }
      result
    }
  }
}