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
 * A liveness error represents an error caused by a certain event not occurring in the trace.
 *
 * An example of a property which can cause a liveness error is the following:
 * 
 * <br>
 * 
 * <pre>
 * class Requirement extends Monitor[Event] {
 *   requirement('CommandMustSucceed) {
 *     case COMMAND(x) =>
 *       hot {
 *         case SUCCESS(x) => ok
 *       }
 *   }
 * }
 * </pre>
 * 
 * <br>
 * 
 * Suppose for example that a ''COMMAND("A")'' is observed, but no ''SUCCESS("A")'' is observed 
 * thereafter. In this case a liveness error will be produced.
 * 
 * @param monitorName name of monitor in which property resides.
 * @param propertyName name of property violated.
 * @param explanation text provided by the ''informal'' method in class [[tracecontract.Monitor]]. Provides an informal
 * explanation of the property.
 * @param errorTrace the sequence of events that lead to the error. For a liveness property typically the event(s)
 * that caused the expectation of the missing event. 
 */

case class LivenessError[Event](
	monitorName  : String, 
  propertyName : String, 
  explanation  : String, 
  errorTrace   : ErrorTrace[Event]) extends Error[Event]
{
  override def toString : String = {
    var result = ""
    def addln(text : String):String = {result += text + "\n"; result}
    val boarderLength = 30
    addln("-" * boarderLength)
    addln("Monitor: " + monitorName)  
    if (propertyName != "")
      addln("Property " + propertyName + " violated due to missing event")
    else
      addln("Property violated due to missing event")
    addln(explanation)
    addln(errorTrace.toString)
    addln("-" * boarderLength)
  }
}

