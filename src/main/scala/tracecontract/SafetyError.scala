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
 * A safety error represents an error caused by an event occurring that should not occur.
 *
 * An example of a property which can cause a safety error is the following, which states that
 * a command should not be issued more than once:
 *
 * <br>
 * 
 * <pre>
 * class Requirement extends Monitor[Event] {
 *   requirement('CommandAtMostOnce) {
 *     case COMMAND(x) =>
 *       state {
 *         case COMMAND(`x`) => error
 *       }
 *   }
 * }
 * </pre>
 * 
 * <br>
 * 
 * A trace of the form <code>COMMAND("A").COMMAND("A")</code>  will for example cause a safety error.
 * 
 * @param monitorName name of monitor in which property resides.
 * @param propertyName name of property violated.
 * @param explanation text provided by the ''informal'' method in class [[tracecontract.Monitor]]. Provides an informal
 * explanation of the property.
 * @param errorTrace the sequence of events that lead to the error, including the violating event.
 */

case class SafetyError[Event](
  monitorName: String,
  propertyName: String,
  explanation: String,
  errorTrace: ErrorTrace[Event]) extends Error[Event] {
  override def toString: String = {
    var result = ""
    def addln(text: String): String = { result += text + "\n"; result }
    val boarderLength = 30
    addln("-" * boarderLength)
    addln("Monitor: " + monitorName)
    if (propertyName != "")
      addln("Property " + propertyName + " violated")
    else
      addln("Property violated")
    addln(explanation)
    addln("Violating event number " + errorTrace.getLastIndex + ": " + errorTrace.getLastEvent)
    addln(errorTrace.toString)
    addln("-" * boarderLength)
  }
}
