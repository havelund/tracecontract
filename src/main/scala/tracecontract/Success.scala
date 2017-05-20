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
 * A success represents an event that causes a property to be (partially) satisfied.
 *
 * Consider the following property, which states that
 * a command should eventually be followed by a success:
 *
 * <br>
 * 
 * <pre>
 * class Requirement extends Monitor[Event] {
 *   requirement('CommandMustSucceed) {
 *     case COMMAND(x) =>
 *       hot {
 *         case SUCCESS(`x`) => ok
 *       }
 *   }
 * }
 * </pre>
 * 
 * <br>
 * 
 * A trace of the form <code>COMMAND("A").SUCCESS("A")</code>  will for example cause a success to be reported 
 * when the second event is observed.
 * 
 * @param monitorName name of monitor in which property resides.
 * @param propertyName name of property that (partially) succeeds.
 * @param explanation text provided by the ''informal'' method in class [[tracecontract.Monitor]]. Provides an informal
 * explanation of the property.
 * @param errorTrace the sequence of events that lead to the success, including the success-triggering event.
 */

case class Success[Event](
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
      addln("Property " + propertyName + " succeeds")
    else
      addln("Property succeeds")
    addln(explanation)
    addln("Succeeding event number " + errorTrace.getLastIndex + ": " + errorTrace.getLastEvent)
    addln(errorTrace.toString)
    addln("-" * boarderLength)
  }
}