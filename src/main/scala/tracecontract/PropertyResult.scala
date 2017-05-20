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
 * A property result contains the errors of a particular property after a trace analysis.
 *
 * @param monitorName name of monitor in which property resides.
 * @param propertyName name of property violated.
 */

class PropertyResult[Event](val monitor: Monitor[Event], val propertyName: String) {
  private var safetyErrors: List[SafetyError[Event]] = List()
  private var livenessErrors: List[LivenessError[Event]] = List()
  private var successes: List[Success[Event]] = List()
  private var messages: List[Message] = List()

  private[tracecontract] def add(safetyError: SafetyError[Event]) {
    safetyErrors ++= List(safetyError)
  }

  private[tracecontract] def add(livenessError: LivenessError[Event]) {
    livenessErrors ++= List(livenessError)
  }

  private[tracecontract] def add(success: Success[Event]) {
    successes ++= List(success)
  }

  private[tracecontract] def add(newMessages: List[Message]) {
    messages ++= newMessages
  }

  /**
   * Returns the monitor that the property is part of.
   *
   * @return the monitor that the property is part of.
   */
  def getMonitor: Monitor[Event] = monitor

  /**
   * Returns the name of the property.
   *
   * @return the name of the property.
   */

  def getPropertyName: String = propertyName

  /**
   * Returns the list of safety errors.
   *
   * @return the list of safety errors.
   */
  def getSafetyErrors = safetyErrors

  /**
   * Returns the list of liveness errors.
   *
   * @return the list of liveness errors.
   */
  def getLivenessErrors = livenessErrors

  /**
   * Returns the list of successes.
   *
   * @return the list of successes.
   */
  def getSuccesses = successes

  /**
   * Returns the list of messages.
   *
   * @return the list of messages.
   */
  def getMessages = messages

  /**
   * The total number of safety and liveness errors for the property.
   *
   * @return the number of errors.
   */
  def numberOfErrors = safetyErrors.length + livenessErrors.length

  /**
   * Returns the severity of the monitor in which the property is defined.
   *
   * @return the severity of the monitor in which the property is defined.
   */

  def getSeverity: Severity = monitor.getSeverity

  override def toString: String = {
    var result = ""
    val propertyNameBlank = if (propertyName == "") "" else propertyName + " "
    result ++= "\nMonitor " + monitor.getFullMonitorName + " property " + propertyNameBlank + "violations: " + numberOfErrors + "\n\n"
    safetyErrors foreach (result += _.toString)
    livenessErrors foreach (result += _.toString)
    if (!messages.isEmpty) {
      result += "Messages:\n"
      result += "---------\n"
      messages foreach (result += _.toString)
    }
    result
  }
}