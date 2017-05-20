/*
  © [2011]. California Institute of Technology. 
  ALL RIGHTS RESERVED. U.S. Government sponsorship acknowledged.  
  Any commercial use must be negotiated with the Office of Technology 
  Transfer at the California Institute of Technology. The technical data in 
  this document is controlled under the U.S. Export Regulations, release to 
  foreign persons may require an export authorization.
*/

package tracecontract

import Util._

/**
 * The result of a trace analysis for a monitor.
 *
 * After a trace analysis, all detected errors for a monitor and all its sub-monitors are available
 * in an object of this class. The errors are organized per property: for each property a 
 * [[tracecontract.PropertyResult]] object contains all the errors for that property. 
 */

class MonitorResult[Event](monitorName: String) {
  private var propertyResults: List[PropertyResult[Event]] = List()

  private[tracecontract] def add(propertyResult: PropertyResult[Event]) {
    propertyResults ++= List(propertyResult)
  }

  /**
   * Returns the name of the monitor.
   *
   * @ return the name of the monitor.
   */

  def getMonitorName: String = monitorName

  /**
   * Returns the list of errors detected, per property.
   *
   * For each property in the monitor and all its sub-monitors a [[tracecontract.PropertyResult]] object
   * is returned containing those errors.
   *
   * @return the list of errors, organized per property in the monitor and all its sub-monitors.
   */
  def getPropertyResults: List[PropertyResult[Event]] = propertyResults

  /**
   * returns the number of errors detected.
   *
   * @return the number of errors, computed as the sum of errors for each property.
   */
  def numberOfErrors: Int = {
    var errors = 0
    for (propertyResult <- propertyResults) {
      errors += propertyResult.numberOfErrors
    }
    errors
  }

  override def toString: String = {
    var result = "--- Monitor " + monitorName + " ---\n\n"
    result += "Total number of reports: " + numberOfErrors
    for ((severity, results) <- partition) {
      result += "\n\n" + toHeadline("=", "SEVERITY=" + severity + ":") + "\n"
      results foreach {
        result += _.toString
      }
    }
    result
  }

  private def partition: List[(Severity, List[PropertyResult[Event]])] = {
    var result: List[(Severity, List[PropertyResult[Event]])] = Nil
    var sequence: List[PropertyResult[Event]] = List()
    var severity: Severity = null
    val sortedPropertyResults = propertyResults.sortWith((r1, r2) => r1.getSeverity.level < r2.getSeverity.level)
    for (presult <- sortedPropertyResults) {
      if (severity != presult.getSeverity) {
        if (severity != null) result :+=(severity, sequence)
        severity = presult.getSeverity
        sequence = Nil
      }
      sequence :+= presult
    }
    result :+=(severity, sequence)
    result
  }
}
