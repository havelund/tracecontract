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
 * Type representing severity degrees assigned to a monitor. Concrete severities
 * will have to be defined as ''case objects'' extending this class, and must define
 * a severity level. The smaller the level the higher the severity. A severity with
 * level = 1 has the highest severity.
 */

trait Severity {
  /**
   * Severity level assigned to severity. The smaller the number the higher the severity.
   * When error reports are printed severities with smaller numbers (higher severity)
   * are printed first.
   */


  val level: Int
}

/**
 * The different possible pre-defined severities a monitor can be assigned.
 * ''ERROR'' is the default for a monitor. Levels of these pre-defined severities
 * are defined with gaps (10,20,30)
 * to allow for user-defined severity levels in between.
 */

object Severity {

  /**
   * This severity (''level=10'') represents an error.
   */

  case object ERROR extends Severity {
    val level = 10
  }

  /**
   * This severity (''level=20'') represents a warning, which may represent a problem, which however,
   * may not be reacted upon.
   */

  case object WARNING extends Severity {
    val level = 20
  }

  /**
   * This severity (''level=30'') represents information about the monitoring. This may or may not represent
   * a problem.
   */

  case object INFO extends Severity {
    val level = 30
  }
}
