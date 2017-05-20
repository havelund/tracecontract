
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
 * This class offers features for defining facts, which can be added to or removed from a fact 
 * database (a set). The database can furthermore be queried for fact membership.
 *
 * Facts are used to model past time temporal properties. We store facts about an execution in order to
 * later be able to query them.
 * 
 * The following example illustrates a specification of the property: 
 * ''"a report about a command name can only be issued if a command with that name has been issued and
 * has not succeeded since then"'':
 * 
 * <br>
 * 
 * <pre>
 * class Requirement extends class Monitor[Event] {
 *   case class Commanded(name: String) extends Fact // declare fact: that a command has been issued
 *   
 *   class Requirement extends Monitor[Event] {
 *     require {
 *       case COMMAND(x)  => Commanded(x) +  // add the fact that x has been commanded
 *       case SUCCESS(x)  => Commanded(x) -  // delete the fact that x has been commanded
 *       case REPORT(x,_) => Commanded(x) ?  // test that x has been commanded
 *     }
 *   }
 * }
 * </pre>
 * 
 */

class DataBase {
  /**
   * Facts to be added to and removed from the fact database.
   * 
   * Any fact has to be defined as an object or class extending this class. For example:
   * <pre>
   * case class HasBeenExecuted(command: String) extends Fact
   * </pre>
   */
  abstract class Fact	
	
  private var facts: Set[Fact] = Set()
  private var toRecord: Set[Fact] = Set()
  private var toRemove: Set[Fact] = Set()

  /** Operations on Facts. 
   * 
   * A ''fact'' of type [[tracecontract.DataBase.Fact]] is implicitly lifted to an object of this 
   * type. Various methods are defined for adding and removing the fact to or from the fact database,
   * and query whether the fact belongs to the fact database. 
   * Note that all changes to the fact database are realized in the '''next step''' (not the current).
   * This avoids non-deterministic behavior amongst a set of properties. 
   */

  class FactOps(fact: Fact) {
    /** 
     * Adds the fact to the fact database in the next step. 
     */
     def + : Unit = { toRecord += fact }

    /** 
     * Deletes the fact from the fact database in the next step. 
     */	
    def - : Unit = { toRemove += fact }

    /**
     * Tests that the fact is in the fact database.
     * 
     * @return true iff. fact is in the database.
     */
    def ? : Boolean = facts contains fact

    /**
     * Tests that the fact is not in the database.
     *     
     * @return true iff. fact is not in database.
     */
    def ~ : Boolean = !(facts contains fact)

    /**
     * Tests that the fact is in the database, and removes the fact in the next step.
     * 
     * @return true iff. the fact is in the database.
     */
    def ?- : Boolean = {
      toRemove += fact
      facts contains fact
    }

    /**
     * Tests that the fact is not in the database, and adds the fact in the next step.
     * 
     * @return true iff. the fact is in the database.
     */    
    def ~+ : Boolean = {
      toRecord += fact
      !(facts contains fact)
    }
  }

  /**
   * Tests whether a fact exists in the fact database, which satisfies a predicate.
   * 
   * @param pred the predicate matched against facts in the database. The partial
   * function does not need to be defined for all possible facts. It is interpreted
   * to return false when applied to facts not in its domain. 
   * @return true iff. a fact ''f'' exists in the database, for which ''pred(f)'' holds.
   */
  def factExists(pred : PartialFunction[Fact,Boolean]) : Boolean = {
	  //facts exists (pred(_))
          facts exists (pred orElse { case _ => false})
  }
  /**
   * Converts a fact to a [[tracecontract.DataBase.FactOps]] object, which defines operations on the fact.
   * 
   * @param fact the fact to be converted.
   * @return a FactOps object offering operations on the fact.
   */
  implicit def convFact2FactOps(fact : Fact) : FactOps = new FactOps(fact)

  private[tracecontract] def updateFacts() {
    Debug.debug("to remove: " + toRemove)
    Debug.debug("to record: " + toRecord)
    toRemove foreach (facts -= _)
    toRecord foreach (facts += _)
    toRecord = Set()
    toRemove = Set()
  }
 
  override def toString: String = facts.mkString("facts = {", ",", "}")
}
