/*
  © [2011]. California Institute of Technology. 
  ALL RIGHTS RESERVED. U.S. Government sponsorship acknowledged.  
  Any commercial use must be negotiated with the Office of Technology 
  Transfer at the California Institute of Technology. The technical data in 
  this document is controlled under the U.S. Export Regulations, release to 
  foreign persons may require an export authorization.
*/

package tracecontract

import java.awt.Toolkit
import concurrent.Lock

/**
 * This trait provides the classes and methods with which to construct formulas. Formulas
 * are passed as arguments to the methods ''property(formula: Formula)'' and ''property(name: Symbol)(formula: Formula)'' in
 * class [[tracecontract.Monitor]].
 */

trait Formulas[Event] {
  private[tracecontract] var messages: List[Message] = Nil

  /**
   * Defines the type of transitions out of a state.
   * Transitions are represented as a partial function from events to formulas, referred to as a ''block''.
   * A block ''f'' is defined for an event ''e'' (triggers on ''e'') if ''f.isDefinedAt(e) == true''.
   * The result of applying ''f'' to an event is the formula obtained by the call: ''f(e)''.
   */
  type Block = PartialFunction[Event, Formula]

  /*---------*/
  /* Formula */
  /*---------*/

  /**
   * Each different kind of formula supported by TraceContract is represented by an object or class that
   * extends this class. The class is abstract so it is itself cannot be instantiated to give a formula.
   *
   * The class offers a collection of methods that can be called with infix
   * notation [[http://en.wikipedia.org/wiki/Infix_notation]].
   * For example, assume two formulas ''f,,1,,'' and ''f,,2,,'' - then it is possible to write formulas of
   * the form:
   * <p><br><p>
   * <pre>
   * // --- propositional logic:
   *
   * f1 and f2
   * f1 or f2
   * f1 implies f2
   *
   * // --- temporal logic:
   *
   * f1 until f2   // f1 must hold until f2 holds, and f2 must eventually hold
   * f1 unless f2  // f1 must hold until f2 holds, but f2 may never hold
   *
   * // --- other operators:
   *
   * f1 then f2    // first f1 shall hold and if becoming true f2 must hold
   * f1 causes f2  // if f1 evaluates to true, then thereafter f2 must hold
   * </pre>
   * <p><br><p>
   * Infix operators with two arguments have <i>short-circuit</i> semantics in the following sense:
   * the operators ''and'', ''or'' and ''implies'' do not evaluate their second argument if
   * the result can be determined from the value of the first argument. For example
   * ''False and p'' will not evaluate ''p'', and ''True or p'' will not evaluare ''p'' neither.
   * Likewise, the operators ''until'' and ''unless'' do not evaluate their first argument if the result can be determined
   * from the second argument.
   * <p>
   * Note that these methods can also be called with traditional object oriented programming dot-notation, as in:
   * <p><br><p>
   * <pre>
   * f1.until(f2)
   * </pre>
   */
  abstract class Formula {
    def apply(event: Event): Formula
    private[tracecontract] def reduce(): Formula = this
    private[tracecontract] var onFailText: String = null
    private[tracecontract] var onFailCode: Unit => Unit = null

    /**
     * Defines propositional logical implication.
     * If ''this'' eventually evaluates to True, then ''that'' must eventually evaluate to True.
     * If ''this'' eventually evaluates to False, the implication also evaluates to True.
     * Note that ''this'' may not evaluate to True immediately (as in classical
     * propositional logic) but may take several steps to do so. However, ''that'' is monitored '''right away'''. That
     * is, we do not wait until ''this'' evaluates to either False or True.
     * Take for example the following formula:
     * <p><br><p>
     * <pre>
     * eventually(REPORT("REBOOT")) implies eventually(REPORT("CALL_HOME"))
     * </pre>
     * <p><br><p>
     * This formula states that (from ''"now on"'') if there is eventually
     * a report of a reboot, then there should also eventually in the trace, either before
     * or after the report on the reboot, be a report on call to home. This formula
     * will return true on both of the following traces:
     * <p><br><p>
     * <pre>
     * trace1 = List(...,REPORT("REBOOT"),...,REPORT("CALL_HOME"),...)
     * trace2 = List(...,REPORT("CALL_HOME"),...,REPORT("REBOOT"),...)
     * </pre>
     * <p><br><p>
     *
     * @param that the formula that must be True if ''this'' evaluates to True.
     * @return the formula representing the implication.
     */
    def implies(that: Formula): Formula = Implies(this, that).reduce()

    /**
     * Defines the LTL until operator.
     * The property ''f,,1,, until f,,2,,'' states that eventually ''f,,2,,'' must become True, and until
     * then ''f,,1,,'' must be True in every step (including now, but excluding the point where ''f,,2,,'' becomes True).
     *
     * @param that the formula that must eventually become True, thereby permitting ''this'' to no longer be True.
     * @return the ''until''-formula.
     */
    def until(that: Formula): Formula = Until(this, that).reduce()

    /**
     * Defines the LTL weak until operator, here called unless.
     * The property ''f,,1,, unless f,,2,,'' states that ''f,,1,,'' must be True in every step
     * until ''f,,2,,'' becomes True or till the end of the trace if ''f,,2,,'' never becomes True.
     * The following equality holds:
     * <p><br><p>
     * <pre>
     * f1 unless f2 == (f1 until f2) or always(f1)
     * </pre>
     * <p><br><p>
     * @param that the formula that may eventually become True, thereby permitting ''this'' to no longer be True.
     * @return the ''unless''-formula.
     */
    def unless(that: Formula): Formula = Unless(this, that).reduce()

    /**
     * Logical ''and'' (conjunction) of two formulas.
     * The formula evaluates to True if and only if both formulas evaluate to True.
     *
     * @param that the right hand argument of the conjunction.
     * @return the ''and''-formula.
     */
    def and(that: Formula): Formula = And(this, that).reduce()

    /**
     * Logical ''or'' (disjunction) of two formulas.
     * The formula evaluates to True if and only if one of the two
     * formulas evaluate to True.
     *
     * @param that the right hand argument of the disjunction.
     * @return the ''or''-formula.
     */
    def or(that: Formula): Formula = Or(this, that).reduce()

    /**
     * Sequential composition of formulas.
     * The formula ''f,,1,, then f,,2,,'' has the following interpretation:
     * ''f,,1,,'' is first evaluated. If it evaluates to False, the whole formula evaluates to False.
     * If it evaluates to True, evaluation continues with ''f,,2,,''.
     *
     * @param that the formula that is evaluated after ''this''.
     * @return the ''then''-formula.
     */
    def then(that: Formula): Formula = AndThen(this, that).reduce()

    /**
     * Causal composition of formulas.
     * The formula ''f,,1,, causes f,,2,,'' has the following interpretation:
     * ''f,,1,,'' is first evaluated. If it evaluates to False, the whole formula evaluates to True.
     * If it evaluates to True, evaluation '''at that point''' (and not before) continues with ''f,,2,,''.
     *
     * Take for example the following formula (compare to the semantics of ''f,,1,, implies f,,2,,''):
     * <p><br><p>
     * <pre>
     * eventually(REPORT("REBOOT")) causes eventually(REPORT("CALL_HOME"))
     * </pre>
     * <p><br><p>
     * This formula states that (from now on) if there is eventually
     * a report of a reboot, then '''there after''' there should eventually be a report
     * on call to home. Consider the two traces:
     * <p><br><p>
     * <pre>
     * trace1 = List(...,REPORT("REBOOT"),...,REPORT("CALL_HOME"),...)
     * trace2 = List(...,REPORT("CALL_HOME"),...,REPORT("REBOOT"),...)
     * </pre>
     * <p><br><p>
     * The formula will evaluate to True on the first, but to False on the second.
     * This is on contrast to the formula:
     * <p><br><p>
     * <pre>
     * eventually(REPORT("REBOOT")) implies eventually(REPORT("CALL_HOME"))
     * </pre>
     * <p><br><p>
     * which will evaluate to True on both traces.
     * <p><br><p>
     * @param that the formula that must be True if ''this'' evaluates to True.
     * @return the ''causes''-formula.
     */
    def causes(that: Formula): Formula = Causes(this, that).reduce()

    /**
     * Hierarchical composition of formulas.
     * The formula ''f except b'' has the following interpretation: ''f'' is
     * evaluated until it either evaluates to True or False, '''or''' until the
     * block ''b'' is defined for the incoming event, in which case evaluation continues
     * with ''b(e)''. In this case ''f'' is just ignored (it is __not__ evaluated as if it was the end of the trace).
     *
     * Take for example the following formula:
     * <p><br><p>
     * <pre>
     * never(REPORT("REBOOT")) except {case FAIL(_) => eventually("REBOOT")}
     * </pre>
     * <p><br><p>
     * This formula states that there should be no reboot, unless a fail is observed, in
     * which case a reboot must thereafter eventually occur.
     * <p><br><p>
     * The operator can be used to model hierarchical state charts (as in: ''statemachine except supertransitions'').
     * <p><br><p>
     * @param block the transition block that will release the formula (''this'') from having to be satisfied.
     * @return the ''except''-formula.
     */
    def except(block: Block): Formula = Except(this, block)

    /**
     * Formula bounded by an event.
     * The formula ''f uptoEvent e'' has the following interpretation: ''f'' is
     * evaluated until it either evaluates to True or False, '''or''' until the
     * the event ''e'' occurs, whichever occurs first. In the latter case, what remains
     * of the formula ''f'' is evaluated as if it was the end of the trace, and any missing events will
     * be reported.
     *
     * Take for example the following formula:
     * <p><br><p>
     * <pre>
     * (never(FAIL("DRIVE")) and eventually(SUCCESS("DRIVE"))) uptoEvent COMMAND("REBOOT")
     * </pre>
     * <p><br><p>
     * This formula states that there should be no ''FAIL("DRIVE")'' and eventually a ''SUCCESS("DRIVE")'',
     * up and until a ''COMMAND("REBOOT")'' is observed, if any. Consider the following traces:
     * <p><br><p>
     * <pre>
     * trace1 = List(REPORT("SOFARSOGOOD"),SUCCESS("DRIVE),COMMAND("REBOOT"),FAIL("DRIVE"))
     * trace1 = List(REPORT("SOFARSOGOOD"),COMMAND("REBOOT"),SUCCESS("DRIVE))
     * </pre>
     * <p><br><p>
     * The first trace satisfies the property whereas the second does not.
     * <p><br><p>
     * @param endEvent the event that will terminate evaluation of the formula (''this'').
     * @return the ''uptoEvent''-formula.
     */
    def uptoEvent(endEvent: Event): Formula = UptoMatch(this, (event: Event) => event == endEvent).reduce()

    /**
     * Formula bounded by an event predicate.
     * The formula ''f uptoMatch p'' has the following interpretation: ''f'' is
     * evaluated until it either evaluates to True or False, '''or''' until an
     * event ''e'' occurs for which ''p(e)'' is defined and yields true.
     * In the latter case, what remains of the formula ''f'' is evaluated as if it was the
     * end of the trace, and any missing events will be reported.
     *
     * Take for example the following formula:
     * <p><br><p>
     * <pre>
     * (never(FAIL("DRIVE")) and eventually(SUCCESS("DRIVE"))) uptoMatch {case COMMAND(_) => true}
     * </pre>
     * <p><br><p>
     * This formula states that there should be no ''FAIL("DRIVE")'' and eventually a ''SUCCESS("DRIVE")'',
     * up and until a ''COMMAND'' of any kind is observed, if any. Consider the following traces:
     * <p><br><p>
     * <pre>
     * trace1 = List(REPORT("SOFARSOGOOD"),SUCCESS("DRIVE),COMMAND("PICTURE"),FAIL("DRIVE"))
     * trace2 = List(REPORT("SOFARSOGOOD"),COMMAND("PICTURE"),SUCCESS("DRIVE))
     * </pre>
     * <p><br><p>
     * The first trace satisfies the property whereas the second does not.
     * <p><br><p>
     * @param endPredicate the event predicate that is applied to event to determine when to stop monitor ''this''.
     * @return the ''uptoMatch''-formula.
     */
    def uptoMatch(endPredicate: Event => Boolean): Formula = UptoMatch(this, endPredicate)

    /**
     * Formula bounded by a block of transitions.
     * The formula ''f upto b'' has the following interpretation: ''f'' is
     * evaluated until it either evaluates to True or False, '''or''' until an
     * event ''e'' occurs for which ''b.isDefinedAt(e)'' is true.
     * The latter is not required to happen, but in case it does, what remains of the
     * formula ''f'' is evaluated as if it was the end of the trace, and any missing
     * events will be reported. If no errors occur due to this check, the formula continues as ''b(e)''.
     *
     * Take for example the following formula:
     * <p><br><p>
     * <pre>
     * (never(FAIL("DRIVE")) and eventually(SUCCESS("DRIVE"))) upto {
     *   case COMMAND(x) => eventually(SUCCESS(x))
     * }
     * </pre>
     * <p><br><p>
     * This formula states that there should be no ''FAIL("DRIVE")'' and eventually a ''SUCCESS("DRIVE")'',
     * up and until a ''COMMAND'' of any kind is observed, if any, in which case a success of that
     * command should eventually be observed. Consider the following traces:
     * <p><br><p>
     * <pre>
     * trace1 = List(REPORT("SOFARSOGOOD"),SUCCESS("DRIVE"),COMMAND("PICTURE"),SUCCESS("PICTURE"))
     * trace2 = List(REPORT("SOFARSOGOOD"),COMMAND("PICTURE"),SUCCESS("PICTURE"))
     * </pre>
     * <p><br><p>
     * The first trace satisfies the property whereas the second does not.
     * <p><br><p>
     *
     * @param block the transitions that can terminate the monitoring of ''this'' formula.
     * @return the ''upto''-formula.
     */
    def upto(block: Block): Formula = Upto(this, block)

    /**
     * Formula bounded by a block of transitions, one of which has to trigger eventually.
     * The formula ''f uptoRequired b'' has the following interpretation: ''f'' is
     * evaluated until it either evaluates to True or False, '''or''' until an
     * event ''e'' occurs for which ''b.isDefinedAt(e)'' is true.
     * The latter is required to happen, and when it does, what remains of the
     * formula ''f'' is evaluated as if it was the end of the trace, and any missing
     * events will be reported. If no errors occur due to this check, the formula continues as ''b(e)''.
     *
     * Take for example the following formula:
     * <p><br><p>
     * <pre>
     * (never(FAIL("DRIVE")) and eventually(SUCCESS("DRIVE"))) uptoRequired {
     *   case COMMAND(x) => eventually(SUCCESS(x))
     * }
     * </pre>
     * <p><br><p>
     * This formula states that there should be no ''FAIL("DRIVE")'' and eventually a ''SUCCESS("DRIVE")'',
     * up and until a ''COMMAND'' of any kind is observed, required, in which case a success of that
     * command should eventually be observed. Consider the following traces:
     * <p><br><p>
     * <pre>
     * trace1 = List(REPORT("SOFARSOGOOD"),SUCCESS("DRIVE"),COMMAND("PICTURE"),SUCCESS("PICTURE"))
     * trace2 = List(REPORT("SOFARSOGOOD"),COMMAND("PICTURE"),SUCCESS("PICTURE"))
     * trace3 = List(REPORT("SOFARSOGOOD"),SUCCESS("DRIVE"))
     * </pre>
     * <p><br><p>
     * The first trace satisfies the property whereas the second an third do not.
     * <p><br><p>
     *
     * @param block the transitions that will terminate the monitoring of ''this'' formula.
     * @return the ''uptoRequired''-formula.
     */
    def uptoRequired(block: Block): Formula = UptoRequired(this, block)

    /**
     * Attaches to a liveness formula a string to be printed in case the formula fails to be satisfied before the end of the trace.
     * A formula of the form ''f onfail text'' will evaluate as ''f'' until the end of the trace. In case it at that point
     * evaluates to false as a liveness violation (a missing event), the string ''text'' is printed on standard out
     * in addition to the default error message. This can be used to print additional information about data objects.
     * The following example illustrates how ''onfail'' is used to print the name of a command that does not succeed:
     * <p><br><p>
     * <pre>
     * class Requirement extends Monitor[Event] {
     *   require {
     *     case COMMAND(x) =>
     *       hot {
     *         case SUCCESS(`x`) => ok
     *       } onfail ("command " + x + " did not succeed")
     *   }
     * }
     * </pre>
     * <p><br><p>
     * @param text the string to be printed in case of a liveness violation.
     * @return the original formula. That is, the ''onfail'' method returns the original formula, but has as side effect to
     * record the text message.
     */
    def onfail(text: String): Formula = {
      onFailText = text
      this
    }

    /**
     * Attaches to a liveness formula a block of code to be executed in case the formula fails to be satisfied before the end of the trace.
     * A formula of the form ''f onfail {code}'' will evaluate as ''f'' until the end of the trace. In case it at that point
     * evaluates to false as a liveness violation (a missing event), the code block ''code'' is executed.
     * The following example illustrates how ''onfail'' is used to print the name of a command that does not succeed:
     * <p><br><p>
     * <pre>
     * class Requirement extends Monitor[Event] {
     *   require {
     *     case COMMAND(x) =>
     *       hot {
     *         case SUCCESS(`x`) => ok
     *       } onfail {
     *         println("command " + x + " did not succeed")
     *       }
     *   }
     * }
     * </pre>
     * <p><br><p>
     * @param code the code to be executed in case of a liveness violation.
     * @return the original formula. That is, the ''onfail'' method returns the original formula, but has as side effect to
     * record the code block.
     */
    def onfail(code: => Unit): Formula = {
      onFailCode = ((u: Unit) => code)
      this
    }

  }

  /*---------------------*/
  /* Propositional Logic */
  /*---------------------*/

  /**
   *  The value ''true''.
   */
  case object True extends Formula {
    override def apply(event: Event): Formula = this
  }

  /**
   * The value ''false''.
   */
  case object False extends Formula {
    override def apply(event: Event): Formula = this
  }

  private case class Now(expectation: Event) extends Formula {
    override def apply(event: Event): Formula = if (expectation == event) True else False

    override def toString: String = expectation.toString
  }

  private case class Matches(pat: PartialFunction[Event, Boolean]) extends Formula {
    override def apply(event: Event): Formula = {
      if (pat.isDefinedAt(event))
        pat(event)
      else
        false
    }

    override def toString: String = "{case ... => ...}"
  }

  private case class Pred(pred: Unit => Boolean) extends Formula {
    override def apply(event: Event): Formula = if (pred()) True else False

    override def toString: String = "(... cond ...)"
  }

  private case class And(formula1: Formula, formula2: Formula) extends Formula {
    override def apply(event: Event): Formula = { 
      val formula1_ = formula1(event)
      formula1_ match { 
        case False => False
        case True => formula2(event)
        case _ =>
          val formula2_ = formula2(event)
          formula2_ match { 
            case False => False
            case True => formula1_
            case `formula1_` => formula1_
            case _ => And(formula1_,formula2_)
          }
      }
    }

    override def reduce(): Formula = {
      (formula1, formula2) match {
        case (False, _) => False
        case (_, False) => False
        case (True, _) => formula2
        case (_, True) => formula1
        case (f1, f2) if f1 == f2 => f1
        case _ => this
      }
    }

    override def toString: String = "(" + formula1 + " /\\ " + formula2 + ")"
  }

  private case class Or(formula1: Formula, formula2: Formula) extends Formula {
    override def apply(event: Event): Formula = { 
      val formula1_ = formula1(event)
      formula1_ match { 
        case True => True
        case False => formula2(event)
        case _ =>
          val formula2_ = formula2(event)
          formula2_ match { 
            case True => True
            case False => formula1_
            case `formula1_` => formula1_
            case _ => Or(formula1_,formula2_)
          }
      }
    }

    override def reduce(): Formula = {
      (formula1, formula2) match {
        case (True, _) => True
        case (_, True) => True
        case (False, _) => formula2
        case (_, False) => formula1
        case (f1, f2) if f1 == f2 => f1
        case _ => this
      }
    }

    override def toString: String = "(" + formula1 + " \\/ " + formula2 + ")"
  }

  private case class Implies(formula1: Formula, formula2: Formula) extends Formula {
    override def apply(event: Event): Formula = { 
      val formula1_ = formula1(event)
      formula1_ match { 
        case False => True
        case True  => formula2(event)
        case _ =>
          val formula2_ = formula2(event)
          formula2_ match { 
            case False => Not(formula1_)
            case True => True
            case _ => Implies(formula1_,formula2_)
          }
      }
    }

    override def reduce(): Formula = {
      (formula1, formula2) match {
        case (True, _) => formula2
        case (_, True) => True
        case (False, _) => True
        case (_, False) => Not(formula1)
        case _ => this
      }
    }

    override def toString: String = "(" + formula1 + " ==> " + formula2 + ")"
  }

  private case class Not(formula: Formula) extends Formula {
    override def apply(event: Event): Formula = Not(formula(event)).reduce()

    override def reduce(): Formula = {
      formula match {
        case True => False
        case False => True
        case _ => this
      }
    }

    override def toString: String = "!(" + formula + ")"
  }

  /*----------------*/
  /* Temporal Logic */
  /*----------------*/

  private[tracecontract] case class Globally(formula: Formula) extends Formula {
    override def apply(event: Event): Formula = And(this, formula(event)).reduce()

    override def toString: String = "globally(" + formula + ")"
  }

  private case class Eventually(formula: Formula) extends Formula {
    override def apply(event: Event): Formula = Or(this, formula(event)).reduce()

    override def toString: String = "eventually(" + formula + ")"
  }

  private case class Never(formula: Formula) extends Formula {
    override def apply(event: Event): Formula = And(this, Not(formula(event)).reduce()).reduce()

    override def toString: String = "never(" + formula + ")"
  }

  private case class Until(formula1: Formula, formula2: Formula) extends Formula {
    override def apply(event: Event): Formula = { 
      val formula2_ = formula2(event)
      formula2_ match { 
        case True => True
        case False => And(this, formula1(event)).reduce()
        case _ => Or(formula2_,And(this, formula1(event)).reduce()).reduce()
      }
    }

    override def toString: String = "(" + formula1 + " until " + formula2 + ")"
  }

  private case class Unless(formula1: Formula, formula2: Formula) extends Formula {
    override def apply(event: Event): Formula = { 
      val formula2_ = formula2(event)
      formula2_ match { 
        case True => True
        case False => And(this, formula1(event)).reduce()
        case _ => Or(formula2_,And(this, formula1(event)).reduce()).reduce()
      }
    }

    override def toString: String = "(" + formula1 + " unless " + formula2 + ")"
  }

  private case class StrongNext(formula: Unit => Formula) extends Formula {
    override def apply(event: Event): Formula = formula()

    override def toString: String = "strongnext(" + formula + ")"
  }

  private case class WeakNext(formula: Unit => Formula) extends Formula {
    override def apply(event: Event): Formula = formula()

    override def toString: String = "weaknextstep(" + formula + ")"
  }

  private case class Exec(formula: Unit => Formula) extends Formula {
    override def apply(event: Event): Formula = formula()

    override def toString: String = "exec(" + formula + ")"
  }

  private case class AndThen(formula1: Formula, formula2: Formula) extends Formula {
    override def apply(event: Event): Formula = {
      val formula1_ = formula1(event).reduce()
      formula1_ match {
        case False => False
        case True => formula2
        case _ => AndThen(formula1_, formula2)
      }
    }

    override def reduce(): Formula = AndThen(formula1.reduce(), formula2)

    override def toString: String = formula1 + ";" + formula2
  }

  private case class Causes(formula1: Formula, formula2: Formula) extends Formula {
    override def apply(event: Event): Formula = {
      val formula1_ = formula1(event)
      formula1_ match {
        case False => True
        case True => formula2
        case _ => Causes(formula1_, formula2)
      }
    }

    override def reduce(): Formula = Causes(formula1.reduce(), formula2)

    override def toString: String = formula1 + " causes " + formula2
  }

  private case class UptoMatch(formula: Formula, pred: Event => Boolean) extends Formula {
    override def apply(event: Event): Formula = {
      if (pred(event)) {
        end(formula)
      } else {
        val formula_ = formula(event)
        formula_ match {
          case False => False
          case True => True
          case _ => UptoMatch(formula_, pred)
        }
      }
    }

    override def toString: String = formula + " uptoMatch {... => ...}"
  }

  private case class Upto(formula: Formula, block: Block) extends Formula {
    override def apply(event: Event): Formula = {
      if (block.isDefinedAt(event)) {
        And(end(formula),block(event)).reduce()
      } else {
        val formula_ = formula(event).reduce()
        formula_ match {
          case False => False
          case True => True
          case `formula` => this
          case _ => Upto(formula_, block)
        }
      }
    }

    override def toString: String = formula + " upto {... => ...}"
  }

  private case class UptoRequired(formula: Formula, block: Block) extends Formula {
    override def apply(event: Event): Formula = {
      if (block.isDefinedAt(event)) {
        And(end(formula),block(event)).reduce()
      } else {
        val formula_ = formula(event).reduce()
        formula_ match {
          case False => False
          case True => True
          case `formula` => this
          case _ => UptoRequired(formula_, block)
        }
      }
    }

    override def toString: String = formula + " uptoRequired {... => ...}"
  }

  private case class Except(formula: Formula, block: Block) extends Formula {
    override def apply(event: Event): Formula = {
      if (block.isDefinedAt(event)) {
        block(event)
      } else {
        val formula_ = formula(event).reduce()
        formula_ match {
          case False => False
          case True => True
          case `formula` => this
          case _ => Except(formula_, block)
        }
      }
    }

    override def reduce(): Formula = Except(formula.reduce(), block)

    override def toString: String = formula + " except { ... }"
  }

  private case class Within(time: Int, var formula: Formula) extends Formula {
    private var lock = new Lock
    private var timedOut = false

    val timer = Timer(time) {
      lock.acquire
      formula = end(formula)
      timedOut = true
      lock.release
    }

    override def apply(event: Event): Formula = {
      lock.acquire
      var result: Formula = this
      if (timedOut)
        result = formula
      else {
        formula = formula(event)
        if (formula == True || formula == False) {
          timer.stop()
          result = formula
        }
      }
      lock.release
      result
    }

    override def reduce(): Formula = {
      lock.acquire
      var result = if (timedOut) formula else this
      lock.release
    }

    override def toString: String = {
      lock.acquire
      var result = "within (" + time + ") " + formula
      lock.release
      result
    }
  }

  /*----------------*/
  /* State Machines */
  /*----------------*/

  private case class Step(block: Block) extends Formula {
    override def apply(event: Event): Formula = {
      if (block.isDefinedAt(event))
        block(event)
      else
        True
    }

    override def toString: String = "step{...}"
  }

  private case class State(block: Block) extends Formula {
    override def apply(event: Event): Formula = {
      if (block.isDefinedAt(event))
        block(event)
      else
        this
    }

    override def toString: String = "state{...}"
  }

  private case class Hot(block: Block) extends Formula {
    override def apply(event: Event): Formula = {
      if (block.isDefinedAt(event))
        block(event)
      else
        this
    }

    override def toString: String = "hot{...}"
  }

  private case class Strong(block: Block) extends Formula {
    override def apply(event: Event): Formula = {
      if (block.isDefinedAt(event))
        block(event)
      else
        False
    }

    override def toString: String = "strong{...}"
  }

  private case class Weak(block: Block) extends Formula {
    override def apply(event: Event): Formula = {
      if (block.isDefinedAt(event))
        block(event)
      else
        False
    }

    override def toString: String = "weak{...}"
  }

  private case class UntilState(block: Block) extends Formula {
    override def apply(event: Event): Formula = {
      if (block.isDefinedAt(event)) {
        val formula = block(event)
        formula match {
          case False => this
          case _ => formula
        }
      } else
        this
    }

    override def toString: String = "until{...}"
  }

  private case class UnlessState(block: Block) extends Formula {
    override def apply(event: Event): Formula = {
      if (block.isDefinedAt(event)) {
        val formula = block(event)
        formula match {
          case False => this
          case _ => formula
        }
      } else
        this
    }

    override def toString: String = "unless{...}"
  }

  /*--------------*/
  /* End Function */
  /*--------------*/

  private[tracecontract] def end(formula: Formula): Boolean = {
    val result =
      formula match {
        case True => true
        case False => true // False must have been reported as SafetyError
        case Now(_) => false
        case Matches(_) => false
        case Pred(_) => false
        case And(formula1, formula2) =>
          end(formula1) && end(formula2)
        case Or(formula1, formula2) =>
          end(formula1) || end(formula2)
        case Implies(formula1, formula2) =>
          !end(formula1) || end(formula2)
        case Not(formula: Formula) =>
          !end(formula)
        case Globally(_) => true
        case Eventually(_) => false
        case Never(_) => true
        case Until(_, _) => false
        case Unless(_, _) => true
        case StrongNext(_) => false
        case WeakNext(_) => true
        case Exec(_) => true
        case AndThen(formula1, _) => end(formula1)
        case Causes(_, _) => true
        case UptoMatch(formula, _) => end(formula)
        case Upto(formula, _) => end(formula)
        case UptoRequired(formula, _) => false
        case Except(formula, _) => end(formula)
        case Within(_, formula) => end(formula)
        case Step(_) => true
        case State(_) => true
        case Hot(_) => false
        case Strong(_) => false
        case Weak(_) => true
        case UntilState(_) => false
        case UnlessState(_) => true
        case IfThenElse(condition, formula1, formula2) =>
          if (end(condition)) 
             end(formula1) && formula1 != False
          else 
             end(formula2) && formula2 != False
        case _ => endUserDefined(formula)
      }
    if (!result) {
      if (formula.onFailText != null)
        println(formula.onFailText)
      if (formula.onFailCode != null)
        formula.onFailCode()
    }
    result
  }

  /**
   * Determines whether a user-defined formula evaluates to true or false at the end of the trace.
   * This function must be overridden by the user whenever a new ''Formula'' is added. That is, whenever
   * a new sub-class of the class ''Formula'' is defined.
   *
   * As an example, consider that we add a formula for checking that a formula becomes true between ''min'' and ''max'' steps from now:
   * <p><br><p>
   * <pre>
   *
   * // Definition of operator, by defining a new monitor class, in which class Formula is extended and in which we override the function endUserDefined:
   *
   * class MyMonitor[Event] extends Monitor[Event] {
   *   case class NextN(now: Int, min: Int, max: Int, formula: Formula) extends Formula {
   *     override def apply(event: Event): Formula = {
   *       val newnow = now + 1
   *       if (newnow < min) {
   *         NextN(newnow, min, max, formula)
   *       } else if (newnow > max) {
   *         False
   *       } else {
   *         formula(event) or NextN(newnow, min, max, formula)
   *       }
   *     }
   *   }
   *
   *   override def endUserDefined(formula: Formula): Boolean =
   *     formula match {
   *       case NextN(_, _, _, _) => false
   *       case _ => false
   *     }
   *
   *   def nextnm(min: Int, max: Int)(formula: Formula): Formula = NextN(0, min, max, formula)
   * }
   *
   *
   * // Example use for defining a requirement:
   *
   * class Requirement extends MyMonitor[Event] {
   *   require {
   *     case A(x) => nextnm(2, 4)(B(x))
   *   }
   * }
   * </pre>
   * <p><br><p>
   *
   * Of course, in this case we could simply define ''endUserDefined'' as:
   *
   *  <p><br><p>
   *  <pre>
   *  override def endUserDefined(formula: Formula): Boolean = false
   *  </pre>
   *  <p><br><p>
   *
   * Or even not override it at all, since the default definition returns false for any argument formula.
   *
   * @param formula a formula remaining at the end of the trace. The function will determine whether it violates a liveness property.
   * @return returns false if and only if the formula violates a liveness property.
   */
  protected def endUserDefined(formula: Formula): Boolean = false

  /*-----------------*/
  /* EventFormulaOps */
  /*-----------------*/

  /**
   * Target if implicit conversion of events.
   * Provides operations on events. An event is converted to an object of this class
   * by implicit conversion when one of the operations is called on the event.
   */
  class EventFormulaOps(e: Event) {

    /**
     * Checks that the event occurs within the next ''steps'' events.
     * For example, ''SUCCESS("DRIVE") within 2'' checks that the the event ''SUCCESS("DRIVE)'' is
     * amongst the next 2 events.
     * @param steps the expected event should be within the next ''steps'' events. For example, if ''steps == 1'', it
     * should be the next event.
     * @return the ''within''-formula.
     */
    def within(steps: Int): Formula = {
      Debug.debug("within: " + steps)
      state {
        case `e` if steps > 0 => ok
        case _ if steps > 0 => Debug.debug("count"); within(steps - 1)
        case _ => error
      }
    }
  }

  /**
   * Implicit conversion from events to [[tracecontract.Formulas.EventFormulaOps]].
   * This offers operations on events.
   * @param event the event to be converted.
   * @return the [[tracecontract.Formulas.EventFormulaOps]] object offering operations on the ''event''.
   */
  implicit def convEvent2EventFormulaOps(event: Event) = new EventFormulaOps(event)

  /*--------*/
  /* IntOps */
  /*--------*/

  /**
   * Generated by implicit conversion from integer.
   * Provides operations on integers. An integer is converted to an object of this class
   * by implicit conversion when one of the operations is called on the integer.
   */
  class IntOps(x: Int) {
    /**
     * Checks that integer is between two integers, both included.
     * For example ''4 inrange (5,12)'' is false
     * @param range pair of integers defining lower and upper bound
     * @return true if integer is within the range.
     */
    def inrange(range: (Int, Int)) =
      range._1 <= x && x <= range._2

    /**
     * Returns bit number ''p'' of an integer, counted from position ''0'' from the right.
     * For example, ''5 bit 0 == 1'', ''5 bit 1 == 0'' and ''5 bit 2 == 1''.
     *
     * @param p position of bit to be returned.
     * @return bit number ''p''.
     */
    def bit(p: Int): Int = (x >> p) & 1

    /**
     * Converts seconds into milliseconds.
     * @return ''x * 1000''.
     */
    def second: Int = x * 1000

    /**
     * Converts seconds into milliseconds.
     * @return ''x * 1000''.
     */
    def seconds: Int = second

    /**
     * Converts minutes into milliseconds.
     * @return ''x * 60 * 1000''.
     */
    def minute: Int = x * 60 * 1000

    /**
     * Converts minutes into milliseconds.
     * @return ''x * 60 * 1000''.
     */
    def minutes: Int = minute

    /**
     * Converts hours into milliseconds.
     * @return ''x * 60 * 60 * 1000''.
     */
    def hour: Int = x * 60 * 60 * 1000

    /**
     * Converts hours into milliseconds.
     * @return ''x * 60 * 60 * 1000''.
     */
    def hours: Int = hour
  }

  /**
   * Implicit conversion from integers to [[tracecontract.Formulas.IntOps]].
   * This offers operations on integers.
   * @param x the integer to be converted.
   * @return the [[tracecontract.Formulas.IntOps]] object offering operations on the integer ''x''.
   */
  implicit def convInt2IntOps(x: Int) = new IntOps(x)

  /*------------*/
  /* IntPairOps */
  /*------------*/

  /**
   * Generated by implicit conversion from integer pair.
   * Provides operations on integer pairs. An integer pair ''(x,y)'' is
   * converted to an object of this class by implicit conversion
   * when one of the operations is called on the integer pair.
   */
  class IntPairOps(x: Int, y: Int) {
    /**
     * Checks that difference between integers is within a limit.
     * Is typically used to measure difference between real time stamps.
     *
     * @param z a pair ''(x,y)'' of integers.
     * @return ''(y - x) <= z''.
     */
    def within(z: Int) = (y - x) <= z

    /**
     * Checks that difference between integers is beyond a limit.
     * Is typically used to measure difference between real time stamps.
     *
     * @param z a pair ''(x,y)'' of integers.
     * @return ''(y - x) > z''.
     */
    def beyond(z: Int) = (y - x) > z
  }

  /**
   * Implicit conversion from integer pairs to [[tracecontract.Formulas.IntPairOps]].
   * This offers operations on integer pairs.
   *
   * @param pair the integer pair to be converted.
   * @return the [[tracecontract.Formulas.IntPairOps]] object offering operations on the ''pair''.
   */
  implicit def convIntPair2IntPairOps(pair: (Int, Int)) = new IntPairOps(pair._1, pair._2)

  /*---------------*/
  /* DSL Functions */
  /*---------------*/

  // propositional logic:

  /**
   * Generated by implicit conversion from Boolean.
   * Provides operations on Booleans. An Boolean is converted to an object of this class
   * by implicit conversion when one of the operations is called on the Boolean.
   */
  class BooleanOps(a: Boolean) {
    /**
     * Boolean implication.
     * The method allows a Boolean expression of the form ''a ==> b'' to
     * mean: ''a implies b''.
     *
     * @param b consequence of implication
     * @return the value: ''!a || b''.
     */
    def ==>(b: => Boolean): Boolean = !a || b

    /**
     * Boolean implication.
     * The method allows a Boolean expression of the form ''a implies b'' to
     * mean: ''a implies b''. It has the same meaning as ''a ==> b''.
     *
     * @param b consequence of implication
     * @return the value: ''!a || b''.
     */
    def implies(b: =>Boolean) = !a || b

    /**
     * Boolean and.
     * The method allows a Boolean expression of the form ''a and b'' to
     * mean: ''a && b''.
     *
     * @param b second argument to ''and''.
     * @return the value: ''a && b''.
     */
    def and(b: =>Boolean)  = a && b 

    /**
     * Boolean or.
     * The method allows a Boolean expression of the form ''a or b'' to
     * mean: ''a || b''.
     *
     * @param b second argument to ''or''.
     * @return the value: ''a || b''.
     */
    def or(b: =>Boolean) = a || b
  }

  /**
   * Implicit conversion from Boolean to [[tracecontract.Formulas.BooleanOps]].
   * This offers operations on Booleans.
   *
   * @param b the Boolean to be converted.
   * @return the [[tracecontract.Formulas.BooleanOps]] object offering operations on ''b''.
   */
  implicit def convBoolean2BooleanOps(b: Boolean) = new BooleanOps(b)

  // temporal logic:

  /**
   * Matches current event against a predicate.
   * The formula ''matches(pred)'' is True if and only if ''pred'' is defined for
   * the '''current''' event ''e'', and furthermore ''pred(e) == true''.
   * For example, the following formula states that eventually there should be a command,
   * the name of which starts with ''"PICTURE"'':
   * <p><br><p>
   * <pre>
   * eventually(matches{case COMMAND(x) => x startsWith "PICTURE"})
   * </pre>
   * <p><br><p>
   * @param predicate that has to match current event for formula to become True
   * @return the value True or False, depending on whether ''pred'' is true on the current event.
   */
  def matches(predicate: PartialFunction[Event, Boolean]): Formula = Matches(predicate)

  /**
   * Boolean negation.
   * Given a formula ''f'', the formula ''not f'' is the negation of ''f''. That is, if
   * ''f'' evaluates to True, ''not f'' evaluates to False, and vice versa.
   *
   * @param formula the formula to be negated.
   * @return the negated formula.
   */
  def not(formula: Formula): Formula = Not(formula)

  /**
   * Globally true (an LTL formula).
   * The formula ''globally(f)'' for some formula ''f'' is True if ''f'' is True
   * from now on and in all future steps.
   *
   * At the end of the trace ''globally(f)'' evaluates to True independently of what ''f'' is.
   *
   * @param formula the formula that must be True in all steps from now on.
   * @return the ''globally''-formula.
   */
  def globally(formula: Formula): Formula = Globally(formula)

  /**
   * Eventually true (an LTL formula).
   * The formula ''eventually(f)'' for some formula ''f'' is True if ''f'' is True
   * at some future point.
   *
   * At the end of the trace ''eventually(f)'' evaluates to False independently of what ''f'' is.
   * It is usually desired that ''f'' becomes True before then, and therefore there will be
   * no ''eventually(f)'' appearing at the end of the trace.
   *
   * @param formula the formula that must be True eventually.
   * @return the ''eventually''-formula.
   */
  def eventually(formula: Formula): Formula = Eventually(formula)

  /**
   * Eventually true between ''m'' and ''n'' steps.
   * The formula ''eventuallyBw(m,n)(f)'' for some formula ''f'' is True if ''f'' is True
   * at some future point in between steps ''m'' and ''n'', counting the initial step as ''1''.
   * The formula ''f'' is not allowed to be True before the ''m'''th step.
   * For example, the following formula states that if an ''A'' is observed then
   * eventually in between 3 and 5 steps there should be a ''B'', counting the current step
   * where ''A'' occurs as 1:
   * <p><br><p>
   * <pre>
   * globally{A implies eventuallyBw(3,5){B}}
   * </pre>
   * <p><br><p>
   * This formula will evaluate as follows on the following traces (x is an event different from A and B):
   * <p><br><p>
   * <pre>
   *   1 2 3 4 5 6 --- step counting
   * x A x B x x x : True
   * x A x x B x x : True
   * x A x x x B x : True
   * x A x x x x B : False
   * x A B x x x x : False
   * x A B B x x x : False
   * x A x x x x x : False
   * </pre>
   * <p><br><p>
   * Note that the step counting starts when the call of eventuallyBw is activated. In the
   * above example it is activated in the same step as ''A'' occurs. In the following specification
   * it is activated in the next step:
   * <p><br><p>
   * <pre>
   * state {
   *   case A => eventuallyBw(3,5){B}
   * }
   * </pre>
   * <p><br><p>
   * Consequently, the ''B'''s in the above traces would have to occur one step further to the right
   * as follows:
   * <p><br><p>
   * <pre>
   *     1 2 3 4 5 6 --- step counting
   * x A x x B x x x : True
   * x A x x x B x x : True
   * x A x x x x B x : True
   * x A x x x x x B : False
   * x A B B B x x x : False
   * x A x x x x x x : False
   * </pre>
   * <p><br><p>
   * At the end of the trace ''eventuallyBw(m,n)(f)'' evaluates to False independently of what ''f'' is.
   *
   * @param m the lower bound, m included, for when formula must be True.
   * @param n the upper bound, n included, for when formula must be True.
   * @param formula the formula that must be True.
   * @return the ''eventuallyBw''-formula.
   */

  def eventuallyBw(m: Int, n: Int, x: Int = 1)(formula: Formula): Formula = {
    if (x < m)
      not(formula) and strongnext { eventuallyBw(m, n, x + 1)(formula) }
    else if (x > n)
      error
    else
      formula or strongnext { eventuallyBw(m, n, x + 1)(formula) }
  }

  /**
   * Eventually true at step ''n''.
   * The formula ''eventuallyEq(n)(f)'' for some formula ''f'' is True if ''f'' is True
   * at future step ''n'', counting the next step as ''1''.
   * The formula ''f'' is not allowed to be True before the ''n''th step.
   * The following equation holds:
   * <p><br><p>
   * <pre>
   * eventuallyEq(n)(f) = eventuallyBw(n, n)(f)
   * </pre>
   * <p><br><p>
   * At the end of the trace ''eventuallyEq(n)(f)'' evaluates to False independently of what ''f'' is.

   * @param n the step at which formula must be True.
   * @param formula the formula that must be True.
   * @return the ''eventuallyEq''-formula.
   */
  def eventuallyEq(n: Int)(formula: Formula): Formula = eventuallyBw(n, n)(formula)

  /**
   * Eventually true in maximally ''n'' steps.
   * The formula ''eventuallyLe(n)(f)'' for some formula ''f'' is True if ''f'' is True
   * at a future step between now and the ''n''th step, ''n'' included, counting the initial step as ''1''.
   * The following equation holds:
   * <p><br><p>
   * <pre>
   * eventuallyLe(n)(f) = eventuallyBw(1, n)(f)
   * </pre>
   * <p><br><p>
   * At the end of the trace ''eventuallyLe(n)(f)'' evaluates to False independently of what ''f'' is.

   * @param n the step before or at which formula must be True.
   * @param formula the formula that must be True.
   * @return the ''eventuallyLe''-formula.
   */
  def eventuallyLe(n: Int)(formula: Formula): Formula = eventuallyBw(1, n)(formula)

  /**
   * Eventually true in less than ''n'' steps.
   * The formula ''eventuallyLt(n)(f)'' for some formula ''f'' is True if ''f'' is True
   * in less than ''n'' future steps, counting the initial step as ''1''.
   * The following equation holds:
   * <p><br><p>
   * <pre>
   * eventuallyLt(n)(f) = eventuallyBw(1, n-1)(f)
   * </pre>
   * <p><br><p>
   * At the end of the trace ''eventuallyLt(n)(f)'' evaluates to False independently of what ''f'' is.
   *
   * @param n the step before which formula must be True.
   * @param formula the formula that must be True.
   * @return the ''eventuallyLt''-formula.
   */
  def eventuallyLt(n: Int)(formula: Formula): Formula = eventuallyBw(1, n - 1)(formula)

  /**
   * Eventually true at or after minimally ''n'' steps.
   * The formula ''eventuallyGe(n)(f)'' for some formula ''f'' is True if ''f'' is True
   * at or later than the ''n'''th step, counting the initial step as ''1''.
   * The formula ''f'' is not allowed to be True before the ''n'''th step.
   * The following equation holds:
   * <p><br><p>
   * <pre>
   * eventuallyGe(n)(f) = eventuallyBw(n, Int.MaxValue)(f)
   * </pre>
   * <p><br><p>
   * At the end of the trace ''eventuallyGe(n)(f)'' evaluates to False independently of what ''f'' is.
   *
   * @param n the step at or later the formula must be True.
   * @param formula the formula that must be True.
   * @return the ''eventuallyGe''-formula.
   */
  def eventuallyGe(n: Int)(formula: Formula): Formula = eventuallyBw(n, Int.MaxValue)(formula)

  /**
   * Eventually true after ''n'' steps.
   * The formula ''eventuallyGt(n)(f)'' for some formula ''f'' is True if ''f'' is True
   * after ''n'' future steps, counting the initial step as ''1''.
   * The formula ''f'' is not allowed to be True before or at the ''n''th step.
   * The following equation holds:
   * <p><br><p>
   * <pre>
   * eventuallyGt(n)(f) = eventuallyBw(n+1, Int.MaxValue)(f)
   * </pre>
   * <p><br><p>
   * At the end of the trace ''eventuallyGt(n)(f)'' evaluates to False independently of what ''f'' is.

   * @param n the step after which formula must be True.
   * @param formula the formula that must be True.
   * @return the ''eventuallyGt''-formula.
   */
  def eventuallyGt(n: Int)(formula: Formula): Formula = eventuallyBw(n + 1, Int.MaxValue)(formula)

  /**
   * Never true (an LTL-inspired formula).
   * The formula ''never(f)'' for some formula ''f'' is True if ''f'' is never True
   * from now on and in all future steps. The formula ''never(f)'' is an abbreviation for the
   * formula: ''always(not(f))''.
   *
   * At the end of the trace ''never(f)'' evaluates to True independently of what ''f'' is.
   *
   * @param formula the formula that must never be True in all steps from now on.
   * @return the ''never''-formula.
   */
  def never(formula: Formula): Formula = Never(formula)

  /**
   * True in the next step (an LTL formula).
   * The formula ''strongnext(f)'' for some formula ''f'' is True iff. there is a next step
   * (it is not the end of the trace), and ''f'' is True in the next step.
   *
   * At the end of the trace ''strongnext(f)'' evaluates to False independently of what ''f'' is.
   *
   * @param formula the formula that must be True in the next step.
   * The formula is called 'by name', which means that it is not evaluated until the next step.
   * @return the ''strongnext''-formula.
   */
  def strongnext(formula: => Formula): Formula = StrongNext((x: Unit) => formula)

  /**
   * True in the next step, if there is a next step (an LTL formula).
   * The formula ''weaknext(f)'' for some formula ''f'' is True iff. either there is no next step
   * (it is the end of the trace), or if ''f'' is True in the next step.
   *
   * At the end of the trace ''weaknext(f)'' evaluates to True independently of what ''f'' is.
   *
   * @param formula the formula that must be True in the next step, if there is a next step.
   * The formula is called 'by name', which means that it is not evaluated until the next step.
   * @return the ''weaknext''-formula.
   */
  def weaknext(formula: => Formula): Formula = WeakNext((x: Unit) => formula)

  /**
   * The formula must become True within the specified time.
   * The time must be given in milliseconds. This construct can be used when
   * monitoring a system online, where events are processed as they are produced, and
   * where the clock of the computer on which the monitor runs, is sufficient for measuring
   * time critical performance.
   *
   * The method should for example not be used to analyze
   * log files containing events that have already previously been generated. Typically
   * such events contain their own time stamps, and those should be used instead, treated
   * just as any other data.
   *
   * @param time, measured in milliseconds, before the ''formula'' must become True.
   * @param formula the formula that must become True within the specified ''time''.
   * @return the ''within''-formula.
   */
  def within(time: Int)(formula: Formula): Formula = Within(time, formula)

  /**
   * Implicit conversion from Event to Formula.
   * This allows to write an event as a formula, with the semantics that the observed
   * event must be equal to it. As an example, this permits to write a formula of the following
   * form, where the argument to ''eventually'' is an event, which by implicit conversion is
   * then lifted to become a formula:
   * <p><br><p>
   * <pre>
   * eventually(SUCCESS("REBOOT"))
   * </pre>
   * <p><br><p>
   * The formula states that eventually an event must be observed wich satisfies the
   * formula ''SUCCESS("REBOOT")'' - that is: is equal to it.
   * <p><br><p>
   * @param event the event to be converted.
   * @return the formula corresponding to the event.
   */
  implicit def convEvent2Formula(event: Event): Formula = Now(event)

  /**
   * Implicit conversion from Boolean to Formula.
   * This allows to write a Boolean expression as a formula, with the obvious semantics,
   * that the Boolean expression must be true in order for the formula to become True.
   * For example, the following property requires that no
   * command name contains the string ''"REBOOT"'' (the right hand side of ''=>'' is a Boolean
   * expression, which is lifted to a Formula):
   * <p><br><p>
   * <pre>
   * require {
   *   case COMMAND(x) => !(x contains "REBOOT")
   * }
   * </pre>
   * <p><br><p>
   * @param cond the Boolean value to be converted.
   * @return the formula (True or False) corresponding to the value (true or false) of the Boolean ''cond''.
   */
  implicit def convBoolean2Formula(cond: Boolean): Formula = if (cond) True else False

  /**
   * Implicit conversion from the Unit value to Formula.
   * This allows to write a block of code (returning the Unit value, hence only
   * executed for its side effect) as a formula. The result is the side effect and the returned
   * formula is True. For example, the following monitor collects the commands observed in a set,
   * and prints them out at the end of the trace:
   * <p><br><p>
   * <pre>
   * class Requirement extends Monitor[Event] {
   *   var commands : Set[String] = Set()
   *
   *   require {
   *     case COMMAND(x) =>
   *       commands += x
   *   }
   *
   *   override def finish() {
   *     println("commands issued: " + (commands.mkString("{",",","}")))
   *   }
   * }
   * </pre>
   * <p><br><p>
   * @param unit the Unit value to be converted.
   * @return the formula True.
   */
  implicit def convUnitToFormula(unit: Unit): Formula = True

  /**
   * Execute code (can for example be used in LTL formula).
   * The formula ''exec{f}'' for some formula ''f'' executes ''f'' as code.
   * Note that a piece of code can occur as a formula. The code can return unit
   * (corresponding to True), a Boolean or a formula. In all these cases the result is a formula.
   * At the end of the trace ''exec{f}'' evaluates to True independently of what ''f'' is.
   *
   * As an example consider the following specification which counts the number of
   * a particular command and checks that there are exactly three. Note that
   * ''implies'' is evaluated with short-circuit semantics: if the left-hand side
   * evaluates to false, the right-hand side is not evaluated.
   * <p><br><p>
   * <pre>
   * class CountSomeCommands extends Monitor[Event] {
   *   var counter = 0
   *
   *   property {
   *     globally {
   *       Command("STOP_DRIVING",1) implies exec {
   *         counter += 1
   *       }
   *     }
   *   }
   *
   *   override def finish() {
   *     if (counter != 3) assert(false, "counter=" + counter)
   *   }
   * }
   * </pre>
   * <p><br><p>
   * @param formula the code that is to be executed, and which can return a formula.
   * @return the ''exec''-formula.
   */
  def exec(formula: => Formula): Formula = Exec((x: Unit) => formula)

  // state machines:

  /**
   * A state waiting for an event to possibly match a transition (not required).
   * The state remains active until the incoming event ''e'' matches the ''block'',
   * that is, until ''block.isDefinedAt(e) == true'', in which case the state formula evaluates
   * to ''block(e)''.
   *
   * At the end of the trace a ''state'' formula evaluates to True.
   *
   * As an example, consider the following monitor, which checks the
   * property: ''"a command x should not be issued more than once"'':
   *
   * <p><br><p>
   * <pre>
   * class Requirement extends Monitor[Event] {
   *   require {
   *     case COMMAND(x) =>
   *       state {
   *         case COMMAND(`x`) => error
   *       }
   *   }
   * }
   * </pre>
   * <p><br><p>
   *
   * @param block partial function representing the transitions leading out of the state.
   * @return the ''state'' formula.
   */
  def state(block: Block): Formula = State(block)

  /**
   * A hot state waiting for an event to eventually match a transition (required).
   * The state remains active until the incoming event ''e'' matches the ''block'',
   * that is, until ''block.isDefinedAt(e) == true'', in which case the state formula evaluates
   * to ''block(e)''.
   *
   * At the end of the trace a ''hot state'' formula evaluates to False.
   *
   * As an example, consider the following monitor, which checks the
   * property: ''"a command x eventually should be followed by a success"'':
   *
   * <p><br><p>
   * <pre>
   * class Requirement extends Monitor[Event] {
   *   require {
   *     case COMMAND(x) =>
   *       hot {
   *         case SUCCESS(`x`) => ok
   *       }
   *   }
   * }
   * </pre>
   * <p><br><p>
   *
   * @param block partial function representing the transitions leading out of the state.
   * @return the ''hot state'' formula.
   */
  def hot(block: Block): Formula = Hot(block)

  /**
   * A hot state waiting for an event to eventually match a transition (required) between ''m'' and ''n'' steps.
   * The initial step is counted as step as 1. The state remains active until the incoming event ''e'' matches the ''block'',
   * that is, until ''block.isDefinedAt(e) == true'', in which case the state formula evaluates
   * to ''block(e)''. An error is emitted if the state triggers before the ''m''th step or does not trigger
   * before or at the ''n''th step.
   *
   * At the end of the trace a ''hot state'' formula evaluates to False.
   *
   * As an example, consider the following monitor, which checks the
   * property: ''"a command x eventually should be followed by a success within 3 to 5 steps"'':
   *
   * <p><br><p>
   * <pre>
   * class Requirement extends Monitor[Event] {
   *   require {
   *     case COMMAND(x) =>
   *       hot(3,5) {
   *         case SUCCESS(`x`) => ok
   *       }
   *   }
   * }
   * </pre>
   * <p><br><p>
   *
   * @param m the minimal number of steps that have to occur before a transition can trigger.
   * The transition can trigger at step m.
   * @param n the maximal number of steps within which a transition must trigger.
   * The transition can trigger at step n.
   * @param block partial function representing the transitions leading out of the state.
   * @return the ''hot state'' formula.
   */
  def hot(m: Int, n: Int)(block: Block): Formula = hot_(m, n, 1)(block)

  private def hot_(m: Int, n: Int, x: Int)(block: Block): Formula =
    if (x > n)
      error
    else if (x < m)
      hot {
        case e =>
          if (block.isDefinedAt(e))
            error
          else
            hot_(m, n, x + 1)(block)
      }
    else
      hot {
        block orElse {
          case _ => hot_(m, n, x + 1)(block)
        }
      }

  /**
   * A step state watching for an event to possibly match a transition in the next step (not required).
   * The state remains active one! step. If the next incoming event ''e'' matches the ''block'',
   * that is: ''block.isDefinedAt(e) == true'', the step formula evaluates
   * to ''block(e)''. Otherwise (if ''block.isDefinedAt(e) == false'') it evaluates to True.
   *
   * At the end of the trace a ''step state'' formula evaluates to True.
   *
   * As an example, consider the following monitor, which checks the
   * property: ''"a command x should not be issued in two consequtive steps right after each other"'':
   *
   * <p><br><p>
   * <pre>
   * class Requirement extends Monitor[Event] {
   *   require {
   *     case COMMAND(x) =>
   *       step {
   *         case COMMAND(`x`) => error
   *       }
   *   }
   * }
   * </pre>
   * <p><br><p>
   *
   * @param block partial function representing the transitions leading out of the state.
   * @return the ''step state'' formula.
   */
  def step(block: Block): Formula = Step(block)

  /**
   * A strong state expecting an event to match a transition in the next step.
   * The state remains active one! step. If the next incoming event ''e'' matches the ''block'',
   * that is: ''block.isDefinedAt(e) == true'', the strong formula evaluates
   * to ''block(e)''. Otherwise (if ''block.isDefinedAt(e) == false'') it evaluates to False.
   *
   * At the end of the trace a ''strong state'' formula evaluates to False. That is, there must be
   * a next event for which ''block'' is defined.
   *
   * As an example, consider the following monitor, which checks the
   * property: ''"a command x must be followed by a success in the next step"'':
   *
   * <p><br><p>
   * <pre>
   * class Requirement extends Monitor[Event] {
   *   require {
   *     case COMMAND(x) =>
   *       strong {
   *         case SUCCESS(`x`) => ok
   *       }
   *   }
   * }
   * </pre>
   * <p><br><p>
   *
   * @param block partial function representing the transitions leading out of the state.
   * @return the ''strong state'' formula.
   */
  def strong(block: Block): Formula = Strong(block)

  /**
   * A weak state expecting an event to match a transition in the next step, if there is a next step.
   * If there is no next step, the formula evaluates to True. Otherwise if the next incoming
   * event ''e'' matches the ''block'', that is: ''block.isDefinedAt(e) == true'', the weak
   * formula evaluates to ''block(e)''. Otherwise (if ''block.isDefinedAt(e) == false'') it
   * evaluates to False.
   *
   * At the end of the trace a ''weak state'' formula evaluates to True. That is, there does not need
   * to be a next event. If there is, however, ''block'' must be defined on it.
   *
   * As an example, consider the following monitor, which checks the
   * property: ''"a command x must be followed by a success in the next step, if there is a next step"'':
   *
   * <p><br><p>
   * <pre>
   * class Requirement extends Monitor[Event] {
   *   require {
   *     case COMMAND(x) =>
   *       weak {
   *         case SUCCESS(`x`) => ok
   *       }
   *   }
   * }
   * </pre>
   * <p><br><p>
   *
   * @param block partial function representing the transitions leading out of the state.
   * @return the ''weak state'' formula.
   */
  def weak(block: Block): Formula = Weak(block)

  /**
   * An always state always waiting for an event to possibly match a transition (not required).
   * The state ''"always"'' remains active (until the end of the trace). For every incoming
   * event ''e'' that matches the ''block'', that is: ''block.isDefinedAt(e) == true'', a new
   * sub-property is added, which monitors ''block(e)''.
   *
   * At the end of the trace an ''always state'' formula evaluates to True.
   *
   * As an example, consider the following monitor, which checks the
   * property: ''"when a command x is issued, any fail from then on of that
   * command should be followed by a retry of that command"'':
   *
   * <p><br><p>
   * <pre>
   * class Requirement extends Monitor[Event] {
   *   property {
   *     always {
   *       case COMMAND(x) =>
   *         always {
   *           case FAIL(`x`) =>
   *             hot {
   *               case RETRY(`x`) => ok
   *             }
   *         }
   *     }
   *   }
   * }
   * </pre>
   * <p><br><p>
   *
   * @param block partial function representing the transitions leading out of the state.
   * @return the ''always state'' formula.
   */
  def always(block: Block): Formula = Globally(step(block))

  /**
   * An until state waiting for an event to eventually match a transition and yield a result different from False (required).
   * The state remains active until the incoming event ''e'' matches the ''block'',
   * that is, until ''block.isDefinedAt(e) == true'', and ''block(e)'' evaluates to a formula different from False
   * (does not need to be True necessarily),
   * in which case the until state evaluates to ''block(e)''.
   *
   * At the end of the trace an ''until state'' evaluates to False.
   *
   * As an example, consider the following monitor, which checks the
   * property: ''"a command x eventually should be followed by a success, and any failure on the way should be reported"'':
   *
   * <p><br><p>
   * <pre>
   * class Requirement extends Monitor[Event] {
   *   require {
   *     case COMMAND(x) =>
   *       until {
   *         case FAIL(`x`) => error
   *         case SUCCESS(`x`) => ok
   *       }
   *   }
   * }
   * </pre>
   * <p><br><p>
   *
   * @param block partial function representing the transitions leading out of the state.
   * @return the ''until state'' formula.
   */
  def until(block: Block): Formula = UntilState(block)

  /**
   * An unless state waiting for an event to eventually match a transition and yield a result different from False
   * (not required).
   * The state remains active until the incoming event ''e'' matches the ''block'',
   * that is, until ''block.isDefinedAt(e) == true'', and ''block(e)'' evaluates to a formula different from False
   * (does not need to be True necessarily),
   * in which case the unless state evaluates to ''block(e)''.
   *
   * At the end of the trace an ''unless state'' evaluates to True.
   *
   * As an example, consider the following monitor, which checks the
   * property: ''"when a command succeeds, report every single additional success unless a new command with the same name is issued"'':
   *
   * <p><br><p>
   * <pre>
   * class Requirement extends Monitor[Event] {
   *   require {
   *     case SUCCESS(x) =>
   *       unless {
   *         case SUCCESS(`x`) => error
   *         case COMMAND(`x`) => ok
   *       }
   *   }
   * }
   * </pre>
   * <p><br><p>
   *
   * @param block partial function representing the transitions leading out of the state.
   * @return the ''unless state'' formula.
   */
  def unless(block: Block): Formula = UnlessState(block)

  // error and ok:

  /**
   * Emits a sound when executed.
   * The method can be used to give an audio-based warning. Can be useful in case lots of
   * visual information is printed on screen and one wants a quick recognition of a special
   * situation occurring.
   */
  def beep {
    Toolkit.getDefaultToolkit().beep();
  }

  /**
   * Emits an error message and evaluates to False.
   * This method is used to indicate an error situation. False can
   * be used instead. However, in some contexts, for example on the left hand side
   * of an implication (''f,,1,, implies f,,2,,'') or on the left hand side of a causal
   * relation (''f,,1,, causes f,,2,,''), one does not want an error message to be printed.
   * In that case False should be used. The following example illustrates a case
   * where ''error'' should not be used. The formula models the requirement that if a drive command
   * is issued and then dispatched without a fail in between, then it should succeed.
   * It is not an errror if it fails before being dispatched, hence False should be used
   * instead of ''error'', just like ''False implies  True'' is True and not an error:
   *
   * <pre>
   * class Requirement extends Monitor[Event] {
   *   def driveDispatched : Formula =
   *     state {
   *       case COMMAND("DRIVE") =>
   *         state {
   *           case FAIL("DRIVE") => False // do not use error here
   *           case DISPATCH("DRIVE") => True
   *         }
   *     }
   *
   *   property {
   *     driveDispatched causes eventually(SUCCESS("DRIVE"))
   *   }
   * }
   * </pre>
   *
   * @return False
   */
  def error: Formula = {
    if (Options.PRINT) println("*** error")
    False
  }

  /**
   * Emits the error message provided as argument and evaluates to False.
   * This method is used to indicate an error situation.
   * Can be used to convey values of data in scope, illustrating what causes an
   * error to occur.
   *
   * See the explanation of the unparameterized ''error'' method for when to use False instead of ''error''.
   *
   * @param message the string to be printed on standard out.
   * @return False
   */
  def error(message: String): Formula = {
    if (Options.PRINT) println("*** error: " + message)
    messages ++= List(Message(false, message))
    False
  }

  /**
   * Equivalent to True.
   * This method is used to indicate a good situation.
   *
   * @return True
   */
  def ok: Formula = True

  /**
   * Emits the message provided as argument and evaluates to True.
   * This method is used to indicate a good situation.
   * Can be used to convey values of data in scope, illustrating why a situation is good.
   *
   * @param message the string to be printed on standard out.
   * @return True
   */
  def ok(message: String): Formula = {
    if (Options.PRINT) println("!!! ok: " + message)
    messages ++= List(Message(true, message))
    True
  }

  //  If Then Else

  /**
   * A construct of the form: If (condition) Then formula1 Else formula2
   *
   * @param condition The condition formula
   * @return the ''ThenPart'', providing the ''Then'' method.
   */
  def If(condition: Formula) = new ThenPart(condition)

  /**
   * The ''Then'' part of an ''If (condition) Then formula1 Else formula2''.
   */
  class ThenPart(condition: Formula) {
    /**
     * The ''Then'' method of ''If (condition) Then formula1 Else formula2''.
     * @param formula1 the then-formula to be checked of the condition is true.
     * @return the ''ElsePart'', providing the ''Else'' method.
     */
    def Then(formula1: Formula) = new ElsePart(condition, formula1)
  }

  /**
   * The ''Else'' part of an ''If (condition) Then formula1 Else formula2''.
   */

  class ElsePart(condition: Formula, formula1: Formula) {
    /**
     * The ''Else'' method of ''If (condition) Then formula1 Else formula2''.
     * @param formula2 the else-formula to be checked of the condition is false.
     * @return the formula representing the semantics of the ''If-Then_Else''.
     */

    def Else(formula2: Formula): Formula = IfThenElse(condition, formula1, formula2)
    //(condition implies formula1) and (not(condition) implies formula2)
  }

  private case class IfThenElse(condition: Formula, formula1: Formula, formula2: Formula) extends Formula { 
    override def apply(event: Event): Formula = {
      val condition_ = condition(event)
      condition_ match {
        case True => formula1(event)
        case False => formula2(event)
        case _ => IfThenElse(condition_,formula1(event), formula2(event))
      }
    }

    override def toString: String = "If " + condition + " Then " + formula1 + " Else " + formula2
  }
}
