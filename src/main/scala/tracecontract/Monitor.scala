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
 * This class offers all the features of TraceContract. The user is expected to extend this class.
 * The class is type parameterized with the event type, and with the severity associated with the monitor,
 * the default value being ''ERROR''. The severity of a monitor can also be set with the method ''setSeverity''.<br>
 *
 * See the the explanation for the [[tracecontract]] package for a full explanation.
 *
 * <p>
 *
 * The following example illustrates the definition of a monitor with
 * two properties: a safety property and a liveness property.
 *
 * <br>
 *
 * <pre>
 * class Requirements extends Monitor[Event] {
 *
 *   requirement('CommandMustSucceed) {
 *     case COMMAND(x) =>
 *       hot {
 *         case SUCCESS(x) => ok
 *       }
 *   }
 *
 *   requirement('CommandAtMostOnce) {
 *     case COMMAND(x) =>
 *       state {
 *         case COMMAND(`x`) => error
 *       }
 *   }
 *
 * }
 * </pre>
 *
 * <br>
 *
 * @tparam Event the type of events being monitored.
 * @param severity the severity associated with the Monitor. The default value is ''ERROR''.
 *
 */

class Monitor[Event](var severity: Severity = Severity.ERROR) extends DataBase with Formulas[Event] {

  private val monitorName = this.getClass.getName.split("\\.").last

  /**
   * Computes the hierarchical monitor name, including names of all super monitors.
   * For example, if the monitor's name is M3, and it is a sub-monitor of a monitor M2,
   * which again is a sub-monitor of a monitor M1, then the returned monitor name is:
   *
   *   M1.M2.M3
   *
   * @return the full monitor name.
   */

  def getFullMonitorName: String = {
    var result = ""
    if (parent != null) {
      result = parent.getFullMonitorName + "."
    }
    result += monitorName
    result
  }

  /**
   * Computes the monitor name given to the monitor by the user in a class definition.
   * For example, given the declaration:
   *
   * <pre>
   *   class R42 extends Monitor[Events] { ... }
   * </pre>
   *
   * The string returned is "R42".
   *
   * @return the user defined monitor name.
   */

  def getMonitorName: String = monitorName

  /*-------*/
  /* State */
  /*-------*/

  private var parent: Monitor[Event] = null
  private var monitors: List[Monitor[Event]] = List()
  private var properties: List[Property] = List()
  private var derived: List[Property] = List()
  private var derivedNext: List[Property] = List()
  private var eventCount: Int = 0
  private var eventLog: java.io.FileWriter = null

  /*----------------*/
  /* Updating State */
  /*----------------*/

  private def addDerivedProperty(formula: Formula, parent: Property, event: Event) {
    Debug.debug("adding derived property: " + formula + " for " + parent.name)
    val derivedProperty = new Property(this, formula, parent.name, Some(parent))
    derivedProperty.trace(event)
    derivedNext ::= derivedProperty
  }

  private def updateDerivedProperties() {
    derived = derived.filterNot(_.isFinal)
    derived ++= derivedNext
    derivedNext = List()
  }

  /*---------*/
  /* Options */
  /*---------*/

  /**
   * Sets the print flag.
   * When the print flag is set to true, information, for example error messages,
   * will be printed to standard out.
   * The default value is true.
   *
   * @param flag set to false if printed information is not desired. Default value is true.
   */
  def setPrint(flag: Boolean) {
    Options.PRINT = flag
  }

  /**
   * Sets the debug flag.
   * When the debug flag is set to true, debugging information will be printed to standard out.
   * The default value is false.
   *
   * @param flag set to true if debugging information is desired. Default value is false.
   */
  def setDebug(flag: Boolean) {
    Options.DEBUG = flag
  }

  /**
   * Sets the error trace flag.
   * When the error trace flag is set to false, error traces will not be recorded.
   * The default value is true. The only reason for turning it off would be to make the
   * system more efficient for very large traces. It should normally not be necessary to change
   * from the default value.
   *
   * @param flag set to false if error traces should not be collected.
   */

  def setErrorTrace(flag: Boolean) {
    Options.ERROR_TRACE = flag
  }

  /**
   * Sets the success trace flag.
   * When the success trace flag is set to true, successes will be reported.
   * The default value is false. The only reason for having it turned it off would be to make the
   * system more efficient for very large traces.
   *
   * @param flag set to true if success traces should be collected.
   */

  def setSuccess(flag: Boolean) {
    Options.SUCCESS = flag
  }

  /**
   * Sets the name of the file to which an event log should be written.
   * When TraceContract is applied, the analyzed trace will be written to the provided file. Events are numbered
   * for reference by the error traces. By default no event log will be produced. An example of a log is the following:
   *
   * <br>
   *
   * <pre>
   * Event Log for: Requirements
   *
   * 1 COMMAND(take picture,1,1000)
   * 2 DISPATCH(take picture,1,2000)
   * 3 EVR(warning,2010)
   * 4 EVR(warning,2090)
   * 5 SUCCESS(take picture,1,3001)
   * 6 COMMAND(take picture,1,1000)
   * 7 DISPATCH(take picture,1,2000)
   * ...
   * </pre>
   *
   * <br>
   *
   * @param eventLogName name of the event log file. The name should be an absolute name, including path from the root.
   */
  def setEventLog(eventLogName: String) {
    eventLog = new java.io.FileWriter(eventLogName)
    eventLog.write("Event Log for: " + monitorName + "\n\n")
  }

  /*---------------*/
  /* Event Logging */
  /*---------------*/

  private def writeEventLog(event: Event) {
    if (eventLog != null) {
      eventLog.write(eventCount + " " + event.toString + "\n")
    }
  }

  private def closeEventLog() {
    if (eventLog != null) eventLog.close
  }

  /*----------*/
  /* Property */
  /*----------*/

  private case class Property(monitor: Monitor[Event], var formula: Formula, name: String = "", parent: Option[Property] = None) {
    private var errorTrace: ErrorTrace[Event] = new ErrorTrace
    private var result: PropertyResult[Event] =
      parent match {
        //case None => new PropertyResult(monitorName, name)
        case None => new PropertyResult(monitor, name) // @@@1
        case Some(p) => p.getResult
      }
    private var explanation: String =
      parent match {
        case None => ""
        case Some(p) => p.getExplanation
      }

    def trace(event: Event) {
      errorTrace.addEvent(eventCount, event)
    }

    def resetErrorTrace() {
      errorTrace = new ErrorTrace
    }

    def getErrorTrace: ErrorTrace[Event] = errorTrace

    def isFinal: Boolean = formula == True || formula == False

    def addExplanation(text: String) {
      explanation ++= text
    }

    def getExplanation: String = explanation

    def getResult: PropertyResult[Event] = result

    def apply(event: Event) {
      formula match {
        case Globally(bodyFormula) =>
          val bodyFormula_ = bodyFormula(event)
          bodyFormula_ match {
            case True =>
            case False =>
              trace(event)
              reportSafetyError()
              resetErrorTrace()
            case _ => addDerivedProperty(bodyFormula_, this, event)
          }
        case _ =>
          val formula_ = formula(event)
          if (formula_ != formula) trace(event)
          if (formula_ == False && formula != False) reportSafetyError()
          if (Options.SUCCESS && formula_ == True && formula != True) reportSuccess()
          formula = formula_
      }
      if (!messages.isEmpty) {
        result.add(messages)
        messages = Nil
      }
    }

    def reportSafetyError() {
      val safetyError = SafetyError[Event](getFullMonitorName, name, explanation, errorTrace)
      if (Options.PRINT) {
        //beep
        println(safetyError)
      }
      result.add(safetyError)
    }

    def reportLivenessError(monitorName: String, propertyName: String, explanation: String, errorTrace: ErrorTrace[Event]) {
      val livenessError = LivenessError[Event](monitorName, propertyName, explanation, errorTrace)
      if (Options.PRINT) {
        //beep
        println(livenessError)
      }
      result.add(livenessError)
    }

    def reportSuccess() {
      val success = Success[Event](getFullMonitorName, name, explanation, errorTrace)
      if (Options.SUCCESS && Options.PRINT) {
        //beep
        println(success)
      }
      result.add(success)
    }

    override def toString: String = {
      val isDerived = if (parent == None) "" else "derived "
      val id = isDerived + "property" + (if (name == "") "" else " " + name)
      id + ":\n" + ("-" * (id.length + 1)) + "\n" +
        formula.toString + "\n" +
        errorTrace
    }
  }

  /*----------*/
  /* Severity */
  /*----------*/

  /**
   * Sets the severity of the monitor.
   *
   * @param severity the severity to be assigned to the monitor.
   */

  def setSeverity(severity: Severity) {
    this.severity = severity
  }

  /**
   * Gets the severity assigned to the monitor.
   *
   * @return the severity assigned to the monitor.
   */

  def getSeverity: Severity = severity

  /*----------------*/
  /* Initialization */
  /*----------------*/

  /**
   * Sets the parent monitor of this monitor.
   *
   * @param monitor the parent monitor.
   */

  private def setParent(monitor: Monitor[Event]) {
    parent = monitor
  }

  /**
   * Gets the parent monitor of this monitor.
   *
   * @param monitor the parent monitor.
   */

  private def getParent(monitor: Monitor[Event]) = {
    parent
  }

  private var explanation = ""

  /**
   * Used to enter explanations of properties in informal language.
   * The method should be applied just before the property it concerns.
   * The explanation will be stored as part of the property, and will be
   * be made part of error reports to ease understanding of these.
   *
   * It is possible to enter such informal explanations without associated
   * properties, for example as part of an initial attempt to formulate
   * requirements.
   *
   * @param explanation the text making up the explanation.
   */
  def informal(explanation: String) {
    this.explanation += explanation
  }

  /**
   * Used to enter explanations of properties in informal language.
   * The method should be applied just before the property it concerns.
   * The explanation will be stored as part of the property, and will be
   * be made part of error reports to ease understanding of these.
   *
   * It is possible to enter such informal explanations without associated
   * properties, for example as part of an initial attempt to formulate
   * requirements.
   *
   * @param name name of the property.
   * @param explanation the text making up the explanation.
   */
  def informal(name: Symbol)(explanation: String) {
    this.explanation += explanation // should really respect the name
  }

  private def addExplanation(property: Property) {
    if (explanation != "") {
      property.addExplanation(explanation)
      explanation = ""
    }
  }

  /**
   * Adds monitors as sub-monitors to the current monitor.
   * Whenever a method is called on the parent monitor (this), this method will also be called
   * on the sub-monitors as well, recursively to the leaf monitors. This allows a hierarchical
   * composition of monitors, and operation of them all in a single call.
   *
   * @param monitors the list of monitors to be added as sub-monitors.
   */
  def monitor(monitors: Monitor[Event]*) {
    for (monitor <- monitors) {
      monitor.setParent(this)
      this.monitors ++= List(monitor)
    }
  }

  /**
   * Defines a ''named'' property to be monitored.
   * The property is defined by its name and a formula.
   *
   * @param name name of property.
   * @param formula the formula to be monitored.
   */
  def property(name: Symbol)(formula: Formula) {
    val property = Property(this, formula, name.toString)
    addExplanation(property)
    this.properties ++= List(property)
  }

  /**
   * Defines an ''unnamed'' property to be monitored.
   * The property is defined by a formula. This method can be used when the
   * monitor only contains one property, in which case the name of the monitor
   * suffices to identify the property.
   *
   * @param formula the formula to be monitored.
   */
  def property(formula: Formula) {
    val property = Property(this, formula)
    addExplanation(property)
    this.properties ++= List(property)
  }

  /**
   * Defines a ''named'' block to be ''always'' monitored.
   * The method provides a shorthand for a call of the ''property'' method with a call
   * of ''always(block)'' as argument formula. That is, a call of the form:
   *
   * <br>
   *
   * <pre>
   *   requirement(name){block}
   * </pre>
   *
   * <br>
   *
   * is a shorthand for:
   *
   * <br>
   *
   * <pre>
   *   property(name){always{block}}
   * </pre>
   *
   * <br>
   *
   * @param name name of property.
   * @param block the block to which the ''always'' method is applied to construct a formula, for which a
   * named property is created.
   */
  def requirement(name: Symbol)(block: Block) {
    val property = Property(this, always { block }, name.toString)
    addExplanation(property)
    this.properties ++= List(property)
  }

  /**
   * Defines an ''unnamed'' block to be ''always'' monitored.
   * The method provides a shorthand for a call of the ''property'' method
   * with ''always(block)'' as argument formula. That is, a call of the form:
   *
   * <br>
   *
   * <pre>
   *   require{block}
   * </pre>
   *
   * <br>
   *
   * is a shorthand for:
   *
   * <br>
   * <pre>
   *   property{always{block}}
   * </pre>
   *
   * <br>
   *
   * @param block the block to which the ''always'' method is applied to construct a formula, for which an
   * unnamed property is created.
   */

  def require(block: Block) {
    val property = Property(this, always { block })
    addExplanation(property)
    this.properties ++= List(property)
  }

  /**
   * Returns the sub-monitors of a monitor.
   * Only the direct sub-monitors are returned. A sub-monitor is a monitor that has been
   * added with one of the methods: ''monitor'' or ''monitors''. The method can for example be called to
   * get access to the error messages of a sub-monitor. However, note that by
   * calling ''getMonitorResult'' one gets access to all errors including those of all sub-monitors.
   * The method is therefore not likely to be needed much.
   *
   * @return the list of direct sub-monitors.
   */
  def getMonitors: List[Monitor[Event]] = this.monitors

  /*-------------*/
  /* Termination */
  /*-------------*/

  /**
   * Method that is called when trace analysis terminates.
   * By default the method has no effect (has an empty body).
   * The method can be overridden in case special actions are wanted at the
   * end of a trace analysis, such as for example printing out statistical information
   * collected in the monitor. An example illustrating a use of this method is shown below:
   *
   * <br>
   *
   * <pre>
   * class CommandsMustSucceed extends Monitor[Event] {
   *   var commands  : Int = 0 // number of commands observed
   *   car successes : Int = 0 // number of succeses observed
   *
   *   require {
   *     case COMMAND(x) =>
   *       commands += 1
   *       hot {
   *         case SUCCESS(x) =>
   *           successes += 1
   *           ok
   *       }
   *   }
   *
   *   override def finish() {
   *     println("percentage of commands succeeded: " +
   *       (successes.asInstanceOf[Float] * 100)/commands)
   *   }
   * }
   * </pre>
   *
   */
  protected def finish() {} // The user can override this.

  /*----------*/
  /* toString */
  /*----------*/

  override def toString: String = {
    val name = this.getClass.getName.split("\\.").last
    val text = "state for " + name + ":"
    var result: String = "\n" + text + "\n"
    result += ("=" * text.length) + "\n\n"
    result += super.toString + "\n"
    for (property <- properties ++ derived) {
      result += "\n" + property.toString + "\n"
    }
    for (monitor <- monitors) {
      result += monitor.toString + "\n"
    }
    result
  }

  /*-----------*/
  /* Reporting */
  /*-----------*/

  private def reportEvent(event: Event): String = {
    var result = "\n"
    val eventStr = event.toString
    val eventLength = eventStr.length
    val restLength = 50 - eventLength
    result += "=== " + eventStr + ("=" * restLength) + "\n"
    result
  }

  private def reportInitial: String = {
    var result = "Initial state:\n\n"
    result += this.toString
    result
  }

  /*-------------------*/
  /* Result Collection */
  /*-------------------*/

  /**
   * Returns the result of a trace analysis for this monitor.
   * The result does not include the results for its sub-monitors.
   *
   * @return the list of results, per property in this monitor.
   */
  def getThisMonitorResult: MonitorResult[Event] = {
    val monitorResult: MonitorResult[Event] = new MonitorResult(getFullMonitorName)
    for (property <- properties) {
      monitorResult.add(property.getResult)
    }
    monitorResult
  }

  /**
   * Returns the result of a trace analysis for this monitor and all its sub-monitors.
   * The result includes all the errors detected for this monitor,
   * and for all its sub-montors recursively to the leaf monitors.
   *
   * @return the list of results, per property in the monitor and in all sub-monitors.
   */
  def getMonitorResult: MonitorResult[Event] = {
    val monitorResult: MonitorResult[Event] = new MonitorResult(getFullMonitorName)
    for (property <- properties) {
      monitorResult.add(property.getResult)
    }
    for (subMonitor <- monitors) {
      val subMonitorResult = subMonitor.getMonitorResult
      subMonitorResult.getPropertyResults foreach { monitorResult.add(_) }
    }
    monitorResult
  }

  /*-----------*/
  /* Filtering */
  /*-----------*/

  private var filter: PartialFunction[Event, Boolean] = { case _ => true }

  private def matchesFilter(event: Event): Boolean = filter.isDefinedAt(event) && filter(event)

  /**
   * Defines a filtering of events to be sent to the monitor.
   * The method takes as argument a partial predicate on events. Calling this method, as in ''select(f)'',
   * ensures that only events ''e'' for which ''f.isDefinedAt(e) && f(e) == true'' are submitted for evaluation
   * by the monitor. It is a way of projecting a trace to only a subset of all events and can be used for
   * example to simplify a specification.
   *
   * <br>
   *
   * As an example, consider the following specification that uses ''select''. The property monitored
   * is ''"A success cannot be issued without a previous command being issued. Once a command has
   * been issued, no other command or success should occur until the command succeeds"'':
   *
   * <br>
   *
   * <pre>
   * class CommandsMustSucceed extends Monitor[Event] {
   *   select {
   *     case COMMAND(_) => true
   *     case SUCCESS(_) => true
   *   }
   *
   *   property { P }
   *
   *   def P: Formula =
   *     strong {
   *       case COMMAND(name) => Q(name)
   *     }
   *
   *   def Q(name: String): Formula =
   *     strong {
   *       case SUCCESS(`name`) => P
   *     }
   * }
   * </pre>
   *
   * <br>
   *
   * The monitor above illustrates the use of ''strong'' states in ''P'' and ''Q'' to enforce transitions
   * to occur in the next step. We can allow this since we have filtered out all events that are not commands or successes.
   *
   * The same specification without a call of ''select'' would look as the following more complicated state machine
   * with additional error transitions:
   *
   * <br>
   *
   * <pre>
   * class CommandsMustSucceed extends Monitor[Event] {
   *   property { P }
   *
   *   def P: Formula =
   *     state {
   *       case COMMAND(name) => Q(name)
   *       case SUCCESS(_) => error
   *     }
   *
   *   def Q(name: String): Formula =
   *     hot {
   *       case SUCCESS(`name`) => P
   *       case SUCCESS(_) => error
   *       case COMMAND(_) => error
   *     }
   *  }
   * </pre>
   *
   * <br>
   *
   * It is of course a judgment whether a call of ''select'' will simplify a specification enough to counter balance the
   * additional call of ''select''.
   *
   * @param filter the filtering function. Only events ''e'' for which ''filter.isDefinedAt(e) && filter(e)'' is true will be
   * monitored by this monitor.
   */
  def select(filter: PartialFunction[Event, Boolean]) {
    this.filter = filter
  }

  /*--------------*/
  /* Verification */
  /*--------------*/

  /**
   * Verifies an individual event against the properties and sub-monitors of the monitor.
   * The event is forwarded to each property evaluator, and to each sub-monitor.
   * <p>
   * When trace analysis is done using this method, the method ''end()'' must be called to
   * terminate the analysis.
   *
   * @param event the event to be monitored.
   */

  def verify(event: Event) {
    verifyBeforeEvent(event)
    eventCount += 1
    Debug.writeln(reportEvent(event))
    writeEventLog(event)
    if (matchesFilter(event)) {
      properties map (_(event))
      derived map (_(event))
      updateDerivedProperties()
      updateFacts()
    }
    for (monitor <- monitors) {
      monitor.verify(event)
    }
    Debug.writeln(this.toString)
    verifyAfterEvent(event)
  }

  /**
   * Ends a trace analysis.
   * This method must be called at the end of the trace analysis in case the
   * method ''verify(event: Event)'' has been called one or more times. These two methods go
   * together. Note that the ''end()'' method should not be called after a call of
   * ''verify(trace: Trace)'' since that method itself calls ''end()'' at the end of the trace.
   */
  def end() {
    end_()
    summary()
  }

  private def end_() {
    val monitorName = this.getClass.getName.split("\\.").last
    if (Options.PRINT) {
      val headline = "Terminating monitor " + monitorName
      val line = "=" * (headline.length + 1)
      println(line)
      println(headline + ":")
      println(line)
      println()
    }
    for (property <- (properties ++ derived) if !end(property.formula)) {
      property.reportLivenessError(getFullMonitorName, property.name, property.getExplanation, property.getErrorTrace)
    }
    for (monitor <- monitors) {
      monitor.end_()
    }
    finish()
    closeEventLog()
  }

  private def summary() {
    if (Options.PRINT) {
      val monitorResult = getMonitorResult
      Util.bannerTraceContract()
      println(monitorResult)
    }
  }

  /**
   * The type of traces.
   * A trace is a list of events.
   */
  type Trace = List[Event]

  /**
   * Verifies a trace (list of events) against the properties and sub-monitors of the monitor.
   * The events are one by one forwarded to each property evaluator, and to each sub-monitor.
   * The method ''end()'' should '''not''' be called after this method.
   *
   * @param trace the list of events to be analyzed. The events are assumed orderd according to time of occurrence from
   * left to right. That is, a trace of the form ''List(e,,1,,,e,,2,,,e,,3,,)'' represents the situation where ''e,,1,,'' has
   * first occurred, followed by ''e,,2,,'', followed by ''e,,3,,''.
   * @return the result of the verification; can be ignored or used as part of an automated evaluation strategy.
   */
  def verify(trace: Trace): MonitorResult[Event] = {
    verifyBeforeTrace(trace)
    Debug.writeln(reportInitial)
    for (event <- trace) {
      verify(event)
    }
    end()
    val result = getMonitorResult
    verifyAfterTrace(result)
    result
  }

  /**
   * This method is called <b>before</b> every call of <code>verify(event: Event)</code>,
   * which in turn is called by <code>verify(trace: Trace)</code> for every event in the trace.
   * It can be overridden by user. Its body is by default empty. 
   * 
   * @param event the event being verified
   */
  def verifyBeforeEvent(event: Event) {}

  /**
   * This method is called <b>after</b> every call of <code>verify(event: Event)</code>,
   * which in turn is called by <code>verify(trace: Trace)</code> for every event in the trace.
   * It can be overridden by user. Its body is by default empty. 
   * 
   * @param event the event being verified
   */
  def verifyAfterEvent(event: Event) {}

  /**
   * This method is called <b>before</b> every call of <code>verify(trace: Trace)</code>.
   * It can be overridden by user. The method is called with the original trace, and its body 
   * is by default empty.
   * 
   * @param trace the trace being verified
   */
  def verifyBeforeTrace(trace: Trace) {}

  /**
   * This method is called <b>after</b> every call of <code>verify(trace: Trace)</code>.
   * It can be overridden by user. The method is called with the monitor result (of class <code>MonitorResult</code>)
   * produced by <code>verify(trace: Trace)</code>, and its body is by default empty.
   * 
   * @param result the result of calling <code>verify(trace: Trace)</code>.
   */
  def verifyAfterTrace(result: MonitorResult[Event]) {}
}
