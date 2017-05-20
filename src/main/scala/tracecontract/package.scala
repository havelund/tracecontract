
/*
  © [2011]. California Institute of Technology. 
  ALL RIGHTS RESERVED. U.S. Government sponsorship acknowledged.  
  Any commercial use must be negotiated with the Office of Technology 
  Transfer at the California Institute of Technology. The technical data in 
  this document is controlled under the U.S. Export Regulations, release to 
  foreign persons may require an export authorization.
*/

/**
 * The tracecontract package facilitates analysis of traces. It allows to write what we refer to 
 * as ''trace contracts''. 
 *
 * A trace is a sequence of events, typically produced by a running software system.
 * A trace contract is a class that extends the [[tracecontract.Monitor]] class, and 
 * specifies which traces are  well-formed and which are not. In other words, a trace 
 * contract can be seen as a predicate on traces. A trace contract consists of one or more 
 * properties, where a property is a named ''formula''. The API provides several methods to 
 * construct formulas in various sub-logics, including LTL (Linear Temporal Logic), state 
 * machines, rule-based systems, and liberal combinations thereof. 
 *
 * <p><br><p>
 *
 * Please read the: 
 * 
 * <p><br><p>
 * 
 * <a href="../../auxdoc/tracecontract-manual.html">TraceContract Manual</a>
 * 
 * <p><br><p>
 * 
 * for more detailed information on how to use this package. It may be convenient to open the manual in a new window
 * (<i>Right click - Open Link in New Window</i>) so that the API documentation can be browsed during reading.
 */

package object tracecontract{}
