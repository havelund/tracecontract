/*
  © [2011]. California Institute of Technology. 
  ALL RIGHTS RESERVED. U.S. Government sponsorship acknowledged.  
  Any commercial use must be negotiated with the Office of Technology 
  Transfer at the California Institute of Technology. The technical data in 
  this document is controlled under the U.S. Export Regulations, release to 
  foreign persons may require an export authorization.
*/

package tracecontract

private object Debug {
	def writeln(message : String) {
		if (Options.DEBUG) {
			println(message)
		}
	}
	
	def debug(message : String) {
		if (Options.DEBUG) {
			println("|- " + message)
		}
	}
}
