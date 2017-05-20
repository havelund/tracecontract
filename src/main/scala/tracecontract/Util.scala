/*
  © [2011]. California Institute of Technology. 
  ALL RIGHTS RESERVED. U.S. Government sponsorship acknowledged.  
  Any commercial use must be negotiated with the Office of Technology 
  Transfer at the California Institute of Technology. The technical data in 
  this document is controlled under the U.S. Export Regulations, release to 
  foreign persons may require an export authorization.
*/

package tracecontract

private object Util {
  def format(length : Int)(str : String) : String =
    str + (" " * (length - str.length))  	
    	
	def error(msg : String) {
		println("*** " + msg)
		assert(false)
	}

  def toHeadline(ch : String, msg : String): String  = {
    val length = msg.length
    val line = ch * length
    line + "\n" + msg + "\n" + line
  }

	def headline(ch : String, msg : String) {
		val length = msg.length
		val line = ch * length
		println()
		println(line)
		println(msg)
		println(line)
		println()
	}
	
	def underline(msg : String) {
		val length = msg.length
		val line = "-" * length
		println()
		println(msg)
		println(line)
		println()		
	}
	
	def headline1(msg : String) {
		headline("=",msg)
	}

	def headline2(msg : String) {
		headline("-",msg)
	}

	def bannerTraceContract() {
		println("""

 _______                  _____            _                  _
|__   __|                / ____|          | |                | |
   | |_ __ __ _  ___ ___| |     ___  _ __ | |_ _ __ __ _  ___| |_
   | | '__/ _` |/ __/ _ \ |    / _ \| '_ \| __| '__/ _` |/ __| __|
   | | | | (_| | (_|  __/ |___| (_) | | | | |_| | | (_| | (__| |_
   |_|_|  \__,_|\___\___|\_____\___/|_| |_|\__|_|  \__,_|\___|\__|

      _____
     |   __|_ _ _____ _____ ___ ___ _ _
     |__   | | |     |     | .'|  _| | |
     |_____|___|_|_|_|_|_|_|__,|_| |_  |
                                   |___|
	""")
	}

}