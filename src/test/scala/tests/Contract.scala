
package tests

// import org.scalatest.FunSuite

//import org.junit.runner.RunWith
// import org.scalatest.junit.JUnitRunner

import tracecontract._

//@RunWith(classOf[JUnitRunner]) @@@
//class Contract[Event] extends FunSuite { @@@
class Contract[Event] {
  // public:

  implicit def conv1(x: Int): List[Int] = List(x)

  implicit def conv2(x: (Int, Int)): List[Int] = List(x._1, x._2)

  implicit def conv3(x: (Int, Int, Int)): List[Int] = List(x._1, x._2, x._3)

  implicit def conv4(x: (Int, Int, Int, Int)): List[Int] = List(x._1, x._2, x._3, x._4)

  implicit def conv5(x: (Int, Int, Int, Int, Int)): List[Int] = List(x._1, x._2, x._3, x._4, x._5)

  implicit def conv5(x: (Int, Int, Int, Int, Int, Int)): List[Int] = List(x._1, x._2, x._3, x._4, x._5, x._6)

  abstract class Spec

  case class safe(monitorName: String, propertyName: String, errorTrace: List[Int]) extends Spec

  case class live(monitorName: String, propertyName: String, errorTrace: List[Int]) extends Spec

  def mustSatisfy(testId: String, monitorResult: MonitorResult[Event], spec: Spec*) {
    val specs = spec.toList
    val propertyResults: List[PropertyResult[Event]] = monitorResult.getPropertyResults
    var errors: List[Error[Event]] = List()
    for (pr <- propertyResults) {
      errors ++= pr.getSafetyErrors
      errors ++= pr.getLivenessErrors
    }

    var notOkMessage = "\n\n" + testId + "\n\n" + "VIOLATES" + "\n\n"
    notOkMessage += "--- actual spec begin: ---\n"
    notOkMessage += errors map (mkSpec(_)) mkString (",\n")
    notOkMessage += "\n"
    notOkMessage += "--- actual spec end: ---\n\n"

    var result = true
    if (errors.length != specs.length)
      result = false
    else for ((error, spec) <- errors zip specs) {
      result &= satisfies(error, spec)
    }
    assert(result, notOkMessage)
  }

  def test(monitor: Monitor[Event])(events: Event*)(specs: Spec*) {
    monitor.setPrint(false)
    var testId = ""
    testId += "TEST PACKAGE: " + this.getClass.getPackage.getName + "\n"
    testId += "TEST MONITOR: " + this.getClass.getName + "\n\n"
    testId += "--- spec begin: ---" + "\n"
    testId += specs.toList mkString (",\n")
    testId += "\n"
    testId += "--- spec end: ---"
    val result = monitor verify events.toList
    mustSatisfy(testId, result, specs: _*)
  }

  def testid: String = this.getClass.getName

  // private:

  private def satisfies(error: Error[Event], spec: Spec): Boolean = {
    var result =
      error match {
        case SafetyError(longMonitorName, propertyName, explanation, errorTrace) =>
          val monitorName = longMonitorName.split("\\.").last
          spec match {
            case safe(`monitorName`, `propertyName`, eventNumbers) if eventNumbers == (errorTrace.getTrace map (_._1)) => true
            case _ => false
          }
        case LivenessError(longMonitorName, propertyName, explanation, errorTrace) =>
          val monitorName = longMonitorName.split("\\.").last
          spec match {
            case live(`monitorName`, `propertyName`, eventNumbers) if eventNumbers == (errorTrace.getTrace map (_._1)) => true
            case _ => false
          }
      }
    if (!result) println(error + "\n\ndid not match\n\n" + spec + "\n\n")
    result
  }

  private def mkSpec(error: Error[Event]): String = {
    def eventNumbers(errorTrace: ErrorTrace[Event]): String =
      "(" + (errorTrace.getTrace map (_._1) mkString (",")) + ")"

    val q = "\""  
      
    error match {
      case SafetyError(longMonitorName, propertyName, _, errorTrace) =>
        val monitorName = longMonitorName.split("\\.").last
        s"safe($q$monitorName$q,$q$propertyName$q,${eventNumbers(errorTrace)})"
      case LivenessError(longMonitorName, propertyName, _, errorTrace) =>
        val monitorName = longMonitorName.split("\\.").last
        s"live($q$monitorName$q,$q$propertyName$q,${eventNumbers(errorTrace)})"
    }
  }
}
