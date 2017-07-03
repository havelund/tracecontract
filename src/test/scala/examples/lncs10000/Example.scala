package examples.lncs10000

import tracecontract._

trait Event

case class Command(time: Int, kind: String, name: String, nr: Int) extends Event

case class DispatchFailure(time: Int, name: String, nr: Int) extends Event

case class Dispatch(time: Int, name: String, nr: Int) extends Event

case class Failure(time: Int, name: String, nr: Int) extends Event

case class Success(time: Int, name: String, nr: Int) extends Event

class P extends Monitor[Event] {
  require {
    case Command(_, "FSW", x, y) if x.startsWith("PWR") =>
      hot {
        case DispatchFailure(_, `x`, `y`) => error
        case Dispatch(t1, `x`, `y`) =>
          hot {
            case Failure(_, `x`, `y`) => error
            case Success(t2, `x`, `y`) if t2 - t1 <= 5 =>
              state {
                case Success(_, `x`, `y`) => error
              }
          }
      } upto {
        case Command(_,"FSW",_,_) => true
      }
  }
}

object TraceAnalysis {

  def main(args: Array[String]) {
    def trace: List[Event] =
      List(
        Command(1, "FSW", "PWR_ON", 1),
        Dispatch(2, "PWR_ON", 1),
        Command(3, "FSW", "PWR_OFF", 2),
        Success(4, "PWR_ON", 1)
      )

    val monitor = new P
    monitor.verify(trace)
  }
}

