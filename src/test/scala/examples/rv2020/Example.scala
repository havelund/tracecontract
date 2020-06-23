package examples.rv2020

import tracecontract._

case class Cmd(name: String, time: Int)

class DistinctTimes extends Monitor[Cmd] {
  require {
    case Cmd(_,time1) => state {
      case Cmd(_,time2) => time2 > time1
    }
  }
}

class ActivateTimely extends Monitor[Cmd] {
  val upperBound : Int = 30

  require {
    case Cmd("power", time1) => hot {
      case Cmd("activate", time2) => time2 - time1 < 30
    }
  }
}

class ActivateTimelyFun extends Monitor[Cmd] {
  val upperBound : Int = 30

  require {
    case Cmd("power", time) => activateTimely(time)
  }

  def activateTimely(powerTime: Int): Formula =
    hot {
      case Cmd("activate", time) => time - powerTime < upperBound
    }
}

class Monitors extends Monitor[Cmd] {
  monitor(new DistinctTimes, new ActivateTimely)
}

object Run {
  def main(args: Array[String]) {
    val monitors = new Monitors
    // val monitors = new DeviceActivation
    val trace =
      List(Cmd("power",100), Cmd("transmit", 130), Cmd("activate", 150))
    monitors.verify(trace)
  }
}

//class DeviceActivation extends Monitor[Cmd] {
//  require {
//    case Cmd(cmd, time) =>
//      if (cmd == "transmit") error
//  }
//}