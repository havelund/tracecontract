package daut.examples.example6

import daut._
import daut.Monitor

/**
  * P: A task acquiring a lock should eventually release it. At most one task
  * can acquire a lock at a time.
  *
  * Q: A task cannot release a lock it has not acquire.
  * */


trait E
case class a(t:Int,x:Int) extends E
case class r(t:Int,x:Int) extends E

class P extends Monitor[E] {
  always {
    case a(t, x) =>
      hot {
        case a(_,`x`) => error
        case r(`t`,`x`) => ok
      }
  }
}

class Q extends Monitor[E] {
  case class L(t:Int,x:Int) extends state {
    watch {
      case r(`t`,`x`) => ok
    }
  }

  always {
    case a(t,x) => L(t,x)
    case r(t,x) if !L(t,x) => error
  }
}

class PQ extends Monitor[E] {
  monitor(new P, new Q)
}

object Main {
  def main(args: Array[String]) {
    val m = new PQ
    m.PRINT = true
    m.verify(a(1, 10))
    m.verify(r(1, 10))
    m.end()
  }
}