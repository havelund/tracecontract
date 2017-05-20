
package tests.test5

import tracecontract._
import tests._
import org.junit.Test

// Past time logic experiments

// =======
// Events:
// =======

abstract class Event
case object P extends Event
case object Q extends Event
case object R extends Event
case object X extends Event

// ==============
// Requirement 1:
// ==============

// [] { (-) (-) P & (-) Q -> <> R }

class R1 extends Monitor[Event] {
  case object YP extends Fact
  case object YQ extends Fact
  case object YYP extends Fact

  property('Check) {
    always {
      case P => YP +
      case _ => YP -
    } and
      always {
        case Q => YQ +
        case _ => YQ -
      } and
      always {
        case _ if YP ? => YYP +
        case _ if YP ~ => YYP -
      } and
      always {
        case _ if (YYP ?) && (YQ ?) =>
          hot {
            case R => ok
          }
      }
  }
}

class Test1_1 extends Contract[Event] {
  @Test def test() {
    test(new R1)(
      X, P, P, Q, X, X, R)()
  }
}

class Test1_2 extends Contract[Event] {
  @Test def test() {
    test(new R1)(
      X, P, P, Q, X, X)(
        live("R1", "'Check", (5)))
  }
}

class Test1_3 extends Contract[Event] {
  @Test def test() {
    test(new R1)(
      X, P, P, Q)( // Trace does not give an error since the observation that the pattern
      // PPQ has occurred only occurs in the state after Q, and there is no
      // such state. The detection is delayed one step.
      )
  }
}

// ==============
// Requirement 2:
// ==============

// [] { (-) (-) P & (-) Q -> <> R }

// modified to react immediately when Q is observed

class R2 extends Monitor[Event] {
  case object YP extends Fact
  case object YYP extends Fact

  property('Check) {
    always {
      case P => YP +
      case _ => YP -
    } and
      always {
        case _ if YP ? => YYP +
        case _ if YP ~ => YYP -
      } and
      always {
        case Q if YYP ? =>
          hot {
            case R => ok
          }
      }
  }
}

class Test2_1 extends Contract[Event] {
  @Test def test() {
    test(new R2)(
      X, P, P, Q, X, X, R)()
  }
}

class Test2_2 extends Contract[Event] {
  @Test def test() {
    test(new R2)(
      X, P, P, Q, X, X)(
        live("R2", "'Check", (4)))
  }
}

class Test2_3 extends Contract[Event] {
  @Test def test() {
    test(new R2)(
      X, P, P, Q)(
        live("R2", "'Check", (4)))
  }
}

// ==============
// Requirement 3:
// ==============

// [] { (-) (-) P & (-) Q -> <> R }

// reformualted as succinctly as possible

class R3 extends Monitor[Event] {
  case object YP extends Fact
  case object YYP extends Fact

  require {
    case P => YP +
    case _ => YP -
  }

  require {
    case _ if YP ? => YYP +
    case _ if YP ~ => YYP -
  }

  require {
    case Q if YYP ? =>
      hot {
        case R => ok
      }
  }
}

class Test3_1 extends Contract[Event] {
  @Test def test() {
    test(new R3)(
      X, P, P, Q, X, X, R)()
  }
}

class Test3_2 extends Contract[Event] {
  @Test def test() {
    test(new R3)(
      X, P, P, Q, X, X)(
        live("R3", "", (4)))
  }
}

class Test3_3 extends Contract[Event] {
  @Test def test() {
    test(new R3)(
      X, P, P, Q)(
        live("R3", "", (4)))
  }
}

