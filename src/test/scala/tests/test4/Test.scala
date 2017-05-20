
package tests.test4

import tracecontract._
import tests._
import org.junit.Test

// =======
// Events:
// =======

abstract class Event
case class COM(id: String, attempt: Int, payload: Any) extends Event
case class ACK(id: String, hash: Int) extends Event
case class OK(id: String) extends Event

// ===============
// Requirement R1:
// ===============

// For any id value the COM(id,attempt,payload) event must use a positive attempt value.

// Ruler:
// ------
// always R1 {
//   com(id: int, attempt: int, payload: obj), attempt <= 0->
//     Error("Non positive attempt:- " + com(id, attempt, payload));
// }

class R1 extends Monitor[Event] {
  require {
    case COM(id, attempt, payload) => attempt > 0
  }
}

// I can also write it in the style used in the above Ruler spec:

class R1_ extends Monitor[Event] {
  require {
    case e @ COM(id, attempt, payload) if attempt <= 0 =>
      error("Non positive attempt:- " + e)
  }
}

// However, TraceContract will print out all the necessary information for you
// automatically, so there is no need to make an effort printing out an error message.

// -----
// Test:
// -----

class Test1_1 extends Contract[Event] {
  @Test def test() {
    test(new R1)(
      COM("A", 100, "file"),
      COM("B", 200, "file"))()
  }
}

class Test1_2 extends Contract[Event] {
  @Test def test() {
    test(new R1)(
      COM("A", 100, "file"),
      COM("B", -200, "file"))(
        safe("R1", "", (2)))
  }
}

// ===============
// Requirement R2:
// ===============

// For every COM(id,attempt,payload) event, every successive 
// COM(id,attempt,payload) event with id = id must have 
// attempt > attempt and payload == payload.

// Ruler:
// ------
// always R2 {
//   com(id: int, attempt: int, payload: obj)
//     {: Track(id, p_attempt: int, p_payload: obj)
//       {| attempt <= p_attempt  ->  
//            Error("Non-increasing attempt: " + com(id, attempt, payload));
//          payload != p_payload  ->  
//            Error("DIfferent payload: " + com(id, attempt, payload);
//       |}
//       default -> Track(id, attempt, payload)
//     :}
// }

class R2 extends Monitor[Event] {
  require {
    case COM(id, attempt, payload) =>
      state {
        case COM(`id`, attempt_, payload_) =>
          attempt_ > attempt & payload_ == payload
      }
  }
}

// Note: the property here focuses on two subsequent commands. It will compare 
// commands pairwise. In Ruler it is a little bit confusing that the Track
// state is not declared anywhere. It's very dynamic.

// -----
// Test:
// -----

class Test2_1 extends Contract[Event] {
  @Test def test() {
    test(new R2)(
      COM("A", 100, "fileA"),
      COM("B", 100, "fileB"),
      COM("A", 200, "fileA"),
      COM("B", 200, "fileB"))()
  }
}

class Test2_2 extends Contract[Event] {
  @Test def test() {
    test(new R2)(
      COM("A", 100, "fileA"),
      COM("B", 100, "fileB"),
      COM("A", 99, "fileA"),
      COM("B", 200, "fileBB"))(
        safe("R2", "", (1, 3)),
        safe("R2", "", (2, 4)))
  }
}

// ===============
// Requirement R3:
// ===============

// For any COM(id,attempt,payload) event and the first following ACK(id,hash) 
// where id = id and, for an agreed hash-function H, hash = H(id,payload), the 
// next event, after that ACK event, involving that id value should be an OK(id) 
// event and the next COM event with that id can have an arbitrary attempt value.

// Comment: this requirement is in conflict with Requirement R2 due to the last line.

// Ruler:
// ------
// always R3 {
//   com(id: int, attempt: int, payload: obj) -> R3a(id, attempt, payload);
// }
//
// state R3a(id: int, attempt: int, payload: obj)  {
//   ack(id, hash: int)
//     {: hash != H(id, payload) -> 
//          Error("Incorrect hash value: " + hash + " for " + com(id, attempt, payload));
//        default -> R3b(id);
//     :}
// }
//
// state R3b(id: int) {
//   ok(id) 
//     {: Track(id, attempt: int, payload: obj) -> !Track(id, attempt, payload);
//        default -> Ok;
//     :}
//   com(id, attempt: int, payload: obj) -> 
//     Error("Required outstanding " + ok(id) + " first"), R3b(id);
//   ack(id, hash: int) -> 
//     Error("Required outstanding " + ok(id) + " first"), R3b(id);
// }

// Comment: the Ruler spec requires the next ACK to have the right hash value.
// This is not what the informal requirement says I think. 
// What is Track used for by the way?

class R3 extends Monitor[Event] {
  def H(id: String, payload: Any) = id.length

  require {
    case COM(id, attempt, payload) =>
      state {
        case ACK(`id`, hash) if hash == H(id, payload) =>
          hot {
            case COM(`id`, _, _) | ACK(`id`, _) => error
            case OK(`id`) => ok
          }
      }
  }
}

// -----
// Test:
// -----

class Test3_1 extends Contract[Event] {
  @Test def test() {
    test(new R3)(
      COM("AA", 100, "fileA"),
      COM("BBB", 50, "fileB"),
      ACK("BBB", 3),
      ACK("AA", 2),
      OK("AA"),
      OK("BBB"))()
  }
}

class Test3_2 extends Contract[Event] {
  @Test def test() {
    test(new R3)(
      COM("AA", 100, "fileA"),
      COM("BBB", 50, "fileB"),
      ACK("BBB", 3),
      ACK("AA", 2),
      ACK("AA", 2),
      OK("BBB"))(
        safe("R3", "", (1, 4, 5)))
  }
}

// ===============
// Requirement R4:
// ===============

// For any COM(id,attempt,payload) event if attempt > 10 then no event 
// COM(id,attempt,payload) should be seen with id = id until an OK(id) event is seen.

// Ruler:
// ------
// always R4{
//   com(id: int, attempt: int, payload: obj)
//     {: attempt > 10 -> R4a(id);
//        default -> Ok;
//     :}
// }
//
// state R4a(id: int) {
//   ok(id) -> Ok;
//   com(n_id: int, attempt: int, payload: obj)
//     {: n_id != id -> 
//          Error("Command " + com(n_id, attempt, payload) + " issued before " + ok(id)), R4a(id);
//        default -> R4a(id);
//     :}
// }

class R4 extends Monitor[Event] {
  require {
    case COM(id, attempt, _) if attempt > 10 =>
      state {
        case COM(id_, _, _) if id_ != id => error
        case OK(`id`) => ok
      }
  }
}

// -----
// Test:
// -----

class Test4_1 extends Contract[Event] {
  @Test def test() {
    test(new R4)(
      COM("A", 11, "fileA"),
      COM("A", 12, "fileA"),
      OK("A"),
      COM("B", 11, "fileB"),
      COM("B", 12, "fileB"),
      OK("B"))()
  }
}

class Test4_2 extends Contract[Event] {
  @Test def test() {
    test(new R4)(
      COM("A", 11, "fileA"),
      COM("A", 12, "fileA"),
      OK("A"),
      COM("B", 11, "fileB"),
      COM("C", 12, "fileC"),
      OK("B"))(
        safe("R4", "", (4, 5)))
  }
}
