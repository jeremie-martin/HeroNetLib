import XCTest
@testable import HeroNetLib
import Interpreter
import Parser

final class HeroNetLibTests: XCTestCase {
  func testExample() {
    do {
      print("\n")
      var an = AlpineNet()

      an.addTransition(pre:["p1": ["x"], "p2": ["f"]], post:["p1": ["f"]], source:"bool.alpine")
      let m0 = ["p1": ["#True"], "p2": ["not"]]

      /* an.addTransition(pre:["p1": ["x"], "p2": ["y"]], post:["p1": ["x"]], source:"bool.alpine") */
      /* let m0 = ["p1": ["a", "b"], "p2": ["c", "d"]] */
      /* an.createNet(initialMarking:m0) */

      /* let a = try! an.transitions.first!.interpreter.eval(string:"not(#True)") */
      /* print(a) */
      /* first.interpreter("#not(#True)") */

      let src = try! String(contentsOfFile:"bool.alpine", encoding:.utf8)
      var interpreter = Interpreter()
      try! interpreter.loadModule(fromString:src)
      let code: String = "not(#True)"
      let value = try! interpreter.eval(string:code)

      let pre: [String: [(String, UInt)]] = ["p1": [("x", 0), ("y", 0)], "p2": [("f", 1)]]
      let inbound = pre.map {
        PredicateArc<String>(place:$0.key, label:$0.value.map { .expr($0.0, $0.1) })
      }
      let post: [String: [String: [String]]] = ["p1": ["f": ["x"]]]
      let outbound = post.map {
        PredicateArc<String>(place:$0.key, label:$0.value.map { .apply($0.key, $0.value) })
      }
      let t1 = PredicateTransition(
        preconditions:Set(inbound), postconditions:Set(outbound), module:src
      )

      /* for (symbol, function) in interpreter.astContext.modules[0].functions { */
      /*   print(symbol.name, function.signature.domain.label) */
      /* } */

      print(value)

      print("\n")
    }
  }

  static var allTests = [("testExample", testExample)]
}
