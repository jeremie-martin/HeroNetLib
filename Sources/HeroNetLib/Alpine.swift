import Interpreter
import Parser
import Foundation

public struct TransitionJSON: Decodable {
  let pre: [String: [String]]
  let post: [String: [String]]
  let condition: [[String: Bool]]
}

public struct HeroJSON: Decodable {
  let transition: [TransitionJSON]
  let marking: [String: [String]]
}

public  enum CondTerm {
  case str(String)
  case value(Value)
}

public struct AlpineNet {

  public typealias Term = Value

  public init(module: String, json: String? = nil) {
    self.interpreter = Interpreter()
    try! self.interpreter.loadModule(fromString:module)
    self.places = []
    self.transitions = []

    if let jsonData = json?.data(using:.utf8)! {
      let heroParse: HeroJSON = try! JSONDecoder().decode(HeroJSON.self, from:jsonData)

      heroParse.transition.forEach {
        self.addTransition(pre:$0.pre, post:$0.post, cond:$0.condition)
      }

      heroParse.marking.forEach {
        var values: [Value] = []
        $0.value.forEach { values.append(try! interpreter.eval(string:$0)) }
        self.initialMarking[$0.key] = values
      }
    }

  }

  /*
    Input : pre = ["p1": ["x", "y"], "p2": ["f"]]
    Output: [PredicateArc(place:"p1", label:[.variable("x"), .variable("y")],
             PredicateArc(place:"p2", label:[.variable("f")]]
  */
  public mutating func addTransition(
    pre: [String: [String]],
    post: [String: [String]],
    cond: [[String: Bool]]
  ) {
    self.places = Set(pre.keys)
      .union(Set(post.keys))
      .union(self.places)

    let inbound = pre.map {
      PredicateArc<Value>(place:$0.key, label:$0.value.map { .variable($0) }, sexpr:nil)
    }

    let outbound = post.map {
      PredicateArc<Value>(place:$0.key, label:$0.value.map { .variable($0) }, sexpr:nil)
    }

    let guards: [(CondTerm, CondTerm)] = cond.map {
      let terms = $0.map {
        ($0.value) ? CondTerm.value(try! interpreter.eval(string:$0.key)) : CondTerm.str($0.key)
      };
      return (terms[0], terms[1])
    }

    self.transitions.insert(PredicateTransition(
      preconditions:Set(inbound), postconditions:Set(outbound), conditions:guards
    ))
  }

  public func createNet(fromMarking: PredicateNet<Term>.MarkingType? = nil) {
    let net = PredicateNet<Term>(
      places:self.places, transitions:self.transitions, interpreter:self.interpreter
    )

    print("Initial marking")
    initialMarking.forEach { print("  - \($0.key): \($0.value)") }
    print("")
    for m in net.simulation(from:initialMarking).prefix(6) {
      if let binding = m.1 {
        print("Binding:")
        binding.forEach { print("  - \($0.key): \($0.value)") }
        print()

        if m.2.count > 0 {
          print("New tokens:")
          m.2.forEach { print("  - \($0.key) |-> \($0.value)") }
          print()
        }

        print("New marking")
        m.0.forEach { print("  - \($0.key): \($0.value)") }
        print()
      } else {
        print("Deadlock.\n")
        break
      }

    }
  }

  var interpreter: Interpreter
  var places: Set<String>
  var transitions: Set<PredicateTransition<Term>>

  var initialMarking: PredicateNet<Term>.MarkingType = [:]
  /* let net: PredicateNet<T>
   * let initialMarking: PredicateNet<T>.MarkingType? */
}
