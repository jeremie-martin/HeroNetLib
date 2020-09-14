import AST
import DDKit
import Foundation
import Interpreter
import Parser

public struct TransitionJSON: Decodable {
  let pre: [String: [String]]
  let post: [String: [String]]
  let condition: [[String]]
}

public struct HeroJSON: Decodable {
  let transition: [TransitionJSON]
  let marking: [String: [String]]
}

extension Array where Element: Equatable {
  func removeDuplicates() -> [Element] {
    var result = [Element]()

    for value in self {
      if result.contains(value) == false {
        result.append(value)
      }
    }

    return result
  }
}

public struct AlpineNet {
  // Lifecycle

  public init(module: String, json: String? = nil) {
    interpreter = Interpreter()
    try! interpreter.loadModule(fromString: module)
    moduleFuncNames = Set(
      interpreter.astContext.modules[0].statements.compactMap { node -> String? in
        switch node {
        case let n as Func:
          return n.name!
        /* case let n as Tuple: */
        /*   return n.label */
        default:
          return nil
        }
      }
    )
    places = []
    transitions = []

    if let jsonData = json?.data(using: .utf8)! {
      let heroParse: HeroJSON = try! JSONDecoder()
        .decode(HeroJSON.self, from: jsonData)

      heroParse.transition.forEach {
        self.addTransition(pre: $0.pre, post: $0.post, cond: $0.condition)
      }

      heroParse.marking.forEach {
        var values: [ValueOrdered] = []
        $0.value.forEach {
          values.append(ValueOrdered(try! interpreter.eval(string: $0)))
        }
        self.initialMarking[$0.key] = values
      }
    }
  }

  // Public

  public var initialMarking: PredicateNet.MarkingType = [:]

  public var interpreter: Interpreter

  /*
     Input : pre = ["p1": ["x", "y"], "p2": ["f"]]
     Output: [PredicateArc(place:"p1", label:[.str("x"), .str("y")], PredicateArc(place:"p2", label:[.str("f")]]
   */
  public mutating func addTransition(
    pre: [String: [String]],
    post: [String: [String]],
    cond: [[String]]
  ) {
    places = Set(pre.keys).union(Set(post.keys)).union(places)

    let inbound = pre.map {
      PredicateArc(place: $0.key, label: $0.value.map { .str($0) })
    }

    let outbound = post.map {
      PredicateArc(place: $0.key, label: $0.value.map { .str($0) })
    }

    let guards: [(PredicateLabel, PredicateLabel, [String])] =
      cond.map {
        var vars: [String] = []
        let terms = $0.map { src -> PredicateLabel in
          let uwu = parseVars(source: src)
          vars += uwu
          if uwu.isEmpty {
            return PredicateLabel.value(try! interpreter.eval(string: src))
          } else {
            return PredicateLabel.str(src)
          }
        }
        return (terms[0], terms[1], vars)
      }

    /* let guardsVar = cond.mapFlat { $0. keysparseVars(source:$0.key) } */

    transitions.insert(PredicateTransition(
      preconditions: Set(inbound),
      postconditions: Set(outbound),
      conditions: guards,
      interpreter: interpreter,
      factory: factory,
      morphisms: morphisms
    ))
  }

  public func createNet(fromMarking _: PredicateNet.MarkingType? = nil) {
    let net = PredicateNet(
      places: places,
      transitions: transitions,
      interpreter: interpreter,
      factory: factory,
      morphisms: morphisms
    )

    /* print("Initial marking") */
    /* initialMarking.forEach { */
    /*   print("  - \($0.key): \($0.value.map { ($0.id, $0.value) })") */
    /* } */
    /* print("") */
    for m in net.simulation(from: initialMarking).prefix(1) {
      break
      if let binding = m.1 {
        print("Binding:")
        binding.forEach {
          print("  - \($0.key): \($0.value.value)")
        }
        print()

        if m.2.count > 0 {
          print("New tokens:")
          m.2.forEach {
            print("  - \($0.key) |-> \($0.value)")
          }
          print()
        }

        print("New marking")
        m.0.forEach {
          print("  - \($0.key): \($0.value.map { ($0.id, $0.value) })")
        }
        print()
      } else {
        print("Deadlock.\n")
        break
      }
    }
  }

  // Internal

  var moduleFuncNames: Set<String>
  var places: Set<String>
  var transitions: Set<PredicateTransition>

  let factory = MFDDFactory<VariableWrapper, ValueOrdered>(bucketCapacity: 1024 * 128)

  var morphisms: MFDDMorphismFactory<VariableWrapper, ValueOrdered> {
    factory.morphisms
  }

  func parseVars(source: String) -> [String] {
    let tokens = try! Lexer(source: source)

    return tokens.filter { $0.kind == TokenKind.identifier }
      .map { tok in tok.value! }
      .removeDuplicates().filter { !moduleFuncNames.contains($0) }
  }
}
