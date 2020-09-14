import DDKit
import Foundation
import Interpreter
import Parser
import PetriKit
import SwiftProductGenerator

public extension Interpreter {
  func eval(predicate: PredicateLabel, binding: [String: Value]) -> Value {
    switch predicate {
    case .value(let v):
      return v
    case .str(let s):
      return try! eval(
        string: s,
        replace: binding
      )
    }
  }
}

public func setSeed(seed _: UInt = 5323) {
  PetriKit.Random.seed = UInt(time(nil))
}

public struct PredicateNet {
  // Lifecycle

  public init(
    places: Set<PlaceType>,
    transitions: Set<PredicateTransition>,
    initialMarking: MarkingType? = nil,
    seed: UInt = 5323,
    interpreter: Interpreter,
    factory: MFDDFactory<VariableOrd, ValueOrd>,
    morphisms: MFDDMorphismFactory<VariableOrd, ValueOrd>
  ) {
    setSeed(seed: seed)

    self.places = places
    self.transitions = transitions
    self.initialMarking = initialMarking

    self.interpreter = interpreter
    self.factory = factory
    self.morphisms = morphisms
  }

  // Public

  public typealias PlaceType = String
  public typealias MarkingType = [PlaceType: [ValueOrd]]

  public unowned let interpreter: Interpreter

  /// The set of places of the Predicate Net.
  public let places: Set<PlaceType>

  /// The set of transitions of the Predicate Net.
  public let transitions: Set<PredicateTransition>

  /// The (optional) initial marking of the Predicate Net.
  public let initialMarking: MarkingType?

  /// Returns a marking reachable after up to `steps` transition firings.
  ///
  /// At each step, one fireable transition is chosen at random and fired to produce a new
  /// marking. If the Predicate Net reaches a deadlock, the remaining steps are ignored and the
  /// state that was reached is returned.
  public func simulate(
    steps: Int,
    from marking: MarkingType
  )
    -> (
      MarkingType,
      [PredicateTransition.Binding],
      [[String: Value]]
    )
  {
    var m = marking
    var bindingChosen: [PredicateTransition.Binding] = []
    var newTokens: [[String: Value]] = []

    // For as many steps are we were instructed to simulate ...
    for _ in 0 ..< steps {
      // Generate for each transition the set of fireable bindings.
      var fireable: [PredicateTransition: AlpineDD] = [:]
      for transition in transitions {
        // Notice how we ignore non-fireable transitions.
        let bindings = transition.fireableBingings(from: m)
        if bindings.count > 0 {
          fireable[transition] = bindings
        }
      }

      // If we reached a deadlock, ignore the remaining transition and return.
      guard !fireable.isEmpty else { return (m, bindingChosen, newTokens) }

      // Choose one transition at random and fire it to produce the next marking.
      let (t, bindings) = PetriKit.Random.choose(from: fireable)
      /* let binding = PetriKit.Random.choose(from:bindings) */
      let binding = bindings.randomElement()!
      bindingChosen.append(binding)
      var new: [String: Value] = [:]
      (m, new) = t.fire(from: m, with: binding)!
      newTokens.append(new)
    }

    /* let end = DispatchTime.now()   // <<<<<<<<<<   end time */
    /* let timeInterval = Double(end.uptimeNanoseconds - start.uptimeNanoseconds)  / 1_000_000_000 // Technically could overflow for long running tests */
    /* print("Time to evaluate problem: \(timeInterval) seconds") */

    return (m, bindingChosen, newTokens)
  }

  // Internal

  unowned let factory: MFDDFactory<VariableOrd, ValueOrd>

  unowned let morphisms: MFDDMorphismFactory<VariableOrd, ValueOrd>
}

/* public struct VariableOrd: Hashable, Comparable, CustomStringConvertible { */
/*   public let value: String */
/*   public let id: Int */
/*  */
/*   public var description: String { "\(value) (\(index))" } */
/*  */
/*   public static func == (lhs: VariableOrd, rhs: VariableOrd) -> Bool { */
/*     lhs.index == rhs.index */
/*   } */
/*  */
/*   public static func < (lhs: VariableOrd, rhs: VariableOrd) -> Bool { */
/*     lhs.index < rhs.index */
/*   } */
/*  */
/*   public func hash(into hasher: inout Hasher) { */
/*     hasher.combine(index) */
/*   } */
/* } */

/// Type of variables on arc labels.
public typealias Variable = String

/// Structure for transitions of predicate nets.
public class PredicateTransition {
  // Lifecycle

  public init(
    preconditions: Set<PredicateArc> = [],
    postconditions: Set<PredicateArc> = [],
    conditions: [(PredicateLabel, PredicateLabel, [Variable])] = [],
    interpreter: Interpreter,
    factory: MFDDFactory<VariableOrd, ValueOrd>,
    morphisms: MFDDMorphismFactory<VariableOrd, ValueOrd>
  ) {
    var inboundPlaces: Set<PredicateNet.PlaceType> = []
    var inboundVariables: Set<Variable> = []

    varInfos = [:]

    for arc in preconditions {
      // Make sure the a doesn't appear twice as a precondition.
      guard !inboundPlaces.contains(arc.place) else {
        preconditionFailure("Place '\(arc.place)' appear twice as precondition.")
      }
      inboundPlaces.insert(arc.place)

      // Store the inbound variables so we can check postconditions for free variables, and
      // Make sure the precondition is labeled with variables only.
      for item in arc.label {
        switch item {
        case .str(let v):
          inboundVariables.insert(v)
        case .value:
          preconditionFailure("Preconditions should be labeled with variables only.")
        }
      }
    }

    self.preconditions = preconditions
    self.postconditions = postconditions
    self.conditions = conditions
    self.interpreter = interpreter
    self.factory = factory
    self.morphisms = morphisms

    // Conditions with lots of variables are at the top of the MFDD
    let conds = conditions.map { ($0.0, $0.1, $0.2.removeDuplicates()) }
    var order = conds
      .sorted { $0.2.count < $1.2.count }
      .flatMap { $0.2 }
      .removeDuplicates()

    // TODO: This code is horrendous
    conditionsOrdered = conds
      .sorted { a, b -> Bool in
        a.2.map { order.lastIndex(of: $0)! }.max()! + a.2.count < b.2
          .map { order.lastIndex(of: $0)! }.max()! + b.2.count
      }
      .map { e1, e2, variables in
        (
          e1,
          e2,
          variables.map { name in
            VariableOrd(value: name, id: order.index(of: name)!)
          }.removeDuplicates().sorted()
        )
      }

    var tmp = self.inboundVariables()
    let varN = tmp.reduce(0) { acc, e in acc + e.value.count }

    func minDist(order: [String], vars: [String], n: Int = 0) -> Int {
      if order.isEmpty { return (2 * varN - vars.count) * (2 * varN - vars.count) }
      if vars.contains(order.last!) { return n * n }
      else {
        return minDist(order: order.dropLast(), vars: vars, n: n + 1)
      }
    }

    while let e = tmp.min { a, b in
      minDist(order: order, vars: a.value) < minDist(order: order, vars: b.value)
    } {
      tmp.removeValue(forKey: e.key)
      inboundVariablesCondArr.append(
        (e.key, e.value.map { name -> VariableOrd in
          if !order.contains(name) { order.append(name) }
          return VariableOrd(value: name, id: order.index(of: name)!)
        }.sorted())
      )
    }

    inboundVariablesCond = Dictionary(
      uniqueKeysWithValues: inboundVariablesCondArr
    )
  }

  // Public

  /// Type for transition bindings.
  public typealias Binding = [VariableOrd: ValueOrd]

  public unowned var interpreter: Interpreter

  public var inboundVariablesCondArr: [(PredicateNet.PlaceType, [VariableOrd])] = []
  public var inboundVariablesCond: [PredicateNet.PlaceType: [VariableOrd]] = [:]

  public var varInfos: [String: Value]

  /// The preconditions of the transition.
  public let preconditions: Set<PredicateArc>

  /// The postconditions of the transition.
  public let postconditions: Set<PredicateArc>

  public let conditions: [(PredicateLabel, PredicateLabel, [String])]
  public let conditionsOrdered: [(PredicateLabel, PredicateLabel, [VariableOrd])]

  /// Compute all possible bindings that make the transition fireable from the given marking.
  ///
  /// The idea of the implementation is to create all possible combinations of values we may
  /// pick from each place in precondition, and to build the feasible bindings from there. To
  /// achieve that, we first create the permutations of the tokens in each inbound place, that
  /// we store in an array. We then compute the cartesian product of that big sequence, which
  /// gives us all possible choices of assignment.
  ///
  /// - Example: Consider a transition with places `p0` and `p1` in precondition, marked with
  ///   `{1, 2}` and `{1, 3}` respectively. The transition is labeled `{x, y}` on its arc from
  ///   `p0` and `{x}` on its arc from from `p1`. The only possible binding is
  ///
  ///       ["x": 1, "y", 2]
  ///
  ///   because we need `x` to be bound to the same value in both `p0` and `p1`. So first we
  ///   create the array of the permutations of their tokens:
  ///
  ///       [[1, 2], [2, 1]], [[1, 3], [3, 1]]
  ///
  ///   Now we iterate throught the cartesian product of each element of this array:
  ///
  ///       [[1, 2], [1, 3]],///       [[2, 1], [1, 3]],///       ...
  ///
  ///   Notice how each element corresponds to one possible arrangement of the tokens of each
  ///   place in order. Now all we have to do is to iterate over the variables we have to bind,///   for each place in each arrangement. If the variable is already isn't bound yet, we pick
  ///   the first value we can, if it is we check if we can match that value. For instance,///   let's say we check the first arrangement. We'll bind `x` to `1` and `y` to `2` for `p0`
  ///   before moving to `p1`. Then, since `x` is already bound to `1`, we'll have no choice but
  ///   to pick `1` once again. Now if we check the second arrangement, we'll bind `x` to `2`
  ///   and `y` to `1` before moving to `p1`. But as we won't be able to match `2` in the tokens
  ///   of `p1`, we'll reject the binding and move to the next arrangement.
  public func fireableBingings(
    from marking: PredicateNet.MarkingType
  ) -> AlpineDD {
    var SWD = Stopwatch()
    /* let markingValue = marking.mapValues { $0.map { $0.value } } // For brute-force */
    SWD.reset()

    var results = inboundVariablesCondArr.reduce(factory.one) { acc, v in
      AlpineDD(
        pointer: factory
          .fusion(
            acc.pointer,
            prod(
              keys: v.1,
              values: marking[v.0]!
            ).pointer
          ),
        factory: factory
      )
    }

    /* // Sort the places so we always bind their variables in the same order. */
    /* let variables = inboundVariables() */
    /* let sortedPlaces = variables.keys.sorted() */
    /* // Compute all permutations of place tokens. */
    /* let choices = sortedPlaces.map { permutations(of: markingValue[$0]!) } */
    /*  */
    /* // Iterate through the cartesian product of all permutations ... */
    /* var results: [[String: Value]] = [] */
    /* outer: for choice in Product(choices) { */
    /*   var binding: [String: Value] = [:] */
    /*  */
    /*   // Iterate through each arrangement of each place. */
    /*   for (place, availableTokens) in zip(sortedPlaces, choice) { */
    /*     // Make sure there's enough variables to bind. */
    /*     guard variables[place]!.count <= availableTokens.count else { continue outer } */
    /*  */
    /*     // Try to find a binding for each variable on the precondition label. */
    /*     var remainingTokens = availableTokens */
    /*     for variable in variables[place]! { */
    /*       if let value = binding[variable] { */
    /*         // If the variable was already bound, try to match it with another token. */
    /*         if let index = remainingTokens.index(of: value) { */
    /*           remainingTokens.remove(at: index) */
    /*         } else { */
    /*           continue outer */
    /*         } */
    /*       } else { */
    /*         // If the variable wasn't bound yet, simply use the current token. */
    /*         binding[variable] = remainingTokens.remove(at: 0) */
    /*       } */
    /*     } */
    /*   } */
    /*  */
    /*   // Add the binding to the return list, unless we already found the same in a previous */
    /*   // iteration. */
    /*   if !results.contains(where: { $0 == binding }) { */
    /*     results.append(binding) */
    /*   } */
    /* } */
    print(
      SWD.elapsed.ns / 1000,
      results.count,
      factory.createdCount,
      separator: ",",
      terminator: ","
    )
    /* print( */
    /*   SWD.elapsed.ns / 1000, */
    /*   results.count, */
    /*   separator: ",", */
    /*   terminator: "," */
    /* ) */

    SWD.reset()

    /* mftimeD = SWD.elapsed */
    /* print("base", mftimeD.humanFormat) */
    /* print(results.count) */
    /* print() */

    /* func permutationsWithoutRepetitionFrom(_ elements: [Value], taking: Int) -> [[Value]] { */
    /*   guard elements.count >= taking else { return [] } */
    /*   guard elements.count >= taking && taking > 0 else { return [[]] } */
    /*  */
    /*   if taking == 1 { */
    /*     return elements.map { [$0] } */
    /*   } */
    /*  */
    /*   var permutations = [[Value]]() */
    /*   for (index, element) in elements.enumerated() { */
    /*     var reducedElements = elements */
    /*     reducedElements.remove(at:index) */
    /*     let owo = permutationsWithoutRepetitionFrom(reducedElements, taking:taking - 1) */
    /*     let uwu = owo.map { [element] + $0 } */
    /*     permutations += uwu */
    /*   } */
    /*  */
    /*   return permutations */
    /* } */

    func prod(keys: [VariableOrd], values: [ValueOrd]) -> AlpineDD {
      /* let domainsFlat = domains.flatMap { (keys, values) in keys.flatMap { ($0, values) } }.sorted { */
      /*   $0.0 < $1.0 */
      /* } */

      var dd = keys.map { key in
        factory.encode(
          family: values.map { val in
            [key: val]
          }
        )
      }

      if keys.count == 1 {
        return dd[0]
      }

      let valFlat = Dictionary(
        uniqueKeysWithValues: keys.flatMap { key in
          (key, values.hashValue)
        }
      )

      var perms = dd.last!
      for IND in (0 ... dd.count - 2).reversed() {
        func test(this _: AlpineDD.Inductive, pointer: AlpineDD.Pointer) -> AlpineDD
          .Inductive.Result {
          AlpineDD.Inductive.Result(
            take: Dictionary(
              uniqueKeysWithValues: pointer.pointee.take.map { val, _ in
                (
                  val,
                  morphisms
                    .constant(
                      perms
                        .removeSame(val, valFlat, valFlat[pointer.pointee.key]!)
                    ).apply(on:)
                )
              }
            ),
            skip: morphisms.constant(factory.zero).apply(on:)
          )
        }

        perms = morphisms.inductive(function: test).apply(on: dd[IND])
        /* var st = Set<Dictionary<String, Value>>() */
        /* dd[IND].pointer.pointee.take = Dictionary( */
        /*   uniqueKeysWithValues:dd[IND].pointer.pointee.take.map { (val, pointDel) in */
        /*     return ( */
        /*       val, dd[IND+1].pointer */
        /*       //.removeSame(val, valFlat, valFlat[dd[IND].pointer.pointee.key]!).pointer */
        /*     ) */
        /*   } */
        /* ) */
      }

      /* perms = perms.removeSame(domains.first!.value[0], valFlat, 5) */
      return perms
    }

    /* let SSS = ["a", "b", "c", "p", "q"] */
    /* let ori: [Value] = Array(1...9) */
    /* let take = 100 */
    /* var SW = Stopwatch() */
    /* var mftime = SW.elapsed */
    /* SW.reset() */
    /* [> var res = prod(domains:LMAO) <] */
    /* [> print(self.preconditions.first!.label[0]) <] */
    /* [> var res = prod(domains:[SSS: ori, ["a", "x", "y"]: [4, 5, 112, 113]]) <] */
    /* var res = prod(keys:SSS, values:ori) */
    /* mftime = SW.elapsed */
    /* print("first", mftime.humanFormat) */
    /* print(res.count, factory.createdCount) */
    /* SW.reset() */
    /* var res2 = prod(keys:["d", "z", "m", "o", "i"], values:[10, 11, 12, 1222, 1333, 1555]) */
    /* mftime = SW.elapsed */
    /* print("second", mftime.humanFormat) */
    /* print(res2.count, factory.createdCount) */
    /* SW.reset() */
    /* var mdr = AlpineDD(pointer:factory.fusion(res.pointer, res2.pointer), factory:factory) */
    /* mftime = SW.elapsed */
    /* print("fusion", mftime.humanFormat) */
    /* print(mdr.count, factory.createdCount) */
    /* var res2 = prod(domains:[["c"]: [10, 11, 12]]) */

    /* return guardFilterAux(pointer, [:]) */

    /* SW.reset() */
    /* let mdrF = AlpineDD(pointer:guardFilter(mdr.pointer), factory:factory) */
    /* mftime = SW.elapsed */
    /* print(mdr.count) */
    /* print("filter", mftime.humanFormat) */
    /* print("filter", mftime.humanFormat) */
    /*  */
    /* print(mdrF.count) */
    /* print(mdr.count) */
    /* print(mdr) */
    /* print(mdr.pointer.pointee) */
    /* print(mdr) */

    /* results.forEach { */
    /*   if res.contains($0 as! Dictionary<String, Value>) == false { */
    /*     print("riiiiiiiiiiiiiiiiip") */
    /*   } */
    /* } */
    /* (res as! Dictionary<String, Value>).forEach { */
    /*   if results.contains($0) == false { */
    /*     print("ruuuuuuuuuuuuup") */
    /*   } */
    /* } */

    /* let p = permutationsWithoutRepetitionFrom(ori, taking:4) */
    /* let pd = p.map { Dictionary(uniqueKeysWithValues:zip(SSS, $0)) } */
    /* let owo = factory.encode(family:pd) */
    /* for x in pd { */
    /*   if res.contains(x) == false { */
    /*     print("ripppppppppppppppppppppppppppppppppppppppppppppppppppppppppppp", x) */
    /*   } */
    /* } */
    /* print(owo.count) */
    /* print("le count", st.count) */
    /* print(owo.count, res.count) */
    /* for x in res { */
    /*   print(x) */
    /* } */

    /* print(res.contains(pd.map { (key, val) in (key, val) })) */
    /* print(res.contains()) */

    /* print(res) */
    /* print(res) */
    /* SW.reset() */
    /* print(res.count, factory.createdCount) */
    /* [> print(res2.count, factory.createdCount) <] */
    /* SW.reset() */

    // Filter out the bindings for which the transition's guards don't hold.
    SWD.reset()

    for (e1, e2, vars) in conditionsOrdered {
      results = results.guardFilter(
        e1: e1,
        e2: e2,
        vars: vars,
        interpreter: interpreter
      )
    }

    /* for (e1, e2, _) in conditions { */
    /*   results = results.filter { binding in */
    /*     eval(e1, binding) == eval(e2, binding) */
    /*   } */
    /* } */

    print(SWD.elapsed.ns / 1000, results.count, factory.createdCount, separator: ",")
    /* print( */
    /*   SWD.elapsed.ns / 1000, */
    /*   results.count, */
    /*   separator: "," */
    /* ) */

    /*  */
    /* mftimeD = SWD.elapsed */
    /* print("base", mftimeD.humanFormat) */
    /* print(results.count) */

    /* exit(0) */
    return results
  }

  /// Returns the marking obtained after firing the transition from a given marking and binding.
  ///
  /// - Note: If the transition isn't fireable with the provided marking and binding, the method
  ///   will return a nil value.
  public func fire(
    from marking: PredicateNet.MarkingType,
    with binding: Binding
  ) -> (PredicateNet.MarkingType, [String: Value])? {
    // Check whether the provided binding is valid.
    let variables = inboundVariablesCond
    var result = marking

    // Apply the preconditions.
    for arc in preconditions {
      for variable in variables[arc.place]! {
        // Note that we can assume this search to be successful, because we know the
        // transition is fireable.
        let index = result[arc.place]!.index(of: binding[variable]!)!
        result[arc.place]!.remove(at: index)
      }
    }

    var new: [String: Value] = [:]
    // Apply the postconditions.
    for arc in postconditions {
      for item in arc.label {
        let m = interpreter.eval(predicate: item, binding: Dictionary(
          uniqueKeysWithValues: binding.map {
            variable, val in (variable.value, val.value)
          }
        ))
      }
    }

    return (result, new)
  }

  // Internal

  unowned let factory: MFDDFactory<VariableOrd, ValueOrd>

  unowned var morphisms: MFDDMorphismFactory<VariableOrd, ValueOrd>

  // Private

  /// Identify the variables that should be bound in inbound each place.
  private func inboundVariables() -> [PredicateNet.PlaceType: [Variable]] {
    var variables: [PredicateNet.PlaceType: [Variable]] = [:]
    for arc in preconditions {
      variables[arc.place] = []
      for variable in arc.label {
        switch variable {
        case .str(let v):
          variables[arc.place]!.append(v)
        default:
          // This code should be unreachable, as we checked that preconditions are labeled with
          // variables only in the initializer.
          assertionFailure()
        }
      }
    }

    return variables
  }
}

extension PredicateTransition: Hashable {
  // Implementation note:
  // Because Swift functions are neither hashable nor equatable, we have no choice but to use
  // reference equality to make `PredicateTransition` conforms to `Hashable`. That's why we
  // declared it as a `class` rather than a `struct`.

  public var hashValue: Int {
    preconditions.hashValue ^ postconditions.hashValue
  }

  public static func == (lhs: PredicateTransition, rhs: PredicateTransition) -> Bool {
    lhs === rhs
  }
}

/// Structure for arcs of predicate nets.
public class ConditionTerm {
  // Lifecycle

  public init(e1: PredicateLabel, e2: PredicateLabel) {
    self.e1 = e1
    self.e2 = e2
  }

  // Internal

  let e1: PredicateLabel
  let e2: PredicateLabel

  /* public static func == (lhs: PredicateArc, rhs: PredicateArc) -> Bool { */
  /*   return lhs === rhs */
  /* } */
}

/// Structure for arcs of predicate nets.
public class PredicateArc: Hashable {
  // Lifecycle

  public init(place: PredicateNet.PlaceType, label: [PredicateLabel]) {
    self.place = place
    self.label = label
  }

  // Public

  public let place: PredicateNet.PlaceType
  public let label: [PredicateLabel]

  public var hashValue: Int {
    place.hashValue
  }

  public static func == (lhs: PredicateArc, rhs: PredicateArc) -> Bool {
    lhs === rhs
  }
}

public enum PredicateLabel {
  case str(Variable)
  case value(Value)
}
