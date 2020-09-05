import PetriKit
import SwiftProductGenerator
import Interpreter
import Foundation
import DDKit

/* extension String: Comparable { */
/*   static func< (lhs: String, rhs: String) -> Bool { */
/*     return lhs < rhs */
/*   } */
/* } */

public func setSeed(seed: UInt = 5323) {
  PetriKit.Random.seed = UInt(time(nil));
}

public struct PredicateNet<T: Equatable> {

  public typealias PlaceType = String
  public typealias MarkingType = [PlaceType: [T]]

  public init(
    places: Set<PlaceType>,
    transitions: Set<PredicateTransition<T>>,
    initialMarking: MarkingType? = nil,
    seed: UInt = 5323,
    interpreter: Interpreter
  ) {
    setSeed(seed:seed)

    self.places = places
    self.transitions = transitions
    self.initialMarking = initialMarking
    self.interpreter = interpreter
  }

  /// Returns a marking reachable after up to `steps` transition firings.
  ///
  /// At each step, one fireable transition is chosen at random and fired to produce a new
  /// marking. If the Predicate Net reaches a deadlock, the remaining steps are ignored and the
  /// state that was reached is returned.
  public func simulate(steps: Int, from marking: MarkingType) -> (
    MarkingType,
    [PredicateTransition<T>.Binding],
    [[String: Value]]
  ) {
    var m = marking
    var bindingChosen: [PredicateTransition<T>.Binding] = []
    var newTokens: [[String: Value]] = []

    let start = DispatchTime.now()

    // For as many steps are we were instructed to simulate ...
    for _ in 0 ..< steps {
      // Generate for each transition the set of fireable bindings.
      var fireable: [PredicateTransition<T>: [PredicateTransition<T>.Binding]] = [:]
      for transition in self.transitions {
        // Notice how we ignore non-fireable transitions.
        let bindings = transition.fireableBingings(from:m, interpreter:interpreter)
        if bindings.count > 0 {
          fireable[transition] = bindings
        }
      }

      // If we reached a deadlock, ignore the remaining transition and return.
      guard !fireable.isEmpty else { return (m, bindingChosen, newTokens) }

      // Choose one transition at random and fire it to produce the next marking.
      let (t, bindings) = PetriKit.Random.choose(from:fireable)
      let binding = PetriKit.Random.choose(from:bindings)
      bindingChosen.append(binding)
      var new: [String: Value] = [:]
      (m, new) = t.fire(from:m, with:binding, interpreter:interpreter)!
      newTokens.append(new)
    }

    /* let end = DispatchTime.now()   // <<<<<<<<<<   end time */
    /* let timeInterval = Double(end.uptimeNanoseconds - start.uptimeNanoseconds)  / 1_000_000_000 // Technically could overflow for long running tests */
    /* print("Time to evaluate problem: \(timeInterval) seconds") */

    return (m, bindingChosen, newTokens)
  }

  public let interpreter: Interpreter

  /// The set of places of the Predicate Net.
  public let places: Set<PlaceType>

  /// The set of transitions of the Predicate Net.
  public let transitions: Set<PredicateTransition<T>>

  /// The (optional) initial marking of the Predicate Net.
  public let initialMarking: MarkingType?

}

/// Type of variables on arc labels.
public typealias Variable = String

/// Structure for transitions of predicate nets.
public class PredicateTransition<T: Equatable> {

  /// Type for transition bindings.
  public typealias Binding = [Variable: T]

  public init(
    preconditions: Set<PredicateArc<T>> = [],
    postconditions: Set<PredicateArc<T>> = [],
    conditions: [(CondTerm, CondTerm)] = []
  ) {

    var inboundPlaces: Set<PredicateNet<T>.PlaceType> = []
    var inboundVariables: Set<Variable> = []

    self.varInfos = [:]

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
        case .variable(let v):
          inboundVariables.insert(v)
        case .function(_):
          preconditionFailure("Preconditions should be labeled with variables only.")
        }
      }
    }

    // Make sure postconditions aren't labeled with free variables.
    for arc in postconditions {
      for item in arc.label {
        switch item {
        case .variable(let v):
          /* varInfos.merge(arc.sexpr!.infos) { (new, old) in (new > old) ? new : old } */
          break
          /* guard inboundVariables.contains(v) else { */
          /*   preconditionFailure("Postconditions shouldn't be labeled with free variables.") */
          /* } */
        case .function(_):
          // Note that we can't make sure the functions don't use free variable, and
          // that transition firing may fail at runtime if they do.
          break
        }
      }
    }

    self.preconditions = preconditions
    self.postconditions = postconditions
    self.conditions = conditions
  }

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
  ///       [[1, 2], [1, 3]],
  ///       [[2, 1], [1, 3]],
  ///       ...
  ///
  ///   Notice how each element corresponds to one possible arrangement of the tokens of each
  ///   place in order. Now all we have to do is to iterate over the variables we have to bind,
  ///   for each place in each arrangement. If the variable is already isn't bound yet, we pick
  ///   the first value we can, if it is we check if we can match that value. For instance,
  ///   let's say we check the first arrangement. We'll bind `x` to `1` and `y` to `2` for `p0`
  ///   before moving to `p1`. Then, since `x` is already bound to `1`, we'll have no choice but
  ///   to pick `1` once again. Now if we check the second arrangement, we'll bind `x` to `2`
  ///   and `y` to `1` before moving to `p1`. But as we won't be able to match `2` in the tokens
  ///   of `p1`, we'll reject the binding and move to the next arrangement.
  public func fireableBingings(
    from marking: PredicateNet<T>.MarkingType,
    interpreter: Interpreter
  ) -> [Binding] {

    // Sort the places so we always bind their variables in the same order.
    let variables = self.inboundVariables()
    let sortedPlaces = variables.keys.sorted()

    var SWD = Stopwatch()
    SWD.reset()
    // Compute all permutations of place tokens.
    let choices = sortedPlaces.map { permutations(of:marking[$0]!) }

    // Iterate through the cartesian product of all permutations ...
    var results: [Binding] = []
    outer: for choice in Product(choices) {
      var binding = Binding()

      // Iterate through each arrangement of each place.
      for (place, availableTokens) in zip(sortedPlaces, choice) {
        // Make sure there's enough variables to bind.
        guard variables[place]!.count <= availableTokens.count else { continue outer }

        // Try to find a binding for each variable on the precondition label.
        var remainingTokens = availableTokens
        for variable in variables[place]! {
          if let value = binding[variable] {
            // If the variable was already bound, try to match it with another token.
            if let index = remainingTokens.index(of:value) {
              remainingTokens.remove(at:index)
            } else {
              continue outer
            }
          } else {
            // If the variable wasn't bound yet, simply use the current token.
            binding[variable] = remainingTokens.remove(at:0)
          }
        }
      }

      // Add the binding to the return list, unless we already found the same in a previous
      // iteration.
      if !results.contains(where:{ $0 == binding }) {
        results.append(binding)
      }
    }

    var mftimeD = SWD.elapsed
    print("base", mftimeD.humanFormat)
    print(results.count)
    print()

    let toValue = { (e: CondTerm, binding: Binding) -> Value in
      switch e {
      case .value(let v):
        return v
      case .str(let s):
        return try! interpreter.eval(string:s, replace:binding as! Dictionary<String, Value>)
      }
    }

    for _ in 1...1 {
      let factory = MFDDFactory<String, Int>(bucketCapacity:1024*128)
      var morphisms: MFDDMorphismFactory<String, Int>{
        factory.morphisms
      }

      typealias ADD = MFDD<String, Int>
      func permutationsWithoutRepetitionFrom<T>(_ elements: [T], taking: Int) -> [[T]] {
        guard elements.count >= taking else { return [] }
        guard elements.count >= taking && taking > 0 else { return [[]] }

        if taking == 1 {
          return elements.map { [$0] }
        }

        var permutations = [[T]]()
        for (index, element) in elements.enumerated() {
          var reducedElements = elements
          reducedElements.remove(at:index)
          let owo = permutationsWithoutRepetitionFrom(reducedElements, taking:taking - 1)
          let uwu = owo.map { [element] + $0 }
          permutations += uwu
        }

        return permutations
      }

      func prod(domains: [[String]: [Int]]) -> ADD {
        let domainsFlat = domains
          .flatMap { (keys, values) in keys.flatMap { ($0, values) } }
          .sorted { $0.0 < $1.0 }

        var dd = domainsFlat.map { key, values in
          factory.encode(
            family:values.map { val in
              [key: val]
            }
          )
        }

        let valFlat = Dictionary(
          uniqueKeysWithValues:domains.flatMap { keys, values in
            keys.flatMap { key in
              (key, values.hashValue)
            }
          }
        )

        var perms = dd.last!
        for IND in (0...dd.count-2).reversed() {
          func test(this: ADD.Inductive, pointer: ADD.Pointer) -> ADD.Inductive.Result {
            return ADD.Inductive.Result(
              take:Dictionary(
                uniqueKeysWithValues:pointer.pointee.take.map { (val, pointDel) in
                  return (
                    val, morphisms.constant(
                      perms.removeSame(val, valFlat, pointer.pointee.key)
                    ).apply(on:)
                  )
                }
              ), skip:morphisms.constant(factory.zero).apply(on:)
            )
          }

          perms = morphisms.inductive(function:test).apply(on:dd[IND])
          /* var st = Set<Dictionary<String, Int>>() */
          /* dd[IND].pointer.pointee.take = Dictionary( */
          /*   uniqueKeysWithValues:dd[IND].pointer.pointee.take.map { (val, pointDel) in */
          /*     return (val, dd[IND+1].removeSame(val, valFlat, dd[IND].pointer.pointee.key).pointer) */
          /*   } */
          /* ) */
        }

        factory.cleanCache()

        return perms

      }

      let SSS = ["a", "b", "c"]
      let ori: [Int] = Array(1...5)
      let take = 100
      var SW = Stopwatch()
      SW.reset()
      /* var res = prod(domains:[SSS: ori]) */
      var res = prod(domains:[SSS: ori, ["f", "g"]: [1, 7, 4]])
      /* var mftime = SW.elapsed */
      /* print("mfdd", mftime.humanFormat) */
      /* print(res.count, factory.createdCount) */

      /* let p = permutationsWithoutRepetitionFrom(ori, taking:3) */
      /* let pd = p.map { Dictionary(uniqueKeysWithValues:zip(SSS, $0)) } */
      /* let owo = factory.encode(family:pd) */
      /* for x in pd { */
      /*   if res.contains(x) == false { */
      /*     print("ripppppppppppppppppppppppppppppppppppppppppppppppppppppppppppp") */
      /*   } */
      /* } */
      /* var st = Set<Dictionary<String, Int>>() */
      /* for x in res { */
      /*   st.insert(x) */
      /*   if Set(x.values).count != 5 { */
      /*     print("riiiiiiiiiiiiip", x) */
      /*   } */
      /* } */
      /* print("check", st.count) */
      /* print("le count", st.count) */
      /* print(owo.count, res.count) */
      /* for x in res { */
      /*   print(x) */
      /* } */

      /* print(res.contains(pd.map { (key, val) in (key, val) })) */
      /* print(res.contains()) */

    }
    /* print(res) */
    /* print(res) */
    /* SW.reset() */
    /* print(res.count, factory.createdCount) */
    /* [> print(res2.count, factory.createdCount) <] */
    /* SW.reset() */

    // Filter out the bindings for which the transition's guards don't hold.
    for (e1, e2) in self.conditions {
      results = results.filter { binding in
        toValue(e1, binding) == toValue(e2, binding)
          /* == */
          /* switch e1 { */
          /* case .value(let v): */
          /*   return v */
          /* case .str(let s): */
          /*   return try! interpreter.eval(string:e2, replace:binding as! Dictionary<String, Value>) */
          /* } */

          /* (try! interpreter.eval( */
          /*     string:e1, replace:binding as! Dictionary<String, Value> */
          /*   )) == (try! interpreter.eval(string:e2, replace:binding as! Dictionary<String, Value>)) */
      }
    }

    return results
  }

  /// Returns the marking obtained after firing the transition from a given marking and binding.
  ///
  /// - Note: If the transition isn't fireable with the provided marking and binding, the method
  ///   will return a nil value.
  public func fire(
    from marking: PredicateNet<T>.MarkingType,
    with binding: Binding,
    interpreter: Interpreter
  ) -> (PredicateNet<T>.MarkingType, [String: Value])? {

    // Check whether the provided binding is valid.
    let variables = self.inboundVariables()
    for (place, requiredVariables) in variables {
      var remainingTokens = marking[place]!
      for variable in requiredVariables {
        guard let value = binding[variable] else { return nil }
        guard let index = remainingTokens.index(of:value) else { return nil }
        remainingTokens.remove(at:index)
      }
    }

    // TODO: ???
    /* // Check whether the transition's guard hold for provided binding. */
    /* for condition in self.conditions { */
    /*   guard condition(binding) else { return nil } */
    /* } */

    var result = marking

    // Apply the preconditions.
    for arc in self.preconditions {
      for variable in variables[arc.place]! {
        // Note that we can assume this search to be successful, because we know the
        // transition is fireable.
        let index = result[arc.place]!.index(of:binding[variable]!)!
        result[arc.place]!.remove(at:index)
      }
    }

    var new: [String: Value] = [:]
    // Apply the postconditions.
    for arc in self.postconditions {
      for item in arc.label {
        switch item {
        case .variable(let v):
          /* print("-------------------------") */
          /* print("eval", v, "with replace") */
          /* (binding as! Dictionary<String, Value>).forEach { print($0.key, $0.value) } */
          let m = try! interpreter.eval(string:v, replace:binding as! Dictionary<String, Value>)
          result[arc.place]!.append(m as! T)
          new[v] = m
          break
        case .function(let f):
          /* result[arc.place]!.append(f(binding)) */
          break
        }
      }
    }

    return (result, new)
  }

  public var varInfos: [String: Int]

  /// The preconditions of the transition.
  public let preconditions: Set<PredicateArc<T>>

  /// The postconditions of the transition.
  public let postconditions: Set<PredicateArc<T>>

  /// The conditions the transition checks on its binding before it can be fired.
  ///
  /// Note that we use an array rather than a set, because Swift functions are not hashable.
  public let conditions: [(CondTerm, CondTerm)]
  // MARK: Internals

  /// Identify the variables that should be bound in inbound each place.
  private func inboundVariables() -> [PredicateNet<T>.PlaceType: [Variable]] {
    var variables: [PredicateNet.PlaceType: [Variable]] = [:]
    for arc in self.preconditions {
      variables[arc.place] = []
      for variable in arc.label {
        switch variable {
        case .variable(let v):
          variables[arc.place]!.append(v)
        default:
          // This code should be unreachable, as we checked that preconditions are labeled with
          // variables only in the initializer.
          assertionFailure()
          break
        }
      }

      // Sort the variables so we always bind them in the same order.
      variables[arc.place]!.sort()
    }

    return variables
  }

}

extension PredicateTransition: Hashable {

  // Implementation note:
  // Because Swift functions are neither hashable nor equatable, we have no choice but to use
  // reference equality to make `PredicateTransition` conforms to `Hashable`. That's why we
  // declared it as a `class` rather than a `struct`.

  public var hashValue: Int{
    return self.preconditions.hashValue ^ self.postconditions.hashValue
  }

  public static func == (lhs: PredicateTransition, rhs: PredicateTransition) -> Bool {
    return lhs === rhs
  }

}

/// Structure for arcs of predicate nets.
public class ConditionTerm {

  public init(e1: CondTerm, e2: CondTerm) {
    self.e1 = e1
    self.e2 = e2
  }

  let e1: CondTerm
  let e2: CondTerm

  /* public static func == (lhs: PredicateArc, rhs: PredicateArc) -> Bool { */
  /*   return lhs === rhs */
  /* } */
}

/// Structure for arcs of predicate nets.
public class PredicateArc<T: Equatable>: Hashable {

  public init(place: PredicateNet<T>.PlaceType, label: [PredicateLabel<T>], sexpr: SExpression?) {
    self.place = place
    self.label = label
    self.sexpr = sexpr
  }

  public let sexpr: SExpression?
  public let place: PredicateNet<T>.PlaceType
  public let label: [PredicateLabel<T>]

  public var hashValue: Int{
    return self.place.hashValue
  }

  public static func == (lhs: PredicateArc, rhs: PredicateArc) -> Bool {
    return lhs === rhs
  }
}

public  enum PredicateLabel <T: Equatable> {

  case variable(Variable)
  case function((PredicateTransition<T>.Binding) -> T)

}
