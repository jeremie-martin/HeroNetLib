import PetriKit
import SwiftProductGenerator
import Interpreter
import Foundation
import DDKit

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

    let toValue = { (e: CondTerm, binding: Binding) -> Value in
      switch e {
      case .value(let v):
        return v
      case .str(let s):
        return try! interpreter.eval(string:s, replace:binding as! Dictionary<String, Value>)
      }
    }

    /* let factory = MFDDFactory<String, Value>() */
    /* var morphisms: MFDDMorphismFactory<String, Value>{ */
    /*   factory.morphisms */
    /* } */

    /* def powerset(lst): */
    /*     """Pseudo-code for generating the power set of lst.""" */
    /*     if empty_list: */
    /*         return "List containing the empty list" */
    /*     else: */
    /*         recursive_result = powerset("Rest of lst") */
    /*         new_elements = "Add first of lst to every elem in recursive_result" */
    /*         return "Combine the elements in recursive_result and new_elements" */

    /* var a: MFDD<String, Value> */
    /* var b: MFDD<String, Value> */
    /* a = factory.one */
    /* b = factory.encode(family:(results as! [[String: Value]])) */
    /* print("createdCount", factory.createdCount) */
    /* let zero = try! interpreter.eval(string:"1") */
    /* let morphism = morphisms.intersection(morphisms.identity, morphisms.constant(b)) */
    /* [> print(a) <] */
    /* [> print(b) <] */
    /* [> print(morphism.apply(on:factory.encode(family:factory.one))) <] */
    /* [> print("createdCount", factory.createdCount) <] */
    /* [> print(a.intersection(b)) <] */
    /* print(b.count) */
    /* print(results.count) */
    /* let morph = factory.morphisms.insert(assignments:["issou": zero]) */
    /* var x = morph.apply(on:b) */
    /* var y: [[String: Value]] = x.map { $0 } */
    /* print("createdCount", factory.createdCount) */
    /* print() */

    /* func perm(_ list: [Int]) -> [[Int]] { */
    /*   if list.count == 1 { */
    /*     return [s] */
    /*   } */
    /*   var results: [[Int]] = [] */
    /*   for (i, v) in list.enumerated() { */
    /*     results.append( */
    /*   } */
    /*   return 1 */
    /* } */

    /* def perms(s):         */
    /* if(len(s)==1): return [s] */
    /* result=[] */
    /* for i,v in enumerate(s): */
    /*     result += [v+p for p in perms(s[:i]+s[i+1:])] */
    /* return result */

    let factory = MFDDFactory<String, Int>(bucketCapacity:1024*1024)
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
      /* var pdd = factory.encode(family:[[]]) */
      for (index, element) in elements.enumerated() {
        var reducedElements = elements
        reducedElements.remove(at:index)
        let owo = permutationsWithoutRepetitionFrom(reducedElements, taking:taking - 1)
        let uwu = owo.map { [element] + $0 }
        /* print(taking, owo, permutations) */
        /* print() */
        permutations += uwu
      }

      return permutations
    }

    /* func permDD(_ elements: [T], taking: Int) -> ADD { */
    /*   guard elements.count >= taking else { return factory.zero } */
    /*   guard elements.count >= taking && taking > 0 else { return factory.zero } */
    /*  */
    /*   if taking == 1 { */
    /*  */
    /*     return elements.map { [$0] } */
    /*   } */
    /*  */
    /*   var permutations = [[T]]() */
    /*   var pdd = factory.encode(family:[[]]) */
    /*   for (index, element) in elements.enumerated() { */
    /*     var reducedElements = elements */
    /*     reducedElements.remove(at:index) */
    /*     let owo = permutationsWithoutRepetitionFrom(reducedElements, taking:taking - 1) */
    /*     let uwu = owo.map { [element] + $0 } */
    /*     print(taking, owo, permutations) */
    /*     print() */
    /*     permutations += uwu */
    /*   } */
    /*  */
    /*   return permutations */
    /* } */
    func combine<T>(lists: [[T]], partial: [T] = []) -> [[T]] {
      // print("combine(lists: \(lists), partial: \(partial))")
      if lists.isEmpty {
        // recursive base case: lists is now empty, so partial
        // is complete, so return it in an enclosing array
        // print("... returning \([partial])")
        return [partial]
      } else {
        // make lists mutable so that we can remove the first sub-array
        var lists = lists

        // remove the first sub-array from lists which is now shorter
        let first = lists.removeFirst()

        // create an array to hold all of the combinations
        var result = [[T]]()

        // take each element from the first sub-array, append it to
        // the partial result, and call combine to continue the
        // process.  Take the results returned from combine and append
        // those to the result array.
        for n in first {
          result += combine(lists:lists, partial:partial + [n])
        }

        // Return the results
        // print("... returning \(result)")
        return result
      }
    }

    func prod(domains: [[String]: [Int]]) -> ADD {
      /* let a = domains.map { (keys, values) in values. } */
      var SW = Stopwatch()
      /* SW.reset() */
      var dd = domains.flatMap { keys, values in
        keys.map { key in
          factory.encode(
            family:values.map { val in
              [key: val]
            }
          )
        }
      }
      /* print("first", SW.elapsed.humanFormat) */
      /*  */
      /* SW.reset() */
      var perms = dd.last!
      for DD in dd[0...dd.count-2].reversed() {
        func test(this: ADD.Inductive, pointer: ADD.Pointer) -> ADD.Inductive.Result {
          let take = pointer.pointee.take.mapValues { _ in
            return morphisms.constant(perms).apply(on:)
          }
          /*   } */
          /* ) */
          let res = ADD.Inductive.Result(take:take, skip:morphisms.identity.apply(on:))
          return res
        }
        perms = morphisms.inductive(function:test).apply(on:DD)
        /* print(perms) */
        /* print(perms.count) */
      }
      /* print("second", SW.elapsed.humanFormat) */
      /* print("aaa") */
      /*  */
      /* SW.reset() */
      let SSSMAP = Dictionary(
        uniqueKeysWithValues:SSS.map { key in
          (key, SSS.filter { $0 > key })
        }
      )

      func del(this: ADD.Inductive, pointer: ADD.Pointer) -> ADD.Inductive.Result {
        /* let tmp = pointer.pointee.take.keys.map { val in */
        /*   fil.flatMap { key in */
        /*     (key, [val]) as! (String, [Int]) */
        /*   } */
        /* } */
        let t = Dictionary(
          uniqueKeysWithValues:pointer.pointee.take.map { (val, point) in
            (
              val, morphisms.composition(
                of:this, with:morphisms.filter(
                  excluding:SSSMAP[pointer.pointee.key]!.flatMap { key in
                    (key, [val, 554]) as! (String, [Int])
                  }
                )
              ).apply(on:)
            )
          }
        )
        /* let t = { val, _ in } */
        /* let tt = t.map { } */
        /* let t = tmp.flatMap { zzz in */
        /*   pointer.pointee.take.mapValues { _ in morphisms.remove(valuesForKeys:zzz).apply(on:) } */
        /* } */
        let iii = ADD.Inductive.Result(
          /* take:[1: morphisms.identity.apply(on:)], skip:morphisms.identity.apply(on:) */
          take:t, skip:morphisms.constant(factory.zero).apply(on:)
        )
        return iii
        /* let phi = morphisms.composition(of:this, with:) */
        /* pointer.pointee.take.mapValues { _ in */
        /*             morphisms.identity.apply(on:) */
        /* let res = ADD.Inductive.Result( */
        /*   [> take:t, skip:morphisms.identity.apply(on:) <] */
        /*   take:[1: morphisms.identity.apply(on:)], skip:morphisms.identity.apply(on:) */
        /* ) */
        /* return res */
      }
      var bbb = morphisms.inductive(function:del).apply(on:perms)
      /* bbb.pointer.pointee = morphisms */
      /*   .inductive(substitutingOneWith:factory.zero, function:del) */
      /*   .apply(on:bbb.pointer.pointee.take) */
      /* print("third", SW.elapsed.humanFormat) */
      /* print(bbb) */
      /* print(bbb.count) */
      return bbb

      func delsmall(this: ADD.Inductive, pointer: ADD.Pointer) -> ADD.Inductive.Result {
        /* if (pointer.pointee.take.count < 1) { */
        /* return ADD.Inductive.Result( */
        /*   take:pointer.pointee.take.mapValues { _ in */
        /*     morphisms.identity.apply(on:) */
        /*   }, skip:morphisms.constant(factory.zero).apply(on:) */
        /* ) */
        /* } */

        let t = pointer.pointee.take.mapValues { _ in
          morphisms.composition(of:this, with:morphisms.identity).apply(on:)
        }
        return ADD.Inductive.Result(take:t, skip:morphisms.constant(factory.zero).apply(on:))
      }
      /* factory.printM(bbb.pointer) */

      return bbb
      /* bbb = morphisms */
      /*   .fixedPoint(of:morphisms.inductive(substitutingOneWith:factory.zero, function:delsmall)) */
      /*   .apply(on:bbb) */
      /* factory.printM(bbb.pointer) */
      /* [> bbb = morphisms.inductive(function:del).apply(on:bbb) <] */
      /* [> let bbb = morphisms.remove(valuesForKeys:[("y", [2])]).apply(on:perms) <] */
      /* print("-------------") */
      /* print(bbb) */
      /* print(bbb.count) */
      /* print("-------------") */

      /* let same = domains.flatMap { keys, values in */
      /*   values.map { value in */
      /*     keys.flatMap { key in */
      /*       [key: value] */
      /*     } */
      /*   } */
      /* } */

      /* let s = domains.first!.value */
      /* var index: [Int] = [Int](repeating:0, count:s.count) */
      /* func permutate(depth: Int) { */
      /*   if depth == s.count { */
      /*     print("lol") */
      /*     for i in 0 ... s.count { */
      /*       print(i) */
      /*       print(index) */
      /*       [> print(s[index[i]], terminator:" ") <] */
      /*     } */
      /*     print() */
      /*     return */
      /*     [> [> for (std::size_t i = 0; i < s.size(); ++i) <] <] */
      /*     [> [> { <] <] */
      /*     [> [>     std::cout << s[index[i]]; <] <] */
      /*     [> [> } <] <] */
      /*     [> [> std::cout << "\n"; <] <] */
      /*     [> [> return; <] <] */
      /*   } */
      /*  */
      /*   for i in 0 ... s.count { */
      /*     index[depth] = i */
      /*     permutate(depth:depth+1) */
      /*   } */
      /* } */
      /* let pt = permutate(depth:0) */

      /* var test = domains.flatMap { keys, values in */
      /*   keys.map { key in */
      /*     factory.encode( */
      /*       family:values.map { val in */
      /*         [key: val] */
      /*       } */
      /*     ) */
      /*   } */
      /* } */
      /*  */
      /* let sameDD = factory.encode(family:same) */
      /* print("***") */
      /* print(sameDD) */
      /* print("***") */
      /* perms = morphisms */
      /*   .symmetricDifference(morphisms.identity, morphisms.constant(sameDD)) */
      /*   .apply(on:perms) */
      return bbb
    }

    let SSS = ["x", "y", "z"]
    let ori: [Int] = Array(1...15)
    let take = 100
    print("AAA")
    var SW = Stopwatch()
    SW.reset()
    let res = prod(domains:[["x", "y", "z"]: ori])
    print("mfdd", SW.elapsed.humanFormat)
    /* var ind = 0 */
    /* for r in res { */
    /*   if (r.count == 3) { */
    /*     ind += 1 */
    /*     print(r) */
    /*   } */
    /* } */
    /*  */
    /* print(ind) */
    /* let res = i.apply(on:fa) */
    print(res.count, factory.createdCount)
    SW.reset()
    let p = permutationsWithoutRepetitionFrom(ori, taking:3)
    let pd = p.map { Dictionary(uniqueKeysWithValues:zip(SSS, $0)) }
    print("zzz", SW.elapsed.humanFormat)
    SW.reset()
    print(p.count)
    /* let i = morphisms.union( */
    /*   morphisms.insert(assignments:[("x", 1), ("y", 2)]), morphisms.constant( */
    /*     factory.encode(family:fb) */
    /*   ) */
    /* ) */
    /* fa.pointer = factory.zero */

    /* let p = permutationsWithoutRepetitionFrom(ori, taking:take) */
    /* let p = combine(lists:[ori, ori, ori]) */
    /* print("BBB") */
    /* let pd = p.map { Dictionary(uniqueKeysWithValues:zip(SSS, $0)) } */
    /* print("CCC") */
    /* let owo = factory.encode(family:pd) */
    /* print("DDD") */
    /* print(owo.count) */
    /* print(factory.createdCount) */
    /* print(Float(factory.createdCount) / Float(owo.count)) */
    /* print(permutationsWithoutRepetitionFrom([1, 2, 3, 4], taking:take)) */

    /* var c = factory.encode(family:factory.one) */
    /* let morphism = morphisms. */
    /* print(c) */
    /* print(permutationsWithoutRepetitionFrom([1, 2, 3, 4], taking:3)) */

    /* let fa = [[1: "a", 2: "b"], [1: "a", 3: "c"], [4: "d", 3: "c"], [1: "b", 2: "c"]] */
    /* let fb = [[1: "d", 3: "e"]] */
    /* let fa = [[1, 2, 3, 4].map { ["a": $0, "b": $0] }][0] */
    /* let fa = [[1, 2, 3, 4].map { ["a": $0] } + [1, 2, 3, 4].map { ["b": $0] }][0] */
    /* let fa = [["x": 1, "y": 2], ["x": 1, "y": 3]] */
    /* let fb = [["f": 0]] */
    /* let i = morphisms.union( */
    /*   morphisms.insert(assignments:[("x", 1), ("y", 2)]), morphisms.constant( */
    /*     factory.encode(family:fb) */
    /*   ) */
    /* ) */
    /* let morphism = morphisms.insert(assignments:["f": 0]) */
    /* let fa = [1, 2, 3].map { ["a": $0] } */
    /*   + [1, 2, 3].map { ["b": $0] } */
    /*   [> let fb = [1, 2, 3, 4].map { ["a": $0] } <] */
    /*   [> let issou = factory.encode(family:[["x": 3]]) <] */
    /*  */
    /* let e = factory.encode(family:fa) */
    /* print(e) */
    /* factory.printM(e.pointer) */
    /* let res = i.apply(on:e) */
    /* factory.printM(res.pointer) */
    /* print(res) */

    /* let i = morphisms.inductive(function:{ [unowned self] this, pointer in */
    /*   return ( */
    /*     take:pointer.pointee.take.mapValues({ _ in */
    /*       morphisms.constant(factory.zero).apply(on:) */
    /*     }), skip:morphisms.identity.apply(on:) */
    /*   ) */
    /* }) */
    /* let morphism = morphisms.composition( */
    /*   of:morphisms.identity, with:morphisms.constant(factory.encode(family:fa)) */
    /* ) */
    /* let res = morphism.apply(on:factory.encode(family:fb)) */
    /* print(res) */
    /* print(c) */
    /* print(c.count) */
    /* print(type(of:c)) */

    exit(0)

    assertionFailure()
    /* var morphisms: MFDDMorphismFactory<Int, String> { factory.morphisms } */
    /* morphism.apply(on: factory.encode(family: [[1: "a", 2: "b"], [1: "a", 3: "c"]])), */
    /* factory.encode(family:results as! [[String: Value]]) */
    /* let morphism = morphisms.filter(excluding:[(key:3, values:["c"]), (key:4, values:["d"])]) */
    /* print(factory.encode(family:(results as! Dictionary<String, T>).map { $0.key, $0.value })) */
    /* print(factory.encode(family:[[1: "a", 2: "b"], [1: "a", 3: "c"]])) */
    /* print(morphism.apply(on:factory.encode(family:[[1: "a", 2: "b"], [1: "a", 3: "c"]]))) */

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
