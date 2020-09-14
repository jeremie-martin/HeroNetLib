import Interpreter

public struct PredicateNetSimulation: Sequence {
  // Lifecycle

  public init(net: PredicateNet, initialMarking: PredicateNet.MarkingType?) {
    self.net = net
    self.initialMarking = initialMarking
  }

  // Public

  public func makeIterator()
    -> AnyIterator<(
      PredicateNet.MarkingType,
      PredicateTransition.Binding?,
      [String: Value]
    )>
  {
    var m = initialMarking
    var b: [PredicateTransition.Binding] = []

    return AnyIterator {
      if let n = m {
        var newTokens: [[String: Value]] = []
        (m, b, newTokens) = self.net.simulate(steps: 1, from: n)
        let bindingRet = (b.count == 1) ? b[0] : nil
        let newRet = (newTokens.count == 1) ? newTokens[0] : [:]
        return (m!, bindingRet, newRet)
      } else {
        return nil
      }
    }
  }

  // Internal

  let net: PredicateNet
  let initialMarking: PredicateNet.MarkingType?
}

extension PredicateNet {
  public func simulation(from marking: MarkingType?) -> PredicateNetSimulation {
    PredicateNetSimulation(net: self, initialMarking: marking)
  }
}
