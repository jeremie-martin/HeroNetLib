import Interpreter

public struct PredicateNetSimulation<T: Equatable>: Sequence {
  public init(net: PredicateNet<T>, initialMarking: PredicateNet<T>.MarkingType?) {
    self.net = net
    self.initialMarking = initialMarking
  }

  public func makeIterator() -> AnyIterator<(
    PredicateNet<T>.MarkingType,
    PredicateTransition<T>.Binding?,
    [String: Value]
  )> {
    var m = self.initialMarking
    var b: [PredicateTransition<T>.Binding] = []

    return AnyIterator {
      if let n = m {
        var newTokens: [[String: Value]] = []
        (m, b, newTokens) = self.net.simulate(steps:1, from:n)
        let bindingRet = (b.count == 1) ? b[0] : nil
        let newRet = (newTokens.count == 1) ? newTokens[0] : [:]
        return (m!, bindingRet, newRet)
      } else {
        return nil
      }
    }
  }

  let net: PredicateNet<T>
  let initialMarking: PredicateNet<T>.MarkingType?
}

extension PredicateNet {

  public func simulation(from marking: MarkingType?) -> PredicateNetSimulation<T> {
    return PredicateNetSimulation(net:self, initialMarking:marking)
  }

}
