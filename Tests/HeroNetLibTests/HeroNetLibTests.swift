import XCTest
@testable import HeroNetLib

final class HeroNetLibTests: XCTestCase {
    func testExample() {
      do {
        enum C: CustomStringConvertible {

          case b, v, o

          var description: String {
            switch self {
            case .b: return "b"
            case .v: return "v"
            case .o: return "o"
            }
          }
        }

        func g(binding: PredicateTransition<C>.Binding) -> C {
          switch binding["x"]! {
          case .b: return .v
          case .v: return .b
          case .o: return .o
          }
        }

        let t1 = PredicateTransition<C>(
          preconditions: [
            PredicateArc(place: "p1", label: [.variable("x")]),
          ],
          postconditions: [
            PredicateArc(place: "p2", label: [.function(g)]),
          ])

        let net = PredicateNet<C>(places: ["p1", "p2"], transitions: [t1])
        let m0: PredicateNet<C>.MarkingType = ["p1": [.b, .b, .v, .v, .b, .o], "p2": []]

        print("\n\n\n")

        print(m0)
        setSeed(seed: 5)
        let m1 = net.simulate(steps: 1, from: m0)
        print(m1)
        let m2 = net.simulate(steps: 1, from: m1)
        print(m2)
        let m3 = net.simulate(steps: 1, from: m2)
        print(m3)
        print()

        setSeed(seed: 5)
        for m in net.simulation(from: m0).prefix(4) {
          print(m)
        }
        
        print("\n\n\n")
      }
    }

    static var allTests = [
        ("testExample", testExample),
    ]
}
