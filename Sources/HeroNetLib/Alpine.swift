import Interpreter
import Parser

public struct AlpineNet {

  public typealias Term = String

  public init() {
    /* let tokens = Array(try! Lexer(source:code)) */
    /* tokens */
    /*   .filter({ */
    /*     $0.kind != TokenKind.newline */
    /*   }) */
    /*   .map({ */
    /*     $0.kind */
    /*   }) */
    self.places = []
    self.transitions = []
  }

  /*
    Input : pre = ["p1": ["x", "y"], "p2": ["f"]]
    Output: [PredicateArc(place:"p1", label:[.variable("x"), .variable("y")],
             PredicateArc(place:"p2", label:[.variable("f")]]
  */
  public mutating func addTransition(pre: [Term: [Term]], post: [String: [String]], source: String) {
    let module = try! String(contentsOfFile:source, encoding:.utf8)

    self.places = Set(pre.keys)
      .union(Set(post.keys))
      .union(self.places)

    self.transitions.insert(PredicateTransition(
      preconditions:Set(
        pre.map { PredicateArc<Term>(place:$0.key, label:$0.value.map { .variable($0) }) }
      ), postconditions:Set(
        post.map { PredicateArc<Term>(place:$0.key, label:$0.value.map { .variable($0) }) }
      ), module:module
    ))
  }

  public func createNet(initialMarking: PredicateNet<Term>.MarkingType) {
    let net = PredicateNet<Term>(places:self.places, transitions:self.transitions)

    print("Start")
    for m in net.simulation(from:initialMarking).prefix(1) {
      print("fjdgjfdjkdhghk")/* print(m) */
    }
    print("End")
  }

  public var places: Set<String>
  public var transitions: Set<PredicateTransition<Term>>

  /* let net: PredicateNet<T>
   * let initialMarking: PredicateNet<T>.MarkingType? */
}
