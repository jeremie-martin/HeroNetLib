import Interpreter

public protocol Ordered: Hashable & CustomStringConvertible {
  associatedtype T

  var id: Int { get }
  var value: T { get }
}

public struct VariableOrd: Ordered & Comparable {
  public typealias T = String

  public let value: String
  public let id: Int

  public var description: String { "\(value)" }

  public static func == (lhs: VariableOrd, rhs: VariableOrd) -> Bool {
    lhs.id == rhs.id
  }

  public static func < (lhs: VariableOrd, rhs: VariableOrd) -> Bool {
    lhs.id < rhs.id
  }

  public func hash(into hasher: inout Hasher) {
    hasher.combine(id)
  }
}

/* public struct VariableOrd: Ordered & Comparable { */
/*   public typealias T = String */
/*  */
/*   public let value: String */
/*   public let id: Int */
/*  */
/*   public var description: String { "\(value) (\(id))" } */
/*  */
/*   public static func == (lhs: VariableOrd, rhs: VariableOrd) -> Bool { */
/*     lhs.value == rhs.value */
/*   } */
/*  */
/*   public static func < (lhs: VariableOrd, rhs: VariableOrd) -> Bool { */
/*     lhs.value < rhs.value */
/*   } */
/*  */
/*   public func hash(into hasher: inout Hasher) { */
/*     hasher.combine(value) */
/*   } */
/* } */

public struct ValueOrd: Ordered {
  // Lifecycle

  public init(_ val: T) {
    value = val
    id = ValueOrd.nextID
    ValueOrd.nextID += 1
  }

  // Public

  public typealias T = Value

  public let id: Int
  public let value: T

  public var description: String { "\(value)" }

  public static func == (lhs: ValueOrd, rhs: ValueOrd) -> Bool {
    lhs.id == rhs.id
  }

  public func hash(into hasher: inout Hasher) {
    hasher.combine(id)
  }

  // Private

  private static var nextID: Int = 0
}
