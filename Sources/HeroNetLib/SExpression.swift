struct Token: Equatable {
  var kind: String
  var value: String

  static func == (lhs: Token, rhs: Token) -> Bool {
    return lhs.kind == rhs.kind
  }
}
struct NodeClone {
  var kind: String
  var value: String

  init(kind: String = "", value: String = "") {
    self.kind = kind
    self.value = value
  }
}
struct Node: Equatable {
  var kind: String
  var value: String
  var name: String
  var callee: NodeClone
  var expression: [Node]
  var body: [Node]
  var params: [Node]
  var arguments: [Node]
  var context: [Node]
  init(
    kind: String = "",
    value: String = "",
    name: String = "",
    callee: NodeClone = NodeClone(),
    expression: [Node] = [],
    body: [Node] = [],
    params: [Node] = [],
    arguments: [Node] = [],
    context: [Node] = []
  ) {
    self.kind = kind
    self.value = value
    self.name = name
    self.callee = callee
    self.expression = expression
    self.body = body
    self.params = params
    self.arguments = arguments
    self.context = context
  }
  static func == (lhs: Node, rhs: Node) -> Bool {
    return lhs.kind == rhs.kind
      && lhs.value == rhs.value
      && lhs.name == rhs.name
      && lhs.callee.kind == rhs.callee.kind
      && lhs.callee.value == rhs.callee.value
  }
}

typealias AST = Node
typealias visitor = [String: (Node, Node) -> ()]

public class SExpression {
  public init(input: [String]) {
    self._ast = AST()
    self.infos = [:]
    self._ast = parser(tokenizer(input.joined()))
  }

  var _ast: AST
  public var infos: [String: Int]

  func tokenizer(_ iinput: String) -> [Token] {
    let input: String = iinput
    var current: Int = 0
    var tokens: [Token] = [Token]()
    while current < input.count {
      var char: String = String(input[current])
      if char == "(" {
        tokens.append(Token(kind:"paren", value:"("))
        current+=1
        continue
      }
      if char == ")" {
        tokens.append(Token(kind:"paren", value:")"))
        current+=1
        continue
      }
      if char == " " {
        current+=1
        continue
      }
      if char.range(of:#"[0-9]"#, options:.regularExpression) != nil {
        var value: String = ""
        while char.range(of:#"[0-9]"#, options:.regularExpression) != nil {
          value+=char
          current+=1
          char = String(input[current])
        }
        tokens.append(Token(kind:"number", value:value))
        continue
      }
      if char.range(of:#"[a-zA-Z]"#, options:.regularExpression) != nil {
        var value: String = ""
        while char.range(of:#"[a-zA-Z]"#, options:.regularExpression) != nil {
          value+=char
          current+=1
          char = String(input[current])
        }
        infos[value] = 0
        tokens.append(Token(kind:"name", value:value))
        continue
      }
      if char == "\"" {
        var value: String = ""
        current+=1
        char = String(input[current])
        while char != "\"" {
          value+=char
          current+=1
          char = String(input[current])
        }
        current+=1
        char = String(input[current])
        tokens.append(Token(kind:"string", value:"\"\(value)\""))
        continue
      }
    }
    return tokens
  }
  var pc: Int = Int()
  var pt: [Token] = [Token]()
  func parser(_ tokens: [Token]) -> AST {
    pc = 0
    pt = tokens
    var ast: AST = AST(kind:"Program", body:[])
    while pc < pt.count {
      ast.body.append(walk())
    }
    return ast
  }
  func walk() -> Node {
    var token: Token = pt[pc]
    if token.kind == "number" {
      pc+=1
      return Node(kind:"NumberLiteral", value:token.value)
    }
    if token.kind == "string" {
      pc+=1
      return Node(kind:"StringLiteral", value:token.value)
    }
    if token.kind == "name" {
      pc+=1
      return Node(kind:"NameLiteral", value:token.value)
    }
    if token.kind == "paren" && token.value == "(" {
      pc+=1
      token = pt[pc]
      var n: Node = Node(kind:"CallExpression", name:token.value, params:[])
      pc+=1
      token = pt[pc]
      while token.kind != "paren" || (token.kind == "paren" && token.value != ")") {
        n.params.append(walk())
        /* if infos[n.name] != nil { */
        /* infos[n.name]! += 1 */
        /* } else { */
        /* infos[n.name] = 1 */
        /* } */
        token = pt[pc]
      }
      if infos[n.name]! < n.params.count {
        infos[n.name]! = n.params.count
      }
      pc+=1
      return n
    }
    fatalError(token.kind)
  }
  var gast = AST()

  public func compiler(replace: [String: String]) -> [String] {
    var out: [String] = []
    var cur = -1
    func owo(_ node: [Node], _ parent: inout Node) {
      var i = 0
      for var n in node {
        i += 1
        switch n.kind {
        case "CallExpression":
          if (parent.kind == "Program") {
            out.append("")
            cur += 1
          }
          if n.params.count == 0 {
            out[cur].append(replace[n.name]!)
            return
          }
          out[cur].append(replace[n.name]! + "(")
          owo(n.params, &n)
          if i == parent.params.count || parent.kind == "Program" {
            out[cur].append(")")
          } else {
            out[cur].append("), ")
          }
        case "NumberLiteral", "StringLiteral", "NameLiteral":
          if i == parent.params.count {
            out[cur].append(replace[n.value]!)
          } else {
            out[cur].append(replace[n.value]! + ", ")
          }
          /* print(n.value, "is value") */
        default:
          break
        }
      }
    }

    owo(_ast.body, &_ast)
    return out
  }
}
extension String {
  static func += (lhs: inout String, rhs: String) {
    lhs = lhs + rhs
  }
  subscript(i: Int) -> Character {
    return self[index(startIndex, offsetBy:i)]
  }
}
