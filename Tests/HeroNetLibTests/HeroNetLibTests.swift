import AST
import DDKit
import HeroNetLib
import Interpreter
import Parser
import Sema
import XCTest

import Foundation

final class HeroNetLibTests: XCTestCase {
  static var allTests = [("testExample", testExample)]

  func testExample() {
    do {
      var module: String = """
      func lt (_ a: Int, _ b: Int) -> Bool ::
        a < b

      func gt (_ a: Int, _ b: Int) -> Bool ::
        a > b
      """

      /* var interpreter = Interpreter(debug) */
      /* try! interpreter.loadModule(fromString: module) */
      /*  */
      /* let n = try! interpreter.eval(string: "@x", replace: ["@x": m]) */
      /* print(m) */
      /* print(n) */
      /* print(n == m) */
      /* let n = try! interpreter.eval(string:"operationCurry(@x, op: add)", replace:["@x": m]) */
      /* let q = try! interpreter.eval(string:"operationCurry(2, op: mul)") */
      /* let s = try! interpreter.eval(string:"@g(@f(@f(@g(7) - @f(10))))", replace:["@f": n, "@g": q]) */

      let JSON = """
      {
        "transition": [
          {
            "pre": {
              "p1": [ "a", "b", "c", "d" ],
            },
            "post": {
              "p2": [  ],
              "p3": [ ],
              "p4": [  ]
            },
            "condition": [
              [
                "a % 6",
                "b"
              ],
              [
                "lt(b, 4)",
                "true"
              ],
              [
                "gt(c, b * 2)",
                "true" 
              ],
              [
                "lt(d + b, a * 2)",
                "true"
              ],
              [
                "lt(a, 10)",
                "true"
              ],
              [
                "lt(b, 10)",
                "true"
              ],
              [
                "lt(c, 10)",
                "true"
              ],
              [
                "lt(d, 10)",
                "true"
              ]
            ]
          },
        ],

        "marking": {
          "p1": [],
          "p2": [],
          "p3": [],
          "p4": []
        }
      }
      """
      /* let JSON = """ */
      /* { */
      /*   "transition": [ */
      /*     { */
      /*       "pre": { */
      /*         "p1": [ "x", "y" ], */
      /*         "p2": [ "f"  ] */
      /*       }, */
      /*       "post": { */
      /*         "p1": [ "g(z, z)" ], */
      /*         "p2": [ "f" ], */
      /*         "p3": [ "f(x,y) + z" ], */
      /*         "p4": [ "curry(x, op: f)" ] */
      /*       }, */
      /*       "condition": [ */
      /*         { */
      /*           "x % 2", */
      /*           "1" */
      /*         } */
      /*       ] */
      /*     }, */
      /*   ], */
      /*  */
      /*   "marking": { */
      /*     "p1": [ ], */
      /*     "p2": [ "add", "sub", "mul", "div" ], */
      /*     "p3": [], */
      /*     "p4": [] */
      /*   } */
      /* } */
      /* """ */
      /* { */
      /*   "pre": { */
      /*     "p3": [ "a", "b" ], */
      /*     "p4": [ "g" ] */
      /*   }, */
      /*   "post": { */
      /*     "p1": [ "g(a)" ], */
      /*   }, */
      /*   "condition": [] */
      /* } */
      /* "p1": [ "0", "1", "2", "3", "4", "5", "6" ], */

      for i in 15 ... 16 {
        var an = AlpineNet(module: module, json: JSON)
        an.initialMarking["p1"] = [Int](1 ... i)
          .map { ValueOrd(try! an.interpreter.eval(string: String($0))) }
        /* an.initialMarking["p1"]!.forEach { print($0.id, $0.value) } */
        an.createNet()
      }

      /* try! String(contentsOfFile:path!, encoding:.utf8) : source! */

      /* [> let source = ["(f x (f (f (h x) z) (h y)))", "(g f)", "(g)"] <] */
      /* [> let binding = ["f": "add", "g": "minus", "h": "addOne", "x": "3", "y": "5", "z": "8"] <] */
      /* [> let lispCompiler = SExpression(input:source) <] */
      /* [>  <] */
      /* [> print("Input:", source) <] */
      /* [> print("\nBinding:") <] */
      /* [> binding.forEach { print("- \($0.key) |-> \($0.value)") } <] */
      /* [> print("\nOutput:", lispCompiler.compiler(replace:binding)) <] */
      /* [> print("\nInfos:") <] */
      /* [> lispCompiler.infos.forEach { print("- \($0.key) takes \($0.value) arg(s)") } <] */
      /*  */
      /* var an = AlpineNet(source:"bool.alpine") */
      /*  */
      /* let pre = ["p1": ["x"], "p2": ["f"]] */
      /* let post = ["p3": ["(f x)"], "p2": ["(f)"]] */
      /* an.addTransition(pre:pre, post:post) */
      /*  */
      /* let pre2 = ["p3": ["x", "y"], "p2": ["f"]] */
      /* let post2 = ["p4": ["(f x y)"], "p2": ["(f)"]] */
      /* an.addTransition(pre:pre2, post:post2) */
      /* an.createNet(initialMarking:[ */
      /*   "p1": ["#True", "#False", "#False", "#False", "#False", "#False", "#False"], */
      /*   "p2": ["myNot", "myAnd"], */
      /*   "p3": [], */
      /*   "p4": [] */
      /* ]) */
    }
  }
}
