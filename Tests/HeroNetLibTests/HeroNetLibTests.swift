import XCTest
import HeroNetLib
import Interpreter
import Parser
import AST
import Sema
import DDKit

import Foundation

final class HeroNetLibTests: XCTestCase {
  func testExample() {
    do {
      var module: String = """
      func add(_ x: Int, _ y: Int) -> Int ::
        x + y

      func sub(_ x: Int, _ y: Int) -> Int ::
        x - y

      func mul(_ x: Int, _ y: Int) -> Int ::
        x * y

      func curry(_ x: Int, op: (Int, Int) -> Int) -> (Int) -> Int ::
        func partialApply(_ y: Int) -> Int ::
          op(x,y);
      """

      var interpreter = Interpreter(debug:false)
      try! interpreter.loadModule(fromString:module)

      let m = try! interpreter.eval(string:"add(1, 2)")
      /* let n = try! interpreter.eval(string:"operationCurry(@x, op: add)", replace:["@x": m]) */
      /* let q = try! interpreter.eval(string:"operationCurry(2, op: mul)") */
      /* let s = try! interpreter.eval(string:"@g(@f(@f(@g(7) - @f(10))))", replace:["@f": n, "@g": q]) */

      let JSON = """
      {
        "transition": [
          {
            "pre": {
              "p1": [ "a", "b", "c" ],
              "p2": [ "f", "g" ]
            },
            "post": {
              "p2": [ "f" ],
              "p3": [ "f(x,y)" ],
              "p4": [ "curry(x, op: f)" ]
            },
            "condition": [
              {
                "x % 2": false,
                "1": true
              },
              {
                "f(x,y) % 2": false,
                "0": true
              }
            ]
          },
        ],
        
        "marking": {
          "p1": [ "0", "1", "2", "3", "4", "5", "6" ],
          "p2": [ "add", "sub", "mul" ],
          "p3": [],
          "p4": []
        }
      }
      """
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

      var an = AlpineNet(module:module, json:JSON)

      an.createNet()

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

  static var allTests = [("testExample", testExample)]
}
