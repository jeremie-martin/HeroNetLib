import AST
import DDKit
import HeroNetLib
import Interpreter
import Parser
import Sema

let json = try String(contentsOfFile: "Example/5v_2f_2c.json", encoding: .utf8)

for i in 5 ... 30 {
  let an = AlpineNet(json: json)
  an.initialMarking["p1"] = [Int](1 ... i)
    .map { ValueOrd(try! an.interpreter.eval(string: String($0))) }
  /* an.initialMarking["p1"]!.forEach { print($0.id, $0.value) } */
  an.createNet()
}
