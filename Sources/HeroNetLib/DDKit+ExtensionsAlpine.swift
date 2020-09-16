import Interpreter
typealias ValueAlpine = Value
import DDKit

public typealias AlpineDD = MFDD<VariableOrd, ValueOrd>
public typealias AlpineDDFactory = MFDDFactory<VariableOrd, ValueOrd>

public extension AlpineDD {
  func guardFilter(
    e1: PredicateLabel,
    e2: PredicateLabel,
    vars: [VariableOrd],
    interpreter: Interpreter
  ) -> Self {
    Self(
      pointer: factory.guardFilter(
        pointer,
        e1,
        e2,
        vars,
        interpreter: interpreter
      ),
      factory: factory
    )
  }
}

extension AlpineDDFactory {
  func guardFilter(
    _ pointer: AlpineDD.Pointer,
    _ e1: PredicateLabel,
    _ e2: PredicateLabel,
    _ vars: [VariableOrd],
    interpreter: Interpreter
  ) -> AlpineDD.Pointer {
    func guardFilterAux(
      _ pointer: AlpineDD.Pointer,
      _ binding: [String: ValueAlpine]
    ) -> MFDD<Key, Value>
      .Pointer {
      if isTerminal(pointer) || pointer.pointee.key > vars.last! {
        return pointer
      }

      if vars.contains(pointer.pointee.key) {
        if binding.count + 1 < vars.count {
          return node(
            key: pointer.pointee.key,
            take: Dictionary(
              uniqueKeysWithValues: pointer.pointee.take
                .map { val, ptr in
                  var bindingNew = binding
                  bindingNew[pointer.pointee.key.value] = val.value
                  return (val, guardFilterAux(ptr, bindingNew))
                }
            ),
            skip: zero.pointer
          )
        }
        // filtrer
        return node(
          key: pointer.pointee.key,
          take: Dictionary(
            uniqueKeysWithValues: pointer.pointee.take
              .filter { val, _ in
                var bindingNew = binding
                bindingNew[pointer.pointee.key.value] = val.value

                return interpreter.eval(predicate: e1, binding: bindingNew) ==
                  interpreter.eval(predicate: e2, binding: bindingNew)
              }
          ),
          skip: zero.pointer
        )
      }

      let result = node(
        key: pointer.pointee.key,
        take: pointer.pointee.take.mapValues {
          guardFilterAux($0, binding)
        },
        skip: zero.pointer
      )
      return result
    }
    return guardFilterAux(pointer, [:])
  }
}
