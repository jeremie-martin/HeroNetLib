import DDKit
import Foundation

extension MFDD {
  public func removeSame(
    _ val: Value,
    _ hashTable: [Key: Int],
    _ hashPlace: Int
  ) -> Self {
    Self(
      pointer: factory.removeSame(pointer, val, hashTable, hashPlace),
      factory: factory
    )
  }
}

extension MFDDFactory {
  public struct TupleWrapper: Hashable {
    // Public

    public static func == (lhs: TupleWrapper, rhs: TupleWrapper) -> Bool {
      lhs.values == rhs.values
    }

    public func hash(into hasher: inout Hasher) {
      hasher.combine(values.0)
      hasher.combine(values.1)
    }

    // Internal

    let values: (Key, Set<Value>)
  }

  public func removeSame(
    _ pointer: MFDD<Key, Value>.Pointer,
    _ val: Value,
    _: [Key: Int],
    _: Int
  ) -> MFDD<Key, Value>.Pointer {
    var cache: [TupleWrapper: MFDD<Key, Value>.Pointer] = [:]

    if pointer == zeroPointer || pointer == onePointer {
      return pointer
    }

    func removeSameAux(
      _ pointer: MFDD<Key, Value>.Pointer,
      _ valCache: Set<Value>,
      _ toAdd: Value
    ) -> MFDD<Key, Value>.Pointer {
      if isTerminal(pointer) {
        return pointer
      }

      var newC = valCache
      newC.insert(toAdd)

      let cacheKey = TupleWrapper(values: (pointer.pointee.key, newC))
      if let res = cache[cacheKey] {
        return res
      }

      let results = node(
        key: pointer.pointee.key,
        take: Dictionary(
          uniqueKeysWithValues: pointer.pointee.take.lazy
            .filter { key, _ in key != val }
            .map { key, ptr in (key, removeSameAux(ptr, newC, key)) }
        ),
        skip: zeroPointer
      )

      cache[cacheKey] = results
      return results
    }

    return removeSameAux(pointer, [], val)
  }

  public func fusion(
    _ lhs: MFDD<Key, Value>.Pointer,
    _ rhs: MFDD<Key, Value>.Pointer
  ) -> MFDD<Key, Value>.Pointer {
    var cache: [[MFDD<Key, Value>.Pointer]: MFDD<Key, Value>.Pointer] = [:]

    if lhs == zeroPointer || rhs == zeroPointer {
      return zeroPointer
    }

    if lhs == onePointer {
      return rhs
    } else if rhs == onePointer {
      return lhs
    }

    if lhs.pointee.key > rhs.pointee.key {
      return fusion(rhs, lhs)
    }

    // Query the cache.
    let cacheKey = [lhs, rhs]
    if let pointer = cache[cacheKey] {
      return pointer
    }

    let result = node(
      key: lhs.pointee.key,
      take: lhs.pointee.take.mapValues { fusion($0, rhs) },
      skip: zeroPointer
    )

    cache[cacheKey] = result
    return result
  }
}
