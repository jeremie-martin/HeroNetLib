import DDKit
import Foundation

extension MFDD {
  public func removeSame(n: Int, k: Int) -> Self {
    Self(
      pointer: factory.removeSame(pointer, n, k),
      factory: factory
    )
  }
}

func quickBinomialCoefficient(_ n: Int, choose k: Int) -> Int {
  var result = 1
  for i in 0 ..< k {
    result *= (n - i)
    result /= (i + 1)
  }
  return result
}

func nb(_ n: Int, choose k: Int) -> Int {
  if k == 1 {
    return n
  }

  return quickBinomialCoefficient(n, choose: k) + nb(n, choose: k - 1)
}

extension MFDDFactory {
  public func removeSame(_ pointer: MFDD<Key, Value>.Pointer, _ n: Int, _ k: Int)
    -> MFDD<Key, Value>.Pointer {
    var cache = [Set<Value>: MFDD<Key, Value>.Pointer](
      minimumCapacity: nb(n, choose: k - 1)
    )
    var valCache = Set<Value>(minimumCapacity: k)

    if isTerminal(pointer) {
      return pointer
    }

    func removeSameAux(
      _ pointer: MFDD<Key, Value>.Pointer,
      _ toAdd: Value
    ) -> MFDD<Key, Value>.Pointer {
      if isTerminal(pointer) {
        return pointer
      }

      valCache.update(with: toAdd)
      if let res = cache[valCache] {
        valCache.remove(toAdd)
        return res
      }

      let results = node(
        key: pointer.pointee.key,
        take: Dictionary(
          uniqueKeysWithValues: pointer.pointee.take
            .compactMap { key, ptr in
              if valCache.contains(key) { return nil }
              else {
                return (key, removeSameAux(ptr, key))
              }
            }
        ),
        skip: zero.pointer
      )

      cache[valCache] = results

      valCache.remove(toAdd)
      return results
    }

    let res = node(
      key: pointer.pointee.key,
      take: Dictionary(
        uniqueKeysWithValues: pointer.pointee.take.map { key, ptr in
          (key, removeSameAux(ptr, key))
        }
      ),
      skip: zero.pointer
    )

    return res
  }

  public func fusion(
    _ lhs: MFDD<Key, Value>.Pointer,
    _ rhs: MFDD<Key, Value>.Pointer
  ) -> MFDD<Key, Value>.Pointer {
    var cache: [[MFDD<Key, Value>.Pointer]: MFDD<Key, Value>.Pointer] = [:]

    if lhs == zero.pointer || rhs == zero.pointer {
      return zero.pointer
    }

    if lhs == one.pointer {
      return rhs
    } else if rhs == one.pointer {
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
      skip: zero.pointer
    )

    cache[cacheKey] = result
    return result
  }
}
