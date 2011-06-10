package cc.refectorie.user.kedarb.dynprog.facgraph

import cc.refectorie.user.kedarb.dynprog.utils.Utils._

/**
 * @author kedarb
 * @since 2/7/11
 */

trait SettingIterator extends Iterator[Int] {
  def reset: Unit
}

class SettingJoinIterator(val vs: Array[SettingIterator]) extends Iterator[Array[Int]] {
  private val values = new Array[Int](vs.size)

  var hasNext = {
    var _hasNext = true
    forIndex(vs.size, {
      i: Int => vs(i).reset
      if (!vs(i).hasNext) _hasNext = false
      else values(i) = vs(i).next
    })
    _hasNext
  }

  def next: Array[Int] = {
    val result = new Array[Int](vs.size)
    Array.copy(values, 0, result, 0, vs.size)
    hasNext = nextValues(vs, 0)
    result
  }

  private def nextValues(_vs: Array[SettingIterator], i: Int): Boolean = {
    if (_vs.size == 0) false
    else if (_vs.head.hasNext) {
      values(i) = _vs.head.next
      true
    }
    else if (_vs.tail != Nil) {
      _vs.head.reset
      values(i) = _vs.head.next
      nextValues(_vs.tail, i + 1)
    }
    else false
  }
}