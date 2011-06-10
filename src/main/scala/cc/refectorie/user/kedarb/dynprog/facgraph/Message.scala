package cc.refectorie.user.kedarb.dynprog.facgraph

import cc.refectorie.user.kedarb.dynprog.utils.Utils._
import cc.refectorie.user.kedarb.tools.utils.NumUtils

/**
 * @author kedarb
 * @since 2/7/11
 */

class Message(val domainSize: Int) {
  private val oldMsg = new Array[Double](domainSize)

  // initialize to uniform message
  private val msg = mapIndex(domainSize, {
    i => math.log(1.0 / domainSize)
  })

  private var residual = Double.PositiveInfinity

  private def update(a: Array[Double]) = {
    require(a.size == domainSize, "arraySize[" + a.size + "] != domainSize[" + domainSize + "]")
    Array.copy(msg, 0, oldMsg, 0, msg.size)
    Array.copy(a, 0, msg, 0, domainSize)
  }

  private def normalize: Unit = {
    var norm = Double.NegativeInfinity
    forIndex(domainSize, {
      i => norm = NumUtils.logAdd(norm, msg(i))
    })
    forIndex(domainSize, {
      i => msg(i) -= norm
    })
  }

  def apply(i: Int) = msg(i)

  def updateAndNormalize(a: Array[Double]) = {
    update(a)
    normalize
    // update residual
    residual = 0.0
    forIndex(domainSize, {
      i => residual += math.abs(math.exp(msg(i)) - math.exp(oldMsg(i)))
    })
  }

  def getResidual(): Double = residual
}

