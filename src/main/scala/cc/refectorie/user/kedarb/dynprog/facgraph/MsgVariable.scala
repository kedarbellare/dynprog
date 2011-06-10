package cc.refectorie.user.kedarb.dynprog.facgraph

import collection.mutable.ArrayBuffer
import cc.refectorie.user.kedarb.dynprog.utils.Utils._
import cc.refectorie.user.kedarb.tools.utils.{NumUtils, ListUtils}

/**
 * @author kedarb
 * @since 2/7/11
 */

trait MsgNode {
  // updates all outgoing messages given current incoming messages
  def update(hardInfer: Boolean): Unit

  def neighbors: Seq[MsgNode]

  def maxResidual: Double
}

trait MsgVariable[Widget] extends MsgNode {
  def domainSize: Int

  def trueValue: Int

  def isEvidence: Boolean

  protected val factors = new ArrayBuffer[MsgFactor[Widget]]

  lazy protected val numFactors = factors.size

  lazy protected val msgs = mapIndex(numFactors, {
    i => new Message(domainSize)
  })

  protected def singleSetting(value: Int) = new SettingIterator {
    var hasNext = true

    def next(): Int = {
      hasNext = false
      value
    }

    def reset = hasNext = true
  }

  protected def allSettings = new SettingIterator {
    var i = -1
    val max = domainSize - 1

    def hasNext = i < max

    def next(): Int = {
      i += 1
      i
    }

    def reset = i = -1
  }

  private[facgraph] def addFactor(f: MsgFactor[Widget]) = factors += f

  def neighbors = factors.toSeq

  def maxResidual = ListUtils.max(msgs.map(_.getResidual))

  def belief: Array[Double]

  def settings = if (isEvidence) singleSetting(trueValue) else allSettings

  def setting(kth: Int) = singleSetting(kth)

  def msgFrom(f: MsgFactor[Widget], msg: Array[Double]) = msgs(factors.indexOf(f)).updateAndNormalize(msg)
}

class BPVariable[Widget](val domainSize: Int, val trueValue: Int, val isEvidence: Boolean) extends MsgVariable[Widget] {
  lazy protected val totalMsgArr = new Array[Double](domainSize)

  lazy protected val msgsArr = mapIndex(numFactors, {
    i => new Array[Double](domainSize)
  })

  def belief = {
    val probs = new Array[Double](domainSize)
    forIndex(numFactors, {
      f => forIndex(domainSize, {
        kth => probs(kth) += msgs(f)(kth)
      })
    })
    NumUtils.expNormalize(probs)
    probs
  }

  def update(hardInfer: Boolean): Unit = {
    forIndex(domainSize, {
      kth => totalMsgArr(kth) = 0
      forIndex(numFactors, {
        f => totalMsgArr(kth) += msgs(f)(kth)
      })
    })
    forIndex(numFactors, {
      f => forIndex(domainSize, {
        kth => msgsArr(f)(kth) = totalMsgArr(kth) - msgs(f)(kth)
      })
      factors(f).msgFrom(this, msgsArr(f))
    })
  }
}

