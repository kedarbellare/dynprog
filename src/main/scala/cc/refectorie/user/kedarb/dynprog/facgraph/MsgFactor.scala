package cc.refectorie.user.kedarb.dynprog.facgraph

import collection.mutable.{HashMap, ArrayBuffer}
import cc.refectorie.user.kedarb.dynprog.utils.Utils._
import cc.refectorie.user.kedarb.tools.utils.{ListUtils, NumUtils}

/**
 * @author kedarb
 * @since 2/7/11
 */

abstract class AFactorInfo[Widget] {
  lazy val weight = getWeight

  def getWeight: Double

  def choose(widget: Widget): Widget

  def setPosterior(prob: Double): Unit
}

trait MsgFactor[Widget] extends MsgNode {
  // variables connected to this factor (ordering is important)
  protected val variables = new ArrayBuffer[MsgVariable[Widget]]

  lazy protected val numVariables = variables.size

  def assignment2index(a: Array[Int]): Int = {
    var mult = 1
    var i = 0
    forIndex(numVariables, {
      j => i += mult * a(j)
      mult *= variables(j).domainSize
    })
    i
  }

  // user specifies the infos
  lazy protected val infos = {
    val _size = variables.map(_.domainSize).foldLeft(1)(_ * _)
    Array.ofDim[AFactorInfo[Widget]](_size)
  }

  lazy protected val msgs = mapIndex(numVariables, {
    i => new Message(variables(i).domainSize)
  })

  protected def settings(vs: Array[SettingIterator]): SettingJoinIterator = new SettingJoinIterator(vs)

  protected def settings: SettingJoinIterator = settings(variables.map(_.settings).toArray)

  protected def settings(v: MsgVariable[Widget], kth: Int): SettingJoinIterator = settings(variables.map {
    nv => if (nv == v) nv.setting(kth) else nv.settings
  }.toArray)

  protected def getWeight(assignment: Array[Int]) = {
    val i = assignment2index(assignment)
    if (infos(i) != null) infos(i).weight else Double.NegativeInfinity
  }

  // setup methods
  private[facgraph] def addVariable(v: MsgVariable[Widget]) = variables += v

  def neighbors = variables.toSeq

  def maxResidual = ListUtils.max(msgs.map(_.getResidual))

  def setInfo(assignment: Array[Int], info: AFactorInfo[Widget]) = infos(assignment2index(assignment)) = info

  def msgFrom(v: MsgVariable[Widget], msg: Array[Double]) = msgs(variables.indexOf(v)).updateAndNormalize(msg)
}

class BPFactor[Widget] extends MsgFactor[Widget] {
  lazy protected val msgToVariables = variables.map(v => new Array[Double](v.domainSize))

  def update(hardInfer: Boolean): Unit = {
    // reset msg to variables
    forIndex(numVariables, {
      i => forIndex(variables(i).domainSize, {
        kth => msgToVariables(i)(kth) = Double.NegativeInfinity
      })
    })
    settings.foreach {
      assignment =>
        var weight = getWeight(assignment)
        if (weight > Double.NegativeInfinity) {
          forIndex(numVariables, {
            i => weight += msgs(i)(assignment(i))
          })
          forIndex(numVariables, {
            i => val kth = assignment(i)
            msgToVariables(i)(kth) = {
              if (hardInfer) math.max(msgToVariables(i)(kth), weight - msgs(i)(kth))
              else NumUtils.logAdd(msgToVariables(i)(kth), weight - msgs(i)(kth))
            }
          })
        }
    }
    forIndex(numVariables, {
      i => variables(i).msgFrom(this, msgToVariables(i))
    })
  }
}

