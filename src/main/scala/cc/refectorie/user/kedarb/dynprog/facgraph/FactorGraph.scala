package cc.refectorie.user.kedarb.dynprog.facgraph

import collection.Seq
import cc.refectorie.user.kedarb.dynprog.utils.Utils._
import collection.mutable.{Queue, HashSet, HashMap}
import cc.refectorie.user.kedarb.tools.utils.ListUtils

/**
 * @author kedarb
 * @since 2/6/11
 */

object MsgVariableType extends Enumeration {
  type MsgVariableType = Value
  val VariableBP = Value
}

object MsgFactorType extends Enumeration {
  type MsgFactorType = Value
  val FactorBP = Value
}

class FactorGraph[Widget](val residualEps: Double) {
  def this() = this (1e-4)

  import MsgVariableType._
  import MsgFactorType._

  val id2variable = new HashMap[Any, MsgVariable[Widget]]
  val id2factor = new HashMap[Any, MsgFactor[Widget]]
  var bfsOrdering: Array[MsgNode] = null

  def addVariable(varId: Any, varType: MsgVariableType, domainSize: Int, trueValue: Int, isEvidence: Boolean): Boolean = {
    if (id2variable.contains(varId)) return false
    id2variable(varId) = varType match {
      case VariableBP => new BPVariable[Widget](domainSize, trueValue, isEvidence)
      case _ => throw fail("Unknown message variable type: " + varType)
    }
    true
  }

  def addFactor(factorId: Any, varIds: Seq[Any], factorType: MsgFactorType): Boolean = {
    if (id2factor.contains(factorId)) return false
    val factor = factorType match {
      case FactorBP => new BPFactor[Widget]
      case _ => throw fail("Unknown message factor type: " + factorType)
    }
    id2factor(factorId) = factor
    // add links from factor to variables and vice versa
    varIds.foreach {
      id =>
        val variable = id2variable(id)
        factor.addVariable(variable)
        variable.addFactor(factor)
    }
    true
  }

  def addInfo(factorId: Any, assignment: Array[Int], factorInfo: AFactorInfo[Widget]): Unit = {
    id2factor(factorId).setInfo(assignment, factorInfo)
  }

  // compute BFS ordering starting from a given node: assumes connected graph
  private def computeBfsOrdering(v: MsgNode): Unit = {
    bfsOrdering = new Array[MsgNode](id2variable.values.size + id2factor.values.size)
    val queue = new Queue[MsgNode]
    queue.enqueue(v)

    val hit = new HashSet[MsgNode]
    var i = 0
    while (queue.size > 0) {
      val n = queue.dequeue
      hit += n
      bfsOrdering(i) = n
      i += 1
      n.neighbors.filter(!hit(_)).foreach(queue.enqueue(_))
    }
    if (i != bfsOrdering.size) throw fail("Factor graph not connected!")
  }

  // keep updating posteriors until top residual is less than epsilon
  def computePosteriors(viterbi: Boolean): Unit = {
    // first iteration choose randomly
    var root: MsgNode = id2variable.values.head
    do {
      // update posteriors with current root
      computePosteriors(viterbi, root)
      // re-estimate root based on residuals
      root = bfsOrdering(ListUtils.maxIndex(bfsOrdering.map(_.maxResidual)))
    } while (root.maxResidual > residualEps)
  }

  private def computePosteriors(viterbi: Boolean, root: MsgNode): Unit = {
    computeBfsOrdering(root)
    // send messages from leaves to root
    var i = bfsOrdering.size - 1
    while (i >= 0) {
      bfsOrdering(i).update(viterbi)
      i -= 1
    }
    // send messages from root to leaves
    i = 0
    while (i < bfsOrdering.size) {
      bfsOrdering(i).update(viterbi)
      i += 1
    }
  }
}

// Sample usage:
//    override def createHypergraph(H: Hypergraph[Widget]): Unit = {
//      import MsgVariableType._
//      import MsgFactorType._
//
//      super.createHypergraph(H)
//      val fg = new FactorGraph[Widget]
//      forIndex(N, {
//        i => fg.addVariable(i, VariableBP, L, ex.trueWidget(i), false)
//      })
//      forIndex(N, {
//        i => fg.addFactor(i, Seq(i), FactorBP)
//        forIndex(L, {
//          a => fg.addInfo(i, Array(a), new AFactorInfo[Widget] {
//            def choose(widget: Widget) = widget
//
//            def setPosterior(prob: Double): Unit = {}
//
//            def getWeight: Double = if (i == 0) scoreStart(a) else score(params.emits0(a), features(i))
//          })
//        })
//      })
//      forIndex(N-1, {
//        i => fg.addFactor((i, i+1), Seq(i, i+1), FactorBP)
//        forIndex(L, {
//          a => forIndex(L, {
//            b => fg.addInfo((i, i+1), Array(a, b), new AFactorInfo[Widget] {
//              def choose(widget: Widget) = widget
//
//              def setPosterior(prob: Double): Unit = {}
//
//              def getWeight: Double = score(params.transitions(a), b)
//            })
//          })
//        })
//      })
//      val startTime = System.currentTimeMillis
//      fg.computePosteriors(hardInfer)
//      totalTime += System.currentTimeMillis - startTime
//    }
