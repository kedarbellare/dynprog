package cc.refectorie.user.kedarb.dynprog.fst

import cc.refectorie.user.kedarb.dynprog._
import types.Hypergraph
import utils.Utils._

/**
 * @author kedarb
 * @since 12/26/10
 */

trait ATaggingInferState[Features, Example <: ATaggingExample[Features], Params <: AParams]
  extends AHypergraphInferState[LblSeq, Example, Params] {
  type Widget = LblSeq

  def features = ex.features

  def N = ex.numTokens

  def L: Int

  def newWidget = new Widget(N)

  def scoreTransition(a: Int, b: Int, i: Int): Double

  def updateTransition(a: Int, b: Int, i: Int, x: Double): Unit

  def scoreStart(a: Int): Double

  def updateStart(a: Int, x: Double): Unit

  def allowedTransition(i: Int, prev_a: Int, a: Int): Boolean

  def allowedStart(a: Int): Boolean

  def createHypergraph(H: Hypergraph[Widget]) = {
    def gen(i: Int, a: Int): Object = {
      // Generate the rest of the sequence from position i with state a
      if (i == N - 1) H.endNode
      else {
        val node = (i, a)
        if (H.addSumNode(node)) forIndex(L, {
          b: Int => if (allowedTransition(i + 1, a, b))
            H.addEdge(node, gen(i + 1, b), new Info {
              def getWeight = scoreTransition(a, b, i + 1)

              def setPosterior(v: Double) = updateTransition(a, b, i + 1, v)

              def choose(widget: Widget) = {
                widget(i + 1) = b;
                widget
              }
            })
        })
        node
      }
    }

    forIndex(L, {
      a: Int => if (allowedStart(a))
        H.addEdge(H.sumStartNode, gen(0, a), new Info {
          // Generate the state at position 0
          def getWeight = scoreStart(a)

          def setPosterior(v: Double) = updateStart(a, v)

          def choose(widget: Widget) = {
            widget(0) = a;
            widget
          }
        })
    })
  }
}
