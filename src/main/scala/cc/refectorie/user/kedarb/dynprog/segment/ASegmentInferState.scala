package cc.refectorie.user.kedarb.dynprog.segment

import cc.refectorie.user.kedarb.dynprog._
import types.Hypergraph
import utils.Utils._

/**
 * @author kedarb
 * @since 12/26/10
 */

trait ASegmentInferState[Params <: AParams, Example <: ASegmentExample]
  extends AHypergraphInferState[Segmentation, Example, Params] {
  type Widget = Segmentation

  def N = ex.numTokens

  def newWidget = new Widget(N)

  // number of labels
  def L: Int

  // everything j <= end is used as end position of the span (j is excluded from the span)
  def end(a: Int, i: Int, N: Int): Int

  // allowed transition for span [i, j) from prev_a -> a
  def allowedTransition(prev_a: Int, a: Int, i: Int, j: Int): Boolean

  // allowed start for span [0, j) from START -> a
  def allowedStart(a: Int, j: Int): Boolean

  def scoreStart(a: Int): Double

  def updateStart(a: Int, x: Double): Unit

  def scoreTransition(a: Int, b: Int): Double

  def updateTransition(a: Int, b: Int, x: Double): Unit

  def scoreEmission(a: Int, i: Int, j: Int): Double

  def updateEmission(a: Int, i: Int, j: Int, x: Double): Unit

  def createHypergraph(H: Hypergraph[Widget]) = {
    def gen(a: Int, i: Int): Object = {
      if (i == N) H.endNode
      else {
        val node = (a, i)
        if (H.addSumNode(node)) {
          forIndex(L, {
            b: Int => forIndex(i + 1, end(b, i, N), {
              j: Int => if (allowedTransition(a, b, i, j)) {
                H.addEdge(node, gen(b, j), new Info {
                  def getWeight = scoreTransition(a, b) + scoreEmission(b, i, j)

                  def setPosterior(v: Double) = {
                    updateTransition(a, b, v)
                    updateEmission(b, i, j, v)
                  }

                  def choose(widget: Widget) = {
                    val seg = Segment(i, j, b)
                    require(widget.prepend(seg), "Could not add segment: " + seg)
                    widget
                  }
                })
              }
            })
          })
        }
        node
      }
    }

    forIndex(L, {
      b: Int => forIndex(1, end(b, 0, N), {
        j: Int => if (allowedStart(b, j)) {
          H.addEdge(H.sumStartNode, gen(b, j), new Info {
            def getWeight = scoreStart(b) + scoreEmission(b, 0, j)

            def setPosterior(v: Double) = {
              updateStart(b, v)
              updateEmission(b, 0, j, v)
            }

            def choose(widget: Widget) = {
              val seg = Segment(0, j, b)
              require(widget.prepend(seg), "Could not add segment: " + seg)
              widget
            }
          })
        }
      })
    })
  }
}
