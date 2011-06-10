package cc.refectorie.user.kedarb.dynprog.segment

import cc.refectorie.user.kedarb.dynprog._
import types.Indexer
import utils.Utils._

/**
 * @author kedarb
 * @since 12/30/10
 */

class SegmentSegmentationEvaluator(val name: String, val labels: Indexer[String],
                                   val isOtherLabel: (String) => Boolean = _ == "O")
  extends APerformance[Segmentation] {
  val segmentLabels = new Indexer[String]
  forIndex(labels.size, {
    i: Int =>
      val s = labels(i)
      if (isOtherLabel(s)) {}
      else segmentLabels += s
  })
  val L = segmentLabels.size
  segmentLabels.lock
  val correctSegments = new Array[Int](L)
  val predSegments = new Array[Int](L)
  val trueSegments = new Array[Int](L)
  var totalCorrectSegments = 0
  var totalPredSegments = 0
  var totalTrueSegments = 0

  def add(trueWidget: Segmentation, predWidget: Segmentation) = {
    require(trueWidget.length == predWidget.length,
      "trueLen(" + trueWidget.length + ") != predLen(" + predWidget.length + ")")

    // count true segment starts
    forIndex(trueWidget.numSegments, {
      i: Int =>
        val trueStart = segmentLabels.indexOf_?(labels(trueWidget.segment(i).label))
        if (trueStart >= 0) {
          totalTrueSegments += 1
          trueSegments(trueStart) += 1
        }
    })

    // count predicted segment starts
    forIndex(predWidget.numSegments, {
      i: Int =>
      val predStart = segmentLabels.indexOf_?(labels(predWidget.segment(i).label))
      if (predStart >= 0) {
        totalPredSegments += 1
        predSegments(predStart) += 1
      }
    })

    // count overlapping segments
    forIndex(trueWidget.numSegments, {
      i: Int =>
        val seg = trueWidget.segment(i)
        val trueStart = segmentLabels.indexOf_?(labels(seg.label))
        if (trueStart >= 0 && predWidget.contains(seg)) {
          totalCorrectSegments += 1
          correctSegments(trueStart) += 1
        }
    })
  }

  def accuracy(puts: (String) => Any): Double = {
    forIndex(L, {
      l: Int =>
        val (pr, re, f1) = countsToPRF1(predSegments(l), trueSegments(l), correctSegments(l))
        puts("%s segment accuracy: %s precision=%.4f recall=%.4f F1=%.4f correct=%d pred=%d true=%d".format(name,
          segmentLabels(l), pr, re, f1, correctSegments(l), predSegments(l), trueSegments(l)))
    })
    val (opr, ore, of1) = countsToPRF1(totalPredSegments, totalTrueSegments, totalCorrectSegments)
    puts("%s segment accuracy: OVERALL precision=%.4f recall=%.4f F1=%.4f correct=%d pred=%d true=%d".format(name,
      opr, ore, of1, totalCorrectSegments, totalPredSegments, totalTrueSegments))
    of1
  }

  def accuracy: Double = {
    def dummy(s: String): Unit = {}
    accuracy(dummy)
  }

  def output(puts: (String) => Any) = {
    puts("")
    accuracy(puts)
  }
}