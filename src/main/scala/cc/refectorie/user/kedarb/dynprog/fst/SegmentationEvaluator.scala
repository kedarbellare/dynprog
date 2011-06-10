package cc.refectorie.user.kedarb.dynprog.fst

import cc.refectorie.user.kedarb.dynprog._
import data.Labels2BIO
import types.Indexer
import utils.Utils._

/**
 * @author kedarb
 * @since Dec 17, 2010
 */

class SegmentationEvaluator(val name: String, val labels: Indexer[String], val otherLabel: String = "O")
        extends APerformance[Array[Int]] {
  val segmentLabels = new Indexer[String]
  forIndex(labels.size, {
    i: Int =>
      val s = labels(i)
      if (s == otherLabel) {}
      else if (s.matches("B-.*") || s.matches("I-.*")) segmentLabels += s.substring(2)
      else segmentLabels += s
  })
  val L = segmentLabels.size
  val segmentStartTags = new Indexer[String]
  val segmentContinueTags = new Indexer[String]
  forIndex(L, {
    i: Int =>
      val s = segmentLabels(i)
      segmentStartTags += "B-%s".format(s)
      segmentContinueTags += "I-%s".format(s)
  })
  segmentLabels.lock
  segmentStartTags.lock
  segmentContinueTags.lock
  val correctSegments = new Array[Int](L)
  val predSegments = new Array[Int](L)
  val trueSegments = new Array[Int](L)
  var totalCorrectSegments = 0
  var totalPredSegments = 0
  var totalTrueSegments = 0

  def add(trueWidget: Array[Int], predWidget: Array[Int]) = {
    require(trueWidget.length == predWidget.length,
      "trueLen(" + trueWidget.length + ") != predLen(" + predWidget.length + ")")
    val len = trueWidget.length
    val trueOutput = Labels2BIO(trueWidget.map(labels(_)), otherLabel)
    val predOutput = Labels2BIO(predWidget.map(labels(_)), otherLabel)

    forIndex(len, {
      i: Int =>
        var trueStart = segmentStartTags.indexOf_?(trueOutput(i))
        // count true segment starts
        if (trueStart >= 0) {
          totalTrueSegments += 1
          trueSegments(trueStart) += 1
        }

        var predStart = segmentStartTags.indexOf_?(predOutput(i))
        // count pred segment starts
        if (predStart >= 0) {
          totalPredSegments += 1
          predSegments(predStart) += 1
        }

        if (trueStart >= 0 && trueStart == predStart) {
          // count correct segment overlap
          val continueTag = segmentContinueTags(predStart)
          def indexOfSegmentEnd(output: Array[String]): Int = {
            forIndex(i + 1, len, {j: Int => if (output(j) != continueTag) return j})
            len
          }
          if (indexOfSegmentEnd(trueOutput) == indexOfSegmentEnd(predOutput)) {
            totalCorrectSegments += 1
            correctSegments(predStart) += 1
          }
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