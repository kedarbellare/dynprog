package cc.refectorie.user.kedarb.dynprog.classify

import cc.refectorie.user.kedarb.dynprog._
import types.Indexer
import utils.Utils._

/**
 * @author kedarb
 * @since Dec 13, 2010
 */

class PerLabelAccuracyEvaluator(val name: String, val labels: Indexer[String]) extends APerformance[Int] {
  val L = labels.size
  val correctLabels = new Array[Int](L)
  val predLabels = new Array[Int](L)
  val trueLabels = new Array[Int](L)

  def add(trueWidget: Int, predWidget: Int) = {
    if (predWidget == trueWidget) correctLabels(predWidget) += 1
    predLabels(predWidget) += 1
    trueLabels(trueWidget) += 1
  }

  def accuracy(puts: (String) => Any): Double = { // calculates the macro-average F1
    var allF1 = 0.0
    forIndex(L, {
      l: Int =>
        val (pr, re, f1) = countsToPRF1(predLabels(l), trueLabels(l), correctLabels(l))
        puts("%s per-label accuracy: %s precision=%.4f recall=%.4f F1=%.4f correct=%d pred=%d true=%d".format(name,
          labels(l), pr, re, f1, correctLabels(l), predLabels(l), trueLabels(l)))
        allF1 += f1
    })
    allF1 /= L
    puts("%s per-label accuracy: MacroF1=%.4f".format(name, allF1))
    allF1
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