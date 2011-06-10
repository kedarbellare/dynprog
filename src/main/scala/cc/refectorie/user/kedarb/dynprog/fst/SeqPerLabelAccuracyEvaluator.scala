package cc.refectorie.user.kedarb.dynprog.fst

import cc.refectorie.user.kedarb.dynprog._
import classify.PerLabelAccuracyEvaluator
import types.Indexer
import utils.Utils._

/**
 * @author kedarb
 * @since 12/15/10
 */

class SeqPerLabelAccuracyEvaluator(val name: String, val labels: Indexer[String]) extends APerformance[Array[Int]] {
  val eval = new PerLabelAccuracyEvaluator(name, labels)

  def add(trueWidget: Array[Int], predWidget: Array[Int]) = {
    require(trueWidget.length == predWidget.length, "trueLength(" +
      trueWidget.length + ") != predLength(" + predWidget.length + ")")
    forIndex(trueWidget.length, {i: Int => eval.add(trueWidget(i), predWidget(i))})
  }

  def accuracy = eval.accuracy

  def output(puts: (String) => Any) = eval.output(puts)
}