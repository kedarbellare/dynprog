package cc.refectorie.user.kedarb.dynprog.segment

import cc.refectorie.user.kedarb.dynprog._
import classify.LabelAccuracyEvaluator
import utils.Utils._

/**
 * @author kedarb
 * @since 12/30/10
 */

class SegmentLabelAccuracyEvaluator(val name: String) extends APerformance[Segmentation] {
  val eval = new LabelAccuracyEvaluator(name)

  def add(trueWidget: Segmentation, predWidget: Segmentation) = {
    require(trueWidget.length == predWidget.length, "trueLength(" +
      trueWidget.length + ") != predLength(" + predWidget.length + ")")
    forIndex(trueWidget.length, {
      i: Int => eval.add(trueWidget.labelAt(i).get, predWidget.labelAt(i).get)
    })
  }

  def accuracy = eval.accuracy

  def output(puts: (String) => Any) = eval.output(puts)
}