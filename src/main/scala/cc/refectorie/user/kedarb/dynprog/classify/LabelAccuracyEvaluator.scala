package cc.refectorie.user.kedarb.dynprog.classify

import cc.refectorie.user.kedarb.dynprog._

/**
 * @author kedarb
 * @since Dec 13, 2010
 */

class LabelAccuracyEvaluator(val name: String) extends APerformance[Int] {
  var totalCount = 0
  var correctCount = 0

  def add(trueLabel: Int, predLabel: Int) = {
    if (trueLabel == predLabel) correctCount += 1
    totalCount += 1
  }

  def accuracy = if (totalCount == 0) 0.0 else (1.0 * correctCount) / totalCount

  def output(puts: (String) => Any) = {
    puts("")
    puts("%s label accuracy: %.4f correct=%d total=%d".format(name, accuracy, correctCount, totalCount))
  }
}