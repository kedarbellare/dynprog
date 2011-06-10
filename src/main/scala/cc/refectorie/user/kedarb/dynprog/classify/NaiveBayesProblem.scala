package cc.refectorie.user.kedarb.dynprog.classify

import cc.refectorie.user.kedarb.dynprog._
import utils.Utils._
import org.apache.log4j.Logger

/**
 * @author kedarb
 * @since Dec 13, 2010
 */

class NaiveBayesProblem(val classify_opts: Options) extends ClassifyProblem {

  class Model extends ClassifyModel {
    def newParams(dense: Boolean) = new Params(newPrVec(dense, L), newPrVecArray(dense, L, F))

    def widgetToFullString(ex: Example, widget: Widget) = "%s\t%s".format(lstr(widget), ex.fvec.toString(fstr(_)))

    def learn(name: String, lopts: LearnOptions) = {
      if (lopts.online) new OnlineModelLearner(this, lopts).learn(name)
      else new BatchModelLearner(this, lopts).learn(name)
    }
  }

  class OnlineModelLearner(val model: Model, val lopts: LearnOptions)
    extends AGenerativeModelLearner[Widget, Example, Params, InferState, Model]
    with AOnlineLearner[Widget, Example, Params, InferState, Model] {
    val logger = Logger.getLogger(this.getClass.getSimpleName)

    val testExamples = createArrayFromIterator(getExampleIterator(classify_opts.testFile))
    val validationExamples = createArrayFromIterator(getExampleIterator(classify_opts.devFile))
    val unlabeledExamples = createArrayFromIterator(getExampleIterator(classify_opts.unlabeledFile))
    val labeledExamples = createArrayFromIterator(getExampleIterator(classify_opts.labeledFile))

    def getTestExampleIterator = testExamples.iterator

    def getValidationExampleIterator = validationExamples.iterator

    def getUnlabeledExampleIterator = shuffle(lopts.onlinePermRandom, unlabeledExamples).toIterator

    def getLabeledExampleIterator = shuffle(lopts.onlinePermRandom, labeledExamples).toIterator

    override def newEvaluators(name: String) = {
      Array(new LabelAccuracyEvaluator(name), new PerLabelAccuracyEvaluator(name, labels))
    }
  }

  class BatchModelLearner(val model: Model, val lopts: LearnOptions)
    extends ABatchGenLearner[Widget, Example, Params, InferState, Model] {
    val logger = Logger.getLogger(this.getClass.getSimpleName)

    val testExamples = createArrayFromIterator(getExampleIterator(classify_opts.testFile))
    val validationExamples = createArrayFromIterator(getExampleIterator(classify_opts.devFile))
    val unlabeledExamples = createArrayFromIterator(getExampleIterator(classify_opts.unlabeledFile))
    val labeledExamples = createArrayFromIterator(getExampleIterator(classify_opts.labeledFile))

    def getTestExampleIterator = testExamples.iterator

    def getValidationExampleIterator = validationExamples.iterator

    def getUnlabeledExampleIterator = unlabeledExamples.toIterator

    def getLabeledExampleIterator = labeledExamples.toIterator

    override def newEvaluators(name: String) = {
      Array(new LabelAccuracyEvaluator(name), new PerLabelAccuracyEvaluator(name, labels))
    }
  }

  def newModel = new Model
}

