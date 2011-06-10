package cc.refectorie.user.kedarb.dynprog.classify

import cc.refectorie.user.kedarb.dynprog._
import org.apache.log4j.Logger
import utils.Utils._
import cc.refectorie.user.kedarb.tools.opts.OptParser

/**
 * @author kedarb
 * @since Dec 13, 2010
 */

object Classifier {
  val logger = Logger.getLogger(this.getClass.getSimpleName)
  val opts = new Options
  val stage1 = new LearnOptions
  val stage2 = new LearnOptions

  def main(args: Array[String]) {
    val parser = new OptParser
    parser.doRegister("exec", opts)
    parser.doRegister("stage1", stage1)
    parser.doRegister("stage2", stage2)

    if (!parser.doParse(args)) System.exit(1)

    val problem = opts.classifier match {
      case Options.Classifier.maxent => {
        logger.info("*** MaxEnt classifier")
        new MaxEntProblem(opts)
      }
      case Options.Classifier.naivebayes => {
        logger.info("*** NaiveBayes classifier")
        new NaiveBayesProblem(opts)
      }
      case _ => throw fail("Unknown classifier: " + opts.classifier)
    }
    val model = problem.newModel
    model.createDictionary
    model.preInit
    model.init(opts.initType, opts.initRandom)
    stopWatch("stage1", logger.info(_)) {
      model.learn("stage1", stage1)
    }
    stopWatch("stage2", logger.info(_)) {
      model.learn("stage2", stage2)
    }
    model.save("classifier.ser~")
  }
}
