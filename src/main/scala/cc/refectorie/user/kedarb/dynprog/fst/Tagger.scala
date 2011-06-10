package cc.refectorie.user.kedarb.dynprog.fst

import cc.refectorie.user.kedarb.dynprog._
import org.apache.log4j.Logger
import utils.Utils._
import cc.refectorie.user.kedarb.tools.opts.OptParser

/**
 * @author kedarb
 * @since 12 /15/10
 */

object Tagger {
  val logger = Logger.getLogger(this.getClass.getSimpleName)
  val opts = new Options
  val tagopts = new TagOptions
  val stage1 = new LearnOptions
  val stage2 = new LearnOptions

  def main(args: Array[String]) {
    val parser = new OptParser
    parser.doRegister("exec", opts)
    parser.doRegister("tag", tagopts)
    parser.doRegister("stage1", stage1)
    parser.doRegister("stage2", stage2)

    if (!parser.doParse(args)) System.exit(1)

    val problem = opts.tagger match {
      case Options.Tagger.hmm =>
        logger.info("*** HMM tagger")
        new HMMProblem(opts, tagopts)
      case Options.Tagger.crf =>
        logger.info("*** CRF tagger")
        new CRFProblem(opts, tagopts)
      case Options.Tagger.coraccrf =>
        logger.info("*** CRF tagger for constrained CORA")
        new CoraConstraintCRFProblem(opts, tagopts)
      case _ => throw fail("Unknown tagger: " + opts.tagger)
    }
    val model = problem.newModel
    model.createDictionary
    model.preInit
    model.init(opts.initType, opts.initRandom)
    stopWatch("Learning stage1", logger.info(_)) {
      model.learn("stage1", stage1)
    }
    stopWatch("Learning stage2", logger.info(_)) {
      model.learn("stage2", stage2)
    }
    model.save("tagger.ser~")
  }
}