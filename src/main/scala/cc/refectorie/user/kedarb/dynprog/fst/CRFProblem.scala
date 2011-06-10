package cc.refectorie.user.kedarb.dynprog.fst

import cc.refectorie.user.kedarb.dynprog._
import utils.Utils._
import types.ParamUtils._
import types.IndexerUtils._
import org.apache.log4j.Logger
import data.{CoraCitationFeatures, ConllChunkingFeatures}
import types.{ParamVec, FtrVec}
import collection.mutable.HashMap

/**
 * @author kedarb
 * @since Dec 16, 2010
 */

class CRFProblem(val gen_opts: Options, val seqtag_opts: TagOptions) extends SequenceTaggingProblem {
  type Features = FtrVec

  val hasOrder0 = seqtag_opts.orders.contains(0)
  val hasOrder1 = seqtag_opts.orders.contains(1)
  val hashedFeatures = seqtag_opts.useHashing
  var featureCounts: HashMap[String, Int] = null

  override def F = {
    if (hashedFeatures) seqtag_opts.numFeatures
    else super.F
  }

  override def fstr(i: Int) = {
    if (hashedFeatures) "hashed%=" + i
    else super.fstr(i)
  }

  class Example(val words: Array[Int], val features: Array[Features], val trueWidget: Widget)
    extends ATaggingExample[Features]

  case class InferState(ex: Example, params: Params, counts: Params, ispec: InferSpec)
    extends ASeqTagInferState[Features, Example](ex, params, counts, ispec) {
    def scoreStart(a: Int) = {
      score(params.starts, a) + {
        if (hasOrder0) score(params.emits0(a), features(0)) else 0.0
      } + {
        if (hasOrder1) score(params.emitsStart(a), features(0)) else 0.0
      }
    }

    def updateStart(a: Int, x: Double) = {
      update(counts.starts, a, x)
      if (hasOrder0) update(counts.emits0(a), features(0), x)
      if (hasOrder1) update(counts.emitsStart(a), features(0), x)
    }

    def scoreTransition(a: Int, b: Int, i: Int) = {
      score(params.transitions(a), b) + {
        if (hasOrder0) score(params.emits0(b), features(i)) else 0.0
      } + {
        if (hasOrder1) score(params.emits1(a)(b), features(i)) else 0.0
      }
    }

    def updateTransition(a: Int, b: Int, i: Int, x: Double) = {
      update(counts.transitions(a), b, x)
      if (hasOrder0) update(counts.emits0(b), features(i), x)
      if (hasOrder1) update(counts.emits1(a)(b), features(i), x)
    }
  }

  class FeatureCountReader(val filename: String) extends ASeqTagDictReader(filename) {
    def fromTokSeq(ts: LbledTokSeq): Unit = {
      seqtag_opts.featureMethod match {
        case TagOptions.FeatureMethod.cora =>
          CoraCitationFeatures.processTokSeq(ts, Array(Array(-2), Array(-1), Array(1), Array(2)),
            seqtag_opts.labelIndex, seqtag_opts.wordIndex)
        case TagOptions.FeatureMethod.conllchunk =>
          ConllChunkingFeatures.processTokSeq(ts, seqtag_opts.labelIndex, seqtag_opts.wordIndex)
        case _ =>
          throw fail("Unknown feature method: " + seqtag_opts.featureMethod)
      }

      ts.features.foreach(_.foreach({
        f: String => featureCounts(f) += 1
      }))
    }
  }

  class DictReader(val filename: String) extends ASeqTagDictReader(filename) {
    def fromTokSeq(ts: LbledTokSeq): Unit = {
      seqtag_opts.featureMethod match {
        case TagOptions.FeatureMethod.cora =>
          CoraCitationFeatures.processTokSeq(ts, Array(Array(-2), Array(-1), Array(1), Array(2)),
            seqtag_opts.labelIndex, seqtag_opts.wordIndex)
        case TagOptions.FeatureMethod.conllchunk =>
          ConllChunkingFeatures.processTokSeq(ts, seqtag_opts.labelIndex, seqtag_opts.wordIndex)
        case _ =>
          throw fail("Unknown feature method: " + seqtag_opts.featureMethod)
      }

      ts.columnIndices(seqtag_opts.wordIndex, words.indexOf_!(_))
      val ls = {
        if (seqtag_opts.bioEncoding) ts.columnToBIOIndices(seqtag_opts.labelIndex, labels.indexOf_!(_))
        else ts.columnIndices(seqtag_opts.labelIndex, labels.indexOf_!(_))
      }
      var prevl = -1
      foreachIndex(ls, {
        (i: Int, currl: Int) =>
        // initialize the allowed starts/transitions
          if (i == 0) allowedStarts += currl
          else allowedTransitions += prevl -> currl
          // update previous label
          prevl = currl
      })
    }
  }

  class ExampleReader(val filename: String) extends ASeqTagExampleReader[Features, Example](filename) {
    def fromTokSeq(ts: LbledTokSeq): Example = {
      seqtag_opts.featureMethod match {
        case TagOptions.FeatureMethod.cora =>
          CoraCitationFeatures.processTokSeq(ts, Array(Array(-2), Array(-1), Array(1), Array(2)),
            seqtag_opts.labelIndex, seqtag_opts.wordIndex)
        case TagOptions.FeatureMethod.conllchunk =>
          ConllChunkingFeatures.processTokSeq(ts, seqtag_opts.labelIndex, seqtag_opts.wordIndex)
        case _ => throw fail("Unknown feature method: " + seqtag_opts.featureMethod)
      }

      val ws = ts.columnIndices(seqtag_opts.wordIndex, words.indexOf_?(_))
      val fvs = {
        if (hashedFeatures) ts.featureVectorSequence(_.hashCode % F)
        else ts.featureVectorSequence(features.indexOf_?(_))
      }
      val ls = {
        if (seqtag_opts.bioEncoding) ts.columnToBIOIndices(seqtag_opts.labelIndex, labels.indexOf_?(_))
        else ts.columnIndices(seqtag_opts.labelIndex, labels.indexOf_?(_))
      }
      new Example(ws, fvs, ls)
    }
  }

  class Model extends AModel[Widget, Example, Params, InferState] {
    val logger = Logger.getLogger(this.getClass.getSimpleName)

    def opts = gen_opts

    def newParams(dense: Boolean) = new Params(newWtVec(dense, L), newWtVecArray(dense, L, L), {
      if (hasOrder0) newWtVecArray(dense, L, F) else Array[ParamVec]()
    }, {
      if (hasOrder1) newWtVecArray(dense, L, F) else Array[ParamVec]()
    }, {
      if (hasOrder1) newWtVecArray2(dense, L, L, F) else Array[Array[ParamVec]]()
    }
    )

    def newInferState(ex: Example, counts: Params, ispec: InferSpec) = InferState(ex, params, counts, ispec)

    def widgetToFullString(ex: Example, widget: Widget) = {
      var wwStr = ">>\n"
      wwStr += ">Words\n"
      val ws = ex.words.map(wstr(_))
      foreachIndex(ws, {
        (i: Int, w: String) => wwStr += "%s\t\"%s\"\n".format(fmt(i), w)
      })
      wwStr += "\n"
      wwStr += ">Mentions\n"
      val ls = widget.map(lstr(_))
      foreachIndex(ls, {
        (i: Int, l: String) => wwStr += "%s\t%s\t\"%s\"\n".format(fmt(i), fmt(i), l)
      })
      wwStr += "\n"
      wwStr
    }

    def createDictionary = {
      if (!hashedFeatures) {
        featureCounts = new HashMap[String, Int] {
          override def default(key: String): Int = {
            this(key) = 0;
            0
          }
        }

        // first count features
        countFeatures(opts.labeledFile)
        countFeatures(opts.unlabeledFile)
        // countFeatures(opts.devFile)
        // countFeatures(opts.testFile)

        // create features dictionary only for counts >= cutoff
        for (f <- featureCounts.keys)
          if (featureCounts(f) >= seqtag_opts.featureCutoff) features.indexOf_!(f)
        logger.info("Input features: " + featureCounts.size +
          " Pruned: " + (featureCounts.size - F) +
          " Remaining: " + F)
        featureCounts = null
      } else {
        logger.info("Using hashing trick, #features: " + F)
      }

      // add unpruned features
      addToDict(opts.labeledFile)
      addToDict(opts.unlabeledFile)
      addToDict(opts.devFile)
      addToDict(opts.testFile)
      logger.info("Allowed starts: " + allowedStarts)
      logger.info("Allowed transitions: " + allowedTransitions)
    }

    override def preInit = {
      features.lock
      words.lock
      labels.lock
    }

    def learn(name: String, lopts: LearnOptions) = {
      if (lopts.online) new OnlineModelLearner(this, lopts).learn(name)
      else new BatchModelLearner(this, lopts).learn(name)
    }

    def serialize(puts: (String) => Any) = {
      serializeIndexer(puts, features)
      serializeIndexer(puts, words)
      serializeIndexer(puts, labels)
      serializeParams(puts, params.starts)
      serializeParamsArray(puts, params.transitions)
      serializeParamsArray(puts, params.emits0)
      serializeParamsArray(puts, params.emitsStart)
      serializeParamsArray2(puts, params.emits1)
      puts(allowedStarts.mkString(" "))
      puts(allowedTransitions.map(p => p._1 + "," + p._2).mkString(" "))
    }

    def deserialize(gets: => String) = {
      deserializeToIndexerString(gets, features)
      deserializeToIndexerString(gets, words)
      deserializeToIndexerString(gets, labels)
      val starts = deserializeParams(gets)
      val transitions = deserializeParamsArray(gets)
      val emits0 = deserializeParamsArray(gets)
      val emitsStart = deserializeParamsArray(gets)
      val emits1 = deserializeParamsArray2(gets)
      allowedStarts ++= gets.split(" ").map(_.toInt)
      allowedTransitions ++= gets.split(" ").map({
        s: String => val p = s.split(",").map(_.toInt); p(0) -> p(1)
      })
      params = new Params(starts, transitions, emits0, emitsStart, emits1)
    }
  }

  def countFeatures(filename: String) = new FeatureCountReader(filename).addToDict

  def addToDict(filename: String) = new DictReader(filename).addToDict

  def getExampleIterator(filename: String): Iterator[Example] = new ExampleReader(filename).exampleIterator

  // model learners
  class OnlineModelLearner(val model: Model, val lopts: LearnOptions)
    extends ADiscriminativeModelLearner[Widget, Example, Params, InferState, Model]
    with AOnlineLearner[Widget, Example, Params, InferState, Model] {
    val logger = Logger.getLogger(this.getClass.getSimpleName)

    val testExamples = createArrayFromIterator(getExampleIterator(gen_opts.testFile))
    val validationExamples = createArrayFromIterator(getExampleIterator(gen_opts.devFile))
    val unlabeledExamples = createArrayFromIterator(getExampleIterator(gen_opts.unlabeledFile))
    val labeledExamples = createArrayFromIterator(getExampleIterator(gen_opts.labeledFile))

    def getTestExampleIterator = testExamples.iterator

    def getValidationExampleIterator = validationExamples.iterator

    def getUnlabeledExampleIterator = shuffle(lopts.onlinePermRandom, unlabeledExamples).toIterator

    def getLabeledExampleIterator = shuffle(lopts.onlinePermRandom, labeledExamples).toIterator

    override def newEvaluators(name: String) = getEvaluators(name)
  }

  class BatchModelLearner(val model: Model, val lopts: LearnOptions)
    extends ABatchDiscParallelLearner[Widget, Example, Params, InferState, Model] {
    val logger = Logger.getLogger(this.getClass.getSimpleName)

    val testExamples = createArrayFromIterator(getExampleIterator(gen_opts.testFile))
    val validationExamples = createArrayFromIterator(getExampleIterator(gen_opts.devFile))
    val unlabeledExamples = createArrayFromIterator(getExampleIterator(gen_opts.unlabeledFile))
    val labeledExamples = createArrayFromIterator(getExampleIterator(gen_opts.labeledFile))

    def getTestExampleIterator = testExamples.iterator

    def getValidationExampleIterator = validationExamples.iterator

    def getUnlabeledExampleIterator = unlabeledExamples.toIterator

    def getLabeledExampleIterator = labeledExamples.toIterator

    override def newEvaluators(name: String) = getEvaluators(name)
  }

  def newModel = new Model
}