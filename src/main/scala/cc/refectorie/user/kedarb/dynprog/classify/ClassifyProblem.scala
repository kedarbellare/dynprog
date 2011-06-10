package cc.refectorie.user.kedarb.dynprog.classify

import cc.refectorie.user.kedarb.dynprog._
import types._
import types.ParamUtils._
import types.IndexerUtils._
import utils.Utils._

/**
 * @author kedarb
 * @since Dec 2, 2010
 */
trait ClassifyProblem extends AProblem {
  type Widget = Int

  def classify_opts: Options

  val features = new Indexer[String]
  val labels = new Indexer[String]

  def F = features.size

  def fstr(i: Int) = features(i)

  def L = labels.size

  def lstr(i: Int) = labels(i)

  class Example(val fvec: FtrVec, // example's features
                val trueWidget: Widget) // example's label
          extends AExample[Widget] {
    val numTokens = 1
    // token == document
  }

  class Params(val priors: ParamVec, val emits: Array[ParamVec]) extends AParams {
    def foreachVec(f: (ParamVec) => Any) = {
      f(priors)
      emits.foreach(f(_))
    }

    def output(puts: String => Any) = {
      foreachIndex(getValues(priors), {
        (i: Int, v: Double) => puts("P %s\t%s".format(lstr(i), fmt(v)))
      })
      puts("")

      foreachIndex(emits, {
        (l: Int, pr: ParamVec) => foreachIndex(getValues(pr), {
          (f: Int, v: Double) => if (v != 0) puts("E %s %s\t%s".format(lstr(l), fstr(f), fmt(v)))
        })
      })
    }
  }

  case class InferState(ex: Example, params: Params, counts: Params, ispec: InferSpec)
          extends AHypergraphInferState[Widget, Example, Params] {
    def newWidget = 0

    def createHypergraph(H: Hypergraph[Widget]) = {
      for (l <- 0 until L) {
        if (!trueInfer || ex.trueWidget == l) {
          H.addEdge(H.sumStartNode, new Info {
            def getWeight = score(params.priors, l) + score(params.emits(l), ex.fvec)

            def choose(widget: Widget) = l

            def setPosterior(prob: Double) = {
              update(counts.priors, l, prob)
              update(counts.emits(l), ex.fvec, prob)
            }
          })
        }
      }
    }
  }

  abstract class ClassifyModel extends AModel[Widget, Example, Params, InferState] {
    def opts = classify_opts

    def createDictionary: Unit = {
      addToDict(opts.labeledFile)
      addToDict(opts.unlabeledFile)
      addToDict(opts.devFile)
      addToDict(opts.testFile)
    }

    def newInferState(ex: Example, counts: Params, ispec: InferSpec) = InferState(ex, params, counts, ispec)

    override def preInit = {
      features.lock;
      labels.lock
    }

    def serialize(puts: String => Any) = {
      serializeIndexer(puts, features)
      serializeIndexer(puts, labels)
      serializeParams(puts, params.priors)
      serializeParamsArray(puts, params.emits)
    }

    def deserialize(gets: => String) = {
      deserializeToIndexerString(gets, features)
      deserializeToIndexerString(gets, labels)
      val priors = deserializeParams(gets)
      val emits = deserializeParamsArray(gets)
      params = new Params(priors, emits)
    }
  }

  // create dictionaries from svm light format
  class SvmLightDictReader(val filename: String) extends ADictReader {
    def addToDict: Unit = {
      if (filename != null) new DataFileReader(filename).forLine {
        str: String =>
          val parts = str.split("\\s+")
          labels.indexOf_!(parts(0)) // first part is label
          forIndex(1, parts.length, {
            i: Int => if (parts(i).length > 0) {
              val ftrval = parts(i).split(":")
              features.indexOf_!(ftrval(0))
            }
          })
      }
    }
  }

  class SvmLightExampleReader(val filename: String) extends AExampleReader[Widget, Example] {
    def exampleIterator: Iterator[Example] = {
      if (filename == null) Iterator[Example]()
      else new DataFileReader(filename).forLazyLine({
        str: String =>
          val parts = str.split("\\s+")
          val trueWidget = labels.indexOf_?(parts(0))
          val fvec = new FtrVec
          forIndex(1, parts.length, {
            i: Int => if (parts(i).length > 0) {
              val ftrval = parts(i).split(":")
              require(ftrval.length == 1 || ftrval.length == 2)
              val index = features.indexOf_?(ftrval(0))
              val value = if (ftrval.length == 1) 1.0 else ftrval(1).toDouble
              if (index >= 0) {
                fvec += index -> value
              }
            }
          })
          new Example(fvec, trueWidget)
      })
    }
  }

  def getExampleIterator(filename: String): Iterator[Example] = new SvmLightExampleReader(filename).exampleIterator

  def addToDict(filename: String): Unit = new SvmLightDictReader(filename).addToDict
}


