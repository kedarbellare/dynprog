package cc.refectorie.user.kedarb.dynprog.fst

import cc.refectorie.user.kedarb.dynprog._
import data._
import types._
import utils.Utils._
import collection.mutable.{ArrayBuffer, HashSet}

/**
 * @author kedarb
 * @since Dec 15, 2010
 */

trait SequenceTaggingProblem extends AProblem {
  type Widget = LblSeq

  def gen_opts: Options

  def seqtag_opts: TagOptions

  val words = new Indexer[String]
  val features = new Indexer[String]
  val labels = new Indexer[String]
  val allowedStarts = new HashSet[Int]
  val allowedTransitions = new HashSet[(Int, Int)]

  def F = features.size

  def fstr(i: Int) = features(i)

  def W = words.size

  def wstr(i: Int) = words(i)

  def L = labels.size

  def lstr(i: Int) = labels(i)

  def getEvaluators(name: String): Array[APerformance[Widget]] = {
    val evals = new ArrayBuffer[APerformance[Widget]]
    if (seqtag_opts.accuracyEval) evals += new SeqLabelAccuracyEvaluator(name)
    if (seqtag_opts.perLabelAccuracyEval) evals += new SeqPerLabelAccuracyEvaluator(name, labels)
    if (seqtag_opts.segmentationEval) evals += new SegmentationEvaluator(name, labels)
    evals.toArray
  }

  class Params(val starts: ParamVec, val transitions: Array[ParamVec],
               val emits0: Array[ParamVec], // 0th order emissions: may be empty
               val emitsStart: Array[ParamVec], // 1st order start emissions: may be empty array
               val emits1: Array[Array[ParamVec]]) // 1st order emissions: may be empty
          extends AParams {
    def foreachVec(f: (ParamVec) => Any) = {
      f(starts)
      transitions.foreach(f(_))
      emits0.foreach(f(_))
      emitsStart.foreach(f(_))
      emits1.foreach(arr => arr.foreach(f(_)))
    }

    def output(puts: (String) => Any) = {
      foreachIndex(getValues(starts), {
        (i: Int, v: Double) => puts("S %s\t%s".format(lstr(i), fmt(v)))
      })
      puts("")

      foreachIndex(transitions, {
        (a: Int, pr: ParamVec) => foreachIndex(getValues(pr), {
          (b: Int, v: Double) => puts("T %s %s\t%s".format(lstr(a), lstr(b), fmt(v)))
        })
      })
      puts("")

      foreachIndex(emits0, {
        (a: Int, pr: ParamVec) => foreachIndex(getValues(pr), {
          (f: Int, v: Double) => puts("E0 %s %s\t%s".format(lstr(a), fstr(f), fmt(v)))
        })
      })
      puts("")

      foreachIndex(emitsStart, {
        (a: Int, pr: ParamVec) => foreachIndex(getValues(pr), {
          (f: Int, v: Double) => puts("Estart %s %s\t%s".format(lstr(a), fstr(f), fmt(v)))
        })
      })
      puts("")

      foreachIndex(emits1, {
        (a: Int, prarr: Array[ParamVec]) => foreachIndex(prarr, {
          (b: Int, pr: ParamVec) => foreachIndex(getValues(pr), {
            (f: Int, v: Double) => puts("E1 %s %s %s\t%s".format(lstr(a), lstr(b), fstr(f), fmt(v)))
          })
        })
      })
      puts("")
    }
  }

  abstract class ASeqTagInferState[Features,
  Example <: ATaggingExample[Features]](ex: Example, params: Params, counts: Params, ispec: InferSpec)
          extends ATaggingInferState[Features, Example, Params] {
    def L = labels.size

    def allowedTransition(i: Int, prev_a: Int, a: Int): Boolean = {
      if (trueInfer) ex.trueWidget(i) == a // check that widget at true position matches current state
      else !seqtag_opts.sparseTransitions || allowedTransitions.contains(prev_a -> a)
    }

    def allowedStart(a: Int): Boolean = {
      if (trueInfer) ex.trueWidget(0) == a
      else !seqtag_opts.sparseTransitions || allowedStarts.contains(a)
    }
  }

  abstract class ASeqTagDictReader(filename: String) extends ADictReader {
    def fromTokSeq(ts: LbledTokSeq): Unit

    def addToDict: Unit = {
      if (filename == null) return
      seqtag_opts.dataFormat match {
        case TagOptions.DataFormat.owpl =>
          new DataFileReader(filename).forLineGroup(seqtag_opts.lineGroupRegex, {
            lines: Array[String] =>
              val tabbedOWPL = lines.map(_.split(seqtag_opts.lineSplitRegex).mkString("\t"))
              if (seqtag_opts.owplDebug) println(tabbedOWPL.mkString("\n") + "\n")
              fromTokSeq(new LbledTokSeq(tabbedOWPL, "\t"))
          })
        case TagOptions.DataFormat.corasgml =>
          new DataFileReader(filename).forLine({
            line: String =>
              val tabbedOWPL = CoraSgml2Owpl(line, "\t", seqtag_opts.bioEncoding)
              if (seqtag_opts.owplDebug) println(tabbedOWPL.mkString("\n") + "\n")
              fromTokSeq(new LbledTokSeq(tabbedOWPL, "\t"))
          })
        case TagOptions.DataFormat.uiuc =>
          new DataFileReader(filename).forLineGroup(seqtag_opts.lineGroupRegex, {
            lines: Array[String] =>
              val tabbedOWPL = DanrTabbed2Owpl(lines, seqtag_opts.lineSplitRegex, "\t")
              if (seqtag_opts.owplDebug) println(tabbedOWPL.mkString("\n") + "\n")
              fromTokSeq(new LbledTokSeq(tabbedOWPL, "\t"))
          })
        case _ => throw fail("Unknown data format: " + seqtag_opts.dataFormat)
      }
    }
  }

  abstract class ASeqTagExampleReader[Features, Example <: ATaggingExample[Features]](filename: String)
          extends AExampleReader[Widget, Example] {
    def fromTokSeq(ts: LbledTokSeq): Example

    def exampleIterator: Iterator[Example] = {
      if (filename == null) return Iterator[Example]()
      seqtag_opts.dataFormat match {
        case TagOptions.DataFormat.owpl =>
          new DataFileReader(filename).forLazyLineGroup(seqtag_opts.lineGroupRegex, {
            lines: Array[String] =>
              val tabbedOWPL = lines.map(_.split(seqtag_opts.lineSplitRegex).mkString("\t"))
              fromTokSeq(new LbledTokSeq(tabbedOWPL, "\t"))
          })
        case TagOptions.DataFormat.corasgml =>
          new DataFileReader(filename).forLazyLine({
            line: String =>
              val tabbedOWPL = CoraSgml2Owpl(line, "\t", seqtag_opts.bioEncoding)
              fromTokSeq(new LbledTokSeq(tabbedOWPL, "\t"))
          })
        case TagOptions.DataFormat.uiuc =>
          new DataFileReader(filename).forLazyLineGroup(seqtag_opts.lineGroupRegex, {
            lines: Array[String] =>
              val tabbedOWPL = DanrTabbed2Owpl(lines, seqtag_opts.lineSplitRegex, "\t")
              fromTokSeq(new LbledTokSeq(tabbedOWPL, "\t"))
          })
        case _ => throw fail("Unknown data format: " + seqtag_opts.dataFormat)
      }
    }
  }
}