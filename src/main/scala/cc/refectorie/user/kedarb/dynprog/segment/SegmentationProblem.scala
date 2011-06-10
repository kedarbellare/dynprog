package cc.refectorie.user.kedarb.dynprog.segment

import cc.refectorie.user.kedarb.dynprog._
import fst.TagOptions
import types._
import utils.Utils._
import collection.mutable.HashSet

/**
 * @author kedarb
 * @since 12/26/10
 */

trait SegmentationProblem extends AProblem {
  type Widget = Segmentation

  def gen_opts: Options

  def seg_opts: TagOptions

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

  class Params(val starts: ParamVec, val transitions: Array[ParamVec],
               val emits: Array[ParamVec]) extends AParams {
    def foreachVec(f: (ParamVec) => Any) = {
      f(starts)
      transitions.foreach(f(_))
      emits.foreach(f(_))
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

      foreachIndex(emits, {
        (a: Int, pr: ParamVec) => foreachIndex(getValues(pr), {
          (f: Int, v: Double) => puts("E %s %s\t%s".format(lstr(a), fstr(f), fmt(v)))
        })
      })
      puts("")
    }
  }

}