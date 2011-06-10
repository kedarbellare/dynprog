package cc.refectorie.user.kedarb.dynprog

import types._
import la.Vector
import java.util.Random
import utils.Utils._
import ParamUtils._

/**
 * Parameters of any model.
 *
 * @author kedarb
 * @since Nov 29, 2010
 */

trait AParams {
  // *** METHODS TO BE IMPLEMENTED IN SUB-CLASSES ***
  def foreachVec(f: ParamVec => Any): Unit

  def output(puts: String => Any): Unit

  // generic functions
  def setUniform_! = foreachVec {
    v: ParamVec =>
      for (pr <- toProbs(v)) {pr.set_!(1); pr.normalize_!}
      for (wt <- toWeights(v)) wt.set_!(0)
  }

  def getValues(v: ParamVec): Array[Double] = {
    for (pr <- toProbs(v)) return pr.getProbs
    for (wt <- toWeights(v)) return wt.getWeights
    throw new Error("Unknown parameter vector: " + v)
  }

  def div_!(scale: Double) = foreachVec {_.div_!(scale)}

  def getVecs: Array[ParamVec] = createArray({add: (ParamVec => Any) => foreachVec(add(_))})

  def add_!(that: AParams, scale: Double) = {
    val thisVecs = this.getVecs
    val thatVecs = that.getVecs
    require(thisVecs.size == thatVecs.size)
    forIndex(thisVecs.size, {i: Int => thisVecs(i).increment_!(thatVecs(i), scale)})
  }

  def numParams = {var count = 0; foreachVec {count += _.size}; count}

  // functions for probabilities only
  def foreachPrVec(f: ProbVec => Any): Unit = foreachVec {v: ParamVec => for (pr <- toProbs(v)) f(pr)}

  def prRandomize_!(random: Random, noise: Double): Unit = {
    foreachPrVec {_.set_! {i => Math.pow(1 + random.nextDouble, noise)}}
  }

  def prAddNoise_!(random: Random, noise: Double): Unit = {
    foreachPrVec {pr => pr.increment_! {i => random.nextDouble * noise}}
  }

  def prDiv_!(scale: Double) = foreachPrVec {_.div_!(scale)}

  def getProbs: Array[ProbVec] = createArray({add: (ProbVec => Any) => foreachPrVec(add(_))})

  def normalize_!(smoothing: Double) = foreachPrVec {_.increment_!(smoothing).normalize_!}

  def normalizeIfTooBig_! = foreachPrVec {_.normalizeIfTooBig_!}

  def numProbs = {var count = 0; foreachPrVec {count += _.size}; count}

  def prAdd_!(that: AParams, scale: Double) = {
    val thisVecs = this.getProbs
    val thatVecs = that.getProbs
    require(thisVecs.size == thatVecs.size)
    forIndex(thisVecs.size, {i: Int => thisVecs(i).increment_!(thatVecs(i), scale)})
  }

  // functions for weights only
  def foreachWtVec(f: WeightVec => Any): Unit = foreachVec {v: ParamVec => for (wt <- toWeights(v)) f(wt)}

  def wtRandomize_!(random: Random, noise: Double): Unit = {
    foreachWtVec {_.set_! {i => (1 - 2 * random.nextDouble) * noise}}
  }

  def wtAddNoise_!(random: Random, noise: Double): Unit = {
    foreachWtVec {wt => wt.increment_! {i => (1 - 2 * random.nextDouble) * noise}}
  }

  def wtDiv_!(scale: Double) = foreachWtVec {_.div_!(scale)}

  def getWts: Array[WeightVec] = createArray({add: (WeightVec => Any) => foreachWtVec(add(_))})

  def getWtVecs: Array[Vector] = getWts.map(_.weights)

  def numWeights = {var count = 0; foreachWtVec {count += _.size}; count}

  def wtTwoNormSquared: Double = wtTwoNormSquared(1.0)

  def wtTwoNormSquared(scale: Double): Double = {
    var normsq = 0.0
    foreachWtVec {normsq += _.twoNormSquared(scale)}
    normsq
  }

  def wtAdd_!(that: AParams, scale: Double) = {
    val thisVecs = this.getWts
    val thatVecs = that.getWts
    require(thisVecs.size == thatVecs.size)
    forIndex(thisVecs.size, {i: Int => thisVecs(i).increment_!(thatVecs(i), scale)})
  }
}