package cc.refectorie.user.kedarb.dynprog

import la.Vector
import types.{FtrVec, ParamUtils, ParamVec, Hypergraph}
import ParamUtils._

/**
 * @author kedarb
 * @since Sep 21, 2010
 */

case class InferSpec(iter: Int, // inference iteration
                     temperature: Double, // temperature if performing annealing
                     hardInfer: Boolean, trueInfer: Boolean, // type of inference
                     bestUpdate: Boolean, sampleUpdate: Boolean, // type of update for getting best widget
                     useProbs: Boolean, // whether to use probability parameters during inference
                     useWts: Boolean, // whether to use weight parameters during inference
                     var suffStatsScale: Double, // scaling applied to weights (L2 regularization during stochastic gradient)
                     var stepSize: Double) // scaling for update (again for SGD)

/**
 * Generic inferencer: can implement sum/max-product or any MAP inference algorithm
 */
trait AInferState[Widget, Example <: AExample[Widget], Params <: AParams] {
  val temperature = ispec.temperature
  val hardInfer = ispec.hardInfer
  val trueInfer = ispec.trueInfer
  val useProbs = ispec.useProbs
  val useWts = ispec.useWts
  val suffStatsScale = ispec.suffStatsScale
  val stepSize = ispec.stepSize

  // methods to be defined
  def ex: Example

  def params: Params

  def counts: Params

  def ispec: InferSpec

  // methods that may be needed
  def log(x: Double) = math.log(x)

  def exp(x: Double) = math.exp(x)

  // generic scoring functions
  def score(v: ParamVec, i: Int): Double = {
    if (useProbs) for (pr <- toProbs(v)) return (pr.logpr(i) / temperature)
    if (useWts) for (wt <- toWeights(v)) return (wt(i) / (temperature * suffStatsScale))
    0.0
  }

  def score(v: ParamVec, f: FtrVec): Double = {
    if (useProbs) for (pr <- toProbs(v)) return (pr.logdot(f) / temperature)
    if (useWts) for (wt <- toWeights(v)) return (wt.dot(f) / (temperature * suffStatsScale))
    0.0
  }

  def score(v: ParamVec, f: Vector): Double = {
    if (useProbs) for (pr <- toProbs(v)) return (pr.logdot(f) / temperature)
    if (useWts) for (wt <- toWeights(v)) return (wt.dot(f) / (temperature * suffStatsScale))
    0.0
  }

  // generic update functions
  def update(v: ParamVec, i: Int, x: Double): Unit = {
    val incr = x * stepSize
    if (useProbs) for (pr <- toProbs(v)) {
      if (incr < 0) throw new RuntimeException("Updating probs with -ve update=%.3f, scale=%.3f".format(x, stepSize))
      if (incr != 0) pr.increment_!(i, incr)
    }
    if (useWts) for (wt <- toWeights(v)) if (incr != 0) wt.increment_!(i, incr)
  }

  def update(v: ParamVec, f: Vector, x: Double): Unit = {
    val incr = x * stepSize
    if (useProbs) for (pr <- toProbs(v)) {
      if (incr < 0) throw new RuntimeException("Updating probs with -ve update=%.3f, scale=%.3f".format(x, stepSize))
      if (incr != 0) pr.increment_!(f, incr)
    }
    if (useWts) for (wt <- toWeights(v)) if (incr != 0) wt.increment_!(f, incr)
  }

  def update(v: ParamVec, f: FtrVec, x: Double): Unit = {
    val incr = x * stepSize
    if (useProbs) for (pr <- toProbs(v)) {
      if (incr < 0) throw new RuntimeException("Updating probs with -ve update=%.3f, scale=%.3f".format(x, stepSize))
      if (incr != 0) pr.increment_!(f, incr)
    }
    if (useWts) for (wt <- toWeights(v)) if (incr != 0) wt.increment_!(f, incr)
  }

  // When an infer state is created, the following quantities should be available
  def logZ: Double

  def logVZ: Double

  // q(z^*|x) = p(z^*, x)/p(x) only when temperature = 1
  def logCZ = if (temperature == 1) logVZ - logZ else Double.NaN

  def elogZ: Double

  def entropy: Double

  def objective = elogZ + temperature * entropy

  def stats = new ProbStats(logZ, logVZ, logCZ, elogZ, entropy, objective)

  def bestWidget: Widget

  def complexity: Int

  // Update the parameters
  def updateCounts: Unit
}

/**
 * Inferencer that specifically uses dynamic programming.
 */
trait AHypergraphInferState[Widget, Example <: AExample[Widget], Params <: AParams]
  extends AInferState[Widget, Example, Params] {
  type Info = Hypergraph.HyperedgeInfo[Widget]

  // Soft inference
  val hypergraph = new Hypergraph[Widget]
  // 1) create hypergraph
  createHypergraph(hypergraph)
  // 2) update posteriors
  hypergraph.computePosteriors(hardInfer)
  // 3) (optionally) compute eLogZ and entropy
  if (computeELogZEntropy) hypergraph.computeELogZEntropy(hardInfer)
  val logZ = hypergraph.getLogZ
  val elogZ = hypergraph.getELogZ * temperature
  val entropy = hypergraph.getEntropy

  // infer widget
  val (bestWidget, logVZ) = {
    if (ispec.bestUpdate) {
      val result = hypergraph.fetchBestHyperpath(newWidget)
      (result.widget, result.logWeight)
    } else if (ispec.sampleUpdate) {
      val result = hypergraph.fetchSampleHyperpath(sampleRandom, newWidget)
      (result.widget, result.logWeight)
    } else {
      (newWidget, Double.NaN)
    }
  }

  def updateCounts {
    counts.synchronized{
      hypergraph.fetchPosteriors(hardInfer)
    }
  }

  // Main functions to override: specifies the entire model
  def createHypergraph(H: Hypergraph[Widget]): Unit

  def newWidget: Widget

  def complexity = hypergraph.numNodes
}

