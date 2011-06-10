package cc.refectorie.user.kedarb.dynprog

import org.apache.log4j.Logger
import utils.Utils._
import collection.mutable.ArrayBuffer
import la.ArrayFromVectors
import optimization.gradientBasedMethods._
import optimization.linesearch._
import optimization.gradientBasedMethods.stats._
import optimization.stopCriteria._
import java.io.{FileOutputStream, PrintStream, File}

/**
 * @author kedarb
 * @since Nov 30, 2010
 */

trait AModelLearner[Widget, Example <: AExample[Widget], Params <: AParams,
InferState <: AInferState[Widget, Example, Params],
Model <: AModel[Widget, Example, Params, InferState]] {
  // logging
  def logger: Logger

  def info(str: String, args: Object*): Unit = info(String.format(str, args: _*))

  def info(msg: String) = logger.info(msg)

  def debug(str: String, args: Object*): Unit = debug(String.format(str, args: _*))

  def debug(msg: String) = logger.debug(msg)

  def error(str: String, args: Object*): Unit = error(String.format(str, args: _*))

  def error(msg: String) = logger.error(msg)

  // model/params
  def model: Model

  def params: Params = model.params

  def params_=(x: Params) = {
    model.params = x
  }

  def lopts: LearnOptions

  // final values
  val hardInfer = lopts.hardInfer

  // learner variables
  var temperature = lopts.initTemperature
  var iteration = 0
  var converged = false
  var suffStatsScale = 1.0
  val objectives = new ArrayBuffer[Double]

  // class for inference output of a single example
  case class InferStateOutput(stats: ProbStats, bestWidget: Widget)

  // class for output of multiple examples
  case class InferStatesOutput(stats: ProbStats, counts: Params)

  def processExampleConstraints(ex: Example, stepSize: Double, counts: Params,
                                exWt: Double, useProbs: Boolean, useWts: Boolean, doUpdate: Boolean = true) = {
    // update with respect to the true counts
    val trueInferState = model.newInferState(ex, counts, InferSpec(iteration, temperature,
      hardInfer, // hard infer
      true, // true infer
      false, // best update
      false, // sample update
      useProbs, useWts, // what parameters to use
      suffStatsScale, stepSize * exWt))
    if (doUpdate) trueInferState.updateCounts // update true counts
    InferStateOutput(trueInferState.stats * exWt, trueInferState.bestWidget)
  }

  def processExampleExpectations(ex: Example, stepSize: Double, counts: Params,
                                 exWt: Double, useProbs: Boolean, useWts: Boolean, doUpdate: Boolean = true) = {
    // update with respect to expected counts
    val expectedInferState = model.newInferState(ex, counts, InferSpec(iteration, temperature,
      hardInfer,
      false, // true infer
      false, // best update
      false, // sample update
      useProbs, useWts, suffStatsScale, stepSize * exWt))
    if (doUpdate) expectedInferState.updateCounts // update expected counts
    InferStateOutput(expectedInferState.stats * exWt, expectedInferState.bestWidget)
  }

  def processExampleBest(ex: Example) = {
    // calculate the best decoding
    val bestInferState = model.newInferState(ex, params, InferSpec(iteration, temperature,
      true, // hard infer
      false, true, false, // true infer, best update, sample update
      true, true, // probs, wts
      suffStatsScale, 0))
    // no count update
    InferStateOutput(bestInferState.stats, bestInferState.bestWidget)
  }

  def learn(name: String): Unit = {
    iteration = 0
    info("")
    info("Training %s (online=%s), #iterations=%s", name, fmt(lopts.online), fmt(lopts.numIters))
    while (iteration < lopts.numIters && !converged) {
      temperature = {
        if (lopts.numIters == 1) lopts.initTemperature
        else lopts.initTemperature +
          (lopts.finalTemperature - lopts.initTemperature) * iteration / (lopts.numIters - 1)
      }
      info("")
      info("Iteration %s/%s, temperature=%s, suffStatsScale=%s", fmt(iteration + 1), fmt(lopts.numIters),
        fmt(temperature), fmt(suffStatsScale))

      // run a single iteration of learning
      learnIteration
      info("After iteration %s, objective=%s", fmt(iteration + 1), fmt(objectives.last))

      // run evaluators
      iteration += 1
      if (iteration % lopts.outputIterFreq == 0) {
        // should output
        if (lopts.outputParams) outputParams
        if (lopts.outputFullPred) outputPredictions
        if (lopts.outputEvaluation) {
          runEvaluators;
          info("")
        }
      }

      converged = checkConvergence
    }

    params.div_!(suffStatsScale) // scale parameters finally
    // should output finally
    info("")
    if (lopts.outputParams) outputParams
    if (lopts.outputFullPred) outputPredictions
    if (lopts.outputEvaluation) {
      runEvaluators;
      info("")
    }
  }

  def checkConvergence: Boolean = {
    if (converged) {
      info("Learning iteration %s converged; saying converged", fmt(iteration + 1))
      true // has already been set
    }
    else if (objectives.length < 2) false
    else {
      val currObj = objectives(objectives.size - 1)
      val prevObj = objectives(objectives.size - 2)
      val diffObj = math.abs(currObj - prevObj)
      lopts.convergenceType match {
        case LearnOptions.ConvergeType.objDiff => {
          val result = diffObj <= lopts.precision
          if (result) info("Objective difference " + diffObj + " <= " + lopts.precision + "; saying converged")
          result
        }
        case LearnOptions.ConvergeType.objRelDiff => {
          val result = diffObj <= (lopts.precision * math.abs(prevObj))
          if (result) info("Objective relative difference " + diffObj + " <= " +
            (lopts.precision * math.abs(prevObj)) + "; saying converged")
          result
        }
        case _ => throw fail("Convergence type " + lopts.convergenceType + " not implemented!")
      }
    }
  }

  def newEvaluators(name: String): Seq[APerformance[Widget]] = new Array[APerformance[Widget]](0)

  // for labeled and unlabeled examples we should probably shuffle them
  def getLabeledExampleIterator: Iterator[Example]

  def getUnlabeledExampleIterator: Iterator[Example]

  def getValidationExampleIterator: Iterator[Example]

  def getTestExampleIterator: Iterator[Example]

  def runEvaluators(name: String, exIter: Iterator[Example]): Unit = {
    val evals = newEvaluators(name)
    var numEx = 0
    if (lopts.numThreads > 1) {
      parallel_iter_foreach(lopts.numThreads, exIter, {
        (i: Int, ex: Example, log: Boolean) =>
          val predWidget = processExampleBest(ex).bestWidget
          evals.synchronized {
            evals.foreach(_.add(ex.trueWidget, predWidget))
            numEx += 1
          }
      })
    } else {
      exIter.foreach {
        ex: Example =>
          val predWidget = processExampleBest(ex).bestWidget
          evals.foreach(_.add(ex.trueWidget, predWidget))
          numEx += 1
      }
    }
    if (numEx > 0) evals.foreach(eval => eval.output(info(_)))
  }

  def outputPredictions(filename: String, exIter: Iterator[Example], gold: Boolean = false): Unit = {
    // create output directory if needed
    new File(lopts.outputDir).mkdirs // assumes this directory is created
    val out = new PrintStream(new FileOutputStream(lopts.outputDir + "/" + filename))
    exIter.foreach {
      ex: Example =>
        val widget = if (gold) ex.trueWidget else processExampleBest(ex).bestWidget
        out.println(model.widgetToFullString(ex, widget))
        out.println
    }
    out.close
  }

  def outputParams: Unit = {
    new File(lopts.outputDir).mkdirs
    val out = new PrintStream(new FileOutputStream(lopts.outputDir + "/params." + iteration))
    params.output(out.println(_))
    out.close
  }

  def runEvaluators: Unit = {
    runEvaluators("Validation", getValidationExampleIterator)
    runEvaluators("Test", getTestExampleIterator)
  }

  def outputPredictions: Unit = {
    outputPredictions("output.dev.gold." + iteration, getValidationExampleIterator, true)
    outputPredictions("output.dev.guess." + iteration, getValidationExampleIterator, false)
    outputPredictions("output.test.gold." + iteration, getTestExampleIterator, true)
    outputPredictions("output.test.guess." + iteration, getTestExampleIterator, false)
  }

  // method to implement
  def processLabeledExample(ex: Example, stepSize: Double, counts: Params): InferStateOutput

  def processUnlabeledExample(ex: Example, stepSize: Double, counts: Params): InferStateOutput

  // batch methods to implement
  def processExamplesConstraints: InferStatesOutput

  def processExamplesExpectations: InferStatesOutput

  def learnIteration: Unit

  // single iteration of learning
}

/**
 * Generative model learner
 */
trait AGenerativeModelLearner[Widget, Example <: AExample[Widget], Params <: AParams,
InferState <: AInferState[Widget, Example, Params],
Model <: AModel[Widget, Example, Params, InferState]]
  extends AModelLearner[Widget, Example, Params, InferState, Model] {
  def processLabeledExample(ex: Example, stepSize: Double, counts: Params) = {
    processExampleConstraints(ex, stepSize, counts, lopts.labeledWeight, true, false)
  }

  def processUnlabeledExample(ex: Example, stepSize: Double, counts: Params) = {
    if (lopts.unlabeledWeight != 0) processExampleExpectations(ex, stepSize, counts, lopts.unlabeledWeight, true, false)
    else InferStateOutput(new ProbStats, ex.trueWidget)
  }

  // constraints are not important in generative model; only model expectations
  def processExamplesConstraints: InferStatesOutput = throw fail("Constraints not implemented for generative models!")

  def processExamplesExpectations = {
    val counts = model.newParams
    val stats = new ProbStats

    // E-step: gather expectations
    if (lopts.numThreads > 1) {
      parallel_iter_foreach(lopts.numThreads, getLabeledExampleIterator, {
        (i: Int, ex: Example, log: Boolean) =>
          val inferOutput = processLabeledExample(ex, 1, counts)
          stats.synchronized(stats += inferOutput.stats)
      })
      parallel_iter_foreach(lopts.numThreads, getUnlabeledExampleIterator, {
        (i: Int, ex: Example, log: Boolean) =>
          val inferOutput = processUnlabeledExample(ex, 1, counts)
          stats.synchronized(stats += inferOutput.stats)
      })
    } else {
      getLabeledExampleIterator.foreach(ex => stats += processLabeledExample(ex, 1, counts).stats)
      getUnlabeledExampleIterator.foreach(ex => stats += processUnlabeledExample(ex, 1, counts).stats)
    }
    InferStatesOutput(stats, counts)
  }
}

/**
 * Discriminative model learner
 */
trait ADiscriminativeModelLearner[Widget, Example <: AExample[Widget], Params <: AParams,
InferState <: AInferState[Widget, Example, Params],
Model <: AModel[Widget, Example, Params, InferState]]
  extends AModelLearner[Widget, Example, Params, InferState, Model] {
  def processLabeledExample(ex: Example, stepSize: Double, counts: Params) = {
    val trueInferState = processExampleConstraints(ex, stepSize, counts, lopts.labeledWeight, false, true)
    val expectedInferState = processExampleExpectations(ex, stepSize, counts, -lopts.labeledWeight, false, true)
    InferStateOutput(trueInferState.stats + expectedInferState.stats, expectedInferState.bestWidget)
  }

  def processUnlabeledExample(ex: Example, stepSize: Double, counts: Params) = {
    InferStateOutput(new ProbStats, ex.trueWidget)
  }

  def processExamplesConstraints = {
    val counts = model.newParams

    // C-step: gather constraint counts for labeled examples only
    if (lopts.numThreads > 1) {
      parallel_iter_foreach(lopts.numThreads, getLabeledExampleIterator, {
        (i: Int, ex: Example, log: Boolean) =>
          processExampleConstraints(ex, 1, counts, lopts.labeledWeight, false, true)
      })
    } else {
      getLabeledExampleIterator.foreach {
        ex => processExampleConstraints(ex, 1, counts, lopts.labeledWeight, false, true)
      }
    }

    InferStatesOutput(new ProbStats, counts)
  }

  def processExamplesExpectations = {
    val counts = model.newParams
    val stats = new ProbStats

    if (lopts.numThreads > 1) {
      parallel_iter_foreach(lopts.numThreads, getLabeledExampleIterator, {
        (i: Int, ex: Example, log: Boolean) =>
          val constraintStats = processExampleConstraints(ex, 0, counts, -lopts.labeledWeight, false, true, false)
          val expectedStats = processExampleExpectations(ex, 1, counts, lopts.labeledWeight, false, true)
          stats.synchronized {
            stats += constraintStats.stats
            stats += expectedStats.stats
          }
      })
    } else {
      getLabeledExampleIterator.foreach {
        ex =>
          stats += processExampleConstraints(ex, 0, counts, -lopts.labeledWeight, false, true, false).stats
          stats += processExampleExpectations(ex, 1, counts, lopts.labeledWeight, false, true).stats
      }
    }

    InferStatesOutput(stats, counts)
  }
}

/**
 * Online learner
 */
trait AOnlineLearner[Widget, Example <: AExample[Widget], Params <: AParams,
InferState <: AInferState[Widget, Example, Params],
Model <: AModel[Widget, Example, Params, InferState]]
  extends AModelLearner[Widget, Example, Params, InferState, Model] {
  var numUpdates = 0

  def learnIteration: Unit = {
    def scaleParamsIfTooBig: Unit = {
      if (suffStatsScale > 1e100) {
        params.div_!(suffStatsScale)
        suffStatsScale = 1
      }
    }

    def stepSize = {
      val alpha = 1.0 / math.pow(numUpdates + lopts.stepSizeOffset, lopts.stepSizeReductionPower)
      if (lopts.convexCombUpdate) {
        require(alpha < 1)
        suffStatsScale /= (1 - alpha)
      }
      alpha * suffStatsScale
    }

    val stats = new ProbStats
    // process labeled examples first
    getLabeledExampleIterator.foreach {
      ex => scaleParamsIfTooBig
      stats += processLabeledExample(ex, stepSize, params).stats
      numUpdates += 1
    }

    // then unlabeled examples
    getUnlabeledExampleIterator.foreach {
      ex => scaleParamsIfTooBig
      stats += processUnlabeledExample(ex, stepSize, params).stats
      numUpdates += 1
    }

    objectives.append(stats.logZ)
    info("Objective = " + stats.logZ + " (iteration " + iteration + ")")
  }
}

trait ABatchGenLearner[Widget, Example <: AExample[Widget], Params <: AParams,
InferState <: AInferState[Widget, Example, Params],
Model <: AModel[Widget, Example, Params, InferState]]
  extends AGenerativeModelLearner[Widget, Example, Params, InferState, Model] {
  def learnIteration: Unit = {
    // E-step
    val infersOutput = processExamplesExpectations

    // M-step
    params = infersOutput.counts
    params.normalize_!(lopts.smoothing)

    objectives.append(infersOutput.stats.logZ)
    info("Objective = " + infersOutput.stats.logZ + " (iteration " + iteration + ")")
  }
}

abstract class ABatchDiscLearner[Widget, Example <: AExample[Widget], Params <: AParams,
InferState <: AInferState[Widget, Example, Params],
Model <: AModel[Widget, Example, Params, InferState]]
  extends Objective with ADiscriminativeModelLearner[Widget, Example, Params, InferState, Model] {
  val paramsArrayFromVectors = new ArrayFromVectors(params.getWtVecs)
  var constraintsOpt: Option[Params] = None
  var objectiveValue = Double.NaN
  var updateConstraintsAfterParamUpdate = false
  val invVariance = 1.0 / lopts.gaussianPriorVariance

  // set parameters and gradient
  parameters = new Array[Double](paramsArrayFromVectors.vectorsArraySize)
  paramsArrayFromVectors.getVectorsInArray(parameters)
  gradient = new Array[Double](paramsArrayFromVectors.vectorsArraySize)

  override def getParameter(index: Int) = parameters(index)

  override def setParameter(index: Int, value: Double) = {
    updateCalls += 1
    objectiveValue = Double.NaN
    parameters(index) = value
    if (updateConstraintsAfterParamUpdate) constraintsOpt = None
  }

  override def getParameters = parameters

  override def setParameters(params: Array[Double]) = {
    updateCalls += 1
    objectiveValue = Double.NaN
    require(params.length == getNumParameters)
    Array.copy(params, 0, parameters, 0, params.length)
    if (updateConstraintsAfterParamUpdate) constraintsOpt = None
  }

  override def setInitialParameters(params: Array[Double]) = setParameters(params)

  def resetConstraints: Unit = {
    // set parameters as they may have changed
    paramsArrayFromVectors.setVectorsFromArray(parameters)
    constraintsOpt = Some(processExamplesConstraints.counts)
  }

  def updateValueAndGradient: Unit = {
    if (constraintsOpt == None) resetConstraints
    // set parameters as they may have changed
    paramsArrayFromVectors.setVectorsFromArray(parameters)
    // calculate expectations
    val infersOutput = processExamplesExpectations
    val expectations = infersOutput.counts
    objectiveValue = infersOutput.stats.logZ
    java.util.Arrays.fill(gradient, 0.0)
    // add weights to objective
    objectiveValue += 0.5 * params.wtTwoNormSquared * invVariance
    // compute gradient
    expectations.add_!(constraintsOpt.get, -1.0)
    expectations.add_!(params, invVariance)
    // move expectations to gradient
    new ArrayFromVectors(expectations.getWtVecs).getVectorsInArray(gradient)
  }


  def getValue = {
    if (objectiveValue.isNaN) {
      functionCalls += 1
      updateValueAndGradient
      info("getValue = " + objectiveValue)
    }
    objectiveValue
  }

  def getGradient = {
    if (objectiveValue.isNaN) {
      gradientCalls += 1
      updateValueAndGradient
    }
    gradient
  }

  override def toString = "objective = " + objectiveValue

  val stats = new OptimizerStats {
    override def collectIterationStats(optimizer: Optimizer, objective: Objective) = {
      super.collectIterationStats(optimizer, objective)

      // increment iteration
      iteration += 1
      temperature = {
        if (lopts.numIters == 1) lopts.initTemperature
        else lopts.initTemperature +
          (lopts.finalTemperature - lopts.initTemperature) * iteration / (lopts.numIters - 1)
      }
      info("After iteration %s/%s, temperature=%s, suffStatsScale=%s", fmt(iteration), fmt(lopts.numIters),
        fmt(temperature), fmt(suffStatsScale))

      // run evaluators
      if (iteration % lopts.outputIterFreq == 0) {
        // should output
        if (lopts.outputParams) outputParams
        if (lopts.outputFullPred) outputPredictions
        if (lopts.outputEvaluation) {
          runEvaluators;
          info("")
        }
      }
    }
  }
  // val ls = new WolfRuleLineSearch(new GenericPickFirstStep(100), 1e-5, 0.9, 100)
  var ls: LineSearchMethod = new ArmijoLineSearchMinimization
  var stop: StopingCriteria = new GradientL2Norm(lopts.precision)
  var optimizer: Optimizer = new LBFGS(ls, 4)

  def learnIteration = throw fail("Not implemented for direct optimization!")

  override def learn(name: String) = {
    info("")
    info("Training %s (online=%s), #iterations=%s", name, fmt(lopts.online), fmt(lopts.numIters))
    stats.reset
    optimizer.setMaxIterations(lopts.numIters)
    converged = optimizer.optimize(this, stats, stop)
    info("Ended optimization\n" + stats.prettyPrint(1))
    info("Solution: " + this.toString)
    iteration = optimizer.getCurrentIteration

    // should output finally
    info("")
    if (lopts.outputParams) outputParams
    if (lopts.outputFullPred) outputPredictions
    if (lopts.outputEvaluation) {
      runEvaluators;
      info("")
    }

    if (converged) info("Ended optimization in " + iteration)
    else info("Failed to optimize")
  }
}