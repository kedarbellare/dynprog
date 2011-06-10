package cc.refectorie.user.kedarb.dynprog

import actors.Actor
import actors.Actor._
import utils.Utils._

/**
 * @author kedarb
 * @since 2/3/11
 */

abstract class ABatchDiscParallelLearner[Widget, Example <: AExample[Widget], Params <: AParams,
InferState <: AInferState[Widget, Example, Params],
Model <: AModel[Widget, Example, Params, InferState]]
  extends ABatchDiscLearner[Widget, Example, Params, InferState, Model] {

  // messages to pass to slaves from master
  case class Task(ex: Example, exWt: Double)

  case object Result

  class ConstraintSlave(val id: Int) extends Actor {
    val counts = model.newParams

    def act(): Unit = {
      loop{
        react{
          case Task(ex, exWt) =>
            processExampleConstraints(ex, 1, counts, exWt, false, true)
          case Result =>
            reply(InferStatesOutput(new ProbStats, counts))
            exit
        }
      }
    }
  }

  class ExpectationSlave(val id: Int) extends Actor {
    val counts = model.newParams
    val stats = new ProbStats

    def act(): Unit = {
      loop{
        react{
          case Task(ex, exWt) =>
            val constraintStats = processExampleConstraints(ex, 0, counts, -exWt, false, true, false)
            val expectedStats = processExampleExpectations(ex, 1, counts, exWt, false, true)
            stats += constraintStats.stats
            stats += expectedStats.stats
          case Result =>
            reply(InferStatesOutput(stats, counts))
            exit
        }
      }
    }
  }

  override def processExamplesConstraints = {
    if (lopts.numThreads > 1) {
      // create as many slaves as threads
      val slaves = mapIndex(lopts.numThreads, {
        id: Int => val slave = new ConstraintSlave(id)
        slave.start
        slave
      })
      // give each slave a slice of labeled examples
      var numEx = 0
      getLabeledExampleIterator.foreach{
        ex: Example => val id = numEx % lopts.numThreads
        slaves(id) ! Task(ex, lopts.labeledWeight)
        numEx += 1
      }
      // sum up constraints from slaves
      val constraints = model.newParams
      for (slave <- slaves) slave !? Result match {
        case infersOutput: InferStatesOutput =>
          constraints.add_!(infersOutput.counts, 1)
      }
      InferStatesOutput(new ProbStats, constraints)
    } else {
      super.processExamplesConstraints
    }
  }

  override def processExamplesExpectations = {
    if (lopts.numThreads > 1) {
      // create as many slaves as threads
      val slaves = mapIndex(lopts.numThreads, {
        id: Int => val slave = new ExpectationSlave(id)
        slave.start
        slave
      })
      // give each slave a slice of labeled examples
      var numEx = 0
      getLabeledExampleIterator.foreach{
        ex: Example => val id = numEx % lopts.numThreads
        slaves(id) ! Task(ex, lopts.labeledWeight)
        numEx += 1
      }
      // sum up constraints from slaves
      val expectations = model.newParams
      val stats = new ProbStats
      for (slave <- slaves) slave !? Result match {
        case infersOutput: InferStatesOutput =>
          expectations.add_!(infersOutput.counts, 1)
          stats += infersOutput.stats
      }
      InferStatesOutput(stats, expectations)
    } else {
      super.processExamplesExpectations
    }
  }
}

trait ABatchGenParallelLearner[Widget, Example <: AExample[Widget], Params <: AParams,
InferState <: AInferState[Widget, Example, Params],
Model <: AModel[Widget, Example, Params, InferState]]
  extends ABatchGenLearner[Widget, Example, Params, InferState, Model] {

  // messages to pass to slaves from master
  case class LabeledTask(ex: Example)

  case class UnlabeledTask(ex: Example)

  case object Result

  class ExpectationSlave(val id: Int) extends Actor {
    val counts = model.newParams
    val stats = new ProbStats

    def act(): Unit = {
      loop{
        react{
          case LabeledTask(ex) =>
            stats += processLabeledExample(ex, 1, counts).stats
          case UnlabeledTask(ex) =>
            stats += processUnlabeledExample(ex, 1, counts).stats
          case Result =>
            reply(InferStatesOutput(stats, counts))
            exit
        }
      }
    }
  }

  override def processExamplesExpectations = {
    if (lopts.numThreads > 1) {
      // create as many slaves as threads
      val slaves = mapIndex(lopts.numThreads, {
        id: Int => val slave = new ExpectationSlave(id)
        slave.start
        slave
      })
      // give each slave a slice of labeled examples
      var numEx = 0
      getLabeledExampleIterator.foreach{
        ex: Example => val id = numEx % lopts.numThreads
        slaves(id) ! LabeledTask(ex)
        numEx += 1
      }
      getUnlabeledExampleIterator.foreach{
        ex: Example => val id = numEx % lopts.numThreads
        slaves(id) ! UnlabeledTask(ex)
        numEx += 1
      }
      // sum up constraints from slaves
      val expectations = model.newParams
      val stats = new ProbStats
      for (slave <- slaves) slave !? Result match {
        case infersOutput: InferStatesOutput =>
          expectations.add_!(infersOutput.counts, 1)
          stats += infersOutput.stats
      }
      InferStatesOutput(stats, expectations)
    } else {
      super.processExamplesExpectations
    }
  }
}
