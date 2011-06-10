package cc.refectorie.user.kedarb.dynprog

/**
 * @author kedarb
 * @since Dec 17, 2010
 */

trait AConstraintInferState[Widget, Example <: AExample[Widget], Params <: AParams, ConstraintParams <: AParams]
  extends AInferState[Widget, Example, Params] {
  def constraintParams: ConstraintParams

  def constraintCounts: ConstraintParams
}

trait AConstraintHypergraphInferState[Widget, Example <: AExample[Widget], Params <: AParams, ConstraintParams <: AParams]
  extends AConstraintInferState[Widget, Example, Params, ConstraintParams]
  with AHypergraphInferState[Widget, Example, Params] {
  override def updateCounts = {
    counts.synchronized{
      constraintCounts.synchronized{
        hypergraph.fetchPosteriors(hardInfer)
      }
    }
  }
}
