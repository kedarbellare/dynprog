package cc.refectorie.user.kedarb.dynprog.fst

import cc.refectorie.user.kedarb.dynprog.AExample

/**
 * @author kedarb
 * @since 12/26/10
 */

trait ATaggingExample[Features] extends AExample[LblSeq] {
  def words: Array[Int]

  def features: Array[Features]

  def numTokens = features.length
}