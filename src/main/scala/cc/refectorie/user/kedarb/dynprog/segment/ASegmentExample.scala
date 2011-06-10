package cc.refectorie.user.kedarb.dynprog.segment

import cc.refectorie.user.kedarb.dynprog.AExample

/**
 * @author kedarb
 * @since 12/26/10
 */

trait ASegmentExample extends AExample[Segmentation] {
  def words: Array[Int]

  def numTokens = words.length
}