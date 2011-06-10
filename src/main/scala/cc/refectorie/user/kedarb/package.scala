package cc.refectorie.user.kedarb

import java.util.Random

package object dynprog {
  // seed for sampling during inference
  var sampleSeed: Long = 0
  implicit lazy val sampleRandom = if (sampleSeed < 0) new Random() else new Random(sampleSeed)
  // whether to compute quantities such as entropy etc. during inference
  var computeELogZEntropy = false
}