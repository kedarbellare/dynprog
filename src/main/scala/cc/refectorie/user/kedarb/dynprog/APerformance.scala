package cc.refectorie.user.kedarb.dynprog

import utils.Utils._

/**
 * @author kedarb
 * @since Sep 16, 2010
 */
class ProbStats(
        // q(z|x) \propto p(x,z)^{1/temperature}
        // z* = argmax_z q(z|x)
        var logZ: Double, // \sum_z p(x, z)^{1/temperature}
        var logVZ: Double, // p(x, z*)
        var logCZ: Double, // q(z* | x)
        var elogZ: Double, // \E_q \log p(x, z)
        var entropy: Double, // H(q)
        var objective: Double) { // objective = \E_q p(z | x) + T H(q)
  def this() = this (0, 0, 0, 0, 0, 0)

  def +=(that: ProbStats) = {
    logZ += that.logZ
    logVZ += that.logVZ
    logCZ += that.logCZ
    elogZ += that.elogZ
    entropy += that.entropy
    objective += that.objective
  }

  def -=(that: ProbStats) = {
    logZ -= that.logZ
    logVZ -= that.logVZ
    logCZ -= that.logCZ
    elogZ -= that.elogZ
    entropy -= that.entropy
    objective -= that.objective
  }

  def *=(v: Double) = {
    logZ *= v
    logVZ *= v
    logCZ *= v
    elogZ *= v
    entropy *= v
    objective *= v
  }

  def *(v: Double): ProbStats = {
    val result = new ProbStats
    result += this
    result *= v
    result
  }

  def +(that: ProbStats): ProbStats = {
    val result = new ProbStats
    result += this
    result += that
    result
  }

  def -(that: ProbStats): ProbStats = {
    val result = new ProbStats
    result += this
    result -= that
    result
  }

  def foreachStat(f: ((String, String) => Any)) = {
    f("logZ", fmt(logZ))
    if (!logVZ.isNaN) f("logVZ", fmt(logVZ))
    if (!logCZ.isNaN) f("logCZ", fmt(logCZ))
    if (!elogZ.isNaN) f("elogZ", fmt(elogZ))
    if (!entropy.isNaN) f("entropy", fmt(entropy))
    if (!objective.isNaN) f("objective", fmt(objective))
  }

  def summary: String = createArray({
    add: (String => Any) =>
      foreachStat({(a: String, b: String) => add(a + " = " + b)})
  }).mkString(", ")
}

trait APerformance[Widget] {
  def countsToPRF1(predC: Int, trueC: Int, corrC: Int): (Double, Double, Double) = {
    val pr = if (predC == 0) 0.0 else (1.0 * corrC) / predC
    val re = if (trueC == 0) 0.0 else (1.0 * corrC) / trueC
    val f1 = if (pr == 0 && re == 0) 0.0 else (2 * pr * re) / (pr + re)
    (pr, re, f1)
  }

  def add(trueWidget: Widget, predWidget: Widget): Unit

  def accuracy: Double

  def output(puts: (String => Any)): Unit
}

