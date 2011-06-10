package cc.refectorie.user.kedarb.dynprog.types

import cc.refectorie.user.kedarb.dynprog.la._
import cc.refectorie.user.kedarb.dynprog.utils.Utils._
import cc.refectorie.user.kedarb.dynprog.utils.VectorUtils._

/**
 * Parameter vector from which probability and weight vectors are produced.
 *
 * @author kedarb
 * @since Nov 29, 2010
 */

object ParamUtils {
  def toProbs(v: ParamVec): Option[ProbVec] = v match {
    case pr: ProbVec => Some(pr); case _ => None
  }

  def toWeights(v: ParamVec): Option[WeightVec] = v match {
    case wt: WeightVec => Some(wt); case _ => None
  }

  def newPrVec(dense: Boolean, len: Int): ParamVec = if (dense) DenseProbVec(len) else SparseProbVec(len)

  def newPrVecArray(dense: Boolean, size: Int, len: Int) = mapIndex(size, {
    i => newPrVec(dense, len)
  })

  def newPrVecArray2(dense: Boolean, size1: Int, size2: Int, len: Int) = mapIndex(size1, {
    i => newPrVecArray(dense, size2, len)
  })

  def newWtVec(dense: Boolean, len: Int): ParamVec = if (dense) DenseWeightVec(len) else SparseWeightVec(len)

  def newWtVecArray(dense: Boolean, size: Int, len: Int) = mapIndex(size, {
    i => newWtVec(dense, len)
  })

  def newWtVecArray2(dense: Boolean, size1: Int, size2: Int, len: Int) = mapIndex(size1, {
    i => newWtVecArray(dense, size2, len)
  })

  // serialization code
  // param types
  val PROBS = 0
  val WTS = 1

  def serializeProbs(puts: (String => Any), pr: ProbVec) = {
    puts(pr.sum.toString)
    serializeVector(puts, pr.counts)
  }

  def deserializeProbs(gets: => String): ProbVec = {
    val sum = gets.toDouble
    val counts = deserializeVector(gets)
    new ProbVec(counts, sum)
  }

  def serializeWts(puts: (String => Any), wt: WeightVec) = serializeVector(puts, wt.weights)

  def deserializeWts(gets: => String): WeightVec = new WeightVec(deserializeVector(gets))

  def serializeParams(puts: (String => Any), v: ParamVec) = {
    v match {
      case pr: ProbVec => {
        puts(fmt(PROBS))
        serializeProbs(puts, pr)
      }
      case wt: WeightVec => {
        puts(fmt(WTS))
        serializeWts(puts, wt)
      }
    }
  }

  def deserializeParams(gets: => String): ParamVec = {
    val pvtype = gets.toInt
    pvtype match {
      case PROBS => deserializeProbs(gets)
      case WTS => deserializeWts(gets)
    }
  }

  def serializeParamsArray(puts: (String => Any), v: Array[ParamVec]) = {
    puts(fmt(v.length))
    v.foreach(serializeParams(puts, _))
  }

  def deserializeParamsArray(gets: => String): Array[ParamVec] = {
    val n = gets.toInt
    mapIndex(n, {
      _: Int => deserializeParams(gets)
    })
  }

  def serializeParamsArray2(puts: (String => Any), v: Array[Array[ParamVec]]) = {
    puts(fmt(v.length))
    v.foreach(serializeParamsArray(puts, _))
  }

  def deserializeParamsArray2(gets: => String): Array[Array[ParamVec]] = {
    val n = gets.toInt
    mapIndex(n, {
      _: Int => deserializeParamsArray(gets)
    })
  }

  def serializeParamsArray3(puts: (String => Any), v: Array[Array[Array[ParamVec]]]) = {
    puts(fmt(v.length))
    v.foreach(serializeParamsArray2(puts, _))
  }

  def deserializeParamsArray3(gets: => String): Array[Array[Array[ParamVec]]] = {
    val n = gets.toInt
    mapIndex(n, {
      _: Int => deserializeParamsArray2(gets)
    })
  }
}

import ParamUtils._

trait ParamVec {
  def set_!(x: Double): ParamVec

  def set_!(f: Int => Double): ParamVec

  def increment_!(i: Int, x: Double): ParamVec

  def increment_!(x: Double): ParamVec

  def increment_!(vec: ParamVec, x: Double): ParamVec

  def increment_!(vec: Vector, x: Double): ParamVec

  def increment_!(fvec: FtrVec, x: Double): ParamVec

  def increment_!(f: Int => Double): ParamVec

  def div_!(x: Double): ParamVec

  def apply(i: Int): Double

  def update(i: Int, x: Double): Unit

  def zero_! = set_!(0)

  def size: Int

  def toArray: Array[Double]

  def fromArray(vec: Array[Double]): Unit
}

case class ProbVec(val counts: Vector, var sum: Double) extends ParamVec {
  def this(size: Int) = this (DenseVector(size)(0), 0)

  def size = counts.length

  def apply(i: Int) = counts(i) / sum

  def update(i: Int, x: Double) = {
    counts(i) = x
  }

  def logpr(i: Int) = math.log(apply(i))

  def getCount(i: Int) = counts(i)

  def getProb(i: Int) = counts(i) / sum

  def div_!(x: Double) = {
    require(x > 0);
    counts /= x
    sum /= x;
    this
  }

  private def computeSum_! = {
    sum = 0
    counts.forActiveDomain{
      i: Int => sum += counts(i)
    }
    this
  }

  def logdot(v: Vector): Double = {
    var sum = 0.0
    v.forActiveDomain{
      i: Int => sum += v(i) * logpr(i)
    }
    sum
  }

  def logdot(fv: FtrVec): Double = {
    var sum = 0.0
    forIndex(fv.length, {
      i: Int => val pair = fv(i); if (pair._1 >= 0) sum += pair._2 * logpr(pair._1)
    })
    sum
  }

  def increment_!(x: Double) = {
    counts += x;
    sum += size * x;
    this
  }

  def increment_!(vec: ParamVec, x: Double): ParamVec = {
    vec match {
      case ProbVec(ocounts, osum) => {
        ocounts.forActiveDomain{
          i: Int => counts(i) += ocounts(i) * x
        }
        sum += osum * x
        this
      }
      case _ => throw new Error("Unknown vector: " + vec)
    }
  }

  def increment_!(vec: Vector, x: Double): ParamVec = {
    vec.forActiveDomain{
      i: Int => counts(i) += vec(i) * x; sum += vec(i) * x
    }
    this
  }

  def increment_!(fvec: FtrVec, x: Double): ParamVec = {
    forIndex(fvec.length, {
      i: Int => val pair = fvec(i); if (pair._1 >= 0) {counts(pair._1) += pair._2 * x; sum += pair._2 * x}
    })
    this
  }

  def increment_!(i: Int, x: Double) = {
    counts(i) += x; sum += x; this
  }

  def increment_!(f: (Int) => Double) = {
    forIndex(size, {
      i: Int => increment_!(i, f(i))
    }); this
  }

  def set_!(f: (Int) => Double) = {
    forIndex(size, {
      i: Int => counts(i) = f(i)
    }); computeSum_!
  }

  def set_!(x: Double) = {
    forIndex(size, {
      i: Int => counts(i) = x
    }); computeSum_!
  }

  def normalize_! = {
    counts /= {if (sum == 0) size else sum}; sum = 1; this
  }

  def normalizeIfTooBig_! = {
    if (sum > 1e20) normalize_!; this
  }

  def getProbs: Array[Double] = mapIndex(size, {
    i: Int => counts(i) / sum
  })

  def getCounts: Array[Double] = mapIndex(size, {
    i: Int => counts(i)
  })

  def fromArray(vec: Array[Double]) = {
    require(vec.size == size)
    forIndex(size, {
      i: Int => counts(i) = vec(i)
    })
    computeSum_!
  }

  def toArray = getProbs
}

object DenseProbVec {
  def apply(size: Int) = new ProbVec(DenseVector(size)(0), 0)
}

object SparseProbVec {
  def apply(size: Int) = new ProbVec(new SparseHashVector(size), 0)
}

// Should include the default weight
case class WeightVec(val weights: Vector) extends ParamVec {
  def this(size: Int) = this (DenseVector(size)(0))

  def size = weights.length

  def div_!(x: Double) = {
    weights /= x; this
  }

  def apply(i: Int) = weights(i)

  def update(i: Int, x: Double) = {
    weights(i) = x
  }

  def increment_!(x: Double) = {
    weights += x; this
  }

  def increment_!(vec: ParamVec, x: Double): ParamVec = {
    vec match {
      case WeightVec(ocounts) => {
        this.increment_!(ocounts, x)
        // weights += (ocounts * x)
        this
      }
      case _ => throw new Error("Unknown vector: " + vec)
    }
  }

  def increment_!(f: Vector, x: Double): ParamVec = {
    f.forActiveDomain{
      i: Int => weights(i) += f(i) * x
    }; this
  }

  def increment_!(fv: FtrVec, x: Double): ParamVec = {
    forIndex(fv.length, {
      i: Int => val pair = fv(i); if (pair._1 >= 0) weights(pair._1) += pair._2 * x
    })
    this
  }

  def increment_!(i: Int, x: Double) = {
    weights(i) += x; this
  }

  def increment_!(f: (Int) => Double) = {
    forIndex(size, {
      i: Int => increment_!(i, f(i))
    }); this
  }

  def set_!(f: (Int) => Double) = {
    forIndex(size, {
      i: Int => weights(i) = f(i)
    }); this
  }

  def set_!(x: Double) = {
    forIndex(size, {
      i: Int => weights(i) = x
    }); this
  }

  def dot(v: Vector): Double = weights.dot(v)

  def dot(fv: FtrVec): Double = {
    var sum = 0.0
    forIndex(fv.length, {
      i: Int => val pair = fv(i); if (pair._1 >= 0) sum += weights(pair._1) * pair._2
    })
    sum
  }

  def getWeights: Array[Double] = mapIndex(size, {
    i: Int => weights(i)
  })

  def twoNormSquared(scale: Double): Double = weights.dot(weights) * scale * scale

  def twoNormSquared: Double = twoNormSquared(1.0)

  def fromArray(vec: Array[Double]) = {
    require(vec.size == size)
    forIndex(vec.size, {
      i: Int => weights(i) = vec(i)
    })
    this
  }

  def toArray = getWeights
}

object DenseWeightVec {
  def apply(size: Int) = new WeightVec(DenseVector(size)(0))
}

object SparseWeightVec {
  def apply(size: Int) = new WeightVec(new SparseHashVector(size))
}
