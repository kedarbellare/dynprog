package cc.refectorie.user.kedarb.dynprog.fst

import cc.refectorie.user.kedarb.dynprog._
import collection.mutable.{HashMap, ArrayBuffer}
import utils.Utils._
import data.Labels2BIO
import types.{FtrVec, TrieDict}

/**
 * @author kedarb
 * @since 12 /16/10
 */

object LbledTokSeq {
  def addFeatureConjunctions(features: Array[ArrayBuffer[String]], conjunctions: Array[Array[Int]],
                             pattern: String = null, boundaryPrefix: String = "_B") = {
    val nrow = features.length
    conjunctions.foreach {
      conj: Array[Int] => forIndex(nrow, {
        ii: Int =>
          val offsets = conj.map(_ + ii) // map conjunctions to offset from current position
          val offsetFeatures = offsets.map({
            i: Int =>
              if (i < 0) Seq("%s%d".format(boundaryPrefix, i))
              else if (i >= nrow) Seq("%s+%d".format(boundaryPrefix, (i - nrow + 1)))
              else features(i).filter({
                s: String => !s.contains("@") && (pattern == null || s.matches(pattern))
              })
          })
          // find cross-product of these features
          features(ii) ++= crossProductFeatures(offsetFeatures, conj, null).map {
            list: List[(String, Int)] => list.sortBy({
              case (f, o) => o + f
            }).map({
              case (f, o) => if (o == 0) f else f + "@" + o
            }).mkString("_&_")
          }
      })
    }
  }

  private def crossProductFeatures(offsetFeatures: Seq[Seq[String]], offsets: Seq[Int],
                                   existing: ArrayBuffer[List[(String, Int)]]): ArrayBuffer[List[(String, Int)]] = {
    val result = new ArrayBuffer[List[(String, Int)]]
    val offset: Int = offsets.head
    val adding: Seq[String] = offsetFeatures.head
    if (existing != null) {
      for (e <- existing; a <- adding) {
        val elt = (a, offset); if (!e.contains(elt)) result += elt :: e
      }
    } else {
      for (a <- adding) result += List((a, offset))
    }
    if (offsets.size == 1) result
    else crossProductFeatures(offsetFeatures.drop(1), offsets.drop(1), result)
  }

  /**
   * Bins a number and returns the bin. If number is negative,
   * bin number will be negative.
   */
  def bin(number: Int, bins: Seq[Int]): Int = {
    val abs = scala.math.abs(number)
    val sign = if (number >= 0) 1 else -1
    for (i <- bins) {
      if (abs < i) return sign * i
    }
    sign * (bins.last + 5)
  }
}

class LbledTokSeq(val lines: Array[String], val splitRegex: String = "\\s+") {
  type StrVec = ArrayBuffer[String]

  val nrow = lines.length
  val matrix = new Array[Array[String]](nrow)
  val features = new Array[StrVec](nrow)
  var ncol = -1
  forIndex(nrow, {
    i: Int =>
      val parts = lines(i).split(splitRegex)
      ncol = parts.length
      matrix(i) = parts
      features(i) = new StrVec
  })
  require(ncol > 0, "#columns <= 0!")
  // construct transposed matrix
  val tmatrix = Array.ofDim[String](ncol, nrow)
  forIndex(nrow, {
    i: Int => forIndex(ncol, {
      j: Int => tmatrix(j)(i) = matrix(i)(j)
    })
  })

  private def id(max: Int, i: Int) = if (i >= 0) i else (max + i)

  def row(i: Int): Array[String] = matrix(id(nrow, i))

  def column(j: Int): Array[String] = tmatrix(id(ncol, j))

  def columnToBIO(j: Int, otherLabel: String = "O"): Array[String] = Labels2BIO(column(j), otherLabel)

  def columnIndices(j: Int, f: String => Int): Array[Int] = column(j).map(f(_))

  def columnToBIOIndices(j: Int, f: String => Int, otherLabel: String = "O"): Array[Int] = columnToBIO(j, otherLabel).map(f(_))

  def cell(i: Int, j: Int) = matrix(id(nrow, i))(id(ncol, j))

  def featureVectorSequence(f: String => Int): Array[FtrVec] = {
    val fvs = new Array[FtrVec](nrow)
    forIndex(nrow, {
      i: Int =>
        val fv = new FtrVec
        features(i).map(f(_)).toSet.foreach {
          id: Int => if (id >= 0) fv += id -> 1.0
        }
        fvs(i) = fv
    })
    fvs
  }

  def foreachRow(ftrPrefix: String, offsets: Array[(Int, Int)], boundaryPrefix: String = "_B", sep: String = "/") = {
    forIndex(nrow, {
      ii: Int =>
        val offsetStrings = new Array[String](offsets.length)
        forIndex(offsets.length, {
          k: Int =>
            val i = ii + offsets(k)._1
            offsetStrings(k) = {
              if (i < 0) "%s%d".format(boundaryPrefix, i)
              else if (i >= nrow) "%s+%d".format(boundaryPrefix, (i - nrow + 1))
              else {
                val j = id(ncol, offsets(k)._2)
                matrix(i)(j)
              }
            }
        })
        features(ii) += (ftrPrefix + offsetStrings.mkString(sep))
    })
  }

  def addFeatureConjuctions(conjunctions: Array[Array[Int]], pattern: String = null, boundaryPrefix: String = "_B") = {
    LbledTokSeq.addFeatureConjunctions(features, conjunctions, pattern, boundaryPrefix)
  }

  def addFeaturesUsingFunctions(j: Int, prefixToFtrFns: HashMap[String, String => Option[String]]): Unit = {
    foreachIndex(column(j), {
      (i: Int, w: String) => for (prefix <- prefixToFtrFns.keys) {
        for (ftrname <- prefixToFtrFns(prefix)(w)) features(i) += "%s%s".format(prefix, ftrname)
      }
    })
  }

  def addTrieFeatures(j: Int, ftrName: String, trie: TrieDict) = {
    var begin = 0
    val words = column(j)
    while (begin < words.length) {
      val end = trie.endIndexOf(words, begin)
      if (end >= begin) {
        forIndex(begin, end + 1, {
          i: Int => features(i) += ftrName
        })
        begin = end + 1
      } else {
        begin += 1
      }
    }
  }
}