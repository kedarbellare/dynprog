package cc.refectorie.user.kedarb.dynprog.utils

import Utils._
import cc.refectorie.user.kedarb.dynprog.la._

/**
 * @author kedarb
 * @since Nov 30, 2010
 */

object VectorUtils {
  // vector types
  val DENSE = 0
  val SPARSE = 1

  def serializeVector(puts: (String => Any), v: Vector) = {
    // type
    v match {
      case dv: DenseVector => puts(fmt(DENSE))
      case sv: SparseHashVector => puts(fmt(SPARSE))
      case _ => throw fail("Unknown vector: " + v)
    }
    puts(fmt(v.length)) // length for re-construction
    puts(v.activeElements.filter(_._2 != 0).map(pair => pair._1 + ":" + pair._2).mkString(" "))
  }

  def deserializeVector(gets: => String): Vector = {
    val vtype = gets.toInt
    val vlen = gets.toInt
    val v = vtype match {
      case DENSE => new DenseVector(vlen)
      case SPARSE => new SparseHashVector(vlen)
      case _ => throw fail("Unknown vector type: " + vtype)
    }
    gets.split(" ").foreach {
      s: String =>
        val a = s.split(":")
        require(a.size == 2, "Invalid index:value string: " + s)
        v.update(a(0).toInt, a(1).toDouble)
    }
    v
  }
}