package cc.refectorie.user.kedarb.dynprog.data

import cc.refectorie.user.kedarb.dynprog.utils.Utils._

/**
 * Converts UIUC file format for extraction into one word per line.
 *
 * @author kedarb
 * @since Dec 17, 2010
 */

object DanrTabbed2Owpl {
  def apply(input: Array[String], splitPattern: String = "\\s+", sep: String = " "): Array[String] = {
    val nc = input.length
    val tmat = new Array[Array[String]](nc)
    var nr = -1
    forIndex(nc, {
      i: Int =>
        val parts = input(i).split(splitPattern)
        nr = parts.length
        tmat(i) = parts
    })
    require(nr > 0, "#rows <= 0!")
    // construct transposed matrix
    val mat = Array.ofDim[String](nr, nc)
    forIndex(nc, {
      i: Int => forIndex(nr, {
        j: Int => mat(j)(i) = tmat(i)(j)
      })
    })
    mat.map(_.mkString(sep))
  }
}