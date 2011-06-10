package cc.refectorie.user.kedarb.dynprog.data

import cc.refectorie.user.kedarb.dynprog.utils.Utils

/**
 * @author kedarb
 * @since Dec 17, 2010
 */

object Labels2BIO {
  def apply(input: Array[String], otherLabel: String = "O"): Array[String] = {
    val result = new Array[String](input.length)

    def isB(s: String): Boolean = s.matches("^B-.*")
    def isI(s: String): Boolean = s.matches("^I-.*")
    def isO(s: String): Boolean = {s == otherLabel}
    def isValid(s: String): Boolean = isB(s) || isI(s) || isO(s)

    val notInBIOFormat = input.exists(!isValid(_))
    if (notInBIOFormat) {
      var prevl: String = null
      Utils.forIndex(input.length, {
        i: Int =>
          val currl = input(i)
          if (isValid(currl)) result(i) = currl
          else {
            if (isO(currl)) result(i) = currl
            else {
              result(i) = {
                if (prevl == null || prevl != currl) "B-%s".format(currl)
                else "I-%s".format(currl)
              }
            }
          }
          prevl = currl
      })
    } else {
      // copy over the labels as they are in BIO format
      Utils.forIndex(input.length, {i: Int => result(i) = "" + input(i)})
    }
    result
  }
}