package cc.refectorie.user.kedarb.dynprog.data

import collection.mutable.ArrayBuffer

/**
 * @author timv
 * @since Dec 16, 2010
 */

object CoraSgml2Owpl extends Sgml2Owpl {
  val Lexer = List("(https?|ftp)://\\S+", /* keep urls together.. might include invalid URL characters */
    // "\"[^\"]+\"", /* keep quoted things together */
    "\\S+@\\S+",
    "[0-9]+\\s*[\\-{#]+\\s*[0-9]+", /* possible page number */
    "[0-9]+(?:st|nd|rd|th)",
    "[A-Z]\\.",
    "Ph\\.?[Dd]\\.",
    // "[Vv]ol(?:\\.|ume)\\s*[0-9]+", /* make "Vol. 3" one token. */
    "(?:Vol|Proc|Dept|Univ|No|Inc)\\s*\\.",
    "pp\\.",
    "\\(\\s*[0-9][0-9][0-9][0-9][a-z]?\\s*\\)", /* e.g. "(1994)" year in parens */
    "[Ee]d(?:s?\\.|itors?)",
    "[0-9][0-9]?\\s*\\([0-9]?\\)", /* e.g. "7(4)" common in volume */
    "\\(\\s*[0-9][0-9]?\\s*\\)", /* e.g. "(4)" common in volume */
    // "\\w+-\\w+",
    "\\+[A-Z]+\\+",
    "\\p{Alpha}+(?:'s)?",
    // "[0-9]+(?:\\.[0-9]+)?", /* 231 or 2.0, but not 2. */
    "[0-9]+",
    "[()\"'\\-\\.,]",
    "\\S+").mkString("(", "|", ")").r
}