package cc.refectorie.user.kedarb.dynprog.fst

import scala.util.matching.Regex
import cc.refectorie.user.kedarb.dynprog.types.TrieDict

/**
 * @author kedarb
 * @since Dec 16, 2010
 */

object TokFtrFns {
  def wordShapeFull(w: String): String = {
    ("" + w).replaceAll("[A-Z]", "A")
            .replaceAll("[a-z]", "a")
            .replaceAll("[0-9]", "1")
            .replaceAll("[\\?\\!]", "!")
            .replaceAll("[\\[\\]\\(\\)\\{\\}]", "{")
            .replaceAll("``", "\"")
            .replaceAll("''", "\"")
            .replaceAll("\\\\", "\"")
  }

  def wordShapeCollapsed(w: String): String = {
    wordShapeFull(w).replaceAll("[A-Z]{2,}", "A+")
            .replaceAll("[a-z]{2,}", "a+")
            .replaceAll("[0-9]{2,}", "1+")
  }

  def wordShape(word: String, maxRepetitions: Int): String = {
    val sb = new StringBuffer
    var i = 0;
    var c = 'x';
    var prevc = 'x';
    var repetitions = 0
    while (i < word.length) {
      val char = word(i);
      if (Character.isUpperCase(char)) c = 'A'
      else if (Character.isLowerCase(char)) c = 'a'
      else if (Character.isDigit(char)) c = '1'
      else if (Character.isWhitespace(char)) c = ' '
      else c = char
      if (c == prevc) repetitions += 1
      else {prevc = c; repetitions = 0}
      if (repetitions < maxRepetitions) sb.append(c)
      i += 1
    }
    sb.toString
  }

  def lowerCase(w: String) = w.toLowerCase

  def matches(patt: String, w: String, nm: String): Option[String] = if (w.matches(patt)) Some(nm) else None

  def find(patt: String, w: String, nm: String): Option[String] = find(patt.r, w, nm)

  def find(r: Regex, w: String, nm: String): Option[String] = if (r.findFirstMatchIn(w) != None) Some(nm) else None

  def prefix(w: String, len: Int): Option[String] = if (w.length >= len) Some(w.substring(0, len)) else None

  def suffix(w: String, len: Int): Option[String] = if (w.length >= len) Some(w.substring(w.length - len)) else None

  def lexicon(dict: scala.collection.Set[String], w: String, nm: String) = if (dict.contains(w)) Some(nm) else None

  def trieLexicon(trie: TrieDict, ws: Seq[String], begin: Int): Int = trie.endIndexOf(ws, begin)
}