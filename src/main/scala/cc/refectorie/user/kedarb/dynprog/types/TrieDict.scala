package cc.refectorie.user.kedarb.dynprog.types

import collection.mutable.HashMap
import cc.refectorie.user.kedarb.dynprog.utils.Utils._
import io.Source
import org.apache.log4j.Logger
import java.io.{File, InputStream}

/**
 * @author kedarb
 * @since 12 /16/10
 */

class TrieDict(val splitRegex: String = "\\s+", val toLC: Boolean = true) {
  val map = new HashMap[String, TrieDict] {
    override def default(key: String) = {
      val trie = new TrieDict(splitRegex, toLC);
      this(key) = trie;
      trie
    }
  }
  var isDone = false

  def add(a: Seq[String]): Unit = {
    var t = this
    forIndex(a.length, {
      k: Int =>
        val ak = if (toLC) a(k).toLowerCase else a(k)
        t = t.map(ak)
    })
    t.isDone = true
  }

  def add(s: String): Unit = add(s.split(splitRegex))

  def contains(a: Seq[String]): Boolean = {
    var t = this
    forIndex(a.length, {
      k: Int =>
        val ak = if (toLC) a(k).toLowerCase else a(k)
        if (!t.map.contains(ak)) return false
        else t = t.map(ak)
    })
    t.isDone
  }

  def contains(s: String): Boolean = contains(s.split(splitRegex))

  /**
   * Returns the end index
   */
  def endIndexOf(a: Seq[String], begin: Int): Int = {
    var t = this
    var end = begin
    while (end < a.length) {
      val ak = if (toLC) a(end).toLowerCase else a(end)
      if (!t.map.contains(ak)) return -1
      else {
        t = t.map(ak)
        if (t.isDone) return end
      }
      end += 1
    }
    if (t.isDone) a.length - 1
    else -1
  }
}

object TrieDict {
  val logger = Logger.getLogger(getClass.getSimpleName)

  def fromFile(filename: String, splitPattern: String = "\\s+", toLC: Boolean = true): TrieDict = {
    fromSource(Source.fromFile(filename), splitPattern, toLC)
  }

  def fromResource(filename: String, splitPattern: String = "\\s+", toLC: Boolean = true): TrieDict = {
    fromSource(Source.fromInputStream(getClass.getClassLoader.getResourceAsStream(filename)), splitPattern, toLC)
  }

  def fromResourceOrFile(filename: String, splitPattern: String = "\\s+", toLC: Boolean = true): TrieDict = {
    try {
      val is: InputStream = getClass.getClassLoader.getResourceAsStream(filename)
      if (is != null) fromSource(Source.fromInputStream(is), splitPattern, toLC)
      else if (new File(filename).exists) {
        logger.warn("Couldn't find file %s in classpath! Using relative path instead.".format(filename))
        fromSource(Source.fromFile(filename), splitPattern, toLC)
      } else {
        logger.warn("Couldn't find file %s in classpath or relative path! Returning empty dict.".format(filename))
        return new TrieDict(splitPattern, toLC)
      }
    } catch {
      case e: Exception =>
        e.printStackTrace
        return new TrieDict(splitPattern, toLC)
    }
  }

  def fromSource(source: Source, splitPattern: String = "\\s+", toLC: Boolean = true): TrieDict = {
    val dict = new TrieDict(splitPattern, toLC)
    for (line <- source.getLines) dict.add(line)
    dict
  }
}