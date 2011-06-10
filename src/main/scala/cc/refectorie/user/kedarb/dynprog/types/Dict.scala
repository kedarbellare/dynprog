package cc.refectorie.user.kedarb.dynprog.types

import collection.mutable.HashSet
import io.Source
import java.io.{File, InputStream}
import org.apache.log4j.Logger

/**
 * @author kedarb
 * @since Dec 16, 2010
 */

class Dict(val name: String, val toLC: Boolean = true) {
  val set = new HashSet[String]

  def add(s: String): Unit = set += {if (toLC) s.toLowerCase else s}

  def contains(s: String): Boolean = set.contains(if (toLC) s.toLowerCase else s)

  override def toString = name + " :: " + set.mkString("\n")
}

object Dict {
  val logger = Logger.getLogger(getClass.getSimpleName)

  def fromFile(filename: String, toLC: Boolean = true): Dict = {
    fromSource(filename, Source.fromFile(filename), toLC)
  }

  def fromResource(filename: String, toLC: Boolean = true): Dict = {
    fromSource(filename, Source.fromInputStream(getClass.getClassLoader.getResourceAsStream(filename)), toLC)
  }

  def fromResourceOrFile(filename: String, toLC: Boolean = true): Dict = {
    try {
      val is: InputStream = getClass.getClassLoader.getResourceAsStream(filename)
      if (is != null) fromSource(filename, Source.fromInputStream(is), toLC)
      else if (new File(filename).exists) {
        logger.warn("Couldn't find file %s in classpath! Using relative path instead.".format(filename))
        fromSource(filename, Source.fromFile(filename), toLC)
      } else {
        logger.warn("Couldn't find file %s in classpath or relative path! Returning empty dict.".format(filename))
        return new Dict(new File(filename).getName, toLC)
      }
    } catch {
      case e: Exception =>
        e.printStackTrace
        return new Dict(new File(filename).getName, toLC)
    }
  }

  def fromSource(filename: String, source: Source, toLC: Boolean = true): Dict = {
    val name = new File(filename).getName
    val dict = new Dict(name, toLC)
    for (line <- source.getLines) dict.add(line)
    dict
  }
}