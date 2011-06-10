package cc.refectorie.user.kedarb.dynprog.types

import collection.mutable.{HashMap, ArrayBuffer}
import cc.refectorie.user.kedarb.dynprog.utils.Utils._

/**
 * @author kedarb
 * @since Nov 29, 2010
 */

object IndexerUtils {
  def serializeIndexer[T](puts: (String => Any), indexer: Indexer[T]) = {
    puts(fmt(indexer.locked))
    puts(fmt(indexer.size))
    forIndex(indexer.size, {i => puts(indexer(i).toString)})
  }

  def deserializeToIndexerInt(gets: => String, indexer: Indexer[Int]): Unit = {
    val locked = gets.toBoolean
    val n = gets.toInt
    forIndex(n, {_ => indexer += gets.toInt})
    if (locked) indexer.lock
  }

  def deserializeIndexerInt(gets: => String): Indexer[Int] = {
    val indexer = new Indexer[Int]
    val locked = gets.toBoolean
    val n = gets.toInt
    forIndex(n, {_ => indexer += gets.toInt})
    if (locked) indexer.lock
    indexer
  }

  def deserializeToIndexerString(gets: => String, indexer: Indexer[String]): Unit = {
    val locked = gets.toBoolean
    val n = gets.toInt
    forIndex(n, {_ => indexer += gets})
    if (locked) indexer.lock
  }

  def deserializeIndexerString(gets: => String): Indexer[String] = {
    val indexer = new Indexer[String]
    val locked = gets.toBoolean
    val n = gets.toInt
    forIndex(n, {_ => indexer += gets})
    if (locked) indexer.lock
    indexer
  }
}

class Indexer[T] {
  private val objects = new ArrayBuffer[T]
  private val indexes = new HashMap[T, Int]
  var locked = false

  def clear {objects.clear; indexes.clear}

  def lock {locked = true}

  def unlock {locked = false}

  def size = objects.size

  def contains(elem: T) = indexes.contains(elem)

  // lookup index of elem; add if not present if _!, otherwise use _?
  def indexOf_!(elem: T) = indexOf(elem, true)

  def indexOf_?(elem: T) = indexOf(elem, false)

  private def indexOf(elem: T, addIfNotPresent: Boolean): Int = {
    if (contains(elem)) indexes(elem)
    else if (addIfNotPresent && !locked) {
      val i = size
      indexes += elem -> i
      objects += elem
      i
    } else -1
  }

  def +=(elem: T): Indexer[T] = {indexOf_!(elem); this}

  // lookup object methods
  def get(index: Int) = objects(index)

  def apply(index: Int) = get(index)

  override def toString = {
    var s = "";
    for (i <- 0 until size) s += "(" + i + ") " + objects(i) + "\n"
    s
  }
}
