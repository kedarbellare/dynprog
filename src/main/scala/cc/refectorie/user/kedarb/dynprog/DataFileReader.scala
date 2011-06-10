package cc.refectorie.user.kedarb.dynprog

import java.io.File
import io.Source
import collection.mutable.ArrayBuffer

/**
 * @author kedarb
 * @since Dec 2, 2010
 */

class DataFileReader(file: File) {
  def this(filename: String) = this (new File(filename))

  def forLine[A](process: String => A): Unit = {
    val iter = forLazyLine(process)
    for (a <- iter) {}
  }

  def forLazyLine[A](process: String => A): Iterator[A] = {
    val source = Source.fromFile(file)
    for (line <- source.getLines) yield process(line)
  }

  def forLineGroup[A](pattern: String, process: Array[String] => A): Unit = {
    val iter = forLazyLineGroup(pattern, process)
    for (a <- iter) {}
  }

  def forLazyLineGroup[A](pattern: String, process: Array[String] => A): Iterator[A] = {
    val regex = pattern.r
    val source = Source.fromFile(file)
    val iter = new Iterator[Array[String]] {
      val buff = new ArrayBuffer[String]
      readNextBlock

      // skip over lines matching patterns first
      def addFirstNonEmptyLine: Unit = {
        for (line <- source.getLines) {
          if (regex.findFirstMatchIn(line) == None) {
            buff.append(line)
            return
          }
        }
      }

      def readNextBlock: Unit = {
        addFirstNonEmptyLine
        for (line <- source.getLines) {
          if (regex.findFirstMatchIn(line) != None) return
          else buff.append(line)
        }
      }

      def hasNext = buff.size > 0

      def next() = {
        val res = buff.toArray;
        buff.clear;
        readNextBlock;
        res
      }
    }
    for (lines <- iter) yield process(lines)
  }
}

trait ADictReader {
  def addToDict: Unit
}

trait AExampleReader[Widget, Example <: AExample[Widget]] {
  def exampleIterator: Iterator[Example]
}