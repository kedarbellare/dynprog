package cc.refectorie.user.kedarb.dynprog.data

import scala.util.matching.Regex
import collection.mutable.ArrayBuffer

/**
 * @author kedarb
 * @since 12/29/10
 */

trait Sgml2Owpl {
  val TagPattern = "<[/]?[a-zA-Z0-9_]+>".r
  val TaggedText = "<([A-Za-z0-9_]+)>([\\w\\W]+?)</([A-Za-z0-9_]+)>|([^<>\\s]+)".r

  def Lexer: Regex

  def apply(input: String, sep: String = " ", bioEncoding: Boolean = false): Array[String] = {
    val x = input.trim.replace("\n", " -NEWLINE- ")
    val seq = new ArrayBuffer[String]
    val m = TaggedText.findAllIn(x)
    while (m.hasNext) {
      var tag = m.group(1)
      if (tag != m.group(3))
        throw new Exception("Opening (%s) and closing (%s) tags do not match in %s.".format(tag, m.group(3), x))
      var content = m.group(2)
      if (content == null) {
        content = m.group(4)
        Lexer.findAllIn(content).foreach{
          s => seq += ("O" + sep + s)
        }
      } else {
        // which encoding to use
        if (bioEncoding) {
          val iter = Lexer.findAllIn(content).toIterator
          seq += ("B-" + tag + sep + iter.next)
          iter.foreach{
            s => seq += ("I-" + tag + sep + s)
          }
        } else {
          Lexer.findAllIn(content).foreach{
            s => seq += (tag + sep + s)
          }
        }
      }
      m.next
    }
    seq.toArray
  }
}