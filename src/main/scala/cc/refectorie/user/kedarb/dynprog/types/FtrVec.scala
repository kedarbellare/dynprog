package cc.refectorie.user.kedarb.dynprog.types

import collection.mutable.ArrayBuffer
import cc.refectorie.user.kedarb.dynprog.la._
import cc.refectorie.user.kedarb.dynprog.utils.Utils._

/**
 * A container for index -> value mapping. Avoids multiple passes over the data.
 * Also has serialization built-in.
 *
 * @author kedarb
 * @since Dec 15, 2010
 */

class FtrVec extends ArrayBuffer[(Int, Double)] {
  def sparseVector(length: Int): SparseHashVector = {
    val vec = new SparseHashVector(length)
    forIndex(this.length, {
      i: Int => val pair = this(i)
      if (pair._2 != 0) vec.increment(pair._1, pair._2)
    })
    vec
  }

  override def toString(): String = toString(_.toString)

  def toString(f: Int => String): String = this.map({
    p => "%s:%s".format(f(p._1), fmt(p._2))
  }).mkString(" ")

  def serialize(puts: (String) => Any) = puts(this.toString())

  def deserialize(gets: => String) = {
    gets.split(" ").foreach{
      s: String => val pair = s.split(":")
      require(pair.length == 2, "Invalid index:value pair: " + s)
      this += pair(0).toInt -> pair(1).toDouble
    }
    this
  }
}

/*
class FtrVecSerializer extends Serializer[FtrVec] {
  private val FtrVecClass = classOf[FtrVec]

  private def ftr2jvalue(f: (Int, Double))(implicit format: Formats): JValue =
    JObject(JField("index", JInt(f._1)) :: JField("value", JDouble(f._2)) :: Nil)

  def deserialize(implicit format: Formats): PartialFunction[(TypeInfo, JValue), FtrVec] = {
    case (TypeInfo(FtrVecClass, _), json) => json match {
      case JArray(lst) => {
        val fv = new FtrVec
        lst.foreach(ftr => ftr match {
          case JObject(JField("index", JInt(i)) :: JField("value", JDouble(v)) :: Nil) => fv += i.toInt -> v
          case unk => throw new MappingException("Can't convert feature " + unk + " to (Int, Double).")
        })
        fv
      }
      case x => throw new MappingException("Can't convert " + x + " to FtrVec.")
    }
  }

  def serialize(implicit format: Formats): PartialFunction[Any, JValue] = {
    case x: FtrVec => JArray(x.map(f => ftr2jvalue(f)).toList)
  }
}
*/
