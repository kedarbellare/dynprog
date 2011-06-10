package cc.refectorie.user.kedarb.dynprog.segment

import collection.mutable.ArrayBuffer
import cc.refectorie.user.kedarb.dynprog.utils.Utils._

/**
 * @author kedarb
 * @since 12/26/10
 */
case class Segment(begin: Int, end: Int, label: Int)

class Segmentation(val length: Int) {
  val indexToSegment = new Array[Segment](length)
  val segments = new ArrayBuffer[Segment]

  def segmentAt(i: Int): Option[Segment] = {
    if (indexToSegment(i) == null) None
    else Some(indexToSegment(i))
  }

  def labelAt(i: Int): Option[Int] = {
    val s = segmentAt(i)
    if (s == None) None
    else Some(s.get.label)
  }

  def numSegments = segments.size

  def segment(i: Int): Segment = segments(i)

  def isValid(s: Segment): Boolean = {
    if (s.begin < 0 || s.begin >= length) false
    else if (s.end < 1 || s.end > length) false
    else if (s.end <= s.begin) false
    else {
      forIndex(s.begin, s.end, {
        i => if (indexToSegment(i) != null) return false
      })
      true
    }
  }

  private def addIndex2SegmentIsValid(s: Segment): Boolean = {
    if (isValid(s)) {
      forIndex(s.begin, s.end, {
        i => indexToSegment(i) = s
      })
      true
    } else {
      false
    }
  }

  def append(s: Segment): Boolean = {
    val success = addIndex2SegmentIsValid(s)
    if (success) segments += s
    success
  }

  def prepend(s: Segment): Boolean = {
    val success = addIndex2SegmentIsValid(s)
    if (success) segments.insert(0, s)
    success
  }

  def contains(s: Segment): Boolean = segments.contains(s)

  override def toString = segments.mkString("[", ", ", "]")
}

object Segmentation {
  def fromBIO(ls: Seq[String], indexOf: (String) => Int, isOtherLabel: (String) => Boolean = _ == "O"): Segmentation = {
    val len = ls.length
    val segmentation = new Segmentation(len)
    // a utility function for getting the label index
    def getLabelIndex(s: String): Int = {
      if (isOtherLabel(s)) indexOf(s) // use the other label directly
      else indexOf(s.substring(2)) // remove the B- or I- to get the label
    }
    // gets the next segment
    def nextSegment(begin: Int): Segment = {
      val labelIndex = getLabelIndex(ls(begin))
      forIndex(begin + 1, len, {
        end =>
          val endLabel = ls(end)
          val endLabelIndex = getLabelIndex(endLabel)
          if (labelIndex != endLabelIndex || endLabel.startsWith("B-")) return Segment(begin, end, labelIndex)
      })
      Segment(begin, len, labelIndex)
    }
    var begin = 0
    while (begin < len) {
      val segment = nextSegment(begin)
      require(segmentation.append(segment), "Could not add segment: " + segment + " len=" + len)
      begin = segment.end
    }
    segmentation
  }
}

/*
class SegmentationSerializer extends Serializer[Segmentation] {
  private val SegmentationClass = classOf[Segmentation]

  private def segment2jvalue(s: Segment)(implicit format: Formats): JValue =
    JObject(JField("begin", JInt(s.begin)) :: JField("end", JInt(s.end)) :: JField("label", JInt(s.label)) :: Nil)

  def serialize(implicit format: Formats): PartialFunction[Any, JValue] = {
    case x: Segmentation => JObject(JField("length", JInt(x.length)) ::
      JField("segments", JArray(x.segments.map(s => segment2jvalue(s)).toList)) :: Nil)
  }

  def deserialize(implicit format: Formats): PartialFunction[(TypeInfo, JValue), Segmentation] = {
    case (TypeInfo(SegmentationClass, _), json) => json match {
      case JObject(JField("length", JInt(len)) :: JField("segments", JArray(segments)) :: Nil) => {
        val segmentation = new Segmentation(len.toInt)
        segments.foreach(s => s match {
          case JObject(JField("begin", JInt(begin)) :: JField("end", JInt(end))
            :: JField("label", JInt(label)) :: Nil) =>
            segmentation.append(Segment(begin.toInt, end.toInt, label.toInt))
          case unk => throw new MappingException("Can't convert " + unk + " to Segment.")
        })
        segmentation
      }
      case x => throw new MappingException("Can't convert " + x + " to Segmentation.")
    }
  }
}
*/
