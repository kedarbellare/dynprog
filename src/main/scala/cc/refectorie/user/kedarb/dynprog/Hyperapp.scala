package cc.refectorie.user.kedarb.dynprog

import data._
import fst.LbledTokSeq
import types._
import types.IndexerUtils._
import types.ParamUtils._
import utils.Utils._
import utils.VectorUtils._
import java.io._

/**
 * @author kedarb
 * @since Nov 29, 2010
 */
object Hyperapp extends Application {
  val idxer = new Indexer[String]
  val stats = new ProbStats
  println("i('hi')=" + idxer.indexOf_!("hi"))
  idxer.indexOf_!("there")
  idxer.indexOf_!("how")
  idxer.indexOf_!("are")
  idxer.indexOf_!("you")
  idxer += "sir"
  idxer += "hi"
  println("size=" + idxer.size)
  println("i('you')=" + idxer.indexOf_?("you"))
  println("o(4)=" + idxer.get(4))
  println("o(4)=" + idxer(4))
  println(idxer.toString)

  // test matching of params
  val dprobs = DenseProbVec(10)
  val sprobs = SparseProbVec(10)
  val dwts = DenseWeightVec(10)
  val swts = SparseWeightVec(10)
  dprobs.increment_!(3, 1.0).increment_!(5, 2.0).increment_!(7, 0.5).normalize_!
  sprobs.increment_!(4, 0.1)
  dwts.increment_!(2, 0.1).increment_!(9, -0.3)
  swts.increment_!(5, 1000.0).div_!(100.0)

  def paramMatch(v: ParamVec) = {
    println
    v match {
      case pr: ProbVec => println("probability")
      case wt: WeightVec => println("weight")
      case _ => println("unknown")
    }
    println(toProbs(v))
    println(toWeights(v))
  }

  paramMatch(dprobs)
  paramMatch(sprobs)
  paramMatch(dwts)
  paramMatch(swts)

  // test serialization of double arrays
  val arr1 = Array(0.0, 1.0, 2.0, 3.0)
  val arr2 = Array(3.0, 4.0, 5.0)
  val arr3 = Array(Array(6.0, 7.0), Array(8.0, 9.0, 10.0))
  val arrOut = new PrintStream(new FileOutputStream("arr.ser~"))

  // feature vector
  val fv = new FtrVec
  fv += 1 -> 1.0
  fv += 10 -> (0.22132312 / 7.0)

  def out(s: String) = arrOut.println(s)

  serializeIndexer(out(_), idxer)
  serializeArray(out(_), arr1)
  serializeArray(out(_), arr2)
  serializeArray2(out(_), arr3)
  serializeVector(out(_), dprobs.counts)
  serializeVector(out(_), sprobs.counts)
  serializeParams(out(_), dprobs)
  serializeParams(out(_), swts)
  serializeFtrVec(out(_), fv)
  arrOut.close
  val arrIn = new BufferedReader(new FileReader("arr.ser~"))

  def in = arrIn.readLine

  println(deserializeIndexerString(in).toString)
  println(deserializeDoubleArray(in).mkString(" "))
  println(deserializeDoubleArray(in).mkString(" "))
  println(deserializeDoubleArray2(in).map(_.mkString(" ")).mkString("\n"))
  println(deserializeVector(in))
  println(deserializeVector(in))
  println(deserializeParams(in))
  println(deserializeParams(in))
  println(deserializeFtrVec(in))
  arrIn.close

  val t = new TrieDict
  t.add(Seq("a", "b"))
  t.add(Seq("a", "b", "c"))
  t.add(Seq("a", "d", "b", "a"))
  t.add(Seq("b", "c"))
  println("t ?= " + t.contains(Seq("a", "b")))
  println("t ?= " + t.contains(Seq("a", "b", "c")))
  println("t ?= " + t.contains(Seq("a", "b", "b", "a")))
  println("t ?= " + t.contains(Seq("b", "c")))
  println("f ?= " + t.contains(Seq("a")))
  println("f ?= " + t.contains(Seq("b", "b")))
  println("f ?= " + t.contains(Seq("a", "b", "c", "d")))
  println("f ?= " + t.contains(Seq("a", "b", "b", "a", "d")))
  val seq = Seq("a", "b", "c", "b", "a", "d", "b", "a", "c")
  for (begin <- 0 until seq.length) {
    val end = t.endIndexOf(seq, begin)
    if (end >= begin) {
      println("found[" + begin + "-" + end + "]: " + seq.drop(begin).take(end - begin + 1))
    }
  }

  val iter = new DataFileReader("data/conll00/train.txt~").forLazyLineGroup("^\\s*$", {
    lines: Array[String] =>
      val ts = new LbledTokSeq(lines)
      ts.foreachRow("U00=", Array((-2, 0)))
      ts.foreachRow("U01=", Array((-1, 0)))
      ts.foreachRow("U02=", Array((0, 0)))
      ts.foreachRow("U03=", Array((1, 0)))
      ts.foreachRow("U04=", Array((2, 0)))
      ts.foreachRow("U05=", Array((-1, 0), (0, 0)))
      ts.foreachRow("U06=", Array((0, 0), (1, 0)))
      println(ts.features.map(_.mkString(" ")).mkString("\n") + "\n")
      ts
  })
  for (i <- 0 until 5) iter.next

  //  val dict = Dict.fromResource("citations/lexicons/personname/ssdi.prfirsthighest")
  //  println(dict.toString)
  val conjunctions = Array(Array(-2), Array(-1), Array(1), Array(2))
  val iter2 = new DataFileReader("data/citation/citation_dev.fs~").forLazyLineGroup("^\\s*$", {
    lines: Array[String] => val ts = CoraCitationFeatures.processLines(lines, "\t", conjunctions)
    println("Column0: " + ts.column(0).mkString("\t"))
    println("Column1: " + ts.column(1).mkString("\t"))
    println(ts.features.map(_.mkString(" ")).mkString("\n") + "\n")
  })
  for (i <- 0 until 5) iter2.next

  val iter3 = new DataFileReader("data/cora-cv/cora_ref.test.1.txt~").forLazyLine({
    line: String => val ts = CoraCitationFeatures.processLines(CoraSgml2Owpl(line, "\t"), "\t", conjunctions)
    println("Column0: " + ts.columnToBIO(0).mkString("\t"))
    println("Column1: " + ts.column(1).mkString("\t"))
    println(ts.features.map(_.mkString(" ")).mkString("\n") + "\n")
  })
  for (i <- 0 until 5) iter3.next

  val iter4 = new DataFileReader("data/citation/citation_train.300.0.txt~").forLazyLineGroup("^\\s*$", {
    lines: Array[String] => println(DanrTabbed2Owpl(lines, "\t", "\t").mkString("\n") + "\n")
  })
  for (i <- 0 until 5) iter4.next
}