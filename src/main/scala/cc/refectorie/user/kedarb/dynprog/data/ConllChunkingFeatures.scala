package cc.refectorie.user.kedarb.dynprog.data

import cc.refectorie.user.kedarb.dynprog.fst.LbledTokSeq

/**
 * From Leon Bottou's excellent sgd-1.3 package (see http://crfpp.sf.net for explanation of template)
 *
 * @author kedarb
 * @since Dec 17, 2010
 */

object ConllChunkingFeatures {
  var debugfeatures = false
  var debuglabels = false

  def processLines(lines: Array[String], splitRegex: String = "\\s+",
                   labelIndex: Int = 0, wordIndex: Int = 1): LbledTokSeq = {
    val ts = new LbledTokSeq(lines, splitRegex)
    processTokSeq(ts, labelIndex, wordIndex)
    ts
  }

  //  # Unigram
  //  U00:%x[-2,0]
  //  U01:%x[-1,0]
  //  U02:%x[0,0]
  //  U03:%x[1,0]
  //  U04:%x[2,0]
  //  U05:%x[-1,0]/%x[0,0]
  //  U06:%x[0,0]/%x[1,0]
  //
  //  U10:%x[-2,1]
  //  U11:%x[-1,1]
  //  U12:%x[0,1]
  //  U13:%x[1,1]
  //  U14:%x[2,1]
  //  U15:%x[-2,1]/%x[-1,1]
  //  U16:%x[-1,1]/%x[0,1]
  //  U17:%x[0,1]/%x[1,1]
  //  U18:%x[1,1]/%x[2,1]
  //
  //  U20:%x[-2,1]/%x[-1,1]/%x[0,1]
  //  U21:%x[-1,1]/%x[0,1]/%x[1,1]
  //  U22:%x[0,1]/%x[1,1]/%x[2,1]
  //
  //  # Bigram
  //  B
  def processTokSeq(ts: LbledTokSeq, labelIndex: Int = 0, wordIndex: Int = 1): LbledTokSeq = {
    // token offset features
    ts.foreachRow("U00:", Array((-2, 0)))
    ts.foreachRow("U01:", Array((-1, 0)))
    ts.foreachRow("U02:", Array((0, 0)))
    ts.foreachRow("U03:", Array((1, 0)))
    ts.foreachRow("U04:", Array((2, 0)))
    ts.foreachRow("U05:", Array((-1, 0), (0, 0)))
    ts.foreachRow("U06:", Array((0, 0), (1, 0)))

    // POS tag offset bigram features
    ts.foreachRow("U10:", Array((-2, 1)))
    ts.foreachRow("U11:", Array((-1, 1)))
    ts.foreachRow("U12:", Array((0, 1)))
    ts.foreachRow("U13:", Array((1, 1)))
    ts.foreachRow("U14:", Array((2, 1)))
    ts.foreachRow("U15:", Array((-2, 1), (-1, 1)))
    ts.foreachRow("U16:", Array((-1, 1), (0, 1)))
    ts.foreachRow("U17:", Array((0, 1), (1, 1)))
    ts.foreachRow("U18:", Array((1, 1), (2, 1)))

    // POS tag offset trigram features
    ts.foreachRow("U20:", Array((-2, 1), (-1, 1), (0, 1)))
    ts.foreachRow("U21:", Array((-1, 1), (0, 1), (1, 1)))
    ts.foreachRow("U22:", Array((0, 1), (1, 1), (2, 1)))

    // print labels
    if (debuglabels) {
      println(ts.column(labelIndex).mkString("\n"))
      println
    }
    // print features
    if (debugfeatures) {
      println(ts.features.map(_.mkString("\t")).mkString("\n"))
      println
    }
    ts
  }
}