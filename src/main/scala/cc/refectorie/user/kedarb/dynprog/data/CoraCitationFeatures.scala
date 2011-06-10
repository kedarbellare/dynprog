package cc.refectorie.user.kedarb.dynprog.data

import cc.refectorie.user.kedarb.dynprog.types.{Dict, TrieDict}
import collection.mutable.{ArrayBuffer, HashMap}
import cc.refectorie.user.kedarb.dynprog.fst.{LbledTokSeq, TokFtrFns}

/**
 * @author kedarb
 * @since Dec 16, 2010
 */
object CoraCitationFeatures {
  var debugfeatures = false
  var debuglabels = false

  // flags for features
  var usetriefeatures = true

  // feature types to use
  val useshapefeatures = true
  val usenerfeatures = true
  val usedblpfeatures = true
  val uselineposition = false
  val useregexfeatures = true
  val useveritcalbins = false
  val usehorizontalbins = true

  val prefixToFtrFns = new HashMap[String, String => Option[String]]
  val prefixToTrieLexicon = new HashMap[String, TrieDict]

  val LEX_ROOT = "citations/lexicons/"

  def lex(filename: String, toLC: Boolean = true): Unit = {
    val dict = Dict.fromResourceOrFile(LEX_ROOT + filename, toLC)
    prefixToFtrFns("LEXICON=" + dict.name) = {
      s: String => if (dict.contains(s)) Some("") else None
    }
  }

  def LexiconResource(name: String, filename: String, toLC: Boolean = true): Unit = {
    val dict = Dict.fromResourceOrFile(LEX_ROOT + filename, toLC)
    prefixToFtrFns("LEXICON=" + name) = {
      s: String => if (s.length > 3 && dict.contains(s)) Some("") else None
    }
  }

  def trieLex(filename: String, splitPattern: String = "\\s+", toLC: Boolean = false): TrieDict = {
    TrieDict.fromResourceOrFile(LEX_ROOT + filename, splitPattern, toLC)
  }

  def regex(name: String, pattern: String): Unit = {
    prefixToFtrFns("REGEX=" + name) = {
      s: String => if (s.matches(pattern)) Some("") else None
    }
  }

  def RegexMatcher(name: String, pattern: String) = regex(name, pattern)

  def RegexMatchOrNot(name: String, pattern: String) = {
    prefixToFtrFns("REGEX_PRESENT_" + name + "=") = {
      s: String => if (s.matches(pattern)) Some("true") else Some("false")
    }
  }

  def tokenText(name: String, fn: String => String = identity(_)): Unit = {
    prefixToFtrFns(name) = {
      s: String => Some(fn(s))
    }
  }

  val CAPS = "[A-Z]"
  val ALPHA = "[A-Za-z]"
  val ALPHANUM = "[A-Za-z0-9]"
  val NUM = "[0-9]"
  val PUNC = "[,\\.;:?!()\"'`]"

  val Month = "(?:January|February|March|April|May|June|July|August|September|October|November|December|" +
    "Jan|Feb|Mar|Apr|Jun|Jul|Aug|Sep|Sept|Oct|Nov|Dec)"
  val YearNotInParens = "(19|20|'|`)\\d\\d[a-z]?"
  val YearInParens = "\\(\\s*(19|20)\\d\\d[a-z]?\\s*\\)"
  val Pages = "[0-9]+\\s*[\\-{#]+\\s*[0-9]+"
  val VolumeNumber = "[0-9][0-9]?\\s*\\([0-9]?\\)"
  val Volume = "\\(\\s*[0-9][0-9]?\\s*\\)"
  val Numeric = "[0-9][0-9,]+\\.?[0-9]*"
  val DoTW = "(?:Mon|Tues?|Wed(?:nes)?|Thurs?|Fri|Satu?r?|Sun)(?:day)"
  val CardinalDirection = "(?ii)\\b(?:north|south|east|west)\\s*?(?:east|west|)\\b"

  // add token text fn
  // tokenText("WORD=")

  // add simplified word fn
  def simplify(word: String): String = {
    if (word.matches(YearNotInParens) || word.matches(YearInParens)) "<YEAR>"
    else if (word matches Pages) "<PAGES>"
    else if (word matches VolumeNumber) "<VOLUME_NUMBER>"
    else if (word matches Volume) "<VOLUME>"
    else if (word matches Numeric) "<NUMERIC>"
    else if (word matches DoTW) "<DOFTW>"
    else if (word matches Month) "<MONTH>"
    else if (word matches CardinalDirection) "<CARDINAL-DIRECTION>"
    else word.toLowerCase.replaceAll("\\s+", " ").replaceAll("\\d", "1") //replaceAll("[^a-z0-9\\s\\(\\)\\[\\]]+", ":").
  }

  tokenText("SIMPLIFIED=", simplify(_))

  // add shape features
  if (useshapefeatures) {
    tokenText("SHAPE=", TokFtrFns.wordShape(_, 2))
  }

  // add lexicon feature functions
  if (usedblpfeatures) {
    LexiconResource("DBLPTITLESTARTHIGH", "title.start.high")
    LexiconResource("DBLPTITLESTARTMED", "title.start.med")
    LexiconResource("DBLPTITLEHIGH", "title.high")
    // LexiconResource("DBLPTITLEMED",      "title.med"),
    // LexiconResource("DBLPTITLELOW",      "title.low"),
    LexiconResource("DBLPAUTHORFIRST", "author-first")
    LexiconResource("DBLPAUTHORMIDDLE", "author-middle")
    LexiconResource("DBLPAUTHORLAST", "author-last")
    LexiconResource("CONFABBR", "conferences.abbr")
    // LexiconResource("DBLPPUBLISHER", "publisher")
    // LexiconResource("JOURNAL",           "journals"),
    // LexiconResource("TECH", "tech.txt")
    LexiconResource("PLACES", "places")
    // LexiconResource("NOTEWORDS", "note-words.txt")
    // add trie lexicons
    prefixToTrieLexicon += "TECH" -> trieLex("tech.txt")
    prefixToTrieLexicon += "JOURNAL" -> trieLex("journals")
    prefixToTrieLexicon += "CONFFULL" -> trieLex("conferences.full")
    prefixToTrieLexicon += "NOTEWORDS" -> trieLex("note-words.txt")
    prefixToTrieLexicon += "DBLPPUBLISHER" -> trieLex("publisher")
  }

  if (usenerfeatures) {
    LexiconResource("INSTITUTELEX", "institute-words.txt")
    LexiconResource("FirstHighest", "personname/ssdi.prfirsthighest")
    LexiconResource("FirstHigh", "personname/ssdi.prfirsthigh")
    // LexiconResource("FirstMed",   "personname/ssdi.prfirstmed"),
    // LexiconResource("FirstLow",   "personname/ssdi.prfirstlow"),
    LexiconResource("LastHigest", "personname/ssdi.prlasthighest")
    LexiconResource("LastHigh", "personname/ssdi.prlasthigh")
    // LexiconResource("LastMed",    "personname/ssdi.prlastmed"),
    // LexiconResource("LastLow",    "personname/ssdi.prlastlow"),
    LexiconResource("Honorific", "personname/honorifics")
    LexiconResource("NameSuffix", "personname/namesuffixes")
    LexiconResource("NameParicle", "personname/name-particles")
    // LexiconResource("Nickname",   "personname/nicknames"),
    LexiconResource("Day", "days")
    LexiconResource("Month", "months")
    // LexiconResource("Country", "countries")
    // LexiconResource("State", "US-states")
    LexiconResource("StateAbbrev", "state_abbreviations")
    // LexiconResource("CapitalCity",  "country-capitals"),
    LexiconResource("Stopword", "stopwords")
    // LexiconResource("University", "utexas/UNIVERSITIES")
    prefixToTrieLexicon += "UNIVERSITY" -> trieLex("utexas/UNIVERSITIES")
    prefixToTrieLexicon += "State" -> trieLex("US-states")
    prefixToTrieLexicon += "Country" -> trieLex("countries")
  }

  if (useregexfeatures) {
    RegexMatcher("CONTAINSDOTS", "[^\\.]*\\..*")
    RegexMatcher("CONTAINSCOMMA", ".*,.*")
    RegexMatcher("CONTAINSDASH", ALPHANUM + "+-" + ALPHANUM + "*")
    RegexMatcher("ACRO", "[A-Z][A-Z\\.]*\\.[A-Z\\.]*")
    RegexMatcher("PUNC", PUNC)

    RegexMatcher("URL1", "www\\..*|https?://.*|ftp\\..*|.*\\.edu/?.*")
    //RegexMatcher("URL2",            "www.*\\.(com|es|org)"),
    //RegexMatcher("EMAIL",           "\\S+@\\S+|e-mail.*|email.*|Email.*|.*\\.edu"),

    // patterns involving numbers
    RegexMatcher("PossibleYear", "(" + YearInParens + "|" + YearNotInParens + ")")
    RegexMatcher("PossiblePage", "[0-9]+\\s*[\\-{#]+\\s*[0-9]+")
    RegexMatcher("PossibleVol", "[0-9][0-9]?\\s*\\([0-9]+\\)")
    RegexMatcher("1-digit", "[0-9]")
    RegexMatcher("2-digit", "[0-9][0-9]")
    RegexMatcher("3-digit", "[0-9][0-9][0-9]")
    RegexMatcher("4-digit", "[0-9][0-9][0-9][0-9]")
    RegexMatcher("5+digit", "[0-9][0-9][0-9][0-9][0-9]+")
    //RegexMatchOrNot("HasDigit",  ".*[0-9].*"),
    //RegexMatcher("AllDigits",      "[0-9]+"),
    //RegexMatcher("Numerical",      "[0-9\\.]*\\.[0-9\\.]+"),
    RegexMatcher("YearInParens", YearInParens)
    RegexMatcher("YearNotInParens", YearNotInParens)

    //RegexMatcher("USZIP",           "[0-9][0-9][0-9][0-9][0-9](?:-[0-9][0-9][0-9][0-9])?"),
    //RegexMatcher("USPHONEAREACODE", "\\([0-9][0-9][0-9]\\)"),

    //RegexMatcher("INPARENS",        "\\(.*\\)"),
    //RegexMatcher("ALPHA_THEN_NUM", "[A-Za-z]+[0-9]+"),
    //RegexMatcher("NUM_THEN_ALPHA", "[0-9]+[A-Za-z]+"),

    RegexMatcher("PAGEWORDS", "(?:pp\\.|[Pp]ages?|[\\-,\\.()]|[0-9]\\s+)+")

    // Ordinals
    RegexMatcher("ORDINAL1", "(?ii)[0-9]+(?:st|nd|rd|th)")
    RegexMatcher("ORDINAL2", ("(?ii)(?:"
      + "first|second|third|fourth|fifth|sixth|seventh|eighth|ninth|tenth"
      + "|eleventh|twelfth|thirteenth|fourteenth|fifteenth|sixteenth"
      + "|seventeenth|eighteenth|nineteenth"
      + "|twentieth|thirtieth|fou?rtieth|fiftieth|sixtieth|seventieth"
      + "|eightieth|ninetieth|twentieth"
      + "|hundredth|thousandth|millionth|billionth"
      + ")"))

    /* keywords
    RegexMatcher("VENUEWORDS",      List("Proceedings", "Journal", "International", "Conference", "Workshop",
                                         "Annual", "Transactions", "Record", "Bulletin", "Society", "Symposium",
                                         "Computer", "Computing", "System", "Science", "Sciences",
                                         "Communications", "Communications", "Notices", "Letters", "AAAI",
                                         "ACM", "NIPS", "National", "European").mkString("(?:", "|", ")")),
    */

    // Punctuation
    RegexMatcher("LeadQuote", "[\"'`]")
    RegexMatcher("EndQuote", "[\"'`][^s]?")
    RegexMatcher("MultiHyphen", "\\S*-\\S*-\\S*")
    RegexMatcher("ContainsPunc", "[\\-,\\:\\;]")
    RegexMatcher("StopPunc", "[\\!\\?\\.\"\']")
    //RegexMatcher("Braces",        "[\\(\\[\\{\\<].+[\\)\\]\\}\\>].?"),

    // Character-based
    RegexMatcher("LONELYINITIAL", CAPS + "\\.")
    RegexMatcher("SINGLECHAR", ALPHA)
    RegexMatcher("CAPLETTER", CAPS)
    RegexMatcher("ALLCAPS", CAPS + "+")
    RegexMatcher("INITCAP", CAPS + ".*")
  }

  RegexMatcher("HASEDITOR", "(ed\\.|editor|editors|eds\\.)")
  RegexMatcher("HASINSTITUTE", "(University|Universite|Universiteit|Univ\\.?|Dept\\.?|Institute|Corporation|Department|Laboratory|Laboratories|Labs)")

  def processLines(lines: Array[String], splitRegex: String = "\\s+", conjunctions: Array[Array[Int]] = null,
                   labelIndex: Int = 0, wordIndex: Int = 1): LbledTokSeq = {
    val ts = new LbledTokSeq(lines, splitRegex)
    processTokSeq(ts, conjunctions, labelIndex, wordIndex)
    ts
  }

  def processTokSeq(ts: LbledTokSeq, conjunctions: Array[Array[Int]] = null,
                    labelIndex: Int = 0, wordIndex: Int = 1): LbledTokSeq = {
    // apply normal feature functions
    ts.addFeaturesUsingFunctions(wordIndex, prefixToFtrFns)
    // add trie features
    if (usetriefeatures) prefixToTrieLexicon.keys.foreach {
      prefix: String => ts.addTrieFeatures(wordIndex, prefix, prefixToTrieLexicon(prefix))
    }
    // add offset conjuctions
    if (conjunctions != null && conjunctions.size > 0)
      ts.addFeatureConjuctions(conjunctions)
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