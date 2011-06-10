package cc.refectorie.user.kedarb.dynprog.fst

import cc.refectorie.user.kedarb.dynprog._
import types._
import utils.Utils._
import types.ParamUtils._
import types.IndexerUtils._
import org.apache.log4j.Logger
import data.CoraCitationFeatures
import collection.mutable.HashMap
import optimization.projections.BoundsProjection
import optimization.gradientBasedMethods._
import cc.refectorie.user.kedarb.dynprog.la._
import optimization.linesearch._
import stats.OptimizerStats
import optimization.stopCriteria.{AverageValueDifference, ProjectedGradientL2Norm}

/**
 * Learns on unlabeled examples as well using constraints.
 *
 * @author kedarb
 * @since 12/23/10
 */

class CoraConstraintCRFProblem(val gen_opts: Options, val seqtag_opts: TagOptions) extends SequenceTaggingProblem {
  type Features = FtrVec

  val constraintFeatures = new Indexer[String]

  def CF = constraintFeatures.size

  def cfstr(i: Int) = constraintFeatures(i)

  // 1. constraint feature functions
  def _newField(l: Int) = "NEWFIELD=" + lstr(l)

  // 2. State transitions only on punctuation marks
  // TODO: Gives worse results with this feature
  // val _noTransOrPrevHasPunc = "NOTRANSITION v HasPuncPrev"
  // val _noTransOrPrevHasPuncNeg = "-" + _noTransOrPrevHasPunc
  // 3. Starts with AUTHOR or EDITOR
  val _startFieldAuthorEditor = "STARTFIELD=author v STARTFIELD=editor"
  val _startFieldAuthorEditorNeg = "-" + _startFieldAuthorEditor
  // 4. No pages feature or PAGES
  val _noPagesOrPageField = "NOPAGES v FIELD=page"
  val _noPagesOrPageFieldNeg = "-" + _noPagesOrPageField
  // 5. No year feature or DATE
  val _noYearOrDateField = "NOYEAR v FIELD=date"
  val _noYearOrDateFieldNeg = "-" + _noYearOrDateField
  // 6. Quotations in title
  val _noQuoteOrTitleField = "NOQUOTE v FIELD=title"
  val _noQuotaOrTitleFieldNeg = "-" + _noQuoteOrTitleField
  // 7. No note word or field=note
  val _noNoteOrNoteField = "NONOTE v FIELD=note"
  val _noNoteOrNoteFieldNeg = "-" + _noNoteOrNoteField
  // 8. No location word or field=location
  val _noLocOrLocField = "NOLOC v FIELD=location"
  val _noLocOrLocFieldNeg = "-" + _noLocOrLocField
  // 9. No tech word or field=tech
  val _noTechOrTechField = "NOTECH v FIELD=tech"
  val _noTechOrTechFieldNeg = "-" + _noTechOrTechField
  // 10. No proc words or field=journal or booktitle
  val _noProcWordsOrJB = "NOPROC v FIELD=journal/booktitle"
  val _noProcWordsOrJBNeg = "-" + _noProcWordsOrJB
  // 11. No editor words oe field=editor
  val _noEditorOrEditorField = "NOEDITOR v FIELD=editor"
  val _noEditorOrEditorFieldNeg = "-" + _noEditorOrEditorField

  // new constraints
  // val _noAuthorToEditor = "NO FIELD=author => FIELD=editor"
  // val _noBooktitleAndVolume = "NO NEWFIELD=booktitle AND NEWFIELD=volume"

  val hasOrder0 = seqtag_opts.orders.contains(0)
  val hasOrder1 = seqtag_opts.orders.contains(1)
  var featureCounts: HashMap[String, Int] = null

  class ConstraintParams(val wts: ParamVec) extends AParams {
    def foreachVec(f: (ParamVec) => Any) = f(wts)

    def output(puts: (String) => Any) = {
      foreachIndex(getValues(wts), {
        (i: Int, v: Double) => puts("CF %s\t%s".format(cfstr(i), fmt(v)))
      })
    }
  }

  class Example(val words: Array[Int], val features: Array[Features], val trueWidget: Widget)
    extends ATaggingExample[Features] {
    // constraint features to be set later
    // foreach ip, prevl, l
    var transitionConstraintFeatures: Array[Array[Array[Features]]] = null

    // E_q[f] <= b
    var b: WeightVec = null
  }

  case class InferState(ex: Example, params: Params, counts: Params, ispec: InferSpec)
    extends ASeqTagInferState[Features, Example](ex, params, counts, ispec) {
    def scoreStart(a: Int) = {
      score(params.starts, a) + {
        if (hasOrder0) score(params.emits0(a), features(0)) else 0.0
      } + {
        if (hasOrder1) score(params.emitsStart(a), features(0)) else 0.0
      }
    }

    def updateStart(a: Int, x: Double) = {
      update(counts.starts, a, x)
      if (hasOrder0) update(counts.emits0(a), features(0), x)
      if (hasOrder1) update(counts.emitsStart(a), features(0), x)
    }

    def scoreTransition(a: Int, b: Int, i: Int) = {
      score(params.transitions(a), b) + {
        if (hasOrder0) score(params.emits0(b), features(i)) else 0.0
      } + {
        if (hasOrder1) score(params.emits1(a)(b), features(i)) else 0.0
      }
    }

    def updateTransition(a: Int, b: Int, i: Int, x: Double) = {
      update(counts.transitions(a), b, x)
      if (hasOrder0) update(counts.emits0(b), features(i), x)
      if (hasOrder1) update(counts.emits1(a)(b), features(i), x)
    }
  }

  case class ConstraintInferState(ex: Example, params: Params, counts: Params,
                                  constraintParams: ConstraintParams, constraintCounts: ConstraintParams,
                                  ispec: InferSpec)
    extends ASeqTagInferState[Features, Example](ex, params, counts, ispec)
    with AConstraintHypergraphInferState[Widget, Example, Params, ConstraintParams] {
    def scoreStart(a: Int) = {
      score(params.starts, a) + {
        if (hasOrder0) score(params.emits0(a), features(0)) else 0.0
      } + {
        if (hasOrder1) score(params.emitsStart(a), features(0)) else 0.0
      } + {
        if (ex.transitionConstraintFeatures(0)(0)(a) != null)
          -score(constraintParams.wts, ex.transitionConstraintFeatures(0)(0)(a))
        else 0.0
      }
    }

    def updateStart(a: Int, x: Double) = {
      update(counts.starts, a, x)
      if (hasOrder0) update(counts.emits0(a), features(0), x)
      if (hasOrder1) update(counts.emitsStart(a), features(0), x)
      if (ex.transitionConstraintFeatures(0)(0)(a) != null)
        update(constraintCounts.wts, ex.transitionConstraintFeatures(0)(0)(a), -x)
    }

    def scoreTransition(a: Int, b: Int, i: Int) = {
      score(params.transitions(a), b) + {
        if (hasOrder0) score(params.emits0(b), features(i)) else 0.0
      } + {
        if (hasOrder1) score(params.emits1(a)(b), features(i)) else 0.0
      } + {
        if (ex.transitionConstraintFeatures(i)(a)(b) != null)
          -score(constraintParams.wts, ex.transitionConstraintFeatures(i)(a)(b))
        else 0.0
      }
    }

    def updateTransition(a: Int, b: Int, i: Int, x: Double) = {
      update(counts.transitions(a), b, x)
      if (hasOrder0) update(counts.emits0(b), features(i), x)
      if (hasOrder1) update(counts.emits1(a)(b), features(i), x)
      if (ex.transitionConstraintFeatures(i)(a)(b) != null)
        update(constraintCounts.wts, ex.transitionConstraintFeatures(i)(a)(b), -x)
    }
  }

  class FeatureCountReader(val filename: String) extends ASeqTagDictReader(filename) {
    def fromTokSeq(ts: LbledTokSeq): Unit = {
      seqtag_opts.featureMethod match {
        case TagOptions.FeatureMethod.cora =>
          CoraCitationFeatures.processTokSeq(ts, Array(Array(-2), Array(-1), Array(1), Array(2)),
            seqtag_opts.labelIndex, seqtag_opts.wordIndex)
        case _ =>
          throw fail("Unknown feature method: " + seqtag_opts.featureMethod)
      }

      ts.features.foreach(_.foreach({
        f: String => featureCounts(f) += 1
      }))
    }
  }

  class DictReader(val filename: String) extends ASeqTagDictReader(filename) {
    def fromTokSeq(ts: LbledTokSeq): Unit = {
      seqtag_opts.featureMethod match {
        case TagOptions.FeatureMethod.cora =>
          CoraCitationFeatures.processTokSeq(ts, Array(Array(-2), Array(-1), Array(1), Array(2)),
            seqtag_opts.labelIndex, seqtag_opts.wordIndex)
        case _ =>
          throw fail("Unknown feature method: " + seqtag_opts.featureMethod)
      }

      val ws = ts.columnIndices(seqtag_opts.wordIndex, words.indexOf_!(_))
      val fvs = ts.featureVectorSequence(features.indexOf_?(_))
      val ls = ts.columnIndices(seqtag_opts.labelIndex, labels.indexOf_!(_))
      var prevl = -1
      foreachIndex(ls, {
        (i: Int, currl: Int) =>
        // initialize the allowed starts/transitions
          if (i == 0) allowedStarts += currl
          else allowedTransitions += prevl -> currl
          // update previous label
          prevl = currl
      })
    }
  }

  class ExampleReader(val filename: String) extends ASeqTagExampleReader[Features, Example](filename) {
    def cfid(s: String): Int = {
      val i = constraintFeatures.indexOf_?(s)
      require(i >= 0, "Constraint feature: " + s + " not in dictionary!")
      i
    }

    def addcf(feats: FtrVec, fname: String, fval: Double, counts: WeightVec, fcnt: Double) = {
      val i = cfid(fname)
      feats += i -> fval
      counts.weights(i) = fcnt
    }

    def fromTokSeq(ts: LbledTokSeq): Example = {
      seqtag_opts.featureMethod match {
        case TagOptions.FeatureMethod.cora =>
          CoraCitationFeatures.processTokSeq(ts, Array(Array(-2), Array(-1), Array(1), Array(2)),
            seqtag_opts.labelIndex, seqtag_opts.wordIndex)
        case _ => throw fail("Unknown feature method: " + seqtag_opts.featureMethod)
      }

      val ws = ts.columnIndices(seqtag_opts.wordIndex, words.indexOf_?(_))
      val fvs = ts.featureVectorSequence(features.indexOf_?(_))
      val ls = ts.columnIndices(seqtag_opts.labelIndex, labels.indexOf_?(_))
      val ex = new Example(ws, fvs, ls)
      val len = ws.length
      ex.transitionConstraintFeatures = Array.ofDim[Features](len, L, L)
      val counts = DenseWeightVec(CF)
      forIndex(len, {
        ip: Int =>
          val wstri = wstr(ws(ip))
          val tftrs = ex.transitionConstraintFeatures(ip)
          forIndex(L, {
            a: Int => forIndex(L, {
              b: Int => val feats = new FtrVec
              val astr = lstr(a)
              val bstr = lstr(b)
              // New field appears at most once
              if (ip == 0 || a != b) {
                // <= 1
                addcf(feats, _newField(b), 1, counts, 1)
                // if (bstr == "booktitle" || bstr == "volume") {
                //  addcf(feats, _noBooktitleAndVolume, 1, counts, 1)
                // }
              }
              // No author -> editor transitions
              // if ((astr == "author" && bstr == "editor") || (astr == "editor" && bstr == "author")) {
              //  addcf(feats, _noAuthorToEditor, 1, counts, 0)
              // }
              // State transitions on punctuations
              // if (ip == 0 || a == b || ts.features(ip - 1).contains("PUNC")) {
              // addcf(feats, _noTransOrPrevHasPunc, 1, counts, len)
              // addcf(feats, _noTransOrPrevHasPuncNeg, -1, counts, -len)
              // }
              // Starts with author or editor
              if (ip == 0 && a == 0 && (bstr == "author" || bstr == "editor")) {
                addcf(feats, _startFieldAuthorEditor, 1, counts, 1)
                addcf(feats, _startFieldAuthorEditorNeg, -1, counts, -1)
              }
              // No pages feature or field=pages
              if (wstri.matches("(pp\\.?|[Pp]ages)") && bstr != "pages") {
                addcf(feats, _noPagesOrPageField, 1, counts, 0)
                addcf(feats, _noPagesOrPageFieldNeg, -1, counts, 0)
              }
              // No year feature or field=date
              if (wstri.matches("\\p{Punct}*(19|20)\\d\\d[a-z]?\\p{Punct}*") && bstr != "date") {
                addcf(feats, _noYearOrDateField, 1, counts, 0)
                addcf(feats, _noYearOrDateFieldNeg, -1, counts, 0)
              }
              // No quote or field=title
              if ((wstri.contains("\"") || wstri.contains("''") || wstri.contains("``")) && bstr != "title") {
                addcf(feats, _noQuoteOrTitleField, 1, counts, 0)
                addcf(feats, _noQuotaOrTitleFieldNeg, -1, counts, 0)
              }
              // No note word or field=note
              if (wstri.matches("([Nn]ote|[Ss]ubmitted|[Aa]ppear)") && bstr != "note") {
                addcf(feats, _noNoteOrNoteField, 1, counts, 0)
                addcf(feats, _noNoteOrNoteFieldNeg, -1, counts, 0)
              }
              // no location word or field=location
              if (wstri.matches("(CA|Australia|NY)") && bstr != "location") {
                addcf(feats, _noLocOrLocField, 1, counts, 0)
                addcf(feats, _noLocOrLocFieldNeg, -1, counts, 0)
              }
              // no tech word or field=tech
              if (wstri.matches("([Tt]ech\\.?|[Tt]echnical)") && bstr != "tech") {
                addcf(feats, _noTechOrTechField, 1, counts, 0)
                addcf(feats, _noTechOrTechFieldNeg, -1, counts, 0)
              }
              // no journal/booktitle words or field=journal/booktitle
              if (wstri.matches("([Pp]roc\\.?|[Jj]ournal|[Pp]roceedings|ACM)") && bstr != "journal" && bstr != "booktitle") {
                addcf(feats, _noProcWordsOrJB, 1, counts, 0)
                addcf(feats, _noProcWordsOrJBNeg, -1, counts, 0)
              }
              // no editor words or field=editor
              if (wstri.matches("(ed\\.?|eds\\.?|Eds?\\.|[Ee]ditors?)") && bstr != "editor") {
                addcf(feats, _noEditorOrEditorField, 1, counts, 0)
                addcf(feats, _noEditorOrEditorFieldNeg, -1, counts, 0)
              }
              // set the features
              if (feats.size > 0) tftrs(a)(b) = feats
            })
          })
      })
      ex.b = counts
      ex
    }
  }

  class Model extends AModel[Widget, Example, Params, InferState] {
    val logger = Logger.getLogger(this.getClass.getSimpleName)

    def opts = gen_opts

    def newParams(dense: Boolean) = new Params(newWtVec(dense, L), newWtVecArray(dense, L, L), {
      if (hasOrder0) newWtVecArray(dense, L, F) else Array[ParamVec]()
    }, {
      if (hasOrder1) newWtVecArray(dense, L, F) else Array[ParamVec]()
    }, {
      if (hasOrder1) newWtVecArray2(dense, L, L, F) else Array[Array[ParamVec]]()
    })

    def newInferState(ex: Example, counts: Params, ispec: InferSpec) = InferState(ex, params, counts, ispec)

    def widgetToFullString(ex: Example, widget: Widget) = {
      var wwStr = ">>\n"
      wwStr += ">Words\n"
      val ws = ex.words.map(wstr(_))
      foreachIndex(ws, {
        (i: Int, w: String) => wwStr += "%s\t\"%s\"\n".format(fmt(i), w)
      })
      wwStr += "\n"
      wwStr += ">Mentions\n"
      val ls = widget.map(lstr(_))
      foreachIndex(ls, {
        (i: Int, l: String) => wwStr += "%s\t%s\t\"%s\"\n".format(fmt(i), fmt(i), l)
      })
      wwStr += "\n"
      wwStr
    }

    def createDictionary = {
      featureCounts = new HashMap[String, Int] {
        override def default(key: String): Int = {
          this(key) = 0;
          0
        }
      }

      // first count features
      countFeatures(opts.labeledFile)
      countFeatures(opts.unlabeledFile)

      // create features dictionary only for counts >= cutoff
      for (f <- featureCounts.keys)
        if (featureCounts(f) >= seqtag_opts.featureCutoff) features.indexOf_!(f)
      logger.info("Input features: " + featureCounts.size +
        " Pruned: " + (featureCounts.size - F) +
        " Remaining: " + F)
      featureCounts = null

      // add unpruned features
      addToDict(opts.labeledFile)
      addToDict(opts.unlabeledFile)
      addToDict(opts.devFile)
      addToDict(opts.testFile)
      logger.info("Allowed starts: " + allowedStarts)
      logger.info("Allowed transitions: " + allowedTransitions)

      // add constraint features for each sequence (length = T)
      forIndex(L, {
        l: Int => constraintFeatures += _newField(l)
      })
      // constraintFeatures += _noTransOrPrevHasPunc
      // constraintFeatures += _noTransOrPrevHasPuncNeg
      // constraintFeatures += _noAuthorToEditor
      // constraintFeatures += _noBooktitleAndVolume

      constraintFeatures += _startFieldAuthorEditor
      constraintFeatures += _startFieldAuthorEditorNeg

      constraintFeatures += _noPagesOrPageField
      constraintFeatures += _noPagesOrPageFieldNeg

      constraintFeatures += _noYearOrDateField
      constraintFeatures += _noYearOrDateFieldNeg

      constraintFeatures += _noQuoteOrTitleField
      constraintFeatures += _noQuotaOrTitleFieldNeg

      constraintFeatures += _noNoteOrNoteField
      constraintFeatures += _noNoteOrNoteFieldNeg

      constraintFeatures += _noLocOrLocField
      constraintFeatures += _noLocOrLocFieldNeg

      constraintFeatures += _noTechOrTechField
      constraintFeatures += _noTechOrTechFieldNeg

      constraintFeatures += _noProcWordsOrJB
      constraintFeatures += _noProcWordsOrJBNeg

      constraintFeatures += _noEditorOrEditorField
      constraintFeatures += _noEditorOrEditorFieldNeg
    }

    override def preInit = {
      features.lock
      constraintFeatures.lock
      words.lock
      labels.lock
    }

    def learn(name: String, lopts: LearnOptions) = {
      if (lopts.online) new OnlineModelLearner(this, lopts).learn(name)
      else new BatchModelLearner(this, lopts).learn(name)
    }

    def serialize(puts: (String) => Any) = {
      serializeIndexer(puts, features)
      serializeIndexer(puts, words)
      serializeIndexer(puts, labels)
      serializeParams(puts, params.starts)
      serializeParamsArray(puts, params.transitions)
      serializeParamsArray(puts, params.emits0)
      serializeParamsArray(puts, params.emitsStart)
      serializeParamsArray2(puts, params.emits1)
      puts(allowedStarts.mkString(" "))
      puts(allowedTransitions.map(p => p._1 + "," + p._2).mkString(" "))
    }

    def deserialize(gets: => String) = {
      deserializeToIndexerString(gets, features)
      deserializeToIndexerString(gets, words)
      deserializeToIndexerString(gets, labels)
      val starts = deserializeParams(gets)
      val transitions = deserializeParamsArray(gets)
      val emits0 = deserializeParamsArray(gets)
      val emitsStart = deserializeParamsArray(gets)
      val emits1 = deserializeParamsArray2(gets)
      allowedStarts ++= gets.split(" ").map(_.toInt)
      allowedTransitions ++= gets.split(" ").map({
        s: String => val p = s.split(",").map(_.toInt); p(0) -> p(1)
      })
      params = new Params(starts, transitions, emits0, emitsStart, emits1)
    }
  }

  def countFeatures(filename: String) = new FeatureCountReader(filename).addToDict

  def addToDict(filename: String) = new DictReader(filename).addToDict

  def getExampleIterator(filename: String): Iterator[Example] = new ExampleReader(filename).exampleIterator

  // model learners
  trait AConstraintModelInferencer extends AModelLearner[Widget, Example, Params, InferState, Model] {
    val projection = new BoundsProjection(0, Double.PositiveInfinity)

    def newConstraintParams: ConstraintParams = new ConstraintParams(DenseWeightVec(CF))

    def optimizeConstraintParamsFor(ex: Example): ConstraintParams = {
      val cparams = newConstraintParams
      val cobjective = new ProjectedObjective {
        val paramsArrayFromVectors = new ArrayFromVectors(cparams.getWtVecs)
        var objectiveValue = Double.NaN
        val b = ex.b
        val constraintInvVariance = 1.0 / lopts.constraintVariance

        // set parameters and gradient
        parameters = new Array[Double](paramsArrayFromVectors.vectorsArraySize)
        paramsArrayFromVectors.getVectorsInArray(parameters)
        gradient = new Array[Double](paramsArrayFromVectors.vectorsArraySize)

        def cparamVec = cparams.wts.asInstanceOf[WeightVec].weights

        override def setParameter(index: Int, value: Double) = {
          updateCalls += 1
          objectiveValue = Double.NaN
          parameters(index) = value
        }

        override def setParameters(params: Array[Double]) = {
          updateCalls += 1
          objectiveValue = Double.NaN
          require(params.length == getNumParameters)
          Array.copy(params, 0, parameters, 0, params.length)
        }

        def updateValueAndGradient: Unit = {
          // set parameters as they may have changed
          paramsArrayFromVectors.setVectorsFromArray(parameters)
          // calculate expectations
          val expectations = newConstraintParams
          objectiveValue = 0.0
          java.util.Arrays.fill(gradient, 0.0)
          // min_{lambda >= 0} b*lambda + log Z(lambda) + 0.5*sigma^-2 ||lambda||^2_2
          // constraints
          objectiveValue += b.dot(cparamVec)
          expectations.add_!(new ConstraintParams(b), 1.0)
          // expectations
          val counts = model.newParams(false)
          val expectationInferState = ConstraintInferState(ex, params, counts, cparams, expectations,
            InferSpec(0, 1, false, false, false, false, true, true, 1, 1))
          expectationInferState.updateCounts
          objectiveValue += expectationInferState.stats.logZ
          // regularization
          objectiveValue += 0.5 * constraintInvVariance * cparams.wtTwoNormSquared
          expectations.add_!(cparams, constraintInvVariance)
          // move expectations to gradient
          new ArrayFromVectors(expectations.getWtVecs).getVectorsInArray(gradient)
        }

        def getValue = {
          if (objectiveValue.isNaN) {
            functionCalls += 1
            updateValueAndGradient
            // info("getValue = " + objectiveValue)
          }
          objectiveValue
        }

        def getGradient = {
          if (objectiveValue.isNaN) {
            gradientCalls += 1
            updateValueAndGradient
          }
          gradient
        }

        def projectPoint(point: Array[Double]) = {
          projection.project(point)
          point
        }

        override def toString = objectiveValue.toString
      }
      val ls = new ArmijoLineSearchMinimizationAlongProjectionArc(new InterpolationPickFirstStep(1))
      val stats = new OptimizerStats
      val optimizer = new ProjectedGradientDescent(ls)
      val stop = new AverageValueDifference(lopts.precision)
      // val stop = new ProjectedGradientL2Norm(lopts.precision)
      val finalIteration = converged || iteration == lopts.numIters
      if (finalIteration) optimizer.setMaxIterations(100)
      else optimizer.setMaxIterations(lopts.numConstraintIters)
      val success = optimizer.optimize(cobjective, stats, stop)
      // info("Ended projected gradient descent (converged=" + success + ") -- " + stats.prettyPrint(1))
      if (finalIteration) cparams.output(info(_))
      // if (success) info("Ended optimization in " + optimizer.getCurrentIteration + " with " + cobjective.toString)
      // else info("Failed to optimize")
      info("Ended projected gradient descent: iteration=" + optimizer.getCurrentIteration +
        " objective=" + cobjective + " converged=" + success)
      cparams
    }
  }

  class OnlineModelLearner(val model: Model, val lopts: LearnOptions)
    extends ADiscriminativeModelLearner[Widget, Example, Params, InferState, Model]
    with AOnlineLearner[Widget, Example, Params, InferState, Model]
    with AConstraintModelInferencer {
    val logger = Logger.getLogger(this.getClass.getSimpleName)

    val testExamples = createArrayFromIterator(getExampleIterator(gen_opts.testFile))
    val validationExamples = createArrayFromIterator(getExampleIterator(gen_opts.devFile))
    val unlabeledExamples = createArrayFromIterator(getExampleIterator(gen_opts.unlabeledFile))
    val labeledExamples = createArrayFromIterator(getExampleIterator(gen_opts.labeledFile))

    def getTestExampleIterator = testExamples.iterator

    def getValidationExampleIterator = validationExamples.iterator

    def getUnlabeledExampleIterator = shuffle(lopts.onlinePermRandom, unlabeledExamples).toIterator

    def getLabeledExampleIterator = shuffle(lopts.onlinePermRandom, labeledExamples).toIterator

    override def processExampleBest(ex: Example) = {
      if (!lopts.constraintInfer) super.processExampleBest(ex)
      else {
        val cparams = optimizeConstraintParamsFor(ex)
        val bestInferState = ConstraintInferState(ex, params, params, cparams, cparams,
          InferSpec(0, 1, true, false, true, false, true, true, 1, 0))
        InferStateOutput(bestInferState.stats, bestInferState.bestWidget)
      }
    }

    override def newEvaluators(name: String) = getEvaluators(name)
  }

  class BatchModelLearner(val model: Model, val lopts: LearnOptions)
    extends ABatchDiscLearner[Widget, Example, Params, InferState, Model]
    with AConstraintModelInferencer {
    val logger = Logger.getLogger(this.getClass.getSimpleName)

    val testExamples = createArrayFromIterator(getExampleIterator(gen_opts.testFile))
    val validationExamples = createArrayFromIterator(getExampleIterator(gen_opts.devFile))
    val unlabeledExamples = createArrayFromIterator(getExampleIterator(gen_opts.unlabeledFile))
    val labeledExamples = createArrayFromIterator(getExampleIterator(gen_opts.labeledFile))

    def getTestExampleIterator = testExamples.iterator

    def getValidationExampleIterator = validationExamples.iterator

    def getUnlabeledExampleIterator = unlabeledExamples.toIterator

    def getLabeledExampleIterator = labeledExamples.toIterator

    override def processExampleBest(ex: Example) = {
      if (!lopts.constraintInfer) super.processExampleBest(ex)
      else {
        val cparams = optimizeConstraintParamsFor(ex)
        val bestInferState = ConstraintInferState(ex, params, params, cparams, cparams,
          InferSpec(0, 1, true, false, true, false, true, true, 1, 0))
        InferStateOutput(bestInferState.stats, bestInferState.bestWidget)
      }
    }

    override def newEvaluators(name: String) = getEvaluators(name)
  }

  def newModel = new Model
}