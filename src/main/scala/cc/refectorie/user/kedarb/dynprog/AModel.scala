package cc.refectorie.user.kedarb.dynprog

import utils.Utils._
import java.io._
import java.util.Random
import types._

/**
 * @author kedarb
 * @since Sep 16, 2010
 */

trait AExample[Widget] {
  def numTokens: Int

  // number of tokens to calculate accuracy
  def trueWidget: Widget

  // a widget can be labeling of a sequence, an alignment, a dependency tree, etc.
}

trait IModel {
  def opts: Options

  def createDictionary: Unit

  def preInit: Unit

  def init(initType: Options.InitType, initRandom: java.util.Random): Unit

  def learn(name: String, lopts: LearnOptions): Unit

  def save(filename: String): Unit

  def load(filename: String): Unit
}

trait AModel[Widget, Example <: AExample[Widget], Params <: AParams,
InferState <: AInferState[Widget, Example, Params]] extends IModel {
  var paramsOpt: Option[Params] = None

  def params: Params = paramsOpt match {
    case Some(x) => x; case None => throw fail("No params")
  }

  def params_=(x: Params) = {
    paramsOpt = Some(x)
  }

  def newParams: Params = newParams(true)

  def newParams(dense: Boolean): Params

  def newPrVec(dense: Boolean, len: Int): ParamVec = ParamUtils.newPrVec(dense, len)

  def newPrVecArray(dense: Boolean, size: Int, len: Int) = ParamUtils.newPrVecArray(dense, size, len)

  def newPrVecArray2(dense: Boolean, size1: Int, size2: Int, len: Int) = ParamUtils.newPrVecArray2(dense, size1, size2, len)

  def newWtVec(dense: Boolean, len: Int): ParamVec = ParamUtils.newWtVec(dense, len)

  def newWtVecArray(dense: Boolean, size: Int, len: Int) = ParamUtils.newWtVecArray(dense, size, len)

  def newWtVecArray2(dense: Boolean, size1: Int, size2: Int, len: Int) = ParamUtils.newWtVecArray2(dense, size1, size2, len)

  def preInit: Unit = {}

  def artificialInitParams: Unit = throw fail("Not supported; please override")

  def init(initType: Options.InitType, initRandom: Random): Unit = {
    params = newParams
    initType match {
      case Options.InitType.artificial => artificialInitParams
      case Options.InitType.uniform =>
        params.setUniform_!
        params.normalize_!(opts.initSmoothing)
      case Options.InitType.random =>
        params.prRandomize_!(opts.initRandom, opts.initProbNoise)
        params.wtRandomize_!(opts.initRandom, opts.initWtNoise)
        params.normalize_!(opts.initSmoothing)
      case _ => throw fail("Not implemented for type: " + initType)
    }
  }

  def newInferState(ex: Example, counts: Params, ispec: InferSpec): InferState

  def widgetToFullString(ex: Example, widget: Widget): String

  // serialize model: write alphabets and parameters
  def serialize(puts: (String => Any))

  def deserialize(gets: => String)

  def save(filename: String) = {
    val modelOut = new PrintStream(new FileOutputStream(filename))
    def out(s: String) = modelOut.println(s)
    serialize(out)
    modelOut.close
  }

  def load(filename: String) = {
    val modelIn = new BufferedReader(new FileReader(filename))
    def in = modelIn.readLine
    deserialize(in)
    modelIn.close
  }
}

trait AProblem {
  def newModel: IModel
}