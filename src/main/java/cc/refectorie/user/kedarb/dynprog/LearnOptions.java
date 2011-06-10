package cc.refectorie.user.kedarb.dynprog;

import cc.refectorie.user.kedarb.tools.opts.Opt;

import java.util.Random;

/**
 * @author kedarb
 * @since Dec 1, 2010
 */
public class LearnOptions {
    @Opt(gloss = "Number of training iterations")
    public int numIters = 0;

    @Opt(gloss = "Number of threads during learning")
    public int numThreads = 1;

    @Opt(gloss = "Initial temperature for annealing")
    public double initTemperature = 1;
    @Opt(gloss = "Final temperature for annealing")
    public double finalTemperature = 1;
    @Opt(gloss = "Weight of labeled examples")
    public double labeledWeight = 1;
    @Opt(gloss = "Weight of unlabeled examples")
    public double unlabeledWeight = 1;
    @Opt(gloss = "Use hard inference during learning")
    public boolean hardInfer = false;

    @Opt(gloss = "Online")
    public boolean online = false;
    @Opt
    public Random onlinePermRandom = new Random(1);
    @Opt
    public double stepSizeOffset = 2;
    @Opt(gloss = "Step size power 1/T^power")
    public double stepSizeReductionPower = 0.5;
    @Opt(gloss = "Add smoothing when compute MAP")
    public double smoothing = 1e-3;
    @Opt(gloss = "Regular stepwise EM")
    public boolean convexCombUpdate = false;

    @Opt(gloss = "Output directory", required = true)
    public String outputDir = null;

    @Opt(gloss = "Output every this number of iterations")
    public int outputIterFreq = 1;
    @Opt(gloss = "Output parameters")
    public boolean outputParams = false;
    @Opt(gloss = "Output performance")
    public boolean outputEvaluation = false;
    @Opt(gloss = "Output full predictions")
    public boolean outputFullPred = false;

    @Opt(gloss = "Perform constrained inference using posterior regularization objective")
    public boolean constraintInfer = false;
    @Opt(gloss = "Number of constraint parameter learning iterations")
    public int numConstraintIters = 10;
    @Opt(gloss = "Variance of constraint parameters")
    public double constraintVariance = 100.0;

    public enum ConvergeType {
        objDiff, objRelDiff
    }

    @Opt
    public ConvergeType convergenceType = ConvergeType.objDiff;

    @Opt(gloss = "Precision of optimizer")
    public double precision = 1e-5;

    @Opt(gloss = "Prior variance of weight parameters")
    public double gaussianPriorVariance = 1.0;
}
