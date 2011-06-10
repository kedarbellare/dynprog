package cc.refectorie.user.kedarb.dynprog;

import cc.refectorie.user.kedarb.tools.opts.Opt;

import java.util.Random;

/**
 * @author kedarb
 * @since Dec 1, 2010
 */
public class Options {
    public enum InitType {
        random, uniform, artificial
    }

    public enum Classifier {
        maxent, naivebayes
    }

    public enum Tagger {
        hmm, crf, coraccrf
    }

    @Opt(gloss = "Filename for labeled data")
    public String labeledFile = null;
    @Opt(gloss = "Filename for unlabeled data")
    public String unlabeledFile = null;
    @Opt(gloss = "Filename for development data")
    public String devFile = null;
    @Opt(gloss = "Filename for test data")
    public String testFile = null;

    @Opt(gloss = "Initialization of parameters")
    public InitType initType = InitType.uniform;
    @Opt
    public Random initRandom = new Random(1);
    @Opt
    public double initProbNoise = 1e-3;
    @Opt
    public double initWtNoise = 1e-3;
    @Opt
    public double initSmoothing = 0.01;

    @Opt
    public Classifier classifier = Classifier.naivebayes;

    @Opt
    public Tagger tagger = Tagger.hmm;
}
