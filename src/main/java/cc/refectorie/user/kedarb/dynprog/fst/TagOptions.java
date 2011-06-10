package cc.refectorie.user.kedarb.dynprog.fst;

import cc.refectorie.user.kedarb.tools.opts.Opt;

/**
 * @author kedarb
 * @since Dec 17, 2010
 */
public class TagOptions {
    public enum DataFormat {
        owpl, corasgml, uiuc
    }

    public enum FeatureMethod {
        cora, conllchunk
    }

    @Opt
    public DataFormat dataFormat = DataFormat.owpl;
    @Opt
    public FeatureMethod featureMethod = FeatureMethod.cora;
    @Opt(gloss = "Only allows transitions seen in training data")
    public boolean sparseTransitions = false;
    @Opt
    public int[] orders = new int[]{0, 1};
    @Opt
    public String lineGroupRegex = "^\\s*$";
    @Opt
    public String lineSplitRegex = "\\s+";
    @Opt(gloss = "Index of the word in line for OWPL format (-ve numbers indicate starting from end)")
    public int wordIndex = 1;
    @Opt
    public int labelIndex = 0;
    @Opt
    public boolean bioEncoding = false;
    @Opt
    public boolean owplDebug = false;
    @Opt(gloss = "Remove features with fewer occurrences")
    public int featureCutoff = 1;

    @Opt
    public boolean useHashing = false;
    @Opt
    public int numFeatures = 10000;

    // types of evaluation
    @Opt(gloss = "Output token labeling accuracy")
    public boolean accuracyEval = false;
    @Opt(gloss = "Output per-label accuracy")
    public boolean perLabelAccuracyEval = false;
    @Opt(gloss = "Output segmentation accuracy")
    public boolean segmentationEval = false;
}
