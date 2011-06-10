package cc.refectorie.user.kedarb.dynprog.types;

import cc.refectorie.user.kedarb.tools.utils.ListUtils;
import cc.refectorie.user.kedarb.tools.utils.NumUtils;
import cc.refectorie.user.kedarb.tools.utils.SampleUtils;

import java.util.*;

/**
 * Provides a module for doing inference over discrete structures such as
 * sequences and parse trees. A distribution over possibly exponentially many
 * structures (widgets) is encoded compactly using a directed hypergraph.
 *
 * Each hyperedge corresponds to a discrete decision which describes part of the
 * structure. At sum nodes, exactly one hyperedge is chosen; at prod nodes, all
 * hyperedges are chosen. The product of all chosen weights on the hyperedges
 * determines the probability of the widget (after appropriate normalization).
 *
 * Notes: - We don't check for cycles. You will get stack overflow if there are
 * cycles. - The children of a product node or the two children of a hyperedge
 * should be disjoint. Otherwise, you will get an assertion failed with invalid
 * posterior probability > 1 due to double counting. NOTE: this is unnecessarily
 * restrictive. We should get rid of this, but have to be careful. Updating
 * posteriors with prob > 1 is fine, but when fetching the widget, user must do
 * it right.
 *
 * To construct the graph: if(!addSumNode(node)) addEdge(node, child1, child2,
 * new HyperedgeInfo<Widget>() { // Implement getWeight(), setPosterior(),
 * choose() here });
 *
 * To do inference and get back results: computePosteriors(): perform inference
 * to compute posterior distribution over hyperpaths computeELogZEntropy():
 * compute some statistics about this inference fetchPosteriors(): call
 * setPosterior on each hyperedge fetchBestHyperpath(widget): call choose on
 * each hyperedge in the best one fetchSampleHyperpath(widget): call choose on
 * each hyperedge in the best one fetchPosteriorHyperpath(widget): call choose
 * on each hyperedge with a weight (TODO: combine it with fetchPosteriors)
 *
 * @author Percy Liang
 */

/**
 * Simple extension of Hypergraph where all operations are in terms of
 * log-probabilities (or weights). We do not use BigDouble as it leads to memory
 * issues.
 *
 * @author kedar
 */
public class Hypergraph<Widget> {
    public enum NodeType {
        // Each node represents either a product or a sum over its children
        prod, sum
    }

    public interface AHyperedgeInfo<Widget> {
        public void setPosterior(double prob);

        public Widget choose(Widget widget); // Return the updated widget
    }

    public interface HyperedgeInfo<Widget> extends AHyperedgeInfo<Widget> {
        public double getWeight();
    }

    public interface ProbHyperedgeInfo<Widget> extends AHyperedgeInfo<Widget> {
        public double getProbability();
    }

    private class NullHyperedgeInfo<Widget> implements HyperedgeInfo<Widget> {
        public double getWeight() {
            // prob = 1, log(prob) = 0
            return 0;
        }

        public void setPosterior(double prob) {
        }

        public Widget choose(Widget widget) {
            return widget;
        }
    }

    private final NullHyperedgeInfo<Widget> nullHyperedgeInfo = new NullHyperedgeInfo<Widget>();

    private static class NodeInfo {
        NodeInfo(Object node, NodeType nodeType) {
            this.node = node;
            this.nodeType = nodeType;
        }

        // Just for visualizing/debugging
        final Object node;
        // Can be changed
        NodeType nodeType;
        // Children
        final List<Hyperedge> edges = new ArrayList<Hyperedge>();
        // Things we compute during inference
        double insideScore = Double.NaN, outsideScore = Double.NaN,
                maxScore = Double.NaN;

        public String toString() {
            return node.toString() + "\t" + edges.toString();
        }
    }

    private static class Hyperedge {
        final NodeInfo dest1, dest2; // Child nodes
        final AHyperedgeInfo info; // Specifies I/O for the hyperedge
        final double weight;

        Hyperedge(NodeInfo dest1, NodeInfo dest2, AHyperedgeInfo info) {
            this.dest1 = dest1;
            this.dest2 = dest2;
            this.info = info;
            if (info instanceof HyperedgeInfo)
                this.weight = ((HyperedgeInfo) info).getWeight();
            else if (info instanceof ProbHyperedgeInfo)
                this.weight = Math.log(((ProbHyperedgeInfo) info)
                        .getProbability());
            else
                throw new RuntimeException("Unknown type of info");

            // XXX Avoid zeros so everything has some positive probability
            // this.weight = (wt == Double.NEGATIVE_INFINITY
            // || wt < VERY_SMALL_WEIGHT ? VERY_SMALL_WEIGHT : wt);
        }

        public String toString() {
            return String.format("%s/%s (%s)", dest1.node, dest2.node, weight);
        }
    }

    // Specifies the hypergraph and stores the computations
    public boolean debug = false;
    // Do we allow nodes with no children?
    public boolean allowEmptyNodes = false;
    private HashMap<Object, NodeInfo> nodes = new HashMap<Object, NodeInfo>();
    private NodeInfo[] topologicalOrdering;

    // Start and end nodes
    // use sum or prod versions
    private final Object startNode = addNodeAndReturnIt("START", NodeType.sum);
    public final Object endNode = addNodeAndReturnIt("END", NodeType.sum);
    public final Object invalidNode = "INVALID";
    private final NodeInfo startNodeInfo = getNodeInfoOrFail(startNode);
    private final NodeInfo endNodeInfo = getNodeInfoOrFail(endNode);
    private Hyperedge terminalEdge = new Hyperedge(endNodeInfo, endNodeInfo,
            nullHyperedgeInfo);

    // Things we're going to compute
    // Normalization constant
    private double logZ = Double.NaN;
    // E_q(z|x) log weight(x,z)
    private double elogZ = Double.NaN;
    // Entropy of the posterior q(z|x)
    private double entropy = Double.NaN;

    public double getLogZ() {
        return logZ;
    }

    public double getELogZ() {
        return elogZ;
    }

    public double getEntropy() {
        return entropy;
    }

    // Add nodes: return whether added something

    public boolean addSumNode(Object node) {
        return addNode(node, NodeType.sum);
    }

    public boolean addProdNode(Object node) {
        return addNode(node, NodeType.prod);
    }

    public Object sumStartNode() {
        getNodeInfoOrFail(startNode).nodeType = NodeType.sum;
        return startNode;
    }

    public Object prodStartNode() {
        getNodeInfoOrFail(startNode).nodeType = NodeType.prod;
        return startNode;
    }

    public int numEdges(Object node) {
        return getNodeInfoOrFail(node).edges.size();
    }

    public int numNodes() {
        return nodes.size();
    }

    public void assertNonEmpty(Object node) {
        assert numEdges(node) > 0 : node
                + " has no children hyperedges (it's empty)";
    }

    // Add edges

    public void addEdge(Object source) {
        addEdge(source, endNode, endNode, nullHyperedgeInfo);
    }

    public void addEdge(Object source, AHyperedgeInfo<Widget> info) {
        addEdge(source, endNode, endNode, info);
    }

    public void addEdge(Object source, Object dest1) {
        addEdge(source, dest1, endNode, nullHyperedgeInfo);
    }

    public void addEdge(Object source, Object dest1, AHyperedgeInfo<Widget> info) {
        addEdge(source, dest1, endNode, info);
    }

    public void addEdge(Object source, Object dest1, Object dest2) {
        addEdge(source, dest1, dest2, nullHyperedgeInfo);
    }

    public void addEdge(Object source, Object dest1, Object dest2,
                        AHyperedgeInfo<Widget> info) {
        assert source != invalidNode;
        // if (debug)
        // LogInfo.dbgs("add %s -> %s %s", source, dest1, dest2);
        if (dest1 == invalidNode || dest2 == invalidNode)
            return;
        assert source != dest1 && source != dest2; // Catch obvious loops
        getNodeInfoOrFail(source).edges.add(new Hyperedge(
                getNodeInfoOrFail(dest1), getNodeInfoOrFail(dest2), info));
    }

    // Helpers
    // Return whether a new node was added

    private boolean addNode(Object node, NodeType nodeType) {
        NodeInfo info = nodes.get(node);
        if (info != null)
            return false;
        nodes.put(node, new NodeInfo(node, nodeType));
        return true;
    }

    // Return the node that we added

    private Object addNodeAndReturnIt(Object node, NodeType nodeType) {
        if (!addNode(node, nodeType))
            throw new IllegalStateException("Can't add node");
        return node;
    }

    private NodeInfo getNodeInfoOrFail(Object node) {
        NodeInfo info = nodes.get(node);
        assert info != null : "Node doesn't exist in hypergraph (need to add nodes before edges containing them): "
                + node;
        return info;
    }

    private void checkGraph() {
        // Make sure that all nodes have children (except end of course)
        if (!allowEmptyNodes) {
            int numBadNodes = 0;
            for (NodeInfo nodeInfo : nodes.values()) {
                if (nodeInfo.edges.size() == 0 && nodeInfo.node != endNode) {
                    System.err
                            .println("Node has no children: " + nodeInfo.node);
                    numBadNodes++;
                }
            }
            if (numBadNodes > 0)
                throw new IllegalStateException(numBadNodes + " bad nodes");
        }

        // TODO: check for cycles to be more graceful
        // Now, we just wait for computeTopologicalOrdering() to stack overflow
    }

    private static class IntRef {
        public int value;

        public IntRef(int val) {
            this.value = val;
        }
    }

    private void computeTopologicalOrdering() {
        if (topologicalOrdering != null)
            return;
        checkGraph();
        topologicalOrdering = new NodeInfo[nodes.size()];
        HashSet<NodeInfo> hit = new HashSet<NodeInfo>();
        IntRef i = new IntRef(nodes.size() - 1);
        computeReverseTopologicalOrdering(hit, startNodeInfo, i);
        if (i.value != -1)
            throw new IllegalStateException(
                    "Not all nodes reachable from startNode");
        assert topologicalOrdering[0] == startNodeInfo;
        if (!allowEmptyNodes)
            assert topologicalOrdering[topologicalOrdering.length - 1] == endNodeInfo;
    }

    private void computeReverseTopologicalOrdering(HashSet<NodeInfo> hit,
                                                   NodeInfo nodeInfo, IntRef i) {
        if (hit.contains(nodeInfo))
            return;
        for (Hyperedge edge : nodeInfo.edges) {
            computeReverseTopologicalOrdering(hit, edge.dest1, i);
            computeReverseTopologicalOrdering(hit, edge.dest2, i);
        }
        topologicalOrdering[i.value--] = nodeInfo;
        hit.add(nodeInfo);
    }

    public void printTopologicalOrdering() {
        if (topologicalOrdering == null)
            computeTopologicalOrdering();
        for (int i = 0; i < topologicalOrdering.length; i++) {
            System.err.println("[" + i + "] " + topologicalOrdering[i]);
        }
    }

    /* ////////////////////////////////////////////////////////// */

    public Hypergraph() {
    }

    public void computePosteriors(boolean viterbi) {
        computeTopologicalOrdering();
        computeInsideMaxScores(viterbi);
        if (!viterbi)
            computeOutsideScores();
        if (viterbi)
            this.logZ = startNodeInfo.maxScore;
        else
            this.logZ = startNodeInfo.insideScore;
    }

    // Methods for updating scores

    private void setScore(NodeInfo nodeInfo, double score, boolean viterbi) {
        if (viterbi)
            nodeInfo.maxScore = score;
        else
            nodeInfo.insideScore = score;
    }

    private boolean updateMaxScore(NodeInfo nodeInfo, double score,
                                   boolean viterbi) {
        assert viterbi : "updateMax is only for viterbi updates!";
        boolean updated = nodeInfo.maxScore < score;
        nodeInfo.maxScore = Math.max(nodeInfo.maxScore, score);

        return updated;
    }

    // Adding in log-space

    private void incrScore(NodeInfo nodeInfo, double score, boolean viterbi) {
        if (viterbi)
            nodeInfo.maxScore = NumUtils.logAdd(nodeInfo.maxScore, score);
        else
            nodeInfo.insideScore = NumUtils.logAdd(nodeInfo.insideScore, score);
    }

    // Multiplying in log-space

    private void multScore(NodeInfo nodeInfo, double score, boolean viterbi) {
        if (viterbi)
            nodeInfo.maxScore += score;
        else
            nodeInfo.insideScore += score;
    }

    private void computeInsideMaxScores(boolean viterbi) {
        if (viterbi && !Double.isNaN(this.startNodeInfo.maxScore))
            return; // Already computed
        if (!viterbi && !Double.isNaN(this.startNodeInfo.insideScore))
            return; // Already computed

        for (int i = topologicalOrdering.length - 1; i >= 0; i--) {
            NodeInfo nodeInfo = topologicalOrdering[i];
            if (nodeInfo == endNodeInfo) {
                // initialize end node's score to 0
                setScore(nodeInfo, 0, viterbi);
                continue;
            }

            // perform sumProduct or maxProduct
            switch (nodeInfo.nodeType) {
                case sum:
                    setScore(nodeInfo, Double.NEGATIVE_INFINITY, viterbi);
                    for (Hyperedge edge : nodeInfo.edges) {
                        if (viterbi)
                            updateMaxScore(nodeInfo, edge.weight
                                    + edge.dest1.maxScore + edge.dest2.maxScore,
                                    viterbi);
                        else
                            incrScore(nodeInfo, edge.weight
                                    + edge.dest1.insideScore
                                    + edge.dest2.insideScore, viterbi);
                    }
                    break;
                case prod:
                    setScore(nodeInfo, 0, viterbi);
                    for (Hyperedge edge : nodeInfo.edges) {
                        if (viterbi)
                            multScore(nodeInfo, edge.weight + edge.dest1.maxScore
                                    + edge.dest2.maxScore, viterbi);
                        else
                            multScore(nodeInfo, edge.weight
                                    + edge.dest1.insideScore
                                    + edge.dest2.insideScore, viterbi);
                    }
                    break;
            }
        }
        if (viterbi)
            assert !Double.isNaN(startNodeInfo.maxScore)
                    && !Double.isInfinite(startNodeInfo.maxScore) : "Max score = "
                    + startNodeInfo.maxScore + "!!";
        else
            assert !Double.isNaN(startNodeInfo.insideScore)
                    && !Double.isInfinite(startNodeInfo.insideScore) : "Marginal score = "
                    + startNodeInfo.insideScore + "!!";
    }

    private void computeOutsideScores() {
        if (!Double.isNaN(startNodeInfo.outsideScore))
            return; // Already computed

        // Initialize values to zero
        for (NodeInfo nodeInfo : topologicalOrdering)
            nodeInfo.outsideScore = Double.NEGATIVE_INFINITY;

        startNodeInfo.outsideScore = 0;
        for (int i = 0; i < topologicalOrdering.length; i++) {
            NodeInfo nodeInfo = topologicalOrdering[i];
            if (nodeInfo.insideScore == Double.NEGATIVE_INFINITY)
                continue; // This happens for dead nodes
            // dbgs("outsideScore(%s) = %s", nodeInfo.node,
            // nodeInfo.outsideScore);
            switch (nodeInfo.nodeType) {
                case sum:
                    for (Hyperedge edge : nodeInfo.edges) {
                        if (edge.dest1 != endNodeInfo)
                            edge.dest1.outsideScore = NumUtils.logAdd(
                                    edge.dest1.outsideScore, nodeInfo.outsideScore
                                            + edge.weight + edge.dest2.insideScore);
                        if (edge.dest2 != endNodeInfo)
                            edge.dest2.outsideScore = NumUtils.logAdd(
                                    edge.dest2.outsideScore, nodeInfo.outsideScore
                                            + edge.weight + edge.dest1.insideScore);
                    }
                    break;
                case prod:
                    for (Hyperedge edge : nodeInfo.edges) {
                        if (edge.dest1 != endNodeInfo)
                            edge.dest1.outsideScore = NumUtils.logAdd(
                                    edge.dest1.outsideScore, nodeInfo.outsideScore
                                            + nodeInfo.insideScore
                                            - edge.dest1.insideScore);
                        if (edge.dest2 != endNodeInfo)
                            edge.dest2.outsideScore = NumUtils.logAdd(
                                    edge.dest2.outsideScore, nodeInfo.outsideScore
                                            + nodeInfo.insideScore
                                            - edge.dest2.insideScore);
                    }
                    break;
            }
        }
    }

    public void fetchPosteriors(boolean viterbi) {
        if (viterbi)
            // Only need to call setPosteriors on the best widget
            fetchPosteriorsMax();
        else
            // Call setPosteriors on each hyperedge
            fetchPosteriorsSum();
    }

    public void computeELogZEntropy(boolean viterbi) {
        if (viterbi) { // Easy case: q(z|x) is degenerate
            this.elogZ = this.logZ;
            this.entropy = 0;
            return;
        }

        this.elogZ = 0;
        this.entropy = 0;
        for (NodeInfo nodeInfo : topologicalOrdering) {
            // dbg(startNodeInfo.insideScore);
            double nodeProb = Math.exp(nodeInfo.outsideScore
                    + nodeInfo.insideScore - startNodeInfo.insideScore);
            if (nodeProb == 0)
                continue;
            switch (nodeInfo.nodeType) {
                case sum:
                    for (Hyperedge edge : nodeInfo.edges) {
                        double edgeProb = Math.exp(nodeInfo.outsideScore
                                + edge.weight + edge.dest1.insideScore
                                + edge.dest2.insideScore
                                - startNodeInfo.insideScore);
                        if (edgeProb == 0)
                            continue;
                        elogZ += edgeProb * edge.weight;
                        entropy -= edgeProb * Math.log(edgeProb / nodeProb);
                    }
                    break;
                case prod:
                    // No uncertainty, so no contribution to entropy
                    for (Hyperedge edge : nodeInfo.edges)
                        elogZ += nodeProb * edge.weight;
                    break;
            }
        }
    }

    private void fetchPosteriorsSum() {
        for (NodeInfo nodeInfo : topologicalOrdering) {
            switch (nodeInfo.nodeType) {
                case sum:
                    for (Hyperedge edge : nodeInfo.edges) {
                        double prob = Math.exp(nodeInfo.outsideScore + edge.weight
                                + edge.dest1.insideScore + edge.dest2.insideScore
                                - startNodeInfo.insideScore);
                        assert prob >= 0 && prob <= 1 + 1e-6 : nodeInfo + " "
                                + edge + " has invalid posterior probability "
                                + prob;
                        // if(prob > 0.1) dbgs("setPosterior sum %s %s", edge,
                        // Fmt.D(prob));
                        edge.info.setPosterior(prob);
                    }
                    break;
                case prod:
                    for (Hyperedge edge : nodeInfo.edges) {
                        double prob = Math.exp(nodeInfo.outsideScore
                                + nodeInfo.insideScore - startNodeInfo.insideScore);
                        assert prob >= 0 && prob <= 1 + 1e-6 : nodeInfo + " "
                                + edge + " has invalid posterior probability "
                                + prob;
                        // if(prob > 0.1) dbgs("setPosterior prod %s %s", edge,
                        // Fmt.D(prob));
                        edge.info.setPosterior(prob);
                    }
                    break;
            }
        }
    }

    private void fetchPosteriorsMax() {
        HyperpathChooser chooser = new HyperpathChooser();
        chooser.viterbi = true;
        chooser.setPosterior = true;
        chooser.recurse(startNodeInfo);
    }

    // Return the best or a sampled solution

    public HyperpathResult<Widget> fetchBestHyperpath(Widget widget) {
        computeInsideMaxScores(true);
        HyperpathChooser chooser = new HyperpathChooser();
        chooser.viterbi = true;
        chooser.widget = widget;
        chooser.choose = true;
        chooser.recurse(startNodeInfo);
        return new HyperpathResult(chooser.widget, chooser.logWeight);
    }

    public HyperpathResult<Widget> fetchSampleHyperpath(Random random,
                                                        Widget widget) {
        computeInsideMaxScores(false);
        HyperpathChooser chooser = new HyperpathChooser();
        chooser.viterbi = false;
        chooser.widget = widget;
        chooser.random = random;
        chooser.choose = true;
        chooser.recurse(startNodeInfo);
        return new HyperpathResult(chooser.widget, chooser.logWeight);
    }

    public static class HyperpathResult<Widget> {
        public HyperpathResult(Widget widget, double logWeight) {
            this.widget = widget;
            this.logWeight = logWeight;
        }

        public final Widget widget;
        public final double logWeight;
    }

    private class HyperpathChooser {
        boolean viterbi;
        Widget widget;
        Random random;
        // Which function to call to return what was chosen
        boolean choose;
        boolean setPosterior;
        // Likelihood of the weight of the hyperpath chosen
        double logWeight;

        private void recurse(NodeInfo nodeInfo) {
            if (nodeInfo == endNodeInfo)
                return;

            switch (nodeInfo.nodeType) {
                case sum:
                    int n = nodeInfo.edges.size();
                    // Compute scores
                    double[] scores = new double[n];
                    for (int i = 0; i < n; i++) {
                        Hyperedge edge = nodeInfo.edges.get(i);
                        if (viterbi)
                            scores[i] = edge.weight + edge.dest1.maxScore
                                    + edge.dest2.maxScore;
                        else
                            scores[i] = edge.weight + edge.dest1.insideScore
                                    + edge.dest2.insideScore;
                    }
                    // Choose edge
                    int chosenIndex;
                    if (viterbi)
                        chosenIndex = ListUtils.maxIndex(scores);
                    else {
                        NumUtils.expNormalize(scores);
                        chosenIndex = SampleUtils.sampleMultinomial(random, scores);
                    }
                    if (chosenIndex == -1)
                        throw new IllegalStateException("Unable to choose from: "
                                + Arrays.toString(scores));
                    Hyperedge chosenEdge = nodeInfo.edges.get(chosenIndex);
                    if (choose)
                        widget = (Widget) chosenEdge.info.choose(widget);
                    // if(choose) dbg("Choose "+widget);
                    if (setPosterior)
                        chosenEdge.info.setPosterior(1.0);
                    logWeight += chosenEdge.weight;

                    recurse(chosenEdge.dest1);
                    recurse(chosenEdge.dest2);
                    break;
                case prod:
                    // Recurse on each edge
                    for (Hyperedge edge : nodeInfo.edges) {
                        if (choose)
                            widget = (Widget) edge.info.choose(widget);
                        if (setPosterior)
                            edge.info.setPosterior(1.0);
                        logWeight += edge.weight;
                        recurse(edge.dest1);
                        recurse(edge.dest2);
                    }
                    break;
            }
        }
    }
}
