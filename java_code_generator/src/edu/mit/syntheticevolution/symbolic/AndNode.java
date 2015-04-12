package edu.mit.syntheticevolution.symbolic;

import de.uka.ilkd.pp.Layouter;
import de.uka.ilkd.pp.NoExceptions;

public class AndNode extends SymbolicExp {
    private SymbolicExp left;
    private SymbolicExp right;

    public AndNode(SymbolicExp left, SymbolicExp right) {
        this.left = left;
        this.right = right;
    }

    @Override
    public boolean equals(Object oth) {
        if (!(oth instanceof AndNode)) return false;

        AndNode othAnd = (AndNode)oth;
        return othAnd.left.equals(left) && othAnd.right.equals(right);
    }

    @Override
    public String toString() {
        return String.format("(%s && %s)", left, right);
    }

    public SymbolicExp simplifyConds() {
        return new AndNode(left.simplifyConds(), right.simplifyConds());
    }

    public void renderAssign(Layouter<NoExceptions> out, String var) { throw new UnsupportedOperationException(); }

    public void renderExp(Layouter<NoExceptions> out) {
        left.renderExp(out);
        out.print(" && "); //FIXME: parens?
        right.renderExp(out);
    }
}
