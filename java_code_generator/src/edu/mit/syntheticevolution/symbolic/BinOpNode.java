package edu.mit.syntheticevolution.symbolic;

import de.uka.ilkd.pp.Layouter;
import de.uka.ilkd.pp.NoExceptions;

public class BinOpNode extends SymbolicExp {
    private String op;
    private SymbolicExp left;
    private SymbolicExp right;

    public BinOpNode(String op, SymbolicExp left, SymbolicExp right) {
        this.op = op;
        this.left = left;
        this.right = right;
    }

    @Override
    public boolean equals(Object oth) {
        if (!(oth instanceof BinOpNode)) return false;

        BinOpNode othEq = (BinOpNode)oth;
        return othEq.op.equals(op) && othEq.left.equals(left) && othEq.right.equals(right);
    }

    @Override
    public String toString() {
        return String.format("(%s %s %s)", left, op, right);
    }

    public SymbolicExp simplifyConds() {
        return new BinOpNode(op, left.simplifyConds(), right.simplifyConds());
    }

    public void renderAssign(Layouter<NoExceptions> out, String var) { throw new UnsupportedOperationException(); }

    public void renderExp(Layouter<NoExceptions> out) {
        left.renderExp(out);
        out.print(" " + op + " "); //FIXME: parens?
        right.renderExp(out);
    }
}
