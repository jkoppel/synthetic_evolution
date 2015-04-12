package edu.mit.syntheticevolution.symbolic;

import de.uka.ilkd.pp.Layouter;
import de.uka.ilkd.pp.NoExceptions;

public class NegationNode extends SymbolicExp {
    private SymbolicExp val;

    public NegationNode(SymbolicExp val) {
        this.val = val;
    }

    @Override
    public boolean equals(Object oth) {
        if (!(oth instanceof NegationNode)) return false;

        return ((NegationNode)oth).equals(val);
    }

    @Override
    public String toString() {
        return "!" + val;
    }

    public SymbolicExp simplifyConds() {
        return new NegationNode(val.simplifyConds());
    }

    public void renderAssign(Layouter<NoExceptions> out, String var) { throw new UnsupportedOperationException(); }

    public void renderExp(Layouter<NoExceptions> out) {
        out.print("!(");
        val.renderExp(out);
        out.print(")");
    }
}
