package edu.mit.syntheticevolution.symbolic;

import de.uka.ilkd.pp.Layouter;
import de.uka.ilkd.pp.NoExceptions;

public class RawStringNode extends SymbolicExp {
    private String val;

    public RawStringNode(String val) {
        this.val = val;
    }

    @Override
    public boolean equals(Object oth) {
        if (!(oth instanceof RawStringNode)) return false;

        return ((RawStringNode)oth).val.equals(val);
    }

    @Override
    public String toString() {
        return val;
    }

    public SymbolicExp simplifyConds() {
        return this;
    }

    public void renderAssign(Layouter<NoExceptions> out, String var) {
        out.print(var + " = " + val + ";");
        out.brk();
    }

    public void renderExp(Layouter<NoExceptions> out) {
        out.print(val);
    }
}
