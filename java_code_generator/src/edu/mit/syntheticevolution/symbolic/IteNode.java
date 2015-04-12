package edu.mit.syntheticevolution.symbolic;

import de.uka.ilkd.pp.Layouter;
import de.uka.ilkd.pp.NoExceptions;
import edu.mit.syntheticevolution.JavaCodeGen;

public class IteNode extends SymbolicExp {
    private SymbolicExp cond;
    private SymbolicExp tcase;
    private SymbolicExp fcase;

    public IteNode(SymbolicExp cond, SymbolicExp tcase, SymbolicExp fcase) {
        this.cond = cond;
        this.tcase = tcase;
        this.fcase = fcase;
    }

    @Override
    public boolean equals(Object oth) {
        if (!(oth instanceof IteNode)) return false;

        IteNode othIte = (IteNode)oth;
        return othIte.cond.equals(cond) && othIte.tcase.equals(tcase) && othIte.fcase.equals(fcase);
    }

    @Override
    public String toString() {
        return String.format("ite(%s, %s, %s)", cond, tcase, fcase);
    }

    public SymbolicExp simplifyConds() {
        if (cond.equals(fcase)) {
            return new AndNode(cond.simplifyConds(), tcase.simplifyConds());
        } else {
            return new IteNode(cond.simplifyConds(), tcase.simplifyConds(), fcase.simplifyConds());
        }
    }

    public void renderAssign(Layouter<NoExceptions> out, String var) {
        out.print("if (");
        cond.renderExp(out);
        out.print(") {");

        out.beginCInd().nl();

        //FIXME: I don't actually know a good way to do this casing
        if (tcase instanceof IteNode) {
            tcase.renderAssign(out, var);
        } else {
            out.print(var + " = ");
            tcase.renderExp(out);
            out.print(";");
        }

        out.end().brk();


        // Here the casing is kinda okay; special-casing else-if
        if (fcase instanceof IteNode) {
            out.print("} else ");
            fcase.renderAssign(out, var);
        } else {
            out.print("} else {");
            out.beginCInd().nl();

            out.print(var + " = ");
            fcase.renderExp(out);
            out.print(";");

            out.end().brk();
            out.print("}");
        }
    }

    public void renderExp(Layouter<NoExceptions> out) { throw new UnsupportedOperationException(); }
}
