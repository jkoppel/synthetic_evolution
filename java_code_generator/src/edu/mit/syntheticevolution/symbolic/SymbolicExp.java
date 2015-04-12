package edu.mit.syntheticevolution.symbolic;

import de.uka.ilkd.pp.Layouter;
import de.uka.ilkd.pp.NoExceptions;

public abstract class SymbolicExp {
    public abstract SymbolicExp simplifyConds();
    public abstract void renderAssign(Layouter<NoExceptions> out, String var);
    public abstract void renderExp(Layouter<NoExceptions> out);
}
