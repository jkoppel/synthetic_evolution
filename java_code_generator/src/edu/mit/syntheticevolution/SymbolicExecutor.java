package edu.mit.syntheticevolution;

import de.uka.ilkd.pp.Layouter;
import de.uka.ilkd.pp.NoExceptions;
import edu.mit.syntheticevolution.symbolic.*;
import sketch.compiler.ast.core.FETypedVisitor;
import sketch.compiler.ast.core.exprs.*;
import sketch.compiler.ast.core.stmts.*;

import java.util.*;

public class SymbolicExecutor extends FETypedVisitor<SymbolicExp> {
    private List<SymbolicExp> pathCondition;
    private Map<String, SymbolicExp> varCtx;

    public SymbolicExecutor(List<SymbolicExp> pathCondition, Map<String, SymbolicExp> varCtx) {
        this.pathCondition = new ArrayList<>(pathCondition);
        this.varCtx = new HashMap<>(varCtx);
    }

    public void addGuard(SymbolicExp exp) {
        pathCondition.add(exp);
    }

    public SymbolicExecutor() {
        this(new ArrayList<SymbolicExp>(), new HashMap<String, SymbolicExp>());
    }

    public void renderValues(Layouter<NoExceptions> out, Map<String, String> nameSubsts) {
        for (String v : nameSubsts.keySet()) {
            SymbolicExp exp = varCtx.get(v).simplifyConds();
            exp.renderAssign(out, nameSubsts.get(v));
            out.nl();
        }
    }

    @Override
    public SymbolicExp visitStmtAssign(StmtAssign stmt) {
        String lhs = stmt.getLHS().toString();
        varCtx.put(lhs, stmt.getRHS().accept(this));

        return null;
    }

    @Override
    public SymbolicExp visitStmtIfThen(StmtIfThen stmt) {
        SymbolicExp guard = stmt.getCond().accept(this);

        SymbolicExecutor leftExec = new SymbolicExecutor(pathCondition, varCtx);
        leftExec.addGuard(guard);
        stmt.getCons().accept(leftExec);

        SymbolicExecutor rightExec = new SymbolicExecutor(pathCondition, varCtx);
        rightExec.addGuard(new NegationNode(guard));

        if (stmt.getAlt() != null) {
            stmt.getAlt().accept(rightExec);
        }

        Map<String, SymbolicExp> leftCtx = leftExec.getVarCtx();
        Map<String, SymbolicExp> rightCtx = rightExec.getVarCtx();
        for (String var : varCtx.keySet()) {
            if (!leftCtx.get(var).equals(rightCtx.get(var))) {
                varCtx.put(var, new IteNode(guard, leftCtx.get(var), rightCtx.get(var)));
            }
        }

        return null;
    }

    @Override
    public SymbolicExp visitStmtBlock(StmtBlock stmt) {
        for (Statement s : stmt.getStmts()) {
            s.accept(this);
        }

        return null;
    }

    @Override
    public SymbolicExp visitStmtVarDecl(StmtVarDecl stmt) {
        for (StmtVarDecl.VarDeclEntry e : stmt) {
            if (e.getInit() != null) {
                varCtx.put(e.getName(), e.getInit().accept(this));
            }
        }

        return null;
    }

    @Override
    public SymbolicExp visitStmtReturn(StmtReturn stmt) {
        //FIXME: Assumes returns all happen at end, where we don't care about them
        return null;
    }

    @Override
    public SymbolicExp visitExprBinary(ExprBinary exp) {
        SymbolicExp e =  new BinOpNode(exp.getOpString(), exp.getLeft().accept(this), exp.getRight().accept(this));
        return e;
    }

    @Override
    public SymbolicExp visitExprField(ExprField exp) {
        return new RawStringNode(exp.toString());
    }

    @Override
    public SymbolicExp visitExprVar(ExprVar exp) {
        if (varCtx.containsKey(exp.getName())) {
            return varCtx.get(exp.getName());
        } else {
            return new RawStringNode(exp.getName());
        }
    }

    @Override
    public SymbolicExp visitExprNullPtr(ExprNullPtr e) {
        return new RawStringNode("null");
    }

    @Override
    public SymbolicExp visitExprNew(ExprNew e) {
        return new RawStringNode(e.toString());
    }

    @Override
    public SymbolicExp visitExprArrayRange(ExprArrayRange e) {
        //FIXME: Assumes all indexing operations can be syntactically identical between Sketch and Java
        return new RawStringNode(e.toString());
    }

    @Override
    public SymbolicExp visitExprConstInt(ExprConstInt e) {
        return new RawStringNode(e.toString());
    }

    public Map<String, SymbolicExp> getVarCtx() {
        return Collections.unmodifiableMap(varCtx);
    }
}
