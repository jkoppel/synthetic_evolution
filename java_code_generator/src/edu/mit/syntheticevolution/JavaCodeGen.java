package edu.mit.syntheticevolution;

import sketch.compiler.ast.core.SymbolTable;
import sketch.compiler.ast.core.typs.StructDef;
import sketch.compiler.passes.lowering.SymbolTableVisitor;
import sketch.util.annot.CodeGenerator;

@CodeGenerator
public class JavaCodeGen extends SymbolTableVisitor {

    public JavaCodeGen() {
        super(new SymbolTable(null));
    }

    @Override
    public Object visitStructDef(StructDef ts) {
        System.out.println("class " + ts.getName() + " { ");
        for (String nam : ts.getFields()) {
            System.out.println("  public int " + nam + ";");
        }
        System.out.println("}\n\n");

        return super.visitStructDef(ts);
    }
}
