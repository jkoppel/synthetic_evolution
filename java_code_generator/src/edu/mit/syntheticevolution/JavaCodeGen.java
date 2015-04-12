package edu.mit.syntheticevolution;

import de.uka.ilkd.pp.Layouter;
import de.uka.ilkd.pp.NoExceptions;
import de.uka.ilkd.pp.StringBackend;
import edu.mit.syntheticevolution.symbolic.SymbolicExp;
import sketch.compiler.ast.core.Function;
import sketch.compiler.ast.core.Program;
import sketch.compiler.ast.core.SymbolTable;
import sketch.compiler.ast.core.typs.StructDef;
import sketch.compiler.passes.lowering.SymbolTableVisitor;
import sketch.util.annot.CodeGenerator;

import java.util.*;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import static java.lang.String.format;

@CodeGenerator
public class JavaCodeGen extends SymbolTableVisitor {

    // public because passing in -INDENT_SIZE seems to be the only way to un-indent
    public static final int INDENT_SIZE = 2;

    private StringBackend finalOutput = new StringBackend(80);
    private Layouter<NoExceptions> out = new Layouter<>(finalOutput, INDENT_SIZE);

    private String highestVersionTypeName = "";
    private int highestVersion = 0;
    private List<String> dataStructFieldNames = Collections.emptyList();

    private static int extractVersion(String name) {
        Pattern p = Pattern.compile("\\d+");
        Matcher m = p.matcher(name);
        m.find();
        return Integer.parseInt(m.group(0));
    }

    private static Map<String, String> buildSubstitutionMap(List<String> fieldNames) {
        Map<String, String> result = new LinkedHashMap<>();

        for (String field : fieldNames) {
            result.put("_out." + field, "result." + field);
        }

        return result;
    }

    public JavaCodeGen() {
        super(new SymbolTable(null));
    }

    @Override
    public Object visitStructDef(StructDef ts) {

        int version = extractVersion(ts.getName());
        if (version > highestVersion) {
            highestVersion = version;
            highestVersionTypeName = ts.getName();
            dataStructFieldNames = new ArrayList<>(ts.getOrderedFields());
        }

        out.beginC();
        out.print("class " + ts.getName() + " { ");

        for (String nam : ts.getOrderedFields()) {
            out.nl();
            out.print("public int " + nam + ";");
        }

        out.brk(0, -INDENT_SIZE);
        out.print("}");
        out.end();
        out.brk();
        out.brk();

        return super.visitStructDef(ts);
    }

    @Override
    public Object visitFunction(Function func) {
        if (func.getName().equals("read")) {
            String typeName = highestVersionTypeName;

            out.beginC();
            out.print(format("public %s read(int[] buf) {", typeName));
            out.brk();
            out.print("int length = buf.length;");
            out.brk();
            out.print(format("%s result = new %s();", typeName, typeName));
            out.brk().brk();

            SymbolicExecutor symb = new SymbolicExecutor();
            func.getBody().accept(symb);
            symb.renderValues(out, buildSubstitutionMap(dataStructFieldNames));

            out.brk();
            out.print("return result;");
            out.brk(0, -INDENT_SIZE);
            out.print("}");
            out.end();
        }

        return super.visitFunction(func);
    }

    @Override
    public Object visitProgram(Program p) {
        out.beginC(0);
        Object result = super.visitProgram(p);
        out.end();

        out.close();
        out.flush();
        System.out.println(finalOutput.getString());

        return result;
    }
}
