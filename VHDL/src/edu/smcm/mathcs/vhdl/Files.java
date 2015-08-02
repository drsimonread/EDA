package edu.smcm.mathcs.vhdl;
import java.io.FileInputStream;
import java.io.PrintStream;

import org.antlr.v4.runtime.*;
import org.antlr.v4.runtime.tree.*;
import edu.smcm.mathcs.vhdl.*;

public class Files {
	public static void main(String[] arguments) throws Exception{
		ANTLRInputStream input;
		PrintStream output;
		VHDLLexer lexer;
		CommonTokenStream tokens;
		VHDLParser parser;
		ParseTree tree;
		
		input = new ANTLRInputStream(new FileInputStream(arguments[0]));
		output = new PrintStream(arguments[1]);
		lexer = new VHDLLexer(input);
        tokens = new CommonTokenStream(lexer);
        parser = new VHDLParser(tokens);
        tree = parser.design_file();
        output.println(tree.toStringTree(parser));
        output.close();
	}
}
