package edu.smcm.mathcs.vhdl;
/**
 * https://github.com/sfinnie/antlr4ide.examples.hello/blob/master/src/antlr4ide/examples/hello/ParserMain.java
 * 
 */

import org.antlr.v4.runtime.*;
import org.antlr.v4.runtime.tree.*;
import edu.smcm.mathcs.vhdl.*;

// import VHDLLexer;
// import VHDLParser;

public class StandardStreams {

	/**
	 * Run the parser on a provided source file.  Largely take from the
	 * Antlr book (https://pragprog.com/book/tpantlr2/the-definitive-antlr-4-reference) p33
	 * @param args
	 */
	public static void main(String[] args) throws Exception{
		//Create a charstream that reads from standard input
		ANTLRInputStream input = new ANTLRInputStream(System.in);
		
		//create a lexer that feeds off of input CharStream
		VHDLLexer lexer = new VHDLLexer(input);

	    // create a buffer of tokens pulled from the lexer
        CommonTokenStream tokens = new CommonTokenStream(lexer);

        // create a parser that feeds off the tokens buffer
        VHDLParser parser = new VHDLParser(tokens);

        ParseTree tree = parser.design_file(); // begin parsing at design_file rule
        System.out.println(tree.toStringTree(parser)); // print LISP-style tree
	}
}
