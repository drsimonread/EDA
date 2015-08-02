package edu.smcm.mathcs.vhdl;
import java.io.FileInputStream;
import java.io.PrintStream;
import java.util.List;
import edu.smcm.mathcs.vhdl.*;

import org.antlr.v4.runtime.*;

public class Tokens {
	public static void main(String[] arguments) throws Exception {
		ANTLRInputStream input;
		PrintStream output;
		VHDLLexer lexer;
		CommonTokenStream tokenizer;
		List<Token> tokens;

		input = new ANTLRInputStream(new FileInputStream(arguments[0]));
		output = new PrintStream(arguments[1]);
		lexer = new VHDLLexer(input);
		tokenizer = new CommonTokenStream(lexer);
		tokenizer.fill();
		tokens = tokenizer.getTokens();
		output.println("Outputting " + tokens.size() + " Tokens.");
		for (Token token : tokens) {
			output.println(token);
		}
		output.close();
	}
}
