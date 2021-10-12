all:
	#happy -gca Lang/Par.y
	#alex -g Lang/Lex.x
	#ghc --make Lang/Test.hs -o Lang/Test
	ghc --make Interpreter.hs -o Interpreter
	
clean:
	-rm -f Lang/*.log Lang/*.aux Lang/*.hi Lang/*.o Lang/*.dvi

distclean: clean
	-rm -f Lang/Doc.* Lang/Lex.* Lang/Par.* Lang/Layout.* Lang/Skel.* Lang/Print.* Lang/Test.* Lang/Abs.* Lang/Test Lang/ErrM.* Lang/SharedString.* Lang/ComposOp.* Lang/Lang.dtd Lang/XML.* Makefile*
		-rmdir -p Lang/

