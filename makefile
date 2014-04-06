all: 
	ocamllex interpreterLEX.mll
	ocamlyacc interpreterYACC.mly
	ocamlc -c interpreterYACC.mli
	ocamlc -c interpreterLEX.ml
	ocamlc -c interpreterYACC.ml
	ocamlc -c interpreter.ml
	@echo "# linking of the lexer, parser & interpreter:"
	ocamlc -o interpreter interpreterLEX.cmo interpreterYACC.cmo interpreter.cmo
	@echo "# the end."

clean:
	/bin/rm -f interpreter interpreter.cmi interpreter.cmo interpreterLEX.cmi interpreterLEX.cmo interpreterLEX.ml interpreterYACC.cmi interpreterYACC.cmo interpreterYACC.ml interpreterYACC.mli 
