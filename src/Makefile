GHC = ghc
SRC_DIR = src
BIN_DIR = bin
EXEC = $(BIN_DIR)/ada
SOURCES = Main.hs Lexer.hs Parser.hs PrintAST.hs SymbolTable.hs IR.hs LivenessAnalysis.hs AnalyzeIR.hs MemoryAllocator.hs CodeGen.hs

Lexer.hs: Lexer.x
	alex Lexer.x
Parser.hs: Parser.y
	happy Parser.y

all: build

build: $(EXEC)

$(EXEC): Lexer.hs Parser.hs $(SOURCES)
	@mkdir -p $(BIN_DIR)
	$(GHC) -outputdir $(BIN_DIR) -o $(EXEC) $(SOURCES)

clean:
	rm -rf $(BIN_DIR)
	rm -f *.hi *.o Lexer.hs Parser.hs


.PHONY: all build clean

