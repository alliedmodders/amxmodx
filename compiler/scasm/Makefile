CPPFILES = amxasm.cpp cexpr.cpp amx_symbol.cpp amx_proc.cpp \
amx_parser.cpp amx_natives.cpp amx_macro.cpp amx_label.cpp \
amx_error.cpp amx_define.cpp amx_data.cpp amx_compiler.cpp

FLAGS = -march=i386

all:	sasm	

sasm:	
	g++ $(FLAGS) -Wall $(CPPFILES) -o sasm -s

clean:
	-rm *.o
	-rm sasm
