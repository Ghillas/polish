# polish
Static analysis of a mini-language

This project is to analyze a simple programming language named polish, with which you can do simple operation on Integer (you have few examples of program in the examples folder)

# compile 
ocamlc polish.ml -o polish

# execute
./polish -op file_name.p

Where op is the operation you want to do : 
	- reprint : print your polish program
	- eval : execute your polish program
	- simpl : simplified your polish program
	- vars : print the variable of your polish program
	
file_name.p : your polish program (with .p extension)
