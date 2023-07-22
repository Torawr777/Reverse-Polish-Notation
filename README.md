This program parses Reverse-polish notation(aka postfix notation), which is a way of writing expressions that avoids the need for parenthesis. 
It is a Turing-complete parser of a simpler dialect of the language, Forth. 
It can call its own functions within the Forth program(input) and utilize comparison operators.

"forth.hs" -- Project implementation<br />
All other files are test input files.<br />

To run this program, type:


"ghc -o forth forth.hs" -- Compiles<br />
"./forth < test_file.4th" -- Use redirection to test the program on one of the input files.<br />
