# Interpreter
The Interpreter contains two versions, interpreter.py and interpreter.sml. Both version employ the same base functionality (albeit in two seperate languages) however the python version further expands upon such as implments more.

Each file will read and write to/from specified text files within the program, noted toward the bottom of each program. Each program will take in various inputs as lines from the input file. Each operation, includes some form of safety against incorrect syntax, or runtime errors which are caught and printed to the output file as such. More detail can be found in the program files. The normal case for each operation will be described below:

- push *val*: pushes an integer, boolean (in form :bool:), string (enclosed with ""), or name (alphanumeric string beginning with a letter)
- pop: removes top value from stack
- add: adds top two values from stack and pushes result back to stack
- sub: *see above*
- mul: *see above*
- div: *see above*
- rem: *see above*
- neg: negative of top stack value
- and: logical AND between top two stack values
- or: *see above*
- not: logical NOT for top value
- cat: string concatanation of top two value
- bind: binds any value to a name field (i.e. 3 -> name1, "string" -> name2) to be used in place of the actual value
- if: write either the first or second value back to the stack based on value of third (:true: or :false:)
- equal: compares two integer value and returns a bool
- lessThan: *see above*
- quit: writes stack contents to output file

The following functionality only exists in interpreter.py:

- let: creates a new environment that will not effect the overlying one
- end: ends the sub environment and returns the last written value to the stack
- fun: creates a new function utilzing the next value as a parameter and following lines as the function
- inOutFun: like fun but returns used parameter with value given within function
- funEnd: designates the end of a funciton definition
- call: calls the function and parameter on the stack
