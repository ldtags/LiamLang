# LiamLang

Welcome to Liam Lang! The barebones interpreter I built over the course of a month.

Test files for the language are given in the *examples* folder.

Specs:
  - All variables must be declared at the beginning of your script
  - The end of a line of code is denoted by a semi-colon ';' char
  
Data Types:
  - int: 32-bit integer
  - bool: has literals *true* and *false*
  
Operators {precedence}:
  - \[]: array access {1}
  - !bool: not {2}
  - -expr: unary minus {2}
  
Methods:
  - print( expression )
    - prints the expression parameter to the console
  
  - println( expression )
    - functionally the same as *print( expression )*, but prints a newline character following the expression

  - printstring( string )
    - prints the string parameter to the console
    
  - printlines( int )
    - prints the amount of newline chars specified by the int parameter
    
  - printspaces( int )
    - prints the amount of white-space chars specified by the int parameter

  - read( idList )
    - reads user input from the console
    - idList can be a single variable or a comma-delimitted list of variables
    - user input is loaded into the variables in the order they are given
