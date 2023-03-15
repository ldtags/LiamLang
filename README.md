# LiamLang

Welcome to Liam Lang! The barebones interpreter I built over the course of a month.

Test files for the language are given in the *examples* folder.

This interpreter uses Lex to perform lexical analysis on the source file, and Yacc to parse the returned token stream.


Requirements:
  - Linux OS
  - A C compiler (gcc, clang, etc.)
  - An installation of Yacc (preferably Bison 3.8.1)
  - An installation of Lex
  - A Java SDK and a mars.jar file (only required if you wish to run the assembly generated)



Interpreting a Source File:
  - make clean
  - make comp
  - ./comp {path to source file} lsting.lst asmCode.asm
    - the listing file and assembly file don't have to already exist
    - lsting.lst and asmCode.asm can be changed to valid paths for the files



Specs:
  - All variables must be declared at the beginning of your script
  - The end of a line of code is denoted by a semi-colon ';' char
  - Only *while* loops are supported


Data Types:
  - int: 32-bit integer
  - bool: has literals *true* and *false*
  - string: can only be used in the *printstring* method, denoted as "string"



Operators {precedence}:
  - \[]:          array access                               {1}
  - !bool:        not                                        {2}
  - -expr:        unary minus                                {2}
  - expr ^ expr:  right-associative exponentiation           {3}
  - expr * expr:  left-associative multiplication            {4}
  - expr / expr:  left-associative integer division          {4}
  - expr % expr:  left-associative modulo                    {4}
  - expr + expr:  left-associative addition                  {5}
  - expr - expr:  left-associative subtraction               {5}
  - bool >= bool: left-associative greater than or equal to  {6}
  - bool > bool:  left-associative greater than              {6}
  - bool <= bool: left-associative less than or equal to     {6}
  - bool < bool:  left-associative less than                 {6}
  - bool == bool: equality                                   {7}
  - bool != bool: inequality                                 {7}
  - bool && bool: boolean and                                {8}
  - bool || bool: boolean or                                 {9}



Statements:
  - All methods listed in I/O
  - id++;
  - id = expression;
  - if (bool) { statementSequence; }
  - if (bool) { statementSequence; } else { statementSequence; }
  - while (bool) { statementSequence; }



I/O:
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
