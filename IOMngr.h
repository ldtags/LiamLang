#include <stdlib.h>
#include <stdio.h>

#define MAXLINE 1024

// Opens file sourceName
//      assume sourceName is a valid file
// If listingName is not NULL, open file listingName
//      if listingName is NULL, output to stdout
// return 1 if files were successfully opened
int openFiles(char * sourceName, char * listingName);

// Closes source file and listing file if opened
void closeFiles();

// Returns the next source char
// Echoes lines from source file to listing file if opened
// Lines in the listing file should be numbered
// Returns EOF when the end of the source file is reached
char getNextSourceChar();

// Writes a line containing a single ^ char in the indicated column
// If there is no listing file, current line is echoed to stdout the first time writeInidcator is ran
void writeIndicator(int column);

// Writes the message on a seperate line
// If there is no listing file, current line is echoed to stdout the first time writeMessage is ran
void writeMessage(char * message);

// returns the current line number
int getCurrentLineNum();

// returns the current column number in the current line
int getCurrentColumnNum();