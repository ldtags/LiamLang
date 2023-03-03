#include "IOMngr.h"

FILE * source, * out;
char * currentLine;
int line, col;
int printed = 0;
int newLine = 1;

int openFiles(char * sourceName, char * listingName) {
    source = fopen(sourceName, "r");
    if(source == NULL)  // file was unable to be created
        return 0;
    
    // source file successfuly opened
    // initializing file location identifiers
    line = 0;
    col = 0;

    if(listingName != NULL && fopen(listingName, "r") != NULL) {
        out = fopen(listingName, "w");
    } else {
        out = stdout;
    }

    return 1;
}

void closeFiles() {
    if(source != NULL)
        fclose(source);

    if(out != NULL)
        fclose(out);
}

char getNextSourceChar() {
    if(source == NULL) // file doesn't exist / hasn't been opened yet
        return EOF;

    if(newLine) {
        // currentLine is empty
        // get newline
        char buf[MAXLINE];
        currentLine = fgets(buf, MAXLINE, source);
        // end of file has been reached
        if(currentLine == NULL)
            return EOF;

        newLine = 0;
        printed = 0;
        col = 0;
        line++;
        if(out != stdout) {
            fprintf(out, "%d. ", getCurrentLineNum());
        }
    }

    char sChar = currentLine[col];
    if(sChar == '\0') { 
        fputc('\n', out);
        return EOF;
    }

    if(sChar == '\n') { // end of line has been reached
        newLine = 1;
        currentLine[col] = '\0';
    }

    if(out != stdout) {
        fputc(sChar, out);
    }
    
    col++;
    return sChar;
}

void writeIndicator(int column) {
    // printing current line if there's no listing file and currentLine hasn't been printed
    if(out == stdout && !printed) {
        printf("%d. %s\n", getCurrentLineNum(), currentLine);
        printed = 1;
    }

    // accounting for line number
    column += 3;
    int lineNum = line;
    while(lineNum / 10 != 0) {
        column++;
        lineNum /= 10;
    }

    for(int i = 0; i < column - 1; i++) {
        fputc(' ', out);
    }
    fputc('^', out);
}

void writeMessage(char * message) {
    // printing current line if there's no listing file and currentLine hasn't been printed
    if(out == stdout && !printed) {
        printf("%d. %s", getCurrentLineNum(), currentLine);
        printed = 1;
    }

    fputc('\n', out);
    fprintf(out, "%s", message);
    fputc('\n', out);
}

int getCurrentLineNum() {
    return line;
}

int getCurrentColumnNum() {
    return col;
}