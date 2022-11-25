#include "SymTab.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

int hash(SymTab *table, char *name);
void printTable(SymTab *table);
SymEntry * createSymEntry(char *name);

SymTab * createSymTab(int size) {
    SymTab *table = malloc(sizeof(SymTab));
    if(table == NULL)
        return NULL;

    table->size = size;
    table->contents = malloc(size * sizeof(SymEntry));
    for(int i = 0; i < size; i++) {
        table->contents[i] = 0;
    }
    table->current = NULL;

    return table;
}

SymEntry * createSymEntry(char *name) {
    SymEntry * entry = malloc(sizeof(SymEntry));
    if(entry == NULL)
        return NULL;
    
    entry->name = strdup(name);
    entry->attribute = NULL;
    entry->next = NULL;
    return entry;
}

void destroySymTab(SymTab *table) {
    int hasMore = startIterator(table);
    SymEntry * entry = table->current;

    while(hasMore) {
        entry = table->current;
        free(entry->name);
        free(entry);
        hasMore = nextEntry(table);
    }
    free(table);
}

int enterName(SymTab *table, char *name) {
    int hashval = hash(table, name);
    SymEntry * current = table->contents[hashval];

    if(current == NULL) { // no conflicts
        SymEntry * entry = createSymEntry(name);
        table->contents[hashval] = entry;
        table->current = entry;
        return 1;
    }

    SymEntry * prev = current;
    int cmp = 1;
    while(current != NULL) { // iterating to end of list and checking for an existing name
        cmp = strcmp(current->name, name); // comparing the name of the current entry to the new name
        if(cmp == 0) {
            // name exists in list
            table->current = current;
            return 0;
        }
        
        prev = current;
        current = current->next;
    }

    SymEntry * entry = createSymEntry(name);
    prev->next = entry;
    table->current = entry;
    return 1;
}

int findName(SymTab *table, char *name) {
    int hashval = hash(table, name);
    SymEntry * current = table->contents[hashval];

    if(current == NULL) {
        // name does not exist in the list
        return 0;
    }

    int cmp = 1;
    while(current != NULL) { //Iterate through the full list
        cmp = strcmp(current->name, name);
        if(cmp == 0){
            //The name exists in the list
            table->current = current;
            return 1; 
        } 
        current = current->next;
    }

    // end of list has been reached and name was not found
    return 0;
}

int hasCurrent(SymTab *table) {
    return table->current == NULL;
}

void setCurrentAttr(SymTab *table, void * attr) {
    table->current->attribute = attr;
}

void * getCurrentAttr(SymTab *table) {
    return table->current->attribute;
}

char * getCurrentName(SymTab *table) {
    return table->current->name;
}

int startIterator(SymTab *table) {
    for(int i = 0; i < table->size; i++) {
        if(table->contents[i] != NULL) {
            table->current = table->contents[i];
            return 1;
        }
    }

    return 0; //end of table reached, therefore table is empty
}

int nextEntry(SymTab *table) {
    SymEntry * current = table->current;
    if(current->next != NULL) {
        // there is another entry in the list
        table->current = current->next;
        return 1;
    }

    int hashval = hash(table, current->name);
    int i = hashval + 1;
    while(i < table->size) {
        if(table->contents[i] != NULL) {
            table->current = table->contents[i];
            return 1;
        }
        i++;
    }

    // all entries have been visited by the iterator
    return 0;
}

// Iterates through the name and creates a number based off of the ascii value of each
// individual char in order
// Uses this new integer to create a hash value for the name
// Returns the hash value of the name
int hash(SymTab *table, char *name) {
    unsigned int size = table->size;
    unsigned int combined_num = name[0];
    int i = 1;

    while(name[i] != '\0') {
        // Adds the ascii value of the char to the end of the current number
        combined_num = combined_num*10 + name[i];
        i++;
    }

    return combined_num % size;
}