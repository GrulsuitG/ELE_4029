/****************************************************/
/* File: symtab.c                                   */
/* Symbol table implementation for the TINY compiler*/
/* (allows only one symbol table)                   */
/* Symbol table is implemented as a chained         */
/* hash table                                       */
/* Compiler Construction: Principles and Practice   */
/* Kenneth C. Louden                                */
/****************************************************/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "symtab.h"


/* the hash function */
static int hash ( char * key )
{ int temp = 0;
  int i = 0;
  while (key[i] != '\0')
  { temp = ((temp << SHIFT) + key[i]) % SIZE;
    ++i;
  }
  return temp;
}

/* the list of line numbers of the source 
 * code in which a variable is referenced
 */
/* the hash table */
/*static BucketList hashTable[SIZE];*/

static Scope Scope_Arr[SIZE];
static Scope Scope_Stack[SIZE];

static int Scope_Arr_Num = 0;
static int Scope_Stack_Num = 0;

Scope scope_top()
{
	if(Scope_Stack_Num == 0)
		return NULL;
	return Scope_Stack[Scope_Stack_Num-1];
}

void scope_pop()
{
	Scope_Stack_Num--;
}

void scope_push(Scope sc)
{
	/*printf("cur : %s\n", sc->name);*/
	Scope_Stack[Scope_Stack_Num++] = sc;
}

Scope scope_make(char* scope_name)
{
	Scope sc = (Scope) malloc(sizeof(struct ScopeListRec));
	if(sc == NULL){
		perror("Failed to memory allocation for Scope");
		exit(1);
	}
	sc->name = scope_name;
	sc->parent = scope_top();
	sc->level = Scope_Stack_Num;

	Scope_Arr[Scope_Arr_Num++] = sc;
	return sc;
}


/* Procedure st_insert inserts line numbers and
 * memory locations into the symbol table
 * loc = memory location is inserted only the
 * first time, otherwise ignored
 */
void st_insert(char * name, ExpType type, int lineno, int loc, TreeNode *t)
{ 
	int h = hash(name);
	Scope sc = scope_top();
	BucketList l =  sc->bucket[h];

	while ((l != NULL) && (strcmp(name,l->name) != 0))
		l = l->next;
	if (l == NULL) /* variable not yet in table */
	{ l = (BucketList) malloc(sizeof(struct BucketListRec));
		l->name = name;
		l->lines = (LineList) malloc(sizeof(struct LineListRec));
		l->lines->lineno = lineno;
		l->memloc = loc;
		l->lines->next = NULL;
		l->next = sc->bucket[h];
		l->type = type;
		l->tree = t;
		sc->bucket[h] = l; 
	}
	else /* found in table, so just add line number */
	{
		LineList newLine = (LineList) malloc(sizeof(struct LineListRec));
		newLine->lineno = lineno;
		newLine->next = NULL;
		LineList tmp = l->lines;
		while(tmp->next != NULL){
			tmp = tmp->next;
		}
		tmp->next = newLine;
	}
} /* st_insert */

/* Function st_lookup returns the memory 
 * location of a variable or -1 if not found
 */
BucketList st_lookup ( char * scope, char * name )
{ 
	int h = hash(name);
  Scope sc;
	BucketList l;

	for(int i = 0; i <= Scope_Arr_Num; i++){
		if(strcmp(Scope_Arr[i]->name, scope) == 0){
			sc = Scope_Arr[i];
			break;
		}
	}
	if(sc == NULL)
		return NULL;

	while(sc != NULL){
		l = sc->bucket[h];
  	while ((l != NULL) && (strcmp(name,l->name) != 0))
    	l = l->next;
  	if (l != NULL) 
			return l;
		sc = sc->parent;
	}
	return NULL;
}

BucketList st_lookup_excluding_parent ( char * scope, char * name)
{
	int h = hash(name);
  Scope sc;
	BucketList l;

	for(int i = 0; i <= Scope_Arr_Num; i++){
		if(strcmp(Scope_Arr[i]->name, scope) == 0){
			sc = Scope_Arr[i];
			break;
		}
	}
	if(sc == NULL)
		return NULL;

	l = sc->bucket[h];
  while ((l != NULL) && (strcmp(name,l->name) != 0))
    l = l->next;
	
	return l;
}
/* Procedure printSymTab prints a formatted 
 * listing of the symbol table contents 
 * to the listing file
 */
void printSymTab(FILE * listing)
{ 

	for(int i = 0; i< Scope_Arr_Num; i++){
  	Scope scope = Scope_Arr[i];
		
		BucketList * bucket = scope->bucket;
		
		fprintf(listing, "Scope Name : %s ", scope->name);
		
		fprintf(listing, "(nested %d)\n", scope->level);
		fprintf(listing,"Variable Name Type      Data Type Location  Line Numbers\n");
  	fprintf(listing,"------------- --------- --------- --------- ------------\n");
		for(int j = 0; j< SIZE; j++)
		{
			if(bucket[j] != NULL)
			{
				BucketList b = bucket[j];
				LineList lines = b->lines;
				printf("%-14s", b->name);

				if(b->tree->child[0] != NULL && b->tree->arraySize == -1)
					fprintf(listing, "Function  ");
				else
					fprintf(listing, "Varaible  ");

				switch(b->type){
					case Void:
						fprintf(listing, "Void      ");
						break;
					case Integer:
						fprintf(listing, "Int       ");
						break;
					case IntegerArr:
						fprintf(listing, "Int[]     ");
						break;
				}
				fprintf(listing, "%-10d", b->memloc);

				while(lines != NULL){
					fprintf(listing, "%d ", lines->lineno);
					lines = lines->next;
				}
				fprintf(listing, "\n");
			}
		}
		fprintf(listing, "\n");
	}
} /* printSymTab */

