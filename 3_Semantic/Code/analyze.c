/****************************************************/
/* File: analyze.c                                  */
/* Semantic analyzer implementation                 */
/* for the TINY compiler                            */
/* Compiler Construction: Principles and Practice   */
/* Kenneth C. Louden                                */
/****************************************************/

#include "globals.h"
#include "symtab.h"
#include "analyze.h"
#include "util.h"

/* counter for variable memory locations */
static int location = 0;
static int globalNum = 0;

static int isfirst = FALSE; 

Scope globalScope;
/* Procedure traverse is a generic recursive 
 * syntax tree traversal routine:
 * it applies preProc in preorder and postProc 
 * in postorder to tree pointed to by t
 */
static void traverse( TreeNode * t,
               void (* preProc) (TreeNode *),
               void (* postProc) (TreeNode *) )
{ if (t != NULL)
  { preProc(t);
    { int i;
      for (i=0; i < MAXCHILDREN; i++)
        traverse(t->child[i],preProc,postProc);
    }
    postProc(t);
    traverse(t->sibling,preProc,postProc);
  }
}

/* nullProc is a do-nothing procedure to 
 * generate preorder-only or postorder-only
 * traversals from traverse
 */
static void afterInsert(TreeNode * t)
{ 
  if (t->nodekind == StmtK)
		if(t->kind.stmt == CompK)
			scope_pop();
	if (t==NULL) return;
	return;
}

static void beforeCheck(TreeNode * t)
{
	if(t->kind.stmt == CompK)
	{
		Scope scope = scope_make(t->scopeName);
		scope_push(scope);
	}
}
static void symbolError(TreeNode * t, char * message)
{ fprintf(listing,"symbol error at line %d: %s\n",t->lineno,message);
  Error = TRUE;
}


/* Procedure insertNode inserts 
 * identifiers stored int into 
 * the symbol table 
 */
static void insertNode( TreeNode * t)
{ switch (t->nodekind)
  { case StmtK:
      switch (t->kind.stmt)
      { 
				/*case SelK:*/
				/*case IterK:*/
				/*
				 *case ReturnK:
				 *{
				 *  BucketList b = st_lookup(scope_top()->name , t->attr.name);
				 *  if(b == NULL)
				 *    symbolError(t, "undefined variable can't be returned");
				 *  break;
				 *}
				 */
				case CompK:
					if(!isfirst){
						Scope sc = scope_make(scope_top()->name);
						sc->parent = scope_top();
						scope_push(sc);
					}
					if(scope_top()->name == NULL)
						printf("top is null\n");
					t->scopeName = scope_top()->name;
					isfirst = FALSE;
					break;
					
					break;
        default:
          break;
      }
      break;
    case ExpK:
      switch (t->kind.exp)
      { 
				case VarDclK:
					if(st_lookup_excluding_parent(scope_top()->name, t->attr.name) !=NULL)
					{
						symbolError(t, "redefinition of variable");
						break;
					}
					t->scopeName = scope_top()->name;
					if(t->type == Void)
					{	
						symbolError(t, "void can't be variable type");
						break;
					}
					if(strcmp(scope_top()->name, "global") == 0){
						st_insert(t->attr.name, t->type, t->lineno, globalNum++, t);
					}
					
					else
					{
						st_insert(t->attr.name, t->type, t->lineno, location++ ,t);
					}
					break;

				case FunDclK:
					{
						location = 0;
						if(st_lookup(scope_top()->name, t->attr.name) != NULL)
							symbolError(t, "fuction can't be redefined");

						if(strcmp(scope_top()->name, "global") == 0){
							st_insert(t->attr.name, t->type, t->lineno, globalNum++, t);
						}
						Scope scope = scope_make(t->attr.name);
						scope->parent = scope_top();
						scope_push(scope);
						t->scopeName = scope_top()->name;
						isfirst = TRUE;
						break;
					}
				case CallK:
				{
					BucketList b = st_lookup(scope_top()->name, t->attr.name);
					if(b == NULL)
						symbolError(t, "undefined symbol can't be called");
					else{
						t->type = b->type;
						LineList newLine = (LineList) malloc(sizeof(struct LineListRec));
						newLine->lineno = t->lineno;
						newLine->next = NULL;
						LineList tmp = b->lines;
						while(tmp->next != NULL)
							tmp = tmp->next;
						tmp->next = newLine;
					}
					break;
				}
				case ParamK:
				{
					if(t->type != Void){
						if(st_lookup_excluding_parent(scope_top()->name, t->attr.name) != NULL){
							symbolError(t, "parmeter can't be redefined");
							break;
						}
						st_insert(t->attr.name, t->type, t->lineno, location++, t);
						t->scopeName = scope_top()->name;
					}
					break;
				}
				case VarK:
				{
					BucketList b = st_lookup(scope_top()->name, t->attr.name);
					if(b == NULL)
					{
						symbolError(t, "undefined variable can't be called");
						break;
					}
					else
					{
						t->type = b->type;
						LineList newLine = (LineList) malloc(sizeof(struct LineListRec));
						newLine->lineno = t->lineno;
						newLine->next = NULL;
						LineList tmp = b->lines;
						while(tmp->next != NULL)
							tmp = tmp->next;
						tmp->next = newLine;
					}
					break;
				}
				default:
          break;
      }
      break;
    default:
      break;
  }
}

void initSymtab()
{
	TreeNode *input, *output;
	TreeNode *arg, *comp, *typeSpec;
	
	globalScope = scope_make("global");
	scope_push(globalScope);
	
	input = newExpNode(FunDclK);
	output = newExpNode(FunDclK);
	arg = newExpNode(ParamK);
	typeSpec = newExpNode(VarK);
	comp = newExpNode(CompK);
	/*arg->child[0] = newExpNode(ParamK);*/

	input->attr.name = (char*) malloc(6);
	output->attr.name = (char*) malloc(7);
	arg->attr.name = (char*) malloc(6);

	strcpy(input->attr.name, "input");
	strcpy(output->attr.name, "output");
	strcpy(arg->attr.name, "value");

	input->type = Integer;
	output->type = Void;
	arg->type = Integer;
	typeSpec->type = Void;
	/*arg->child[0]->type = Integer;*/
	
	input->child[0] = typeSpec;
	output->child[0] = arg;
	output->child[1] = comp;
	comp->child[0] = NULL;
	comp->child[1] = NULL;
	
	input->lineno = 0;
	output->lineno = 0;

	input->scopeName = (char*) malloc(7);
	output->scopeName = (char*) malloc(7);
	
	strcpy(input->scopeName, "global");
	strcpy(output->scopeName, "global");

	st_insert("input", Integer, 0, globalNum++, input);
	st_insert("output", Void, 0, globalNum++, output);
	
	Scope scope = scope_make("output");
	scope_push(scope);
	st_insert("value", Integer, 0, location++, arg);
	scope_pop();
	

}


/* Function buildSymtab constructs the symbol 
 * table by preorder traversal of the syntax tree
 */
void buildSymtab(TreeNode * syntaxTree)
{ 
	initSymtab();
	traverse(syntaxTree,insertNode,afterInsert);
  if (TraceAnalyze)
  { fprintf(listing,"\nSymbol table:\n\n");
    printSymTab(listing);
  }
	scope_pop();
}

static void typeError(TreeNode * t, char * message)
{ fprintf(listing,"Type error at line %d: %s\n",t->lineno,message);
  Error = TRUE;
}

/* Procedure checkNode performs
 * type checking at a single tree node
 */
static void checkNode(TreeNode * t)
{ 
	
	switch (t->nodekind)
  { case ExpK:
      switch (t->kind.exp)
      { 
				case OpK:
				{
					ExpType left = t->child[0]->type;
					ExpType right = t->child[1]->type;
					
					if(left == Void || right == Void )
						typeError(t, "Void can't be Operand in Operation");
					else if(left == IntegerArr && t->child[0]->isArray)
					{
						typeError(t, "IntegerArr can't be Operand");
					}
					else if(right == IntegerArr && t->child[1]->isArray)
					{
							typeError(t, "IntegerArr can't be Operand");
					}
					else if(left != right)
						typeError(t, "operand should be same type");

					else
						t->type = left;
					break;
				}
				case AssignK:
				{
					ExpType left = t->child[0]->type;
					ExpType right = t->child[1]->type;
					
					if(right == IntegerArr)
						typeError(t->child[0], "IntegerArr can't be assigned");
					else if(left == Void)
						typeError(t->child[1], "Void can't be Assigned");
					if(left == IntegerArr)
					{
						if(right == Integer)
							t->type = left;
						else
							typeError(t, "Integer should be Assigned");
					}
					else if(left != right)
						typeError(t, "both operand shoud be same type");
					else
						t->type = left;
					break;
				}
				case CallK:
				{
					BucketList b =st_lookup("global", t->attr.name);

					TreeNode *param = t->child[0];
					TreeNode *arg = b->tree->child[0];
					if(arg == NULL && param != NULL)
					{
						typeError(t, "parameter should be Void");
					}
					while(param != NULL)
					{
						if(arg == NULL)
						{	typeError(t, "parameter Number should be same with function definition");
							break;
						}
						else if(param->type != arg->type)
						{
							typeError(t, "parameter type should be same with function definition");
							break;
						}
						else
						{
							arg = arg->sibling;
							param = param->sibling;
						}
					}
					if(!Error)
						t->type = b->type;
					break;
					
				}
				case ConstK:
					t->type = Integer;
					break;
				case VarK:
				{
					Scope curscope = scope_top();
					BucketList b = st_lookup(curscope->name, t->attr.name);
					if( b == NULL)
						break;
					if(b->type == IntegerArr)
					{
					
						if(t->child[0] != NULL)
						{
							if(t->child[0]->type != Integer)
								typeError(t,"array index should be Integer");
						}
						if(t->isArray)
							t->type = IntegerArr;	
						else
							t->type = Integer;
					}
					else if(b->type == Void)
						typeError(t, "Variable type can't be Void");
					else
						t->type = b->type;
					break;
				}
				case ParamK:
				{	
					if(t->scopeName == NULL)
					{
						if(t->type != Void)
						{
							typeError(t,"parameter type shoud be Void");
							break;
						}
						else
							t->type = Void;
					}
					else
					{
						BucketList b = st_lookup(t->scopeName, t->attr.name);
						if(b->type != t->type)
						{
							typeError(t,"parameter type should be same");
							break;
						}
						else
							t->type = Integer;
					}
					break;
				}
				default:
					break;
			}
			break;
		case StmtK:
			switch (t->kind.stmt)
			{ case CompK:
					scope_pop();
					break;
				case SelK:
					if (t->child[0]->type == Void)
						typeError(t->child[0],"if-test stmt can't be Void");
					break;
				case IterK:
					if (t->child[0]->type == Void)
						typeError(t->child[0],"while-test stmt can't be Void");
					break;
				case ReturnK:
					{
						Scope curScope = scope_top();
						
						BucketList b = st_lookup("global" ,curScope->name);
						if(b == NULL)
							break;
						ExpType rettype = b->type;
					
						if(rettype == Void && (t->child[0] != NULL || t->child[0]->type != Void))
							typeError(t, "Return type should be Void");
						else if (rettype == Integer && (t->child[0] == NULL || t->child[0]->type != Integer))
							typeError(t, "Return type shoud be Integer");
						else
							t->type = rettype;
						break;
					}
				default:
					break;
			}
			break;
    default:
      break;

  }
}

/* Procedure typeCheck performs type checking 
 * by a postorder syntax tree traversal
 */
void typeCheck(TreeNode * syntaxTree)
{	
	globalScope = scope_make("global");
	scope_push(globalScope);

	traverse(syntaxTree,beforeCheck,checkNode);
	scope_pop();
}
