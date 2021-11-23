/* A Bison parser, made by GNU Bison 3.0.4.  */

/* Bison implementation for Yacc-like parsers in C

   Copyright (C) 1984, 1989-1990, 2000-2015 Free Software Foundation, Inc.

   This program is free software: you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation, either version 3 of the License, or
   (at your option) any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program.  If not, see <http://www.gnu.org/licenses/>.  */

/* As a special exception, you may create a larger work that contains
   part or all of the Bison parser skeleton and distribute that work
   under terms of your choice, so long as that work isn't itself a
   parser generator using the skeleton or a modified version thereof
   as a parser skeleton.  Alternatively, if you modify or redistribute
   the parser skeleton itself, you may (at your option) remove this
   special exception, which will cause the skeleton and the resulting
   Bison output files to be licensed under the GNU General Public
   License without this special exception.

   This special exception was added by the Free Software Foundation in
   version 2.2 of Bison.  */

/* C LALR(1) parser skeleton written by Richard Stallman, by
   simplifying the original so-called "semantic" parser.  */

/* All symbols defined below should begin with yy or YY, to avoid
   infringing on user name space.  This should be done even for local
   variables, as they might otherwise be expanded by user macros.
   There are some unavoidable exceptions within include files to
   define necessary library symbols; they are noted "INFRINGES ON
   USER NAME SPACE" below.  */

/* Identify Bison output.  */
#define YYBISON 1

/* Bison version.  */
#define YYBISON_VERSION "3.0.4"

/* Skeleton name.  */
#define YYSKELETON_NAME "yacc.c"

/* Pure parsers.  */
#define YYPURE 0

/* Push parsers.  */
#define YYPUSH 0

/* Pull parsers.  */
#define YYPULL 1




/* Copy the first part of user declarations.  */
#line 7 "cminus.y" /* yacc.c:339  */

#define YYPARSER /* distinguishes Yacc output from other code files */

#include "globals.h"
#include "util.h"
#include "scan.h"
#include "parse.h"

#define YYSTYPE TreeNode *
static char * savedName; /* for use in assignments */
static int savedLineNo;  /* ditto */
static TreeNode * savedTree; /* stores syntax tree for later return */
int yyerror(char * message);
static int yylex(void);

#line 82 "y.tab.c" /* yacc.c:339  */

# ifndef YY_NULLPTR
#  if defined __cplusplus && 201103L <= __cplusplus
#   define YY_NULLPTR nullptr
#  else
#   define YY_NULLPTR 0
#  endif
# endif

/* Enabling verbose error messages.  */
#ifdef YYERROR_VERBOSE
# undef YYERROR_VERBOSE
# define YYERROR_VERBOSE 1
#else
# define YYERROR_VERBOSE 0
#endif

/* In a future release of Bison, this section will be replaced
   by #include "y.tab.h".  */
#ifndef YY_YY_Y_TAB_H_INCLUDED
# define YY_YY_Y_TAB_H_INCLUDED
/* Debug traces.  */
#ifndef YYDEBUG
# define YYDEBUG 0
#endif
#if YYDEBUG
extern int yydebug;
#endif

/* Token type.  */
#ifndef YYTOKENTYPE
# define YYTOKENTYPE
  enum yytokentype
  {
    IF = 258,
    ELSE = 259,
    WHILE = 260,
    RETURN = 261,
    INT = 262,
    VOID = 263,
    ID = 264,
    NUM = 265,
    ASSIGN = 266,
    EQ = 267,
    NE = 268,
    LT = 269,
    LE = 270,
    GT = 271,
    GE = 272,
    PLUS = 273,
    MINUS = 274,
    TIMES = 275,
    OVER = 276,
    SEMI = 277,
    COMMA = 278,
    LPAREN = 279,
    RPAREN = 280,
    LBRACE = 281,
    RBRACE = 282,
    LCURLY = 283,
    RCURLY = 284,
    ERROR = 285,
    OUTER = 286
  };
#endif
/* Tokens.  */
#define IF 258
#define ELSE 259
#define WHILE 260
#define RETURN 261
#define INT 262
#define VOID 263
#define ID 264
#define NUM 265
#define ASSIGN 266
#define EQ 267
#define NE 268
#define LT 269
#define LE 270
#define GT 271
#define GE 272
#define PLUS 273
#define MINUS 274
#define TIMES 275
#define OVER 276
#define SEMI 277
#define COMMA 278
#define LPAREN 279
#define RPAREN 280
#define LBRACE 281
#define RBRACE 282
#define LCURLY 283
#define RCURLY 284
#define ERROR 285
#define OUTER 286

/* Value type.  */
#if ! defined YYSTYPE && ! defined YYSTYPE_IS_DECLARED
typedef int YYSTYPE;
# define YYSTYPE_IS_TRIVIAL 1
# define YYSTYPE_IS_DECLARED 1
#endif


extern YYSTYPE yylval;

int yyparse (void);

#endif /* !YY_YY_Y_TAB_H_INCLUDED  */

/* Copy the second part of user declarations.  */

#line 195 "y.tab.c" /* yacc.c:358  */

#ifdef short
# undef short
#endif

#ifdef YYTYPE_UINT8
typedef YYTYPE_UINT8 yytype_uint8;
#else
typedef unsigned char yytype_uint8;
#endif

#ifdef YYTYPE_INT8
typedef YYTYPE_INT8 yytype_int8;
#else
typedef signed char yytype_int8;
#endif

#ifdef YYTYPE_UINT16
typedef YYTYPE_UINT16 yytype_uint16;
#else
typedef unsigned short int yytype_uint16;
#endif

#ifdef YYTYPE_INT16
typedef YYTYPE_INT16 yytype_int16;
#else
typedef short int yytype_int16;
#endif

#ifndef YYSIZE_T
# ifdef __SIZE_TYPE__
#  define YYSIZE_T __SIZE_TYPE__
# elif defined size_t
#  define YYSIZE_T size_t
# elif ! defined YYSIZE_T
#  include <stddef.h> /* INFRINGES ON USER NAME SPACE */
#  define YYSIZE_T size_t
# else
#  define YYSIZE_T unsigned int
# endif
#endif

#define YYSIZE_MAXIMUM ((YYSIZE_T) -1)

#ifndef YY_
# if defined YYENABLE_NLS && YYENABLE_NLS
#  if ENABLE_NLS
#   include <libintl.h> /* INFRINGES ON USER NAME SPACE */
#   define YY_(Msgid) dgettext ("bison-runtime", Msgid)
#  endif
# endif
# ifndef YY_
#  define YY_(Msgid) Msgid
# endif
#endif

#ifndef YY_ATTRIBUTE
# if (defined __GNUC__                                               \
      && (2 < __GNUC__ || (__GNUC__ == 2 && 96 <= __GNUC_MINOR__)))  \
     || defined __SUNPRO_C && 0x5110 <= __SUNPRO_C
#  define YY_ATTRIBUTE(Spec) __attribute__(Spec)
# else
#  define YY_ATTRIBUTE(Spec) /* empty */
# endif
#endif

#ifndef YY_ATTRIBUTE_PURE
# define YY_ATTRIBUTE_PURE   YY_ATTRIBUTE ((__pure__))
#endif

#ifndef YY_ATTRIBUTE_UNUSED
# define YY_ATTRIBUTE_UNUSED YY_ATTRIBUTE ((__unused__))
#endif

#if !defined _Noreturn \
     && (!defined __STDC_VERSION__ || __STDC_VERSION__ < 201112)
# if defined _MSC_VER && 1200 <= _MSC_VER
#  define _Noreturn __declspec (noreturn)
# else
#  define _Noreturn YY_ATTRIBUTE ((__noreturn__))
# endif
#endif

/* Suppress unused-variable warnings by "using" E.  */
#if ! defined lint || defined __GNUC__
# define YYUSE(E) ((void) (E))
#else
# define YYUSE(E) /* empty */
#endif

#if defined __GNUC__ && 407 <= __GNUC__ * 100 + __GNUC_MINOR__
/* Suppress an incorrect diagnostic about yylval being uninitialized.  */
# define YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN \
    _Pragma ("GCC diagnostic push") \
    _Pragma ("GCC diagnostic ignored \"-Wuninitialized\"")\
    _Pragma ("GCC diagnostic ignored \"-Wmaybe-uninitialized\"")
# define YY_IGNORE_MAYBE_UNINITIALIZED_END \
    _Pragma ("GCC diagnostic pop")
#else
# define YY_INITIAL_VALUE(Value) Value
#endif
#ifndef YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN
# define YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN
# define YY_IGNORE_MAYBE_UNINITIALIZED_END
#endif
#ifndef YY_INITIAL_VALUE
# define YY_INITIAL_VALUE(Value) /* Nothing. */
#endif


#if ! defined yyoverflow || YYERROR_VERBOSE

/* The parser invokes alloca or malloc; define the necessary symbols.  */

# ifdef YYSTACK_USE_ALLOCA
#  if YYSTACK_USE_ALLOCA
#   ifdef __GNUC__
#    define YYSTACK_ALLOC __builtin_alloca
#   elif defined __BUILTIN_VA_ARG_INCR
#    include <alloca.h> /* INFRINGES ON USER NAME SPACE */
#   elif defined _AIX
#    define YYSTACK_ALLOC __alloca
#   elif defined _MSC_VER
#    include <malloc.h> /* INFRINGES ON USER NAME SPACE */
#    define alloca _alloca
#   else
#    define YYSTACK_ALLOC alloca
#    if ! defined _ALLOCA_H && ! defined EXIT_SUCCESS
#     include <stdlib.h> /* INFRINGES ON USER NAME SPACE */
      /* Use EXIT_SUCCESS as a witness for stdlib.h.  */
#     ifndef EXIT_SUCCESS
#      define EXIT_SUCCESS 0
#     endif
#    endif
#   endif
#  endif
# endif

# ifdef YYSTACK_ALLOC
   /* Pacify GCC's 'empty if-body' warning.  */
#  define YYSTACK_FREE(Ptr) do { /* empty */; } while (0)
#  ifndef YYSTACK_ALLOC_MAXIMUM
    /* The OS might guarantee only one guard page at the bottom of the stack,
       and a page size can be as small as 4096 bytes.  So we cannot safely
       invoke alloca (N) if N exceeds 4096.  Use a slightly smaller number
       to allow for a few compiler-allocated temporary stack slots.  */
#   define YYSTACK_ALLOC_MAXIMUM 4032 /* reasonable circa 2006 */
#  endif
# else
#  define YYSTACK_ALLOC YYMALLOC
#  define YYSTACK_FREE YYFREE
#  ifndef YYSTACK_ALLOC_MAXIMUM
#   define YYSTACK_ALLOC_MAXIMUM YYSIZE_MAXIMUM
#  endif
#  if (defined __cplusplus && ! defined EXIT_SUCCESS \
       && ! ((defined YYMALLOC || defined malloc) \
             && (defined YYFREE || defined free)))
#   include <stdlib.h> /* INFRINGES ON USER NAME SPACE */
#   ifndef EXIT_SUCCESS
#    define EXIT_SUCCESS 0
#   endif
#  endif
#  ifndef YYMALLOC
#   define YYMALLOC malloc
#   if ! defined malloc && ! defined EXIT_SUCCESS
void *malloc (YYSIZE_T); /* INFRINGES ON USER NAME SPACE */
#   endif
#  endif
#  ifndef YYFREE
#   define YYFREE free
#   if ! defined free && ! defined EXIT_SUCCESS
void free (void *); /* INFRINGES ON USER NAME SPACE */
#   endif
#  endif
# endif
#endif /* ! defined yyoverflow || YYERROR_VERBOSE */


#if (! defined yyoverflow \
     && (! defined __cplusplus \
         || (defined YYSTYPE_IS_TRIVIAL && YYSTYPE_IS_TRIVIAL)))

/* A type that is properly aligned for any stack member.  */
union yyalloc
{
  yytype_int16 yyss_alloc;
  YYSTYPE yyvs_alloc;
};

/* The size of the maximum gap between one aligned stack and the next.  */
# define YYSTACK_GAP_MAXIMUM (sizeof (union yyalloc) - 1)

/* The size of an array large to enough to hold all stacks, each with
   N elements.  */
# define YYSTACK_BYTES(N) \
     ((N) * (sizeof (yytype_int16) + sizeof (YYSTYPE)) \
      + YYSTACK_GAP_MAXIMUM)

# define YYCOPY_NEEDED 1

/* Relocate STACK from its old location to the new one.  The
   local variables YYSIZE and YYSTACKSIZE give the old and new number of
   elements in the stack, and YYPTR gives the new location of the
   stack.  Advance YYPTR to a properly aligned location for the next
   stack.  */
# define YYSTACK_RELOCATE(Stack_alloc, Stack)                           \
    do                                                                  \
      {                                                                 \
        YYSIZE_T yynewbytes;                                            \
        YYCOPY (&yyptr->Stack_alloc, Stack, yysize);                    \
        Stack = &yyptr->Stack_alloc;                                    \
        yynewbytes = yystacksize * sizeof (*Stack) + YYSTACK_GAP_MAXIMUM; \
        yyptr += yynewbytes / sizeof (*yyptr);                          \
      }                                                                 \
    while (0)

#endif

#if defined YYCOPY_NEEDED && YYCOPY_NEEDED
/* Copy COUNT objects from SRC to DST.  The source and destination do
   not overlap.  */
# ifndef YYCOPY
#  if defined __GNUC__ && 1 < __GNUC__
#   define YYCOPY(Dst, Src, Count) \
      __builtin_memcpy (Dst, Src, (Count) * sizeof (*(Src)))
#  else
#   define YYCOPY(Dst, Src, Count)              \
      do                                        \
        {                                       \
          YYSIZE_T yyi;                         \
          for (yyi = 0; yyi < (Count); yyi++)   \
            (Dst)[yyi] = (Src)[yyi];            \
        }                                       \
      while (0)
#  endif
# endif
#endif /* !YYCOPY_NEEDED */

/* YYFINAL -- State number of the termination state.  */
#define YYFINAL  9
/* YYLAST -- Last index in YYTABLE.  */
#define YYLAST   112

/* YYNTOKENS -- Number of terminals.  */
#define YYNTOKENS  32
/* YYNNTS -- Number of nonterminals.  */
#define YYNNTS  32
/* YYNRULES -- Number of rules.  */
#define YYNRULES  67
/* YYNSTATES -- Number of states.  */
#define YYNSTATES  108

/* YYTRANSLATE[YYX] -- Symbol number corresponding to YYX as returned
   by yylex, with out-of-bounds checking.  */
#define YYUNDEFTOK  2
#define YYMAXUTOK   286

#define YYTRANSLATE(YYX)                                                \
  ((unsigned int) (YYX) <= YYMAXUTOK ? yytranslate[YYX] : YYUNDEFTOK)

/* YYTRANSLATE[TOKEN-NUM] -- Symbol number corresponding to TOKEN-NUM
   as returned by yylex, without out-of-bounds checking.  */
static const yytype_uint8 yytranslate[] =
{
       0,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     1,     2,     3,     4,
       5,     6,     7,     8,     9,    10,    11,    12,    13,    14,
      15,    16,    17,    18,    19,    20,    21,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    31
};

#if YYDEBUG
  /* YYRLINE[YYN] -- Source line where rule number YYN was defined.  */
static const yytype_uint16 yyrline[] =
{
       0,    34,    34,    38,    49,    52,    53,    56,    61,    70,
      75,    82,    91,    92,    98,   109,   112,   117,   124,   131,
     142,   145,   156,   159,   160,   161,   162,   163,   166,   167,
     170,   175,   183,   190,   191,   197,   202,   205,   209,   216,
     222,   225,   229,   233,   237,   241,   245,   251,   257,   260,
     264,   270,   276,   279,   283,   288,   289,   290,   291,   294,
     300,   301,   304,   315,   317,   322,   326,   327
};
#endif

#if YYDEBUG || YYERROR_VERBOSE || 0
/* YYTNAME[SYMBOL-NUM] -- String name of the symbol SYMBOL-NUM.
   First, the terminals, then, starting at YYNTOKENS, nonterminals.  */
static const char *const yytname[] =
{
  "$end", "error", "$undefined", "IF", "ELSE", "WHILE", "RETURN", "INT",
  "VOID", "ID", "NUM", "ASSIGN", "EQ", "NE", "LT", "LE", "GT", "GE",
  "PLUS", "MINUS", "TIMES", "OVER", "SEMI", "COMMA", "LPAREN", "RPAREN",
  "LBRACE", "RBRACE", "LCURLY", "RCURLY", "ERROR", "OUTER", "$accept",
  "program", "declaration_list", "declaration", "var_declaration",
  "type_specifier", "fun_declaration", "params", "param_list", "param",
  "compound_stmt", "local_declarations", "statement_list", "statement",
  "expression_stmt", "selection_stmt", "iteration_stmt", "return_stmt",
  "expression", "var", "simple_expression", "relop", "additive_expression",
  "addop", "term", "mulop", "factor", "call", "args", "arg_list", "id",
  "num", YY_NULLPTR
};
#endif

# ifdef YYPRINT
/* YYTOKNUM[NUM] -- (External) token number corresponding to the
   (internal) symbol number NUM (which must be that of a token).  */
static const yytype_uint16 yytoknum[] =
{
       0,   256,   257,   258,   259,   260,   261,   262,   263,   264,
     265,   266,   267,   268,   269,   270,   271,   272,   273,   274,
     275,   276,   277,   278,   279,   280,   281,   282,   283,   284,
     285,   286
};
# endif

#define YYPACT_NINF -73

#define yypact_value_is_default(Yystate) \
  (!!((Yystate) == (-73)))

#define YYTABLE_NINF -14

#define yytable_value_is_error(Yytable_value) \
  0

  /* YYPACT[STATE-NUM] -- Index in YYTABLE of the portion describing
     STATE-NUM.  */
static const yytype_int8 yypact[] =
{
      57,   -73,   -73,     4,    57,   -73,   -73,     3,   -73,   -73,
     -73,   -73,    26,   -73,    62,    -5,   -10,     3,     2,     6,
     -73,   -73,    -5,    -5,     5,    15,    39,    57,   -73,   -73,
      33,    49,   -73,   -73,   -73,   -73,   -73,    57,   -73,     3,
      25,    16,    56,    68,    -1,   -73,    27,   -73,   -73,   -73,
     -73,   -73,   -73,   -73,    71,    83,   -73,    70,    58,   -73,
     -73,    -2,   -73,    27,    27,   -73,    73,    74,   -73,    27,
     -73,   -73,   -73,   -73,   -73,   -73,   -73,   -73,    27,    27,
     -73,   -73,    27,    27,    27,    75,    76,   -73,   -73,   -73,
     -73,    72,    58,   -73,   -73,    77,    80,    69,    53,    53,
     -73,    27,   -73,    93,   -73,   -73,    53,   -73
};

  /* YYDEFACT[STATE-NUM] -- Default reduction number in state STATE-NUM.
     Performed when YYTABLE does not specify something else to do.  Zero
     means the default is an error.  */
static const yytype_uint8 yydefact[] =
{
       0,     9,    10,     0,     2,     4,     5,     0,     6,     1,
       3,    64,     0,     7,     0,     0,    10,     0,     0,    12,
      15,    65,     0,     0,     0,    16,     0,     0,    66,    67,
       0,     0,    20,    11,    14,     8,    17,    22,    19,     0,
       0,     0,     0,     0,     0,    29,     0,    18,    24,    21,
      23,    25,    26,    27,     0,    56,    36,    40,    48,    52,
      57,    37,    58,     0,     0,    33,     0,     0,    28,     0,
      45,    46,    42,    41,    44,    43,    49,    50,     0,     0,
      53,    54,     0,    61,     0,     0,     0,    34,    55,    35,
      56,    39,    47,    51,    63,     0,    60,     0,     0,     0,
      59,     0,    38,    30,    32,    62,     0,    31
};

  /* YYPGOTO[NTERM-NUM].  */
static const yytype_int8 yypgoto[] =
{
     -73,   -73,   -73,    94,    67,   -11,   -73,   -73,   -73,    78,
      81,   -73,   -73,   -38,   -73,   -73,   -73,   -73,   -44,   -72,
     -73,   -73,    28,   -73,    29,   -73,    30,   -73,   -73,   -73,
      -6,    51
};

  /* YYDEFGOTO[NTERM-NUM].  */
static const yytype_int8 yydefgoto[] =
{
      -1,     3,     4,     5,     6,     7,     8,    18,    19,    20,
      48,    37,    40,    49,    50,    51,    52,    53,    54,    55,
      56,    78,    57,    79,    58,    82,    59,    60,    95,    96,
      61,    62
};

  /* YYTABLE[YYPACT[STATE-NUM]] -- What to do in state STATE-NUM.  If
     positive, shift that token.  If negative, reduce the rule whose
     number is the opposite.  If YYTABLE_NINF, syntax error.  */
static const yytype_int8 yytable[] =
{
      66,    12,    67,    17,     9,    21,    90,    90,    11,    21,
      90,    25,    11,    22,    23,   -13,    17,    22,    23,    85,
      86,    65,    83,    46,    84,    89,    39,    26,    42,    27,
      43,    44,    30,    41,    11,    21,    11,    21,    13,    94,
      97,    31,    15,    22,    23,    22,    23,    45,    13,    46,
      14,    46,    15,    32,    47,    35,    42,   105,    43,    44,
     103,   104,    11,    21,     1,     2,    24,    32,   107,     1,
      16,    22,    23,    28,    29,    45,    36,    46,    80,    81,
      63,    32,    70,    71,    72,    73,    74,    75,    76,    77,
      76,    77,    64,    68,    69,    87,   102,   106,    10,    88,
      98,    99,   100,   101,    38,    34,    91,    33,    92,     0,
       0,     0,    93
};

static const yytype_int8 yycheck[] =
{
      44,     7,    46,    14,     0,    10,    78,    79,     9,    10,
      82,    17,     9,    18,    19,    25,    27,    18,    19,    63,
      64,    22,    24,    24,    26,    69,    37,    25,     3,    23,
       5,     6,    27,    39,     9,    10,     9,    10,    22,    83,
      84,    26,    26,    18,    19,    18,    19,    22,    22,    24,
      24,    24,    26,    28,    29,    22,     3,   101,     5,     6,
      98,    99,     9,    10,     7,     8,    15,    28,   106,     7,
       8,    18,    19,    22,    23,    22,    27,    24,    20,    21,
      24,    28,    12,    13,    14,    15,    16,    17,    18,    19,
      18,    19,    24,    22,    11,    22,    27,     4,     4,    25,
      25,    25,    25,    23,    37,    27,    78,    26,    79,    -1,
      -1,    -1,    82
};

  /* YYSTOS[STATE-NUM] -- The (internal number of the) accessing
     symbol of state STATE-NUM.  */
static const yytype_uint8 yystos[] =
{
       0,     7,     8,    33,    34,    35,    36,    37,    38,     0,
      35,     9,    62,    22,    24,    26,     8,    37,    39,    40,
      41,    10,    18,    19,    63,    62,    25,    23,    63,    63,
      27,    26,    28,    42,    41,    22,    27,    43,    36,    37,
      44,    62,     3,     5,     6,    22,    24,    29,    42,    45,
      46,    47,    48,    49,    50,    51,    52,    54,    56,    58,
      59,    62,    63,    24,    24,    22,    50,    50,    22,    11,
      12,    13,    14,    15,    16,    17,    18,    19,    53,    55,
      20,    21,    57,    24,    26,    50,    50,    22,    25,    50,
      51,    54,    56,    58,    50,    60,    61,    50,    25,    25,
      25,    23,    27,    45,    45,    50,     4,    45
};

  /* YYR1[YYN] -- Symbol number of symbol that rule YYN derives.  */
static const yytype_uint8 yyr1[] =
{
       0,    32,    33,    34,    34,    35,    35,    36,    36,    37,
      37,    38,    39,    39,    40,    40,    41,    41,    42,    43,
      43,    44,    44,    45,    45,    45,    45,    45,    46,    46,
      47,    47,    48,    49,    49,    50,    50,    51,    51,    52,
      52,    53,    53,    53,    53,    53,    53,    54,    54,    55,
      55,    56,    56,    57,    57,    58,    58,    58,    58,    59,
      60,    60,    61,    61,    62,    63,    63,    63
};

  /* YYR2[YYN] -- Number of symbols on the right hand side of rule YYN.  */
static const yytype_uint8 yyr2[] =
{
       0,     2,     1,     2,     1,     1,     1,     3,     6,     1,
       1,     6,     1,     1,     3,     1,     2,     4,     4,     2,
       0,     2,     0,     1,     1,     1,     1,     1,     2,     1,
       5,     7,     5,     2,     3,     3,     1,     1,     4,     3,
       1,     1,     1,     1,     1,     1,     1,     3,     1,     1,
       1,     3,     1,     1,     1,     3,     1,     1,     1,     4,
       1,     0,     3,     1,     1,     1,     2,     2
};


#define yyerrok         (yyerrstatus = 0)
#define yyclearin       (yychar = YYEMPTY)
#define YYEMPTY         (-2)
#define YYEOF           0

#define YYACCEPT        goto yyacceptlab
#define YYABORT         goto yyabortlab
#define YYERROR         goto yyerrorlab


#define YYRECOVERING()  (!!yyerrstatus)

#define YYBACKUP(Token, Value)                                  \
do                                                              \
  if (yychar == YYEMPTY)                                        \
    {                                                           \
      yychar = (Token);                                         \
      yylval = (Value);                                         \
      YYPOPSTACK (yylen);                                       \
      yystate = *yyssp;                                         \
      goto yybackup;                                            \
    }                                                           \
  else                                                          \
    {                                                           \
      yyerror (YY_("syntax error: cannot back up")); \
      YYERROR;                                                  \
    }                                                           \
while (0)

/* Error token number */
#define YYTERROR        1
#define YYERRCODE       256



/* Enable debugging if requested.  */
#if YYDEBUG

# ifndef YYFPRINTF
#  include <stdio.h> /* INFRINGES ON USER NAME SPACE */
#  define YYFPRINTF fprintf
# endif

# define YYDPRINTF(Args)                        \
do {                                            \
  if (yydebug)                                  \
    YYFPRINTF Args;                             \
} while (0)

/* This macro is provided for backward compatibility. */
#ifndef YY_LOCATION_PRINT
# define YY_LOCATION_PRINT(File, Loc) ((void) 0)
#endif


# define YY_SYMBOL_PRINT(Title, Type, Value, Location)                    \
do {                                                                      \
  if (yydebug)                                                            \
    {                                                                     \
      YYFPRINTF (stderr, "%s ", Title);                                   \
      yy_symbol_print (stderr,                                            \
                  Type, Value); \
      YYFPRINTF (stderr, "\n");                                           \
    }                                                                     \
} while (0)


/*----------------------------------------.
| Print this symbol's value on YYOUTPUT.  |
`----------------------------------------*/

static void
yy_symbol_value_print (FILE *yyoutput, int yytype, YYSTYPE const * const yyvaluep)
{
  FILE *yyo = yyoutput;
  YYUSE (yyo);
  if (!yyvaluep)
    return;
# ifdef YYPRINT
  if (yytype < YYNTOKENS)
    YYPRINT (yyoutput, yytoknum[yytype], *yyvaluep);
# endif
  YYUSE (yytype);
}


/*--------------------------------.
| Print this symbol on YYOUTPUT.  |
`--------------------------------*/

static void
yy_symbol_print (FILE *yyoutput, int yytype, YYSTYPE const * const yyvaluep)
{
  YYFPRINTF (yyoutput, "%s %s (",
             yytype < YYNTOKENS ? "token" : "nterm", yytname[yytype]);

  yy_symbol_value_print (yyoutput, yytype, yyvaluep);
  YYFPRINTF (yyoutput, ")");
}

/*------------------------------------------------------------------.
| yy_stack_print -- Print the state stack from its BOTTOM up to its |
| TOP (included).                                                   |
`------------------------------------------------------------------*/

static void
yy_stack_print (yytype_int16 *yybottom, yytype_int16 *yytop)
{
  YYFPRINTF (stderr, "Stack now");
  for (; yybottom <= yytop; yybottom++)
    {
      int yybot = *yybottom;
      YYFPRINTF (stderr, " %d", yybot);
    }
  YYFPRINTF (stderr, "\n");
}

# define YY_STACK_PRINT(Bottom, Top)                            \
do {                                                            \
  if (yydebug)                                                  \
    yy_stack_print ((Bottom), (Top));                           \
} while (0)


/*------------------------------------------------.
| Report that the YYRULE is going to be reduced.  |
`------------------------------------------------*/

static void
yy_reduce_print (yytype_int16 *yyssp, YYSTYPE *yyvsp, int yyrule)
{
  unsigned long int yylno = yyrline[yyrule];
  int yynrhs = yyr2[yyrule];
  int yyi;
  YYFPRINTF (stderr, "Reducing stack by rule %d (line %lu):\n",
             yyrule - 1, yylno);
  /* The symbols being reduced.  */
  for (yyi = 0; yyi < yynrhs; yyi++)
    {
      YYFPRINTF (stderr, "   $%d = ", yyi + 1);
      yy_symbol_print (stderr,
                       yystos[yyssp[yyi + 1 - yynrhs]],
                       &(yyvsp[(yyi + 1) - (yynrhs)])
                                              );
      YYFPRINTF (stderr, "\n");
    }
}

# define YY_REDUCE_PRINT(Rule)          \
do {                                    \
  if (yydebug)                          \
    yy_reduce_print (yyssp, yyvsp, Rule); \
} while (0)

/* Nonzero means print parse trace.  It is left uninitialized so that
   multiple parsers can coexist.  */
int yydebug;
#else /* !YYDEBUG */
# define YYDPRINTF(Args)
# define YY_SYMBOL_PRINT(Title, Type, Value, Location)
# define YY_STACK_PRINT(Bottom, Top)
# define YY_REDUCE_PRINT(Rule)
#endif /* !YYDEBUG */


/* YYINITDEPTH -- initial size of the parser's stacks.  */
#ifndef YYINITDEPTH
# define YYINITDEPTH 200
#endif

/* YYMAXDEPTH -- maximum size the stacks can grow to (effective only
   if the built-in stack extension method is used).

   Do not make this value too large; the results are undefined if
   YYSTACK_ALLOC_MAXIMUM < YYSTACK_BYTES (YYMAXDEPTH)
   evaluated with infinite-precision integer arithmetic.  */

#ifndef YYMAXDEPTH
# define YYMAXDEPTH 10000
#endif


#if YYERROR_VERBOSE

# ifndef yystrlen
#  if defined __GLIBC__ && defined _STRING_H
#   define yystrlen strlen
#  else
/* Return the length of YYSTR.  */
static YYSIZE_T
yystrlen (const char *yystr)
{
  YYSIZE_T yylen;
  for (yylen = 0; yystr[yylen]; yylen++)
    continue;
  return yylen;
}
#  endif
# endif

# ifndef yystpcpy
#  if defined __GLIBC__ && defined _STRING_H && defined _GNU_SOURCE
#   define yystpcpy stpcpy
#  else
/* Copy YYSRC to YYDEST, returning the address of the terminating '\0' in
   YYDEST.  */
static char *
yystpcpy (char *yydest, const char *yysrc)
{
  char *yyd = yydest;
  const char *yys = yysrc;

  while ((*yyd++ = *yys++) != '\0')
    continue;

  return yyd - 1;
}
#  endif
# endif

# ifndef yytnamerr
/* Copy to YYRES the contents of YYSTR after stripping away unnecessary
   quotes and backslashes, so that it's suitable for yyerror.  The
   heuristic is that double-quoting is unnecessary unless the string
   contains an apostrophe, a comma, or backslash (other than
   backslash-backslash).  YYSTR is taken from yytname.  If YYRES is
   null, do not copy; instead, return the length of what the result
   would have been.  */
static YYSIZE_T
yytnamerr (char *yyres, const char *yystr)
{
  if (*yystr == '"')
    {
      YYSIZE_T yyn = 0;
      char const *yyp = yystr;

      for (;;)
        switch (*++yyp)
          {
          case '\'':
          case ',':
            goto do_not_strip_quotes;

          case '\\':
            if (*++yyp != '\\')
              goto do_not_strip_quotes;
            /* Fall through.  */
          default:
            if (yyres)
              yyres[yyn] = *yyp;
            yyn++;
            break;

          case '"':
            if (yyres)
              yyres[yyn] = '\0';
            return yyn;
          }
    do_not_strip_quotes: ;
    }

  if (! yyres)
    return yystrlen (yystr);

  return yystpcpy (yyres, yystr) - yyres;
}
# endif

/* Copy into *YYMSG, which is of size *YYMSG_ALLOC, an error message
   about the unexpected token YYTOKEN for the state stack whose top is
   YYSSP.

   Return 0 if *YYMSG was successfully written.  Return 1 if *YYMSG is
   not large enough to hold the message.  In that case, also set
   *YYMSG_ALLOC to the required number of bytes.  Return 2 if the
   required number of bytes is too large to store.  */
static int
yysyntax_error (YYSIZE_T *yymsg_alloc, char **yymsg,
                yytype_int16 *yyssp, int yytoken)
{
  YYSIZE_T yysize0 = yytnamerr (YY_NULLPTR, yytname[yytoken]);
  YYSIZE_T yysize = yysize0;
  enum { YYERROR_VERBOSE_ARGS_MAXIMUM = 5 };
  /* Internationalized format string. */
  const char *yyformat = YY_NULLPTR;
  /* Arguments of yyformat. */
  char const *yyarg[YYERROR_VERBOSE_ARGS_MAXIMUM];
  /* Number of reported tokens (one for the "unexpected", one per
     "expected"). */
  int yycount = 0;

  /* There are many possibilities here to consider:
     - If this state is a consistent state with a default action, then
       the only way this function was invoked is if the default action
       is an error action.  In that case, don't check for expected
       tokens because there are none.
     - The only way there can be no lookahead present (in yychar) is if
       this state is a consistent state with a default action.  Thus,
       detecting the absence of a lookahead is sufficient to determine
       that there is no unexpected or expected token to report.  In that
       case, just report a simple "syntax error".
     - Don't assume there isn't a lookahead just because this state is a
       consistent state with a default action.  There might have been a
       previous inconsistent state, consistent state with a non-default
       action, or user semantic action that manipulated yychar.
     - Of course, the expected token list depends on states to have
       correct lookahead information, and it depends on the parser not
       to perform extra reductions after fetching a lookahead from the
       scanner and before detecting a syntax error.  Thus, state merging
       (from LALR or IELR) and default reductions corrupt the expected
       token list.  However, the list is correct for canonical LR with
       one exception: it will still contain any token that will not be
       accepted due to an error action in a later state.
  */
  if (yytoken != YYEMPTY)
    {
      int yyn = yypact[*yyssp];
      yyarg[yycount++] = yytname[yytoken];
      if (!yypact_value_is_default (yyn))
        {
          /* Start YYX at -YYN if negative to avoid negative indexes in
             YYCHECK.  In other words, skip the first -YYN actions for
             this state because they are default actions.  */
          int yyxbegin = yyn < 0 ? -yyn : 0;
          /* Stay within bounds of both yycheck and yytname.  */
          int yychecklim = YYLAST - yyn + 1;
          int yyxend = yychecklim < YYNTOKENS ? yychecklim : YYNTOKENS;
          int yyx;

          for (yyx = yyxbegin; yyx < yyxend; ++yyx)
            if (yycheck[yyx + yyn] == yyx && yyx != YYTERROR
                && !yytable_value_is_error (yytable[yyx + yyn]))
              {
                if (yycount == YYERROR_VERBOSE_ARGS_MAXIMUM)
                  {
                    yycount = 1;
                    yysize = yysize0;
                    break;
                  }
                yyarg[yycount++] = yytname[yyx];
                {
                  YYSIZE_T yysize1 = yysize + yytnamerr (YY_NULLPTR, yytname[yyx]);
                  if (! (yysize <= yysize1
                         && yysize1 <= YYSTACK_ALLOC_MAXIMUM))
                    return 2;
                  yysize = yysize1;
                }
              }
        }
    }

  switch (yycount)
    {
# define YYCASE_(N, S)                      \
      case N:                               \
        yyformat = S;                       \
      break
      YYCASE_(0, YY_("syntax error"));
      YYCASE_(1, YY_("syntax error, unexpected %s"));
      YYCASE_(2, YY_("syntax error, unexpected %s, expecting %s"));
      YYCASE_(3, YY_("syntax error, unexpected %s, expecting %s or %s"));
      YYCASE_(4, YY_("syntax error, unexpected %s, expecting %s or %s or %s"));
      YYCASE_(5, YY_("syntax error, unexpected %s, expecting %s or %s or %s or %s"));
# undef YYCASE_
    }

  {
    YYSIZE_T yysize1 = yysize + yystrlen (yyformat);
    if (! (yysize <= yysize1 && yysize1 <= YYSTACK_ALLOC_MAXIMUM))
      return 2;
    yysize = yysize1;
  }

  if (*yymsg_alloc < yysize)
    {
      *yymsg_alloc = 2 * yysize;
      if (! (yysize <= *yymsg_alloc
             && *yymsg_alloc <= YYSTACK_ALLOC_MAXIMUM))
        *yymsg_alloc = YYSTACK_ALLOC_MAXIMUM;
      return 1;
    }

  /* Avoid sprintf, as that infringes on the user's name space.
     Don't have undefined behavior even if the translation
     produced a string with the wrong number of "%s"s.  */
  {
    char *yyp = *yymsg;
    int yyi = 0;
    while ((*yyp = *yyformat) != '\0')
      if (*yyp == '%' && yyformat[1] == 's' && yyi < yycount)
        {
          yyp += yytnamerr (yyp, yyarg[yyi++]);
          yyformat += 2;
        }
      else
        {
          yyp++;
          yyformat++;
        }
  }
  return 0;
}
#endif /* YYERROR_VERBOSE */

/*-----------------------------------------------.
| Release the memory associated to this symbol.  |
`-----------------------------------------------*/

static void
yydestruct (const char *yymsg, int yytype, YYSTYPE *yyvaluep)
{
  YYUSE (yyvaluep);
  if (!yymsg)
    yymsg = "Deleting";
  YY_SYMBOL_PRINT (yymsg, yytype, yyvaluep, yylocationp);

  YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN
  YYUSE (yytype);
  YY_IGNORE_MAYBE_UNINITIALIZED_END
}




/* The lookahead symbol.  */
int yychar;

/* The semantic value of the lookahead symbol.  */
YYSTYPE yylval;
/* Number of syntax errors so far.  */
int yynerrs;


/*----------.
| yyparse.  |
`----------*/

int
yyparse (void)
{
    int yystate;
    /* Number of tokens to shift before error messages enabled.  */
    int yyerrstatus;

    /* The stacks and their tools:
       'yyss': related to states.
       'yyvs': related to semantic values.

       Refer to the stacks through separate pointers, to allow yyoverflow
       to reallocate them elsewhere.  */

    /* The state stack.  */
    yytype_int16 yyssa[YYINITDEPTH];
    yytype_int16 *yyss;
    yytype_int16 *yyssp;

    /* The semantic value stack.  */
    YYSTYPE yyvsa[YYINITDEPTH];
    YYSTYPE *yyvs;
    YYSTYPE *yyvsp;

    YYSIZE_T yystacksize;

  int yyn;
  int yyresult;
  /* Lookahead token as an internal (translated) token number.  */
  int yytoken = 0;
  /* The variables used to return semantic value and location from the
     action routines.  */
  YYSTYPE yyval;

#if YYERROR_VERBOSE
  /* Buffer for error messages, and its allocated size.  */
  char yymsgbuf[128];
  char *yymsg = yymsgbuf;
  YYSIZE_T yymsg_alloc = sizeof yymsgbuf;
#endif

#define YYPOPSTACK(N)   (yyvsp -= (N), yyssp -= (N))

  /* The number of symbols on the RHS of the reduced rule.
     Keep to zero when no symbol should be popped.  */
  int yylen = 0;

  yyssp = yyss = yyssa;
  yyvsp = yyvs = yyvsa;
  yystacksize = YYINITDEPTH;

  YYDPRINTF ((stderr, "Starting parse\n"));

  yystate = 0;
  yyerrstatus = 0;
  yynerrs = 0;
  yychar = YYEMPTY; /* Cause a token to be read.  */
  goto yysetstate;

/*------------------------------------------------------------.
| yynewstate -- Push a new state, which is found in yystate.  |
`------------------------------------------------------------*/
 yynewstate:
  /* In all cases, when you get here, the value and location stacks
     have just been pushed.  So pushing a state here evens the stacks.  */
  yyssp++;

 yysetstate:
  *yyssp = yystate;

  if (yyss + yystacksize - 1 <= yyssp)
    {
      /* Get the current used size of the three stacks, in elements.  */
      YYSIZE_T yysize = yyssp - yyss + 1;

#ifdef yyoverflow
      {
        /* Give user a chance to reallocate the stack.  Use copies of
           these so that the &'s don't force the real ones into
           memory.  */
        YYSTYPE *yyvs1 = yyvs;
        yytype_int16 *yyss1 = yyss;

        /* Each stack pointer address is followed by the size of the
           data in use in that stack, in bytes.  This used to be a
           conditional around just the two extra args, but that might
           be undefined if yyoverflow is a macro.  */
        yyoverflow (YY_("memory exhausted"),
                    &yyss1, yysize * sizeof (*yyssp),
                    &yyvs1, yysize * sizeof (*yyvsp),
                    &yystacksize);

        yyss = yyss1;
        yyvs = yyvs1;
      }
#else /* no yyoverflow */
# ifndef YYSTACK_RELOCATE
      goto yyexhaustedlab;
# else
      /* Extend the stack our own way.  */
      if (YYMAXDEPTH <= yystacksize)
        goto yyexhaustedlab;
      yystacksize *= 2;
      if (YYMAXDEPTH < yystacksize)
        yystacksize = YYMAXDEPTH;

      {
        yytype_int16 *yyss1 = yyss;
        union yyalloc *yyptr =
          (union yyalloc *) YYSTACK_ALLOC (YYSTACK_BYTES (yystacksize));
        if (! yyptr)
          goto yyexhaustedlab;
        YYSTACK_RELOCATE (yyss_alloc, yyss);
        YYSTACK_RELOCATE (yyvs_alloc, yyvs);
#  undef YYSTACK_RELOCATE
        if (yyss1 != yyssa)
          YYSTACK_FREE (yyss1);
      }
# endif
#endif /* no yyoverflow */

      yyssp = yyss + yysize - 1;
      yyvsp = yyvs + yysize - 1;

      YYDPRINTF ((stderr, "Stack size increased to %lu\n",
                  (unsigned long int) yystacksize));

      if (yyss + yystacksize - 1 <= yyssp)
        YYABORT;
    }

  YYDPRINTF ((stderr, "Entering state %d\n", yystate));

  if (yystate == YYFINAL)
    YYACCEPT;

  goto yybackup;

/*-----------.
| yybackup.  |
`-----------*/
yybackup:

  /* Do appropriate processing given the current state.  Read a
     lookahead token if we need one and don't already have one.  */

  /* First try to decide what to do without reference to lookahead token.  */
  yyn = yypact[yystate];
  if (yypact_value_is_default (yyn))
    goto yydefault;

  /* Not known => get a lookahead token if don't already have one.  */

  /* YYCHAR is either YYEMPTY or YYEOF or a valid lookahead symbol.  */
  if (yychar == YYEMPTY)
    {
      YYDPRINTF ((stderr, "Reading a token: "));
      yychar = yylex ();
    }

  if (yychar <= YYEOF)
    {
      yychar = yytoken = YYEOF;
      YYDPRINTF ((stderr, "Now at end of input.\n"));
    }
  else
    {
      yytoken = YYTRANSLATE (yychar);
      YY_SYMBOL_PRINT ("Next token is", yytoken, &yylval, &yylloc);
    }

  /* If the proper action on seeing token YYTOKEN is to reduce or to
     detect an error, take that action.  */
  yyn += yytoken;
  if (yyn < 0 || YYLAST < yyn || yycheck[yyn] != yytoken)
    goto yydefault;
  yyn = yytable[yyn];
  if (yyn <= 0)
    {
      if (yytable_value_is_error (yyn))
        goto yyerrlab;
      yyn = -yyn;
      goto yyreduce;
    }

  /* Count tokens shifted since error; after three, turn off error
     status.  */
  if (yyerrstatus)
    yyerrstatus--;

  /* Shift the lookahead token.  */
  YY_SYMBOL_PRINT ("Shifting", yytoken, &yylval, &yylloc);

  /* Discard the shifted token.  */
  yychar = YYEMPTY;

  yystate = yyn;
  YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN
  *++yyvsp = yylval;
  YY_IGNORE_MAYBE_UNINITIALIZED_END

  goto yynewstate;


/*-----------------------------------------------------------.
| yydefault -- do the default action for the current state.  |
`-----------------------------------------------------------*/
yydefault:
  yyn = yydefact[yystate];
  if (yyn == 0)
    goto yyerrlab;
  goto yyreduce;


/*-----------------------------.
| yyreduce -- Do a reduction.  |
`-----------------------------*/
yyreduce:
  /* yyn is the number of a rule to reduce with.  */
  yylen = yyr2[yyn];

  /* If YYLEN is nonzero, implement the default value of the action:
     '$$ = $1'.

     Otherwise, the following line sets YYVAL to garbage.
     This behavior is undocumented and Bison
     users should not rely upon it.  Assigning to YYVAL
     unconditionally makes the parser a bit smaller, and it avoids a
     GCC warning that YYVAL may be used uninitialized.  */
  yyval = yyvsp[1-yylen];


  YY_REDUCE_PRINT (yyn);
  switch (yyn)
    {
        case 2:
#line 35 "cminus.y" /* yacc.c:1646  */
    { savedTree = (yyvsp[0]);}
#line 1351 "y.tab.c" /* yacc.c:1646  */
    break;

  case 3:
#line 39 "cminus.y" /* yacc.c:1646  */
    {	YYSTYPE t = (yyvsp[-1]);
												if (t != NULL)
												{ while (t->sibling != NULL)
														t = t->sibling;
													t->sibling = (yyvsp[0]);
													(yyval) = (yyvsp[-1]);
												}
												else
													(yyval) = (yyvsp[0]);
											}
#line 1366 "y.tab.c" /* yacc.c:1646  */
    break;

  case 4:
#line 49 "cminus.y" /* yacc.c:1646  */
    {(yyval) = (yyvsp[0]);}
#line 1372 "y.tab.c" /* yacc.c:1646  */
    break;

  case 5:
#line 52 "cminus.y" /* yacc.c:1646  */
    {(yyval) = (yyvsp[0]);}
#line 1378 "y.tab.c" /* yacc.c:1646  */
    break;

  case 6:
#line 53 "cminus.y" /* yacc.c:1646  */
    {(yyval) = (yyvsp[0]);}
#line 1384 "y.tab.c" /* yacc.c:1646  */
    break;

  case 7:
#line 57 "cminus.y" /* yacc.c:1646  */
    { (yyval) = newExpNode(VarDclK);
												(yyval)->attr.name = (yyvsp[-1])->attr.name;
												(yyval)->type = (yyvsp[-2])->type;
											}
#line 1393 "y.tab.c" /* yacc.c:1646  */
    break;

  case 8:
#line 62 "cminus.y" /* yacc.c:1646  */
    { (yyval) = newExpNode(VarDclK);
												(yyval)->child[0] = (yyvsp[-2]);
												(yyval)->type = ((yyvsp[-5])->type == Integer) ? IntegerArr : VoidArr;
												(yyval)->attr.name = (yyvsp[-4])->attr.name;
												(yyval)->arraySize =  (yyvsp[-2])->attr.val;
											}
#line 1404 "y.tab.c" /* yacc.c:1646  */
    break;

  case 9:
#line 71 "cminus.y" /* yacc.c:1646  */
    { /*{ $$ = newTypeNode(TypeK);*/
												(yyval) = newExpNode(VarK);
												(yyval)->type = Integer;
								 			}
#line 1413 "y.tab.c" /* yacc.c:1646  */
    break;

  case 10:
#line 76 "cminus.y" /* yacc.c:1646  */
    {/*{ $$ = newTypeNode(TypeK);*/
												(yyval) = newExpNode(VarK);
												(yyval)->type = Void;
											}
#line 1422 "y.tab.c" /* yacc.c:1646  */
    break;

  case 11:
#line 83 "cminus.y" /* yacc.c:1646  */
    {	(yyval) = newExpNode(FunDclK);
												(yyval)->type = (yyvsp[-5])->type;
												(yyval)->attr.name = (yyvsp[-4])->attr.name;
												(yyval)->child[0] = (yyvsp[-2]);
												(yyval)->child[1] = (yyvsp[0]);
											}
#line 1433 "y.tab.c" /* yacc.c:1646  */
    break;

  case 12:
#line 91 "cminus.y" /* yacc.c:1646  */
    { (yyval) = (yyvsp[0]); }
#line 1439 "y.tab.c" /* yacc.c:1646  */
    break;

  case 13:
#line 93 "cminus.y" /* yacc.c:1646  */
    { (yyval) = newExpNode(ParamK);
												(yyval)->type = Void;
											}
#line 1447 "y.tab.c" /* yacc.c:1646  */
    break;

  case 14:
#line 99 "cminus.y" /* yacc.c:1646  */
    { YYSTYPE t = (yyvsp[-2]);
												if( t != NULL )
												{	while(t->sibling != NULL)
														t = t->sibling;
													t->sibling = (yyvsp[0]);
													(yyval) = (yyvsp[-2]);
												}
												else 
													(yyval) = (yyvsp[0]);
											}
#line 1462 "y.tab.c" /* yacc.c:1646  */
    break;

  case 15:
#line 109 "cminus.y" /* yacc.c:1646  */
    { (yyval) = (yyvsp[0]); }
#line 1468 "y.tab.c" /* yacc.c:1646  */
    break;

  case 16:
#line 113 "cminus.y" /* yacc.c:1646  */
    {	(yyval) = newExpNode(ParamK);
												(yyval)->type = (yyvsp[-1])->type;
												(yyval)->attr.name = (yyvsp[0])->attr.name;
											}
#line 1477 "y.tab.c" /* yacc.c:1646  */
    break;

  case 17:
#line 118 "cminus.y" /* yacc.c:1646  */
    {	(yyval) = newExpNode(ParamK);
												(yyval)->type = ((yyvsp[-3])->type == Integer) ? IntegerArr : VoidArr;
												(yyval)->attr.name = (yyvsp[-2])->attr.name;
											}
#line 1486 "y.tab.c" /* yacc.c:1646  */
    break;

  case 18:
#line 125 "cminus.y" /* yacc.c:1646  */
    {	(yyval) = newStmtNode(CompK);
												(yyval)->child[0] = (yyvsp[-2]);
												(yyval)->child[1] = (yyvsp[-1]);
											}
#line 1495 "y.tab.c" /* yacc.c:1646  */
    break;

  case 19:
#line 132 "cminus.y" /* yacc.c:1646  */
    { YYSTYPE t = (yyvsp[-1]);
												if( t != NULL )
												{	while(t->sibling != NULL)
														t = t->sibling;
													t->sibling = (yyvsp[0]);
													(yyval) = (yyvsp[-1]);
												}
												else
													(yyval) = (yyvsp[0]);
											}
#line 1510 "y.tab.c" /* yacc.c:1646  */
    break;

  case 20:
#line 142 "cminus.y" /* yacc.c:1646  */
    {(yyval) = NULL;}
#line 1516 "y.tab.c" /* yacc.c:1646  */
    break;

  case 21:
#line 146 "cminus.y" /* yacc.c:1646  */
    { YYSTYPE t = (yyvsp[-1]);
												if( t != NULL )
												{	while(t->sibling != NULL)
														t = t->sibling;
													t->sibling = (yyvsp[0]);
													(yyval) = (yyvsp[-1]);
												}
												else
													(yyval) = (yyvsp[0]);
											}
#line 1531 "y.tab.c" /* yacc.c:1646  */
    break;

  case 22:
#line 156 "cminus.y" /* yacc.c:1646  */
    {(yyval) = NULL;}
#line 1537 "y.tab.c" /* yacc.c:1646  */
    break;

  case 23:
#line 159 "cminus.y" /* yacc.c:1646  */
    { (yyval) = (yyvsp[0]); }
#line 1543 "y.tab.c" /* yacc.c:1646  */
    break;

  case 24:
#line 160 "cminus.y" /* yacc.c:1646  */
    { (yyval) = (yyvsp[0]); }
#line 1549 "y.tab.c" /* yacc.c:1646  */
    break;

  case 25:
#line 161 "cminus.y" /* yacc.c:1646  */
    { (yyval) = (yyvsp[0]); }
#line 1555 "y.tab.c" /* yacc.c:1646  */
    break;

  case 26:
#line 162 "cminus.y" /* yacc.c:1646  */
    { (yyval) = (yyvsp[0]); }
#line 1561 "y.tab.c" /* yacc.c:1646  */
    break;

  case 27:
#line 163 "cminus.y" /* yacc.c:1646  */
    { (yyval) = (yyvsp[0]); }
#line 1567 "y.tab.c" /* yacc.c:1646  */
    break;

  case 28:
#line 166 "cminus.y" /* yacc.c:1646  */
    { (yyval) = (yyvsp[-1]); }
#line 1573 "y.tab.c" /* yacc.c:1646  */
    break;

  case 29:
#line 167 "cminus.y" /* yacc.c:1646  */
    { (yyval) = NULL; }
#line 1579 "y.tab.c" /* yacc.c:1646  */
    break;

  case 30:
#line 171 "cminus.y" /* yacc.c:1646  */
    {	(yyval) = newStmtNode(SelK);
												(yyval)->child[0] = (yyvsp[-2]);
												(yyval)->child[1] = (yyvsp[0]);
											}
#line 1588 "y.tab.c" /* yacc.c:1646  */
    break;

  case 31:
#line 176 "cminus.y" /* yacc.c:1646  */
    {	(yyval) = newStmtNode(SelK);
												(yyval)->child[0] = (yyvsp[-4]);
												(yyval)->child[1] = (yyvsp[-2]);
												(yyval)->child[2] = (yyvsp[0]);
											}
#line 1598 "y.tab.c" /* yacc.c:1646  */
    break;

  case 32:
#line 184 "cminus.y" /* yacc.c:1646  */
    { (yyval) = newStmtNode(IterK);
                   			(yyval)->child[0] = (yyvsp[-2]);
                   			(yyval)->child[1] = (yyvsp[0]);
                		 	}
#line 1607 "y.tab.c" /* yacc.c:1646  */
    break;

  case 33:
#line 190 "cminus.y" /* yacc.c:1646  */
    { (yyval) = newStmtNode(ReturnK); }
#line 1613 "y.tab.c" /* yacc.c:1646  */
    break;

  case 34:
#line 192 "cminus.y" /* yacc.c:1646  */
    {	(yyval) = newStmtNode(ReturnK);
												(yyval)->child[0] = (yyvsp[-1]);
											}
#line 1621 "y.tab.c" /* yacc.c:1646  */
    break;

  case 35:
#line 198 "cminus.y" /* yacc.c:1646  */
    {	(yyval) = newExpNode(AssignK);
												(yyval)->child[0] = (yyvsp[-2]);
												(yyval)->child[1] = (yyvsp[0]);
											}
#line 1630 "y.tab.c" /* yacc.c:1646  */
    break;

  case 36:
#line 202 "cminus.y" /* yacc.c:1646  */
    { (yyval) = (yyvsp[0]); }
#line 1636 "y.tab.c" /* yacc.c:1646  */
    break;

  case 37:
#line 206 "cminus.y" /* yacc.c:1646  */
    {	(yyval) = newExpNode(VarK);
												(yyval)->attr.name = (yyvsp[0])->attr.name;
											}
#line 1644 "y.tab.c" /* yacc.c:1646  */
    break;

  case 38:
#line 210 "cminus.y" /* yacc.c:1646  */
    {	(yyval) = newExpNode(VarK);
												(yyval)->attr.name = (yyvsp[-3])->attr.name;
												(yyval)->child[0] = (yyvsp[-1]);
											}
#line 1653 "y.tab.c" /* yacc.c:1646  */
    break;

  case 39:
#line 217 "cminus.y" /* yacc.c:1646  */
    { (yyval) = newExpNode(OpK);
                   			(yyval)->child[0] = (yyvsp[-2]);
                   			(yyval)->child[1] = (yyvsp[0]);
                   			(yyval)->attr.op = (yyvsp[-1])->attr.op;
                 			}
#line 1663 "y.tab.c" /* yacc.c:1646  */
    break;

  case 40:
#line 222 "cminus.y" /* yacc.c:1646  */
    { (yyval) = (yyvsp[0]); }
#line 1669 "y.tab.c" /* yacc.c:1646  */
    break;

  case 41:
#line 226 "cminus.y" /* yacc.c:1646  */
    {	(yyval) = newExpNode(OpK);
												(yyval)->attr.op = LE;
											}
#line 1677 "y.tab.c" /* yacc.c:1646  */
    break;

  case 42:
#line 230 "cminus.y" /* yacc.c:1646  */
    {	(yyval) = newExpNode(OpK);
												(yyval)->attr.op = LT;
											}
#line 1685 "y.tab.c" /* yacc.c:1646  */
    break;

  case 43:
#line 234 "cminus.y" /* yacc.c:1646  */
    {	(yyval) = newExpNode(OpK);
												(yyval)->attr.op = GE;
											}
#line 1693 "y.tab.c" /* yacc.c:1646  */
    break;

  case 44:
#line 238 "cminus.y" /* yacc.c:1646  */
    {	(yyval) = newExpNode(OpK);
												(yyval)->attr.op = GT;
											}
#line 1701 "y.tab.c" /* yacc.c:1646  */
    break;

  case 45:
#line 242 "cminus.y" /* yacc.c:1646  */
    {	(yyval) = newExpNode(OpK);
												(yyval)->attr.op = EQ;
											}
#line 1709 "y.tab.c" /* yacc.c:1646  */
    break;

  case 46:
#line 246 "cminus.y" /* yacc.c:1646  */
    {	(yyval) = newExpNode(OpK);
												(yyval)->attr.op = NE;
											}
#line 1717 "y.tab.c" /* yacc.c:1646  */
    break;

  case 47:
#line 252 "cminus.y" /* yacc.c:1646  */
    { (yyval) = newExpNode(OpK);
												(yyval)->child[0] = (yyvsp[-2]);
												(yyval)->child[1] = (yyvsp[0]);
												(yyval)->attr.op = (yyvsp[-1])->attr.op;
											}
#line 1727 "y.tab.c" /* yacc.c:1646  */
    break;

  case 48:
#line 257 "cminus.y" /* yacc.c:1646  */
    { (yyval) = (yyvsp[0]); }
#line 1733 "y.tab.c" /* yacc.c:1646  */
    break;

  case 49:
#line 261 "cminus.y" /* yacc.c:1646  */
    { (yyval) = newExpNode(OpK);
												(yyval)->attr.op = PLUS;
											}
#line 1741 "y.tab.c" /* yacc.c:1646  */
    break;

  case 50:
#line 265 "cminus.y" /* yacc.c:1646  */
    {	(yyval) = newExpNode(OpK);
												(yyval)->attr.op = MINUS;
											}
#line 1749 "y.tab.c" /* yacc.c:1646  */
    break;

  case 51:
#line 271 "cminus.y" /* yacc.c:1646  */
    { (yyval) = newExpNode(OpK);
                   			(yyval)->child[0] = (yyvsp[-2]);
                   			(yyval)->child[1] = (yyvsp[0]);
                   			(yyval)->attr.op = (yyvsp[-1])->attr.op;
                 			}
#line 1759 "y.tab.c" /* yacc.c:1646  */
    break;

  case 52:
#line 276 "cminus.y" /* yacc.c:1646  */
    { (yyval) = (yyvsp[0]); }
#line 1765 "y.tab.c" /* yacc.c:1646  */
    break;

  case 53:
#line 280 "cminus.y" /* yacc.c:1646  */
    {	(yyval) = newExpNode(OpK);
												(yyval)->attr.op = TIMES;
											}
#line 1773 "y.tab.c" /* yacc.c:1646  */
    break;

  case 54:
#line 284 "cminus.y" /* yacc.c:1646  */
    {	(yyval) = newExpNode(OpK);
												(yyval)->attr.op = OVER;
											}
#line 1781 "y.tab.c" /* yacc.c:1646  */
    break;

  case 55:
#line 288 "cminus.y" /* yacc.c:1646  */
    { (yyval) = (yyvsp[-1]); }
#line 1787 "y.tab.c" /* yacc.c:1646  */
    break;

  case 56:
#line 289 "cminus.y" /* yacc.c:1646  */
    { (yyval) = (yyvsp[0]); }
#line 1793 "y.tab.c" /* yacc.c:1646  */
    break;

  case 57:
#line 290 "cminus.y" /* yacc.c:1646  */
    { (yyval) = (yyvsp[0]); }
#line 1799 "y.tab.c" /* yacc.c:1646  */
    break;

  case 58:
#line 291 "cminus.y" /* yacc.c:1646  */
    { (yyval) = (yyvsp[0]); }
#line 1805 "y.tab.c" /* yacc.c:1646  */
    break;

  case 59:
#line 295 "cminus.y" /* yacc.c:1646  */
    { (yyval) = newExpNode(CallK);
												(yyval)->attr.name = (yyvsp[-3])->attr.name;
												(yyval)->child[0] = (yyvsp[-1]);
											}
#line 1814 "y.tab.c" /* yacc.c:1646  */
    break;

  case 60:
#line 300 "cminus.y" /* yacc.c:1646  */
    { (yyval) = (yyvsp[0]); }
#line 1820 "y.tab.c" /* yacc.c:1646  */
    break;

  case 61:
#line 301 "cminus.y" /* yacc.c:1646  */
    { (yyval) = NULL; }
#line 1826 "y.tab.c" /* yacc.c:1646  */
    break;

  case 62:
#line 305 "cminus.y" /* yacc.c:1646  */
    {	YYSTYPE t = (yyvsp[-2]);
												if( t != NULL)
												{ while( t->sibling != NULL)
														t = t->sibling;
													t->sibling = (yyvsp[0]);
													(yyval) = (yyvsp[-2]);
												}
												else
													(yyval) = (yyvsp[0]);
											}
#line 1841 "y.tab.c" /* yacc.c:1646  */
    break;

  case 63:
#line 315 "cminus.y" /* yacc.c:1646  */
    { (yyval) = (yyvsp[0]); }
#line 1847 "y.tab.c" /* yacc.c:1646  */
    break;

  case 64:
#line 318 "cminus.y" /* yacc.c:1646  */
    {	(yyval) = newExpNode(VarK);
												(yyval)->attr.name = copyString(tokenString);
											}
#line 1855 "y.tab.c" /* yacc.c:1646  */
    break;

  case 65:
#line 323 "cminus.y" /* yacc.c:1646  */
    { (yyval) = newExpNode(ConstK);
												(yyval)->attr.val = atoi(tokenString);
											}
#line 1863 "y.tab.c" /* yacc.c:1646  */
    break;

  case 66:
#line 326 "cminus.y" /* yacc.c:1646  */
    { (yyval) = (yyvsp[0]); }
#line 1869 "y.tab.c" /* yacc.c:1646  */
    break;

  case 67:
#line 328 "cminus.y" /* yacc.c:1646  */
    {	(yyval) = (yyvsp[0]);
												(yyval)->attr.val = (-1) * ((yyvsp[0])->attr.val);
											}
#line 1877 "y.tab.c" /* yacc.c:1646  */
    break;


#line 1881 "y.tab.c" /* yacc.c:1646  */
      default: break;
    }
  /* User semantic actions sometimes alter yychar, and that requires
     that yytoken be updated with the new translation.  We take the
     approach of translating immediately before every use of yytoken.
     One alternative is translating here after every semantic action,
     but that translation would be missed if the semantic action invokes
     YYABORT, YYACCEPT, or YYERROR immediately after altering yychar or
     if it invokes YYBACKUP.  In the case of YYABORT or YYACCEPT, an
     incorrect destructor might then be invoked immediately.  In the
     case of YYERROR or YYBACKUP, subsequent parser actions might lead
     to an incorrect destructor call or verbose syntax error message
     before the lookahead is translated.  */
  YY_SYMBOL_PRINT ("-> $$ =", yyr1[yyn], &yyval, &yyloc);

  YYPOPSTACK (yylen);
  yylen = 0;
  YY_STACK_PRINT (yyss, yyssp);

  *++yyvsp = yyval;

  /* Now 'shift' the result of the reduction.  Determine what state
     that goes to, based on the state we popped back to and the rule
     number reduced by.  */

  yyn = yyr1[yyn];

  yystate = yypgoto[yyn - YYNTOKENS] + *yyssp;
  if (0 <= yystate && yystate <= YYLAST && yycheck[yystate] == *yyssp)
    yystate = yytable[yystate];
  else
    yystate = yydefgoto[yyn - YYNTOKENS];

  goto yynewstate;


/*--------------------------------------.
| yyerrlab -- here on detecting error.  |
`--------------------------------------*/
yyerrlab:
  /* Make sure we have latest lookahead translation.  See comments at
     user semantic actions for why this is necessary.  */
  yytoken = yychar == YYEMPTY ? YYEMPTY : YYTRANSLATE (yychar);

  /* If not already recovering from an error, report this error.  */
  if (!yyerrstatus)
    {
      ++yynerrs;
#if ! YYERROR_VERBOSE
      yyerror (YY_("syntax error"));
#else
# define YYSYNTAX_ERROR yysyntax_error (&yymsg_alloc, &yymsg, \
                                        yyssp, yytoken)
      {
        char const *yymsgp = YY_("syntax error");
        int yysyntax_error_status;
        yysyntax_error_status = YYSYNTAX_ERROR;
        if (yysyntax_error_status == 0)
          yymsgp = yymsg;
        else if (yysyntax_error_status == 1)
          {
            if (yymsg != yymsgbuf)
              YYSTACK_FREE (yymsg);
            yymsg = (char *) YYSTACK_ALLOC (yymsg_alloc);
            if (!yymsg)
              {
                yymsg = yymsgbuf;
                yymsg_alloc = sizeof yymsgbuf;
                yysyntax_error_status = 2;
              }
            else
              {
                yysyntax_error_status = YYSYNTAX_ERROR;
                yymsgp = yymsg;
              }
          }
        yyerror (yymsgp);
        if (yysyntax_error_status == 2)
          goto yyexhaustedlab;
      }
# undef YYSYNTAX_ERROR
#endif
    }



  if (yyerrstatus == 3)
    {
      /* If just tried and failed to reuse lookahead token after an
         error, discard it.  */

      if (yychar <= YYEOF)
        {
          /* Return failure if at end of input.  */
          if (yychar == YYEOF)
            YYABORT;
        }
      else
        {
          yydestruct ("Error: discarding",
                      yytoken, &yylval);
          yychar = YYEMPTY;
        }
    }

  /* Else will try to reuse lookahead token after shifting the error
     token.  */
  goto yyerrlab1;


/*---------------------------------------------------.
| yyerrorlab -- error raised explicitly by YYERROR.  |
`---------------------------------------------------*/
yyerrorlab:

  /* Pacify compilers like GCC when the user code never invokes
     YYERROR and the label yyerrorlab therefore never appears in user
     code.  */
  if (/*CONSTCOND*/ 0)
     goto yyerrorlab;

  /* Do not reclaim the symbols of the rule whose action triggered
     this YYERROR.  */
  YYPOPSTACK (yylen);
  yylen = 0;
  YY_STACK_PRINT (yyss, yyssp);
  yystate = *yyssp;
  goto yyerrlab1;


/*-------------------------------------------------------------.
| yyerrlab1 -- common code for both syntax error and YYERROR.  |
`-------------------------------------------------------------*/
yyerrlab1:
  yyerrstatus = 3;      /* Each real token shifted decrements this.  */

  for (;;)
    {
      yyn = yypact[yystate];
      if (!yypact_value_is_default (yyn))
        {
          yyn += YYTERROR;
          if (0 <= yyn && yyn <= YYLAST && yycheck[yyn] == YYTERROR)
            {
              yyn = yytable[yyn];
              if (0 < yyn)
                break;
            }
        }

      /* Pop the current state because it cannot handle the error token.  */
      if (yyssp == yyss)
        YYABORT;


      yydestruct ("Error: popping",
                  yystos[yystate], yyvsp);
      YYPOPSTACK (1);
      yystate = *yyssp;
      YY_STACK_PRINT (yyss, yyssp);
    }

  YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN
  *++yyvsp = yylval;
  YY_IGNORE_MAYBE_UNINITIALIZED_END


  /* Shift the error token.  */
  YY_SYMBOL_PRINT ("Shifting", yystos[yyn], yyvsp, yylsp);

  yystate = yyn;
  goto yynewstate;


/*-------------------------------------.
| yyacceptlab -- YYACCEPT comes here.  |
`-------------------------------------*/
yyacceptlab:
  yyresult = 0;
  goto yyreturn;

/*-----------------------------------.
| yyabortlab -- YYABORT comes here.  |
`-----------------------------------*/
yyabortlab:
  yyresult = 1;
  goto yyreturn;

#if !defined yyoverflow || YYERROR_VERBOSE
/*-------------------------------------------------.
| yyexhaustedlab -- memory exhaustion comes here.  |
`-------------------------------------------------*/
yyexhaustedlab:
  yyerror (YY_("memory exhausted"));
  yyresult = 2;
  /* Fall through.  */
#endif

yyreturn:
  if (yychar != YYEMPTY)
    {
      /* Make sure we have latest lookahead translation.  See comments at
         user semantic actions for why this is necessary.  */
      yytoken = YYTRANSLATE (yychar);
      yydestruct ("Cleanup: discarding lookahead",
                  yytoken, &yylval);
    }
  /* Do not reclaim the symbols of the rule whose action triggered
     this YYABORT or YYACCEPT.  */
  YYPOPSTACK (yylen);
  YY_STACK_PRINT (yyss, yyssp);
  while (yyssp != yyss)
    {
      yydestruct ("Cleanup: popping",
                  yystos[*yyssp], yyvsp);
      YYPOPSTACK (1);
    }
#ifndef yyoverflow
  if (yyss != yyssa)
    YYSTACK_FREE (yyss);
#endif
#if YYERROR_VERBOSE
  if (yymsg != yymsgbuf)
    YYSTACK_FREE (yymsg);
#endif
  return yyresult;
}
#line 333 "cminus.y" /* yacc.c:1906  */


int yyerror(char * message)
{ fprintf(listing,"Syntax error at line %d: %s\n",lineno,message);
  fprintf(listing,"Current token: ");
  printToken(yychar,tokenString);
  Error = TRUE;
  return 0;
}

/* yylex calls getToken to make Yacc/Bison output
 * compatible with ealier versions of the TINY scanner
 */
static int yylex(void)
{ return getToken(); }

TreeNode * parse(void)
{ yyparse();
  return savedTree;
}

