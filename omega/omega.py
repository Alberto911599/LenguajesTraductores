import ply.lex as lex
import ply.yacc as yacc
import sys

tokens = [
	#Used for variables and subroutines names
	"identifier",

	# Data Types
	"int_number",			#Hacen referencia al tipo de número
	"double_number",
	"int",					#Hacen referencia al tipo de dato
	"double",
	"string",

	# Grammar operators
	"comma",
	"semicolon",
	"open_parenthesis",
	"close_parenthesis",
	"open_brace",
	"close_brace",
	"open_bracket",
	"close_bracket",

	#Logic Operators
	"is_equal",
	"not_equal",
	"less_or_equal",
	"greater_or_equal",
	"less_than",
	"greater_than",
	"and",
	"or",

	# Arithmetic Operators
	"equal",
	"plus",
	"minus",
	"star",
	"slash",

	# Uninary Operators
	"plus_plus",
	"minus_minus",

	# Llamadas a metodos
	"call",
	"main",

	# Condicionales
	"if",
	"elseif",
	"else",

	# Ciclos
	"do",
	"while",
	"for",

	# Entrada y salida de datos
	"cin",
	"cout",
	"in",
	"out",

	# Seperadores de código
	"Begin_Variables_Declaration",
	"End_Variables_Declaration",
	"Begin_Subroutines_Declaration",
	"End_Subroutines_Declaration"
]

# Diccionario para palabras reservadas
reserved = {
	"int" 									: "int",
	"double" 								: "double",
	"call" 									: "call",
	"main"									: "main",
	"if" 									: "if",
	"elseif" 								: "elseif",
	"else" 									: "else",
	"do" 									: "do",
	"while" 								: "while",
	"for" 									: "for",
	"cin"									: "cin",
	"cout"									: "cout",
	"and"									: "and",
	"or"									: "or",
	"Begin_Variables_Declaration" 			: "Begin_Variables_Declaration",
	"End_Variables_Declaration" 			: "End_Variables_Declaration",
	"Begin_Subroutines_Declaration" 		: "Begin_Subroutines_Declaration",
	"End_Subroutines_Declaration"			: "End_Subroutines_Declaration"
}


t_comma = r"\,"
t_semicolon = r"\;"
t_open_parenthesis = r"\("
t_close_parenthesis = r"\)"
t_open_bracket = r"\["
t_close_bracket = r"\]"
t_equal = r"\="
t_greater_or_equal = r"\>\="
t_less_or_equal = r"\<\="
t_is_equal = r"\=\="
t_not_equal = r"\!\="
t_greater_than = r"\>"
t_less_than = r"\<"
t_plus = r"\+"
t_minus = r"\-"
t_star = r"\*"
t_slash = r"\/"
t_plus_plus = r"\+\+"
t_minus_minus = r"\-\-"
t_open_brace = r"\{"
t_close_brace = r"\}"
t_in = r"\>\>"
t_out = r"\<\<"
t_and = r"\&\&"
t_or = r"\|\|"
t_string = r'\"[a-zA-Z0-9 \.\?\:\t\r\n\f()\[\]\&\!\@\#\$\%\^\-\=\+\/\,]*\"'

t_ignore_COMMENT = r'\#.*'
t_ignore = " \t\n"

def t_double_number(t):
    r"\d+\.\d+"
    t.value = float(t.value)
    return t

def t_int_number(t):
	r"\d+"
	t.value = int(t.value)
	return t

def t_identifier(t):
    r'[a-zA-Z_][a-zA-Z_0-9]*'
    if t.value in reserved:
        t.type = reserved[ t.value ]
    else:  
        t.type = 'identifier'
    return t


# Error handling rule
def t_error(t):
    print("Illegal character '%s'" % t.value[0])
    t.lexer.skip(1)

lexer = lex.lex()

def p_ProgramFlow(programFlow):
	'''
		ProgramFlow : VariablesDeclaration SubroutinesDeclaration Main
	'''
	print('\nCorrecto!!')

def p_AssignmentStatement(variable):
	'''
		AssignmentStatement : identifier equal ArithmeticExpression
							| MatrixElement equal ArithmeticExpression
	'''

def p_VariablesDeclaration(variablesDeclaration):
	'''
		VariablesDeclaration : Begin_Variables_Declaration SingleTypeVariableDeclaration End_Variables_Declaration
	'''

def p_SingleTypeVariableDeclaration(singleTypeVariableDeclaration):
	'''
		SingleTypeVariableDeclaration : SimpleTypes SequenceOfIdentifiers semicolon SingleTypeVariableDeclaration
									  |
	'''

def p_SimpleTypes(types):
	'''
		SimpleTypes : int
					| double 
	'''

def p_SequenceOfIdentifiers(sequenceOfIdentifiers):
	'''
		SequenceOfIdentifiers 		:	identifier 
									|	identifier comma SequenceOfIdentifiers
									| 	MatrixElement 
									| 	MatrixElement comma SequenceOfIdentifiers
									|	AssignmentStatement
									|	AssignmentStatement comma SequenceOfIdentifiers
	'''

def p_MatrixElement(mat):
	'''
		MatrixElement : identifier open_bracket Number close_bracket
					  | identifier open_bracket Number close_bracket open_bracket Number close_bracket
	'''

def p_ArithmeticExpression(arithmeticExpression):
	'''
		ArithmeticExpression : Number
				   | UnaryOperation
				   | MatrixElement
				   | open_parenthesis ArithmeticExpression close_parenthesis
				   | open_parenthesis ArithmeticExpression close_parenthesis plus ArithmeticExpression
				   | open_parenthesis ArithmeticExpression close_parenthesis minus ArithmeticExpression
				   | open_parenthesis ArithmeticExpression close_parenthesis star ArithmeticExpression
				   | open_parenthesis ArithmeticExpression close_parenthesis slash ArithmeticExpression
				   | Number plus ArithmeticExpression
				   | Number minus ArithmeticExpression
				   | Number star ArithmeticExpression
				   | Number slash ArithmeticExpression
				   | MatrixElement plus ArithmeticExpression
				   | MatrixElement minus ArithmeticExpression
				   | MatrixElement star ArithmeticExpression
				   | MatrixElement slash ArithmeticExpression
				   | UnaryOperation plus ArithmeticExpression
				   | UnaryOperation minus ArithmeticExpression
				   | UnaryOperation star ArithmeticExpression
				   | UnaryOperation slash ArithmeticExpression
	'''

def p_UnaryOperation(p):
	'''
		UnaryOperation : plus_plus identifier
					   | minus_minus identifier
					   | identifier plus_plus
					   | identifier minus_minus
	'''

def p_Number(number):
	'''
		Number : int_number
			   | double_number
			   | identifier
	'''

def p_BooleanExpression(booleanExpression):
	'''
		BooleanExpression : ArithmeticExpression
						  | ArithmeticExpression LogicOperator ArithmeticExpression
						  | BooleanExpression LogicOperator BooleanExpression
	'''

def p_LogicOperator(logicOperator):
	'''
		LogicOperator : is_equal
					  |	not_equal
					  |	less_or_equal
					  |	greater_or_equal
					  |	less_than
					  |	greater_than
					  | and
					  | or
	'''

def p_ifCondition(p):
	'''
		ifCondition : if open_parenthesis BooleanExpression close_parenthesis open_brace Routine close_brace elseIfCondition
	'''

def p_elseIfCondition(p):
	'''
		elseIfCondition : elseif open_parenthesis BooleanExpression close_parenthesis open_brace Routine close_brace elseIfCondition
						| elseCondition
	'''

def p_elseCondition(p):
	'''
		elseCondition : else open_brace Routine close_brace
					  |
	'''

def p_whileLoop(p):
	'''
		whileLoop : while open_parenthesis BooleanExpression close_parenthesis open_brace Routine close_brace
	'''

def p_doWhileLoop(p):
	'''
		doWhileLoop : do open_brace Routine close_brace while open_parenthesis BooleanExpression close_parenthesis semicolon
	'''

def p_forLoop(p):
	'''
		forLoop : for open_parenthesis SequenceOfIdentifiers semicolon BooleanExpression semicolon UpdateVariables close_parenthesis open_brace Routine close_brace
	'''
def p_UpdateVariables(p):
 	'''
	UpdateVariables : AssignmentStatement
					| AssignmentStatement comma UpdateVariables
					| UnaryOperation
					| UnaryOperation comma UpdateVariables
 	'''

def p_Routine(p):
	'''
		Routine : AssignmentStatement semicolon Routine
				| UnaryOperation semicolon Routine
				| ifCondition Routine
				| whileLoop Routine
				| doWhileLoop Routine
				| forLoop Routine
				| call identifier semicolon Routine
				| InOut Routine
				|
	'''

def p_SubroutinesDeclaration(p):
	'''
		SubroutinesDeclaration : Begin_Subroutines_Declaration SingleSubroutine End_Subroutines_Declaration
	'''

def p_SingleSubroutine(p):
	'''
		SingleSubroutine : identifier open_brace Routine close_brace SingleSubroutine
						 |
	'''

def p_InOut(inout):
	'''
		InOut : cin in identifier RecursiveIn semicolon
			  | cin in MatrixElement RecursiveIn semicolon
			  | cout out ArithmeticExpression RecursiveOut semicolon
			  | cout out string RecursiveOut semicolon
	'''

def p_RecursiveIn(i):
	'''
		RecursiveIn : in identifier RecursiveIn
					| in MatrixElement RecursiveIn
					|
	'''

def p_RecursiveOut(o):
	'''
		RecursiveOut : out ArithmeticExpression RecursiveOut
					 | out string RecursiveOut
					 |
	'''

def p_Main(m):
	'''
		Main : main open_brace Routine close_brace
	'''

# Error rule for syntax errors
def p_error(p): 
    print("Syntax error in input!");


#Build Parser
parser = yacc.yacc()

if (len(sys.argv) > 1):
    programName = sys.argv[1]
    programFile = open(programName, "r")
    program = programFile.read().replace('\\n', '\n')
    parser.parse(program)
    programFile.close()
else:
    raise Exception('''Espesifique un archivo''')