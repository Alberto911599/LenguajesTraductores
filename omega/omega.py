import ply.lex as lex
import ply.yacc as yacc
import sys
import json

currentIndex = 50
variablesTable = {}

quadruplets = []
quadrupletIndex = 1

operandsStack = []
operatorsStack = []
typesStack = []
avail = []

for i in range(50):
	avail.append(str(i))

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

	# Logic Constants
	"true",
	"false",

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
	"out"
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
	"true"									: "true",
	"false"									: "false"
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

precedence = (
	('left', 'is_equal', 'not_equal', 'less_or_equal', 'greater_or_equal', 'less_than', 'greater_than', 'and', 'or'),
	('left', 'plus', 'minus'),
	('left', 'star', 'slash'),
)


def addVariable(varName, varType):
	global currentIndex
	initialValue = 0 if varType == 'int' else 0.0
	variablesTable[varName] = {
		'varName' : varName,
        'varType':  varType,
        'value': initialValue,
        'address': currentIndex
    }
	currentIndex += 1

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

################### Flujo del Programa ###################

def p_ProgramFlow(p):
	'''
		ProgramFlow : VariablesDeclaration SubroutinesDeclaration Main
	'''
	print(operandsStack)
	print(operatorsStack)
	print(quadruplets)
	print(json.dumps(variablesTable, indent=4))
	# for key in variablesTable:
	# 	print(variablesTable[key]['varName'] + "\t" + variablesTable[key]['varType'] + "\t" + str(variablesTable[key]['address']))
	print('\nCorrecto!!')

def p_Main(p):
	'''
		Main : main open_brace Routine close_brace
	'''
################### Definicion y declaracion de variables ###################

def p_Number(p):
	'''
		Number : int_number action_insert_int_operand
			   | double_number action_insert_double_operand
	'''

def p_variable(p):
	'''
		variable : identifier dimensions
	'''
	p[0] = p[1]

def p_dimensions(p):
	'''
		dimensions : open_bracket variable close_bracket dimensions
				   | 
	'''

def p_VariablesDeclaration(p):
	'''	
		VariablesDeclaration : type RecVariableDeclaration semicolon VariablesDeclaration
							 |
	'''
	if(len(p) > 1):
		for name in p[2]:
			addVariable(name, p[1])

def p_RecVariableDeclaration(p):
	'''
		RecVariableDeclaration  :	variable 
								|	variable comma RecVariableDeclaration
								|   variable equal ArithmeticExpression
								|	variable equal ArithmeticExpression comma RecVariableDeclaration
	'''
	if(len(p) == 2 or p[2] == '='):
		p[0] = [p[1]]
	else:
		p[0] = [p[1]] + p[len(p)-1]

def p_type(p):
	'''
		type : int	
			 | double
	'''
	p[0] = p[1]

################### Operaciones y Expresiones ###################


def p_expression_plus_minus(p):
    '''
	ArithmeticExpression  : ArithmeticExpression plus action_insert_operator term action_generate_arith_quadruplet
						  | ArithmeticExpression minus action_insert_operator term action_generate_arith_quadruplet
						  | term
	'''
     
def p_term_times_div(p):
	'''
    term		: factor
				| term star action_insert_operator factor action_generate_arith_quadruplet
				| term slash action_insert_operator factor action_generate_arith_quadruplet

	'''
def p_factor(p):
	'''
    factor : Number
		   | variable action_insert_variable
		   | UnaryOperation
		   | open_parenthesis ArithmeticExpression close_parenthesis
	'''

def p_BooleanExpression(p):
	'''
		BooleanExpression : true
						  | false
						  | ArithmeticExpression
						  | ArithmeticExpression LogicOperator BooleanExpression
	'''

def p_AssignmentStatement(p):
	'''
		AssignmentStatement : variable action_insert_variable equal ArithmeticExpression action_assignation
	'''

def p_UnaryOperation(p):
	'''
		UnaryOperation : plus_plus variable
					   | minus_minus variable
					   | variable plus_plus
					   | variable minus_minus
	'''

def p_LogicOperator(p):
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

################### Saltos Condicionales ###################

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

################### Ciclos ###################

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
		forLoop : for open_parenthesis RecVariableDeclaration semicolon BooleanExpression semicolon UpdateVariables close_parenthesis open_brace Routine close_brace
	'''
def p_UpdateVariables(p):
 	'''
	UpdateVariables : AssignmentStatement
					| AssignmentStatement comma UpdateVariables
					| UnaryOperation
					| UnaryOperation comma UpdateVariables
 	'''

################### Rutinas ###################

def p_SubroutinesDeclaration(p):
	'''
		SubroutinesDeclaration : identifier open_brace Routine close_brace SubroutinesDeclaration
							   |
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

################### Entrada y Salida ###################

def p_InOut(p):
	'''
		InOut : cin in variable RecursiveIn semicolon
			  | cout out ArithmeticExpression RecursiveOut semicolon
			  | cout out string RecursiveOut semicolon
	'''

def p_RecursiveIn(p):
	'''
		RecursiveIn : in variable RecursiveIn
					|
	'''

def p_RecursiveOut(p):
	'''
		RecursiveOut : out ArithmeticExpression RecursiveOut
					 | out string RecursiveOut
					 |
	'''

def p_action_insert_variable(p):
	"action_insert_variable :"
	if p[-1] not in variablesTable:
		raise Exception(f'The variable {p[-1]} was not declared in this scope')
	address = variablesTable[p[-1]]['address']
	operandsStack.append(f'${address}')

def p_action_insert_int_operand(p):
	"action_insert_int_operand :"
	operandsStack.append(p[-1])
	typesStack.append('int')

def p_action_insert_double_operand(p):
	"action_insert_double_operand :"
	operandsStack.append(p[-1])
	typesStack.append('double')

def p_action_insert_operator(p):
	"action_insert_operator : "
	operatorsStack.append(p[-1])

def p_action_generate_arith_quadruplet(p):
	"action_generate_arith_quadruplet :"
	global quadrupletIndex
	global avail
	operator = operatorsStack.pop()
	operand2 = operandsStack.pop()
	operand1 = operandsStack.pop()
	temp = avail.pop(0)
	operandsStack.append(temp)
	quadruplets.append(str(operator) + ' ' + str(operand1) + ' ' + str(operand2) + ' ' + str(temp))
	quadrupletIndex += 1

def p_action_assignation(p):
	"action_assignation :"
	operand2 = operandsStack.pop()
	operand1 = operandsStack.pop()
	quadruplets.append('= ' + str(operand2) + '    ' + str(operand1))


# Error rule for syntax errors
def p_error(p): 
    print("Syntax error in input!")

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