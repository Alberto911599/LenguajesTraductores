# TODO : Types Validations
# TODO : Jerarquias para operadores lógicos
# TODO : Matrices


import ply.lex as lex
import ply.yacc as yacc
import sys
import json

currentIndex = 50
symbolsTable = {}

quadruplets = []
quadrupletIndex = 0

operandsStack = []
operatorsStack = []
ifStack = []
jumpStack = []
typesStack = []
avail = []

for i in range(50):
	avail.append('T' + str(i))

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

def isTemp(operand):
	if type(operand) is not str:
		return False
	return operand[0] == 'T'

def addVariable(varName, varType):
	global currentIndex
	initialValue = 0.0 if varType == 'double' else 0
	symbolsTable[varName] = {
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
	# print(operandsStack)
	# print(operatorsStack)
	# print(quadruplets)
	# print(json.dumps(symbolsTable, indent=4))
	for q in range(len(quadruplets)):
		print(str(q) + '\t' + quadruplets[q])
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
		dimensions : open_bracket int_number close_bracket dimensions
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
	ArithmeticExpression  : ArithmeticExpression plus action_insert_operator term action_generate_quadruplet
						  | ArithmeticExpression minus action_insert_operator term action_generate_quadruplet
						  | term
	'''
     
def p_term_times_div(p):
	'''
    term		: factor
				| term star action_insert_operator factor action_generate_quadruplet
				| term slash action_insert_operator factor action_generate_quadruplet

	'''
def p_factor(p):
	'''
    factor : Number
		   | variable action_insert_variable_as_operand
		   | UnaryOperation
		   | open_parenthesis ArithmeticExpression close_parenthesis
	'''

def p_BooleanExpression(p):
	'''
		BooleanExpression : true 
						  | false 
						  | ArithmeticExpression
						  | ArithmeticExpression LogicOperator action_insert_operator BooleanExpression action_generate_quadruplet
	'''

def p_AssignmentStatement(p):
	'''
		AssignmentStatement : variable action_insert_variable_as_operand equal ArithmeticExpression action_assignation
							| UnaryOperation
	'''

def p_UnaryOperation(p):
	'''
		UnaryOperation : plus_plus action_insert_operator variable action_insert_variable_as_operand action_generate_unary_operation_quadruplet
					   | minus_minus action_insert_operator variable action_insert_variable_as_operand action_generate_unary_operation_quadruplet
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
	p[0] = p[1]

################### Saltos Condicionales ###################

def p_ifCondition(p):
	'''
		ifCondition : if open_parenthesis BooleanExpression action_pushToIfStack action_pushToJumpStack action_generateEmptyGotoFalse_quadruplet close_parenthesis open_brace Routine close_brace elseIfCondition
	'''

def p_elseIfCondition(p):
	'''
		elseIfCondition : action_fillPreviousGotoFalse action_pushToJumpStack action_generateUnconditionalGoto elseif open_parenthesis BooleanExpression action_pushToJumpStack action_generateEmptyGotoFalse_quadruplet close_parenthesis open_brace Routine close_brace elseIfCondition
						| elseCondition
	'''

def p_elseCondition(p):
	'''
		elseCondition : action_fillPreviousGotoFalse action_pushToJumpStack action_generateUnconditionalGoto  else open_brace Routine close_brace action_fillUnconditionalGotosWithEnd
					  | action_fillUnconditionalGotosWithEnd
	'''

################### Ciclos ###################

def p_whileLoop(p):
	'''
		whileLoop : while open_parenthesis action_pushToJumpStack BooleanExpression  action_pushToJumpStack  action_generateEmptyGotoFalse_quadruplet close_parenthesis open_brace Routine close_brace action_fillPreviousGotoFalse action_generateGotoBooleanExpression
	'''

def p_doWhileLoop(p):
	'''
		doWhileLoop : do open_brace action_pushToJumpStack Routine close_brace while open_parenthesis BooleanExpression action_pushToJumpStack action_generateEmptyGotoFalse_quadruplet action_fillPreviousGotoFalse  action_generateUnconditionalGoto action_fillWithStartUpdateDirection close_parenthesis semicolon
	'''

def p_forLoop(p):
	'''
		forLoop : for open_parenthesis initializeForLoop semicolon action_pushToJumpStack BooleanExpression semicolon action_pushToJumpStack action_generateEmptyGotoFalse_quadruplet action_pushToJumpStack action_generateUnconditionalGoto action_pushToJumpStack UpdateVariables action_generateUnconditionalGoto action_fillWithTestingDirection close_parenthesis open_brace action_fillWithStartDirectionForRoutine Routine action_generateUnconditionalGoto action_fillWithStartUpdateDirection close_brace action_fillWithEndLoop
	'''
def p_UpdateVariables(p):
 	'''
	UpdateVariables : AssignmentStatement
					| AssignmentStatement comma UpdateVariables
 	'''

def p_initializeForLoop(p):
	'''
		initializeForLoop : UpdateVariables
						  |
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

def p_action_insert_variable_as_operand(p):
	"action_insert_variable_as_operand :"
	if p[-1] not in symbolsTable:
		raise Exception(f'The variable {p[-1]} was not declared in this scope')
	address = symbolsTable[p[-1]]['address']
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

def p_action_generate_aquadruplet(p):
	"action_generate_quadruplet :"
	global quadrupletIndex
	global avail
	operator = operatorsStack.pop()
	operand2 = operandsStack.pop()
	operand1 = operandsStack.pop()
	temp = avail.pop(0)
	operandsStack.append(temp)
	quadruplets.append(str(operator) + '\t' + str(operand1) + '\t' + str(operand2) + '\t' + str(temp))
	quadrupletIndex += 1
	if (isTemp(operand2)):
		avail = [operand2] + avail
	if (isTemp(operand1)):
		avail = [operand1] + avail

def p_action_assignation(p):
	"action_assignation :"
	global quadrupletIndex
	global avail
	operand2 = operandsStack.pop()
	operand1 = operandsStack.pop()
	quadruplets.append('=' + '\t' + str(operand2) + '\t' + '\t' + '\t' + str(operand1))
	quadrupletIndex += 1
	#operand1 is always a variable that takes the value of operand2
	if isTemp(operand2):
		avail = [operand2] + avail

def p_action_generate_unary_operation_quadruplet(p):
	"action_generate_unary_operation_quadruplet :"
	global quadrupletIndex
	operand = operandsStack.pop()
	operator = operatorsStack.pop()
	quadruplets.append(str(operator)[0] + '\t' + str(operand) + '\t' + str(1) + '\t' + str(operand))
	quadrupletIndex += 1

def p_action_pushToJumpStack(p):
	"action_pushToJumpStack :"
	global quadrupletIndex
	global jumpStack
	jumpStack.append(quadrupletIndex)

def p_action_generateEmptyGotoFalse_quadruplet(p):
	"action_generateEmptyGotoFalse_quadruplet :"
	global quadrupletIndex
	global jumpStack
	global quadruplets
	global avail
	operand = operandsStack.pop()
	quadruplets.append("gotoF" + '\t' + str(operand) + '\t')
	quadrupletIndex += 1
	if isTemp(operand):
		avail = [operand] + avail

def p_action_generateUnconditionalGoto(p):
	"action_generateUnconditionalGoto :"
	global quadrupletIndex
	global quadruplets
	quadruplets.append("goto" + '\t')
	quadrupletIndex += 1

def p_action_fillPreviousGotoFalse(p):
	"action_fillPreviousGotoFalse :"
	global quadrupletIndex
	global quadruplets
	global jumpStack
	fillAddr = jumpStack.pop()
	quadruplets[fillAddr] += str(quadrupletIndex+1)

def p_action_fillWithStartDirectionForRoutine(p):
	"action_fillWithStartDirectionForRoutine : "
	global quadrupletIndex
	fillAddr = jumpStack.pop(-2)
	quadruplets[fillAddr] += str(quadrupletIndex)

def p_action_fillWithTestingDirection(p):
	"action_fillWithTestingDirection : "
	global quadrupletIndex
	fillAddr = jumpStack.pop(-4)
	quadruplets[-1] += str(fillAddr)

def p_action_fillWithStartUpdateDirection(p):
	"action_fillWithStartUpdateDirection : "
	fillAddr = jumpStack.pop()
	quadruplets[-1] += str(fillAddr)

def p_action_fillUnconditionalGotosWithEnd(p):
	"action_fillUnconditionalGotosWithEnd :"
	global quadrupletIndex
	limit = ifStack.pop()
	while len(jumpStack) > 0 and limit <= jumpStack[-1]:
		quadruplets[jumpStack[-1]] += str(quadrupletIndex)
		jumpStack.pop()

def p_action_fillWithEndLoop(p):
	"action_fillWithEndLoop : "
	global quadrupletIndex
	global quadruplets
	global jumpStack
	fillAddr = jumpStack.pop()
	quadruplets[fillAddr] += str(quadrupletIndex)

def p_action_pushToIfStack(p):
	"action_pushToIfStack :"
	global quadrupletIndex
	global ifStack
	ifStack.append(quadrupletIndex)


def p_action_generateGotoBooleanExpression(p):
	"action_generateGotoBooleanExpression :"
	global quadrupletIndex
	booleanEvaluation = jumpStack.pop()
	quadruplets.append("goto" + '\t' + str(booleanEvaluation))
	quadrupletIndex += 1

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