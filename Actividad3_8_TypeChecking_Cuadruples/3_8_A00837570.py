# Fernando Espidio Santamaria - A00837570
# Luis Santiago Sauma Peñaloza - A00836418

# Actividad 3.8 - Type checking y quadruples

import ply.lex as lex
import ply.yacc as yacc

# Estructuras implementadas
errores = [] # lista de mensajes de error de lexico, sintaxis o semantica
tablaSimbolos = {} # la tabla de simbolos con el nombre y la informacion de cada id declarado
pilaOperandos = [] # lista de tuplas (valor, tipo) para operandos de expresiones
cuadruplos = [] # la tabla vista como en los ejemplos de la clase con columnas num, op, argL, argR, res, tipo
pilaSaltos = [] # pila de saltos pendientes, ósea los numeros de quad por rellenar
tempList = [] # lista temporal de ids durante una declaracion

tempCounter = 0 # contador de variables temporales
quadCounter = 0 # contador de quadruples 1-index


'''
Función para crear el cubo semántico, se implemento de esta manera ya que 
es mas fácil de leer y modificarlo que a dejarlo hardcodeado como en el ejemplo de la clase.

Se indican las reglas de cada operador y se llena el cubo con los resultados esperados, si una combinación no esta en el cubo se considera un error
tal cual se mostraba en los ejemplos.
'''
def buildCubo():
    # se define el cubo como un diccionario con claves y valores el tipo resultante
    c = {}

    # Lista de tipos para iterar
    tipos = ['int', 'float']

    # Operaciones aritmeticas que conservan int o se vuelven float 
    for op in ['+', '-', '*']:
        c[('int', 'int', op)] = 'int'
        c[('int', 'float', op)] = 'float'
        c[('float', 'int', op)] = 'float'
        c[('float', 'float', op)] = 'float'

    # Division siempre produce float
    for li in tipos:
        for ld in tipos:
            c[(li, ld, '/')] = 'float'

    # Operadores relacionales producen bool
    for op in ['>', '<', '>=', '<=', '!=', '==']:
        for li in tipos:
            for ld in tipos:
                c[(li, ld, op)] = 'bool'

    # Asignacion: (tipo_variable, tipo_expresion, '=') -> tipo_variable
    # int = int  -> int     (ok)
    # float = float -> float (ok)
    # float = int -> float  (ok, promocion sin perdida)
    # int = float -> error  (perdida de precision, no esta en el cubo)

    # Se llenan las reglas de asignacion en el cubo segun los resultados esperados,
    # que serían, de un int a un int, de un float a un float, y de un int a un float, pero no al reves
    # porque eso causaría perdida de precision, por lo tanto no se incluye en el cubo y se considera un error
    c[('int', 'int', '=')] = 'int'
    c[('float', 'float', '=')] = 'float'
    c[('float', 'int', '=')] = 'float'

    # se regresa el cubo completo
    return c

# se construye el cubo semantico al inicio del programa
cubo = buildCubo()


# Lo siguiente que se implementa son las funciones de acciones semanticas
# Estas funciones implementan la logica de cada punto verificando errores y generando los cuadruplos correspondientes

'''
Función para declarar un temporal, se verifica que el id no exista ni en la tabla de simbolos ni en la lista temporal, 
si existe se agrega un error a la lista de errores, si no existe se agrega a la lista temporal para ser declarada posteriormente 
con su tipo cuando se conozca.
'''
def declarar_temp(nombre):
    if nombre in tablaSimbolos or nombre in tempList:
        errores.append("Error semantico: la variable '%s' ya fue declarada" % nombre)
    else:
        tempList.append(nombre)

'''
Función para usar un id, se verifica que el id exista en la tabla de simbolos, 
si no existe se agrega un error a la lista de errores y se agrega un operando con tipo error para evitar muchos errores
'''
def usar_id(nombre):
    # Al ver un ID dentro de una expresion: verifica que exista y empuja
    # su (nombre, tipo) a la pila de operandos.
    if nombre not in tablaSimbolos:
        errores.append("Error semantico: la variable '%s' no ha sido declarada" % nombre)
        pilaOperandos.append((nombre, 'error'))
    else:
        pilaOperandos.append((nombre, tablaSimbolos[nombre]['tipo']))

'''
Función para generar una operación, se extraen los operandos del stack, se consulta el cubo con los tipos y el operador,
si el resultado es None se agrega un error de type mismatch, si no se genera un nuevo temporal para el resultado,
 se genera el cuadruplo correspondiente y se agrega el resultado con su tipo
'''
def gen_operacion(op):
    # se indica que las variables tempCounter y quadCounter hacen referencia a las globales
    global tempCounter, quadCounter

    # se extraen los operandos del stack, primero el derecho y luego el izquierdo porque es una pila
    arg_r, tipo_r = pilaOperandos.pop()
    arg_l, tipo_l = pilaOperandos.pop()

    # se consulta el cubo con los tipos y el operador, si el resultado es None se agrega un error de type mismatch,
    tipo_res = cubo.get((tipo_l, tipo_r, op))

    if tipo_res is None:
        errores.append("Error semantico: type mismatch en la operacion '%s' entre %s y %s"
                        % (op, tipo_l, tipo_r))
        pilaOperandos.append(('error', 'error'))
        return

    # si no hay error se genera un nuevo temporal para el resultado, 
    # se genera el cuadruplo correspondiente y se agrega el resultado con su tipo
    tempCounter += 1
    temp = 't' + str(tempCounter)
    quadCounter += 1
    cuadruplos.append([quadCounter, op, arg_l, arg_r, temp, tipo_res])
    pilaOperandos.append((temp, tipo_res))

'''
Función para generar una asignación, se extrae el operando del stack, se verifica que la variable exista en la tabla de simbolos,
se consulta el cubo con el tipo de la variable, el tipo del operando y el operador '=', si el resultado es None se agrega un error de type mismatch,
'''
def gen_asignacion(nombre_var):
    # se indica que la variable quadCounter hace referencia a la global
    global quadCounter

    # se extrae el operando del stack, si no hay operandos se regresa sin hacer nada para evitar errores 
    if not pilaOperandos:
        return
    arg, tipo = pilaOperandos.pop()

    # se verifica que la variable exista en la tabla de simbolos, si no existe se agrega un error a la lista de errores y se termina la función
    if nombre_var not in tablaSimbolos:
        errores.append("Error semantico: la variable '%s' no ha sido declarada" % nombre_var)
        return

    # si es que si existe, se consulta el cubo con el tipo de la variable, el tipo del operando y el operador '=', 
    tipo_var = tablaSimbolos[nombre_var]['tipo']
    tipo_res = cubo.get((tipo_var, tipo, '='))

    # si el resultado es None se agrega un error de type mismatch, si no se genera el cuadruplo correspondiente
    if tipo_res is None:
        errores.append("Error semantico: type mismatch en la asignacion a '%s' (%s = %s)"
                       % (nombre_var, tipo_var, tipo))
        return
    
    # si no hay error se genera el cuadruplo correspondiente y se agrega el resultado con su tipo
    quadCounter += 1
    cuadruplos.append([quadCounter, '=', arg, '', nombre_var, tipo_var])

'''
Función para generar el gotof del if, se extrae el operando del stack, se verifica que sea de tipo bool, 
si no se agrega un error a la lista de errores 
'''
def gen_gotof():
    # se indica que la variable quadCounter hace referencia a la global
    global quadCounter

    # nuevamente se extrae el operando del stack, si no hay operandos se regresa sin hacer nada para evitar errores 
    if not pilaOperandos:
        return
    arg, tipo = pilaOperandos.pop()

    # se verifica que sea de tipo bool, si no se agrega un error a la lista de errores y se termina la función
    if tipo != 'bool':
        errores.append("Error semantico: la expresion de la condicion debe ser bool, se obtuvo %s" % tipo)
        return
    
    # si no hay error se genera el cuadruplo correspondiente con un resultado pendiente '?', y se 
    # agrega el numero del quad para rellenar el salto posteriormente
    quadCounter += 1
    cuadruplos.append([quadCounter, 'gotof', arg, '', '?', ''])
    pilaSaltos.append(quadCounter)

'''
Función para generar el goto del else, se genera el cuadruplo correspondiente, se rellena el gotof pendiente 
hacia el inicio del else, y se agrega el nuevo salto a la pila de saltos para ser rellenado posteriormente al finalizar el else
'''
def gen_goto_else():
    # se indica que la variable quadCounter hace referencia a la
    global quadCounter

    # se genera el cuadruplo correspondiente con un resultado pendiente '?', y se agrega el 
    # numero del quad para rellenar el salto posteriormente
    quadCounter += 1
    cuadruplos.append([quadCounter, 'goto', '', '', '?', ''])

    # se rellena el gotof pendiente hacia el inicio del else, y se agrega el nuevo salto a la pila 
    # de saltos para ser rellenado posteriormente al finalizar el else
    pendiente = pilaSaltos.pop()
    cuadruplos[pendiente - 1][4] = quadCounter + 1
    pilaSaltos.append(quadCounter)

'''
Función para backpatch al finalizar el bloque del if o del else, se rellena el salto pendiente del tope hacia el siguiente quad, 
si la pila de saltos esta vacia se regresa sin hacer nada para evitar errores 
'''
def backpatch_fin():
    # se checa que la pila de saltos no este vacia, si esta vacia se regresa sin hacer nada para evitar errores
    if not pilaSaltos:
        return
    
    # se rellena el salto pendiente del tope hacia el siguiente quad, que es el inicio del bloque siguiente al if o al else
    pendiente = pilaSaltos.pop()
    cuadruplos[pendiente - 1][4] = quadCounter + 1


# Implementación reducida de Lexer solo para las definiciones de 
# statement ::= asigancion | condicional, pero se necesitan los tokens para para poder generar los cuadruplos correspondientes
# se empieza desde program porque se necesitan los tokens de var, main y end para la gramatica, aunque no se usen en los cuadruplos, 
# pero si se necesitan para la sintaxis del lenguaje, y se incluyen los tokens de tipos para poder declarar variables y generar errores de tipo en las asignaciones

# lista de palabras reservadas
reserved = {
    'program': 'PROGRAM',
    'var': 'VAR',
    'main': 'MAIN',
    'end': 'END',
    'if': 'IF',
    'else': 'ELSE',
    'int': 'INT',
    'float': 'FLOAT',
    'string': 'STRING',
}

# lista de tokens, se incluyen los tokens de operadores y delimitadores necesarios para la gramatica y la generación de cuadruplos, 
# se suma la lista de palabras reservadas para que el lexer las reconozca como tokens y no como IDs
tokens = [
    'ID', 'CTE_INT', 'CTE_FLOAT',
    'ASSIGN', 'PLUS', 'MINUS', 'MULT', 'DIV',
    'GT', 'LT', 'GE', 'LE', 'NEQ', 'EQ',
    'LPAREN', 'RPAREN', 'LBRACE', 'RBRACE',
    'SEMICOL', 'COLON', 'COMMA',
] + list(reserved.values())

# definicion de tokens con expresiones regulares que no requieren procesamiento adicional
t_ASSIGN = r'='
t_PLUS = r'\+'
t_MINUS = r'-'
t_MULT = r'\*'
t_DIV = r'/'
t_NEQ = r'!='
t_GE = r'>='
t_LE = r'<='
t_EQ = r'=='
t_GT = r'>'
t_LT = r'<'
t_LPAREN = r'\('
t_RPAREN = r'\)'
t_LBRACE = r'\{'
t_RBRACE = r'\}'
t_SEMICOL = r';'
t_COLON = r':'
t_COMMA = r','
t_ignore = ' \t\r'

# definición de token tipo float según la expresión regular
def t_CTE_FLOAT(t):
    r"[0-9]+\.[0-9]+\b"
    t.value = float(t.value)
    return t

# definición de token tipo int según la expresión regular
def t_CTE_INT(t):
    r"[0-9]+\b"
    t.value = int(t.value)
    return t

# definición de token tipo ID según la expresión regular, se verifica si el valor es una palabra reservada para asignarle el tipo correcto
def t_ID(t):
    r"[a-zA-Z]\w*\b"
    t.type = reserved.get(t.value, 'ID')
    return t

# definición de token para nuevas líneas, se incrementa el número de línea en el lexer para poder reportar errores con la línea correcta
def t_newline(t):
    r'\n+'
    t.lexer.lineno += len(t.value)

# definición de token para comentarios, se ignoran los comentarios para que no afecten el análisis léxico
def t_error(t):
    errores.append("Error lexico: caracter ilegal '%s' en la linea %d"
                   % (t.value[0], t.lineno))
    t.lexer.skip(1)


# implementación reducida de Parser, se implementa la gramatica necesaria para las asignaciones y condicionales considerando 
# la semantica de cada una para generar los cuadruplos correspondientes, se empieza desde programa porque se necesitan los tokens de var, main y end para la gramatica, 
# aunque no se usen en los cuadruplos,
start = 'programa'

# se define la regla de programa que es el punto de entrada
def p_programa(p):
    "programa : PROGRAM ID SEMICOL opt_vars MAIN body END"

# se definen las reglas para las variables, se permite que no haya variables con la regla opt_vars,
#  y si hay variables se definen con la regla vars, que a su vez se define como listas de lista, 
# donde cada lista es una lista de ids seguidos de un tipo, al procesar cada lista se agrega cada id a la tabla de simbolos con su tipo correspondiente

# función opcional para variables, permite que no haya variables en el programa
def p_opt_vars(p):
    """opt_vars : vars 
                | empty"""

# función para variables, se define como listas de lista, donde cada lista es una lista de ids seguidos de un tipo
def p_vars(p):
    "vars : VAR listas"

# función para listas de variables, se define como una lista de lista de variables, donde cada lista es 
# una lista de ids seguidos de un tipo, se permite que haya una sola lista o varias listas seguidas como indican los diagramas
def p_listas(p):
    """listas : listas lista 
              | lista"""

# función para lista de variables, se define como una lista de ids seguidos de un tipo, 
# al procesar cada lista se agrega cada id a la tabla de simbolos con su tipo correspondiente
def p_lista(p):
    "lista : lista_ids COLON type SEMICOL"
    tipo = p[3]
    for nombre in tempList:
        tablaSimbolos[nombre] = {'tipo': tipo, 'scope': 'global'}
    del tempList[:]

# función para lista de ids, se define como una lista de ids separados por comas, al procesar cada id se llama a la función declarar_temp 
# para verificar que no haya errores de redeclaracion y agregarlo a la lista temporal
def p_lista_ids_one(p):
    "lista_ids : ID"
    declarar_temp(p[1])

# función para lista de ids, se define como una lista de ids separados por comas, al procesar cada 
# id se llama a la función declarar_temp
def p_lista_ids_many(p):
    "lista_ids : lista_ids COMMA ID"
    declarar_temp(p[3])

# función para los types permitidos en el lenguaje, se regresa el type para ser asignado a las variables en la tabla de simbolos
def p_type(p):
    """type : INT 
            | FLOAT 
            | STRING"""
    p[0] = p[1]

# función para el body de código
def p_body(p):
    "body : LBRACE statements RBRACE"

# función para statements, se define como una lista de statements o vacía, 
# donde cada statement puede ser una asignación o una condición, se permite que no haya statements con la regla empty
def p_statements(p):
    """statements : statements statement 
                  | empty"""

# función para statement pero cuando solo hay un statement, pero sigue la misma lógica de que cada statement puede ser una asignación o una condición
def p_statement(p):
    """statement : asignacion 
                 | condicion"""

# función para asignación que simplemente sigue la regla de asignación, 
# pero se llama a la función gen_asignacion para generar el cuadruplo correspondiente y verificar errores de tipo
def p_asignacion(p):
    "asignacion : ID ASSIGN expresion SEMICOL"
    gen_asignacion(p[1])

# función para condición, se sigue la regla de if con su expresión entre paréntesis, el bloque del if y el bloque del else opcional,
def p_condicion(p):
    "condicion : IF LPAREN expresion RPAREN m_gotof body resto_cond"

# función para generar el gotof del if, se llama a la función gen_gotof 
# para generar el cuadruplo correspondiente y verificar errores de tipo
def p_m_gotof(p):
    "m_gotof : empty"
    gen_gotof()

# función para el resto de la condición, se define como un bloque opcional de else, 
# si no hay else se llama a la función backpatch_fin para rellenar el salto pendiente del if,
def p_resto_cond_noelse(p):
    "resto_cond : SEMICOL"
    backpatch_fin()

# función para el resto de la condición, se define como un bloque opcional de else,
# si hay else se llama a la función gen_goto_else para generar el cuadruplo correspondiente al
# goto del else, rellenar el salto pendiente del if hacia el inicio del else, y agregar el nuevo 
# salto pendiente del else para ser rellenado posteriormente al finalizar el else
def p_resto_cond_else(p):
    "resto_cond : ELSE m_goto body SEMICOL"
    backpatch_fin()

# función para generar el goto del else, se llama a la función gen_goto_else para generar el cuadruplo correspondiente al
def p_m_goto(p):
    "m_goto : empty"
    gen_goto_else()

# función de la expresión 
def p_expresion_simple(p):
    "expresion : exp"

# función para exp
def p_exp(p):
    """expresion : exp GT exp 
                 | exp LT exp 
                 | exp GE exp 
                 | exp LE exp 
                 | exp NEQ exp 
                 | exp EQ exp"""
    gen_operacion(p[2])

# función para exp simple que es solo un termino
def p_exp_simple(p):
    "exp : termino"

# función para exp con operador, se llama a la función gen_operacion para generar el cuadruplo correspondiente y verificar errores de tipo
def p_exp_op(p):
    """exp : exp PLUS termino
           | exp MINUS termino"""
    gen_operacion(p[2])

# función para termino simple que es solo un factor
def p_termino_simple(p):
    "termino : factor"

# función para termino con operador, se llama a la función gen_operacion para generar el cuadruplo correspondiente y verificar errores de tipo
def p_termino_op(p):
    """termino : termino MULT factor
               | termino DIV factor"""
    gen_operacion(p[2])

# función para factor entre paréntesis, simplemente sigue la regla de factor pero no se genera ningún cuadruplo ni se verifica
#  ningún error porque el resultado de la expresión entre paréntesis ya se encuentra en la pila de operandos
def p_factor_paren(p):
    "factor : LPAREN expresion RPAREN"

# función para factor que es un ID, se llama a la función usar_id para verificar que el id exista 
# en la tabla de simbolos y agregarlo a la pila de operandos con su tipo
def p_factor_id(p):
    "factor : ID"
    usar_id(p[1])

# función para factor que es una constante int, se agrega a la pila de operandos con su tipo
def p_factor_cte_int(p):
    "factor : CTE_INT"
    pilaOperandos.append((p[1], 'int'))

# función para factor que es una constante float, se agrega a la pila de operandos con su tipo
def p_factor_cte_float(p):
    "factor : CTE_FLOAT"
    pilaOperandos.append((p[1], 'float'))

# función empty para permitir reglas opcionales, simplemente no hace nada
def p_empty(p):
    "empty :"
    pass

# función de error de sintaxis, se agrega un mensaje de error a la lista de errores indicando el token donde ocurrió el error y la línea,
def p_error(p):
    if p:
        errores.append("Error de sintaxis en '%s' en la linea %d" % (p.value, p.lineno))
    else:
        errores.append("Error de sintaxis: fin de archivo inesperado")


#Función para imprimir los resultados, se formatea cada valor para que no se imprima None sino una cadena vacía,

# Función para formatear los valores de los cuadruplos y la tabla de simbolos, si el valor es None se regresa una cadena vacía,
def format(valor):
    if valor == '':
        return ''
    return str(valor)

# Función para imprimir los cuadrupos con el formato visto en los ejemplos de la clase, se formatean 
# los valores para que no se imprima None sino una cadena vacía, y se imprime cada columna con un ancho 
# fijo para que se vea ordenado
def printQuads():
    print("QUADRUPLOS")
    print()
    print("%-5s %-7s %-8s %-8s %-8s %-6s" % ("num", "op", "argL", "argR", "res", "tipo"))
    for q in cuadruplos:
        print("%-5s %-7s %-8s %-8s %-8s %-6s" % (
            format(q[0]), format(q[1]), format(q[2]), format(q[3]), format(q[4]), format(q[5])))

# Función para imprimir la tabla de simbolos también en formato tabla, se imprime cada columna con un ancho fijo para que se vea ordenado
def printSymbols():
    print("TABLA DE SIMBOLOS")
    print()
    print("%-10s %-7s %-7s" % ("nombre", "tipo", "scope"))
    for nombre in tablaSimbolos:
        info = tablaSimbolos[nombre]
        print("%-10s %-7s %-7s" % (nombre, info['tipo'], info['scope']))

# Función para imprimir los errores encontrados, se imprime un encabezado y luego cada error en una nueva línea
def printErrors():
    print("Errores Encontrados: ")
    print()
    for e in errores:
        print(e)

# se construye el lexer y el parser
lexer = lex.lex()
parser = yacc.yacc(debug=False, write_tables=False)

# se lee el archivo de entrada, solamente uno como se indica en la actividad
input = open("input.txt").read()

# se parsea el input, el análisis léxico y sintáctico se realiza durante el parseo, y las acciones semánticas se 
# ejecutan durante el parseo para generar los cuadruplos y llenar la tabla de simbolos, así como para detectar errores
parser.parse(input, lexer=lexer)

# finalmente se imprimen los resultados, si hay errores se imprimen los errores encontrados y la tabla de simbolos, si no hay errores se imprimen los cuadruplos y la tabla de simbolos
if errores:
    printErrors()
    print()
    printSymbols()
else:
    printQuads()
    print()
    printSymbols()