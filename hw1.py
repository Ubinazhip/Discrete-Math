import ply.lex as lex
import re
import itertools
'''
Aslan Ubingazhibov: 

resources I have used to learn: 
https://www.dabeaz.com/ply/ply.html#ply_nn3
series of youtube videos:
https://www.youtube.com/watch?v=Hh49BXmHxX8

'''
def negation(results_cnf):
    '''

    This function just negate the given formulae.

    '''

    number_elements = len(results_cnf) - results_cnf.count('not')
    results_dnf = []
    is_neg = 0
    for i in results_cnf:
        if i == 'not':
            is_neg = 1
            continue
        elif i == 'and':
            results_dnf.append('or')
        elif i == 'or':
            results_dnf.append('and')
        else:
            if is_neg == 1:
                results_dnf.append(i)
                is_neg = 0
            else:    
                results_dnf.append('not')
                results_dnf.append(i)
    return results_dnf            
tokens = (
    'Variable',
    'conjunction',
    'disjunction',
    'implication',
    'negation',
    'LPAREN',
    'RPAREN',
)

t_conjunction = r'\/\\'
t_disjunction = r'\\\/'
t_implication = r'\->'
t_negation = r'\~'
t_LPAREN = r'\('
t_RPAREN = r'\)'


 
def t_newline(t):
    r'\n+'
    t.lexer.lineno += len(t.value)

  
t_ignore = ' \t'
def t_Variable(t):
    r'[a-z]'
    t.type = "Variable"
    return t

	
def t_error(t):
    print("Illegal character '%s'" % t.value[0])
    t.lexer.skip(1)


lexer = lex.lex()

def p_expression(t):
    'expression : LPAREN Variable RPAREN'
    t[0] = ['(', t[2], ")"]

    #print(t[0])

def p_expression_variable(t):
    '''expression : Variable'''
    t[0] = t[1]
def p_expression_variable_neq(t):
    '''expression : negation Variable'''
    t[0] = ["not",t[2]]


def p_expression_paren_neg(t):
    ''' expression : negation LPAREN expression RPAREN '''
    print(t[2])
    print(t[3])
    if t[2] == "(" and t[4] ==")":

        t[0] = ['(', negation(t[3]), ')']
        #print(t[0])
   


def p_expression_paren(t):
    ''' expression : LPAREN expression RPAREN '''
    if t[1] == "(" and t[3] ==")":
        t[0] = ['(', t[2], ')']
    
def p_expression_build(t):
    ''' expression :  expression conjunction expression
                    | expression disjunction expression
                    | expression implication expression'''

    if t[2][0] == "/" and t[2][1] == "\\": 

        t[0] = [t[1], "and",  t[3]]

    elif t[2][0] == "\\" and t[2][1] == "/":

        t[0] = [t[1], "or", t[3]]

    elif t[2] == '->':
        if t[1][0] == 'not':
            t[1].remove(t[1][0])
            t[0] = [ t[1][0], "or", t[3]]
        else:    


            temp = [ "not", t[1] ]
            t[0] = [temp, 'or', t[3]]
 

def p_error(t):
    print("Syntax error at '%s'" % t.value)

import ply.yacc as yacc
parser = yacc.yacc()
def parse(formulae):
    return parser.parse(formulae)


def flattenNestedList(nestedList):
    #https://thispointer.com/python-convert-list-of-lists-or-nested-list-to-flat-list/
    ''' Converts a nested list to a flat list '''
    flatList = []
    for elem in nestedList:
        if isinstance(elem, list):
            flatList.extend(flattenNestedList(elem))
        else:
            flatList.append(elem)    
    return flatList
def listToString(s):
    '''

    given a list converts it to string

    '''   
    str1 = " " 
    return (str1.join(s))     

def findVariables(list1):
    '''

    we have list of ~,and,or,variables and 
    we want only variables

    '''
    var = []
    for i in list1:
        if i != "or" and i != "and" and i != "(" and i!=")" and i != "not":
            var.append(i)
    return sorted(set(var))        
def getTruthValues(var):
    '''

    given a variables, get all combinations of truth values

    '''
    return list(itertools.product([False, True], repeat = len(var)))

def replace_variables(string, var,t):
    '''

    replaces variables of boolean formula to 
    all combinations of truth values
    Also, replacing and, or, since when you use variables r, a,
    replace_variables function replaces 'a' of and, 'r' of or,
    so better to change it for a sec

    '''
    string = string.replace("and","&")
    string = string.replace("or","|") 
    string = string.replace("not","~")
   
    for i in range(0, len(var)): 
        string = string.replace(var[i],'t[j][' + str(i) + ']')
    string = string.replace('&','and')
    string = string.replace('|','or')
    string = string.replace("~","not")
    
    return string    
    

def get_dnf(truth_list, variables,truth):
    '''

    get cnf form. look at table, where result value is True

    '''
    res = []
    for i in range(0, len(truth_list)):
        if truth_list[i] == True:
            res.append('(')
            for j in range(0,len(variables)):
  
                if truth[i][j] == False:

                    res.append('~')
                res.append(variables[j])
                if j != (len(variables) - 1):
                    res.append('/\\')
            res.append(')')
            if i != (len(truth_list) - 1):

                res.append('\\/')
    if len(res) > 0:
        if res[len(res) - 1] == '\\/':
            res.pop(len(res) - 1)            


    return res 

def get_cnf(truth_list, variables,truth):
    '''

    get cnf form. look at table, where result value is False.
    But here we need to negate variables.

    '''
    res = []
    for i in range(0, len(truth_list)):
        if truth_list[i] == False:
            res.append('(')
            for j in range(0,len(variables)):
  
                if truth[i][j] == False:
                    res.append(variables[j])
                else:
                    res.append('~')
                    res.append(variables[j])
                if j != (len(variables) - 1):
                    res.append('\\/')
            res.append(')')
            if i != (len(truth_list) - 1):

                res.append('/\\')
    if len(res) > 0:
        if res[len(res) - 1] == '/\\':
            res.pop(len(res) - 1)
                
    return res            
    

   

while True:
    try: 
        print('Your formulae:')
        s = input('')
    except EOFError:
        break
    a = parse(s)
    flatten_list = flattenNestedList(a)
    my_string = listToString(flatten_list)
    variables = findVariables(flatten_list)
    t = getTruthValues(variables)

    my_string = replace_variables(my_string,variables,t)
  
    result = [] 
    for j in range(2**len(variables)):
        result += [eval(my_string)]    
    cnf = get_cnf(result,variables,t)
    dnf = get_dnf(result,variables,t)
    print("CNF: " + str(listToString(cnf)))
    print("DNF: " + str(listToString(dnf)))
    print('------------------------------------------------')   
