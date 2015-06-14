"""
SICP, Chapter 4.1
"""
from evaluatorLibrary import *
from consLibrary import *

###########################
# Ex. 4.1
###########################

def list_of_values(exps, env):
    """
    exps: (e1 e2 e3 ... en)
    """
    if isNoOperands(exps):
        return List()
    else:
        return cons(m_eval(first_operand(exps), env),
                    list_of_values(rest_operands(exps), env))

def list_of_values_left(list_of_exps, env):
    """
    exps: (e1 e2 e3)
    evaluates expressions left to right
    (BEGIN e1 e2 e3)
    """
    return m_eval(seq2exp(list_of_exps), env)

def list_of_values_right(exps, env):
    """
    exps: operands: (arg1 arg2 arg3)
    """
    if isNoOperands(exps):
        return EmptyList
    right = list_of_values_right(rest_operands(exps), env)
    if isNull(right):
        display_list(right)
    else:
        pprint(right)
    f = first_operand(exps)
    left = m_eval(first_operand(exps), env)
    if isList(left):
        pprint(left)
    else:
        print(left)
    return List(left, right)

###################################
# Ex. 4.2
###################################
# a. ('define x 3)
# Louis will eval('define), and eval x and eval 3. x is undefined. (Unbound variable error)

# b. Change syntax of procedure application.
# Help him by changing the syntax of the evaluated language so that procedure applications start with call. For example, instead of (factorial 3) we will now have to write (call factorial 3) and instead of (+ 1 2) we will have to write (call + 1 2).

#def isApplication(exp):
#    return isTaggedList(exp, Symbol("call"))
#def operator(exp):
#    return cadr(exp)
#def operands(exp):
#    return cddr(exp)
#def isNoOperands(args):
#    # no change
#    return isNull(args)
#def first_operand(args):
#    return cadr(args)
#def rest_operands(args):
#    return cddr(args)

####################################
# Ex. 4.3
 
#Rewrite eval so that the dispatch is 
#done in data-directed style. Compare this with the 
#data-directed differentiation procedure of exercise 2.73. 
#(You may use the car of a compound expression as the type 
#the expression, as is appropriate for the syntax 
#implemented in this section.) .

# See Ex. 2.73

meta_eval = Symbol("meta_eval")

####################################





######################## 
if __name__ == "__main__":
    env = setup_environment()
    x = Symbol("x")

    # test Ex. 4.1
    m_eval(List(DEFINE, x, 100), env)
    exp = List(PLUS, x, 10, 20)
    #exp = List(MINUS, 1, 3)
    pprint(operands(exp))

    #val_left = list_of_values_left(operands(exp), env)
    #val_right = list_of_values_right(operands(exp), env)
    #print(val_right)


