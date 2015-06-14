"""
This module implements ex. 4.4

and: The expressions are evaluated from left to right.
If any expression evaluates to false, false is returned; remaining expressions are not evaluated
If all evaluate to True, the value of the last expression is returned.
If there are no expressions, then true is returned.

or: The expressions are evaluated from left to right. If any expression evaluates to a true value, that value is returned; any remaining expressions are not evaluated. If all expressions evaluate to false, or if there are no expressions, then false is returned.

Install 'and' and 'or' as new special forms for the evaluator by defining appropriate syntax procedures 
and evaluation procedures eval-and and eval-or.

Alternatively, show how to implement and and or as derived expressions.
"""

from evaluatorLibrary import *
from consLibrary import *

AND = Symbol("and")
OR = Symbol("or")
ARROW = Symbol("=>")

### The Core Evaluator
def m_eval(exp, env):
    if isSelfEvaluating(exp):
        return exp
    elif isVariable(exp):
        return lookup_variable_value(exp, env)
    elif isQuoted(exp):
        return text_of_quotation(exp)
    elif isAssignment(exp):
        return eval_assignment(exp, env)
    elif isDefinition(exp):
        return eval_definition(exp, env)
    elif isIf(exp):
        pprint(exp)
        return eval_if(exp, env)
    elif isLambda(exp):
        return make_procedure(lambda_params(exp), lambda_body(exp), env)
    elif isBegin(exp):
        return eval_sequence(begin_actions(exp), env)
    elif isCond(exp):
        return m_eval(cond2if(exp), env)
    elif isAnd(exp):
        return eval_and(exp, env)
    elif isOr(exp):
        return eval_or(exp, env)
    elif isApplication(exp):
        return m_apply(m_eval(operator(exp), env), list_of_values(operands(exp), env))
    else:
        raise TypeError("Unknown expression type -- EVAL")

# and expressions
def isAnd(exp):
    """
    exp: a tagged scheme list
    """
    return isTaggedList(exp, AND)

def make_and(AND, *exps):
    """
    returns a scheme list: ('and e1 e2 e3)
    """
    return List(AND, *exps)

def and_exps(exp):
    """
    exp: ('and e1 e2 e3)
    """
    return cdr(exp)

def eval_sequence_and(exps, env):
    """
    (e1 e2 e3) => val of last evaluated exp or False
    """
    val = m_eval(first_exp(exps), env)
    if isEq(val, FALSE):  # scheme false is #f
        return FALSE # 4-15 changed False to FALSE
    if isLastExp(exps):
        return val
    return eval_sequence_and(rest_exps(exps), env)

def eval_and_1(exp, env):
    """
    evaluates left to right
    if an exp evaluates to False, return False without evaluating rest
    else return value of last expression
    """
    return eval_sequence_and(and_exps(exp), env)

# or expressions
def isOr(exp):
    return isTaggedList(exp, OR)

def make_or(OR, *exps):
    """
    returns a scheme list: ('or e1 e2 e3)
    """
    return List(OR, *exps)

def or_exps(exp):
    """
    exp: ('or e1 e2 e3)
    """
    return cdr(exp)

def eval_sequence_or(exps, env):
    """
    (e1 e2 e3) => (v1 v2 v3)
    returns the value of the last exp
    """
    val = m_eval(first_exp(exps), env)
    if isLastExp(exps):
        if val == FALSE:
            return FALSE
        else:
            # val is not FALSE
            return val
    else:
        if val != FALSE:
            return val
        return eval_sequence_or(rest_exps(exps), env)

def eval_or_1(exp, env):
    return eval_sequence_or(or_exps(exp), env)
        
# derived expressions
def and2if(exp):
    """
    exp: (AND e1 e2 e3)
    returns (if e1 (if e2 e3 FALSE) FALSE)
    """
    def helper(exps):
        first = car(exps)
        rest = cdr(exps)
        if isNull(rest):
            return first
        else:
            return make_if(first, helper(rest), FALSE)
    return helper(and_exps(exp))



def eval_and(exp, env):
    if isNull(and_exps(exp)):
        return True
    return m_eval(and2if(exp), env)

def or2if(exp):
    """
    exp: (OR e1 e2 e3)
    return (if e1 e1 (if e2 e2 e3)) 
    """
    def helper(exps):
        first = car(exps)
        rest = cdr(exps)
        if isNull(rest):
            return first
        else:
            return make_if(first, first, helper(rest))
    return helper(or_exps(exp))


def eval_or(exp, env):
    return m_eval(or2if(exp), env)

############
if __name__ == "__main__":
    the_global_environment = setup_environment()

    # test and
    e = make_and(AND)
    e = make_and(AND, 1, FALSE, 3)
    print(e)
    x = m_eval(e, the_global_environment)
    print(x)

    #e = make_and(AND, 1, 2, FALSE)
    #e = make_and(AND, FALSE, 2, FALSE)
    e = make_and(AND, 0, 2, TRUE)
    x = m_eval(and2if(e), the_global_environment)
    print(x)

    # test or
    e = make_or(OR, FALSE, 0, 4)
    e = make_or(OR, FALSE, FALSE, FALSE, 0, 4)
    x = m_eval(e, the_global_environment)
    print(x)


