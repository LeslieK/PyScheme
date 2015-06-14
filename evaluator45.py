"""
Exercise 4.5.  Scheme allows an additional syntax for cond clauses, (<test> => <recipient>). If <test> evaluates
to a true value, then <recipient> is evaluated. Its value must be a procedure of one argument; this procedure is
then invoked on the value of the <test>, and the result is returned as the value of the cond expression. For example:

(cond ((assoc 'b '((a 1) (b 2))) => cadr)
      (else false))

returns 2. Modify the handling of cond so that it supports this extended syntax.
"""

from evaluatorLibrary import *
from consLibrary import *

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
        return eval_if(exp, env)
    elif isLambda(exp):
        return make_procedure(lambda_params(exp), lambda_body(exp), env)
    elif isBegin(exp):
        return eval_sequence(begin_actions(exp), env)
    elif isCond(exp):
        return m_eval(cond2if(exp), env)
    elif isApplication(exp):
        return m_apply(m_eval(operator(exp), env), list_of_values(operands(exp), env))
    else:
        raise TypeError("Unknown expression type -- EVAL")


# ('cond  (c1 n1) (c2 n2) (else n3) )
def make_cond(*clauses):
    return List(COND, *clauses)

def isCond(exp):
    return isTaggedList(exp, COND)

def cond_clauses(cond_exp):
    return cdr(cond_exp) 

def first_clause(list_of_clauses):
    return car(list_of_clauses)

def rest_clauses(list_of_clauses):
    return cdr(list_of_clauses)

def cond_predicate(clause):
    """
    returns c of (c e)
    """
    return car(clause)

def cond_actions(clause):
    """
    (c e1 e2 e3) is a list
    clause can have 1 or more actions
    """
    return cdr(clause)

def isElseClause(clause):
    return cond_predicate(clause) == ELSE

def cond_recipient(clause):  # new procedure
    """
    (c => recipient)
    returns: recipient
    """
    return caddr(clause)

def isRecipientClause(clause):  # new procedure
    """
    (test => recipient)
    """
    return cadr(clause) == ARROW

def cond2if(cond_exp):
    """
    ("'cond"  (c1 e1)
              (c2 e2)
              (c3 e3)
              ('"else" e4)
    )
    produces a nested if_exp
    (if c1 e1
          (if c2 e2
                 (if c3 e3)))
    """
    def expand_clauses(list_of_clauses):        
        if isNull(list_of_clauses):
            return FALSE
        first = first_clause(list_of_clauses)
        rest = rest_clauses(list_of_clauses)
        if isElseClause(first):
            if not isNull(rest):
                raise ValueError("ELSE is not last clause -- cond2if")
            return seq2exp(cond_actions(first))
        else:
            if isRecipientClause(first):
                return make_if(cond_predicate(first),
                            List(cond_recipient(first), cond_predicate(first)),
                            expand_clauses(rest))
            return make_if(
                    cond_predicate(first),
                    seq2exp(cond_actions(first)), 
                    expand_clauses(rest))
    return expand_clauses(cond_clauses(cond_exp)) 

##############
if __name__ == "__main__":
    the_global_environment = setup_environment()

    a = Symbol("a")
    b = Symbol("b")

    test = List(ASSOC, quote(b), quote(List(List(a, 1), List(b, 2))))
    pprint(test)

    exp = make_cond(List(test, ARROW, CADR), List(ELSE, FALSE))
    pprint(exp)

    y = m_eval(exp, the_global_environment)
    print(y)




