from evaluatorLibrary import *
from consLibrary import *

"""
Ex. 4.6
(let ((<var1> <exp1>) ... (<varn> <expn>)) <body>)

implement syntactic transformation let2combination that reduces let expressions to evaluating combinations of the type shown above.
Add appropriate clause to eval to handle let expressions.
"""
# (LAMBDA (p1 p2) (exp1) (exp2))

def make_bindings(*args):
    """
    args: [x, 1], [y, 2]
    """
    bindings = EmptyList
    for pair in reversed(args):
        name, e = pair
        bindings = cons(List(name, e), bindings)
    return bindings

def make_let(bindings, *exps):
    seq = list(reversed(exps))
    body = EmptyList
    for e in seq:
        body = cons(e, body)
    e = cons(LET, cons(bindings, body))
    return e

def isLet(exp):
    return isTaggedList(exp, LET)

def let_names(exp):
    """
    input: (LET ((var1 exp1) ... (varn expn)) <body>)
    output: (var1 ... varn)
    """
    return mapp(car, cadr(exp))

def let_body(exp):
    """
    cddr(exp) ---> (<body>)
    output: single exp
    """
    return cddr(exp)

def let_exps(exp):
    """
    cadr(exp) ----> ( (var1 e1)  (var2 e2) ... (varn en) )
    output: (e1 e2 ... en)
    """
    return mapp(cadr, cadr(exp))

def let2combination(exp):
    """
    output: ( (LAMBDA (var1 ... varn) <body>) e1 e2 ... en )
    """
    names = let_names(exp)
    body = let_body(exp)
    return cons(make_lambda(names, body), let_exps(exp))

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
    elif isLet(exp):
        pprint(let2combination(exp))
        return m_eval(let2combination(exp), env)
    elif isApplication(exp):
        return m_apply(m_eval(operator(exp), env), list_of_values(operands(exp), env))
    else:
        raise TypeError("Unknown expression type -- EVAL")

if __name__ == "__main__":
    env = setup_environment()
    x = Symbol("x")
    y = Symbol("y")

    #e = make_let(make_bindings([x, 1], [y, 2]), List(PLUS, x, y))
    bindings = make_bindings([x, 1], [y, List(MULT, List(PLUS, 21, 22), List(MULT, 3, 5))])
    e = make_let(bindings, List(PLUS, x, y))
    pprint(e)                                       # ---> (LET ((x 1) (y 2)) (+ x y))
    list_of_exps = let_exps(e)
    pprint(list_of_exps)
    print()
    body = let_body(e)
    pprint(body)
    print()
    lambda_e = make_lambda(let_names(e), List(let_body(e)))
    pprint(lambda_e)                                # ---> (LAMBDA (x y) (+ x y))
    print()
    params = lambda_params(lambda_e)
    pprint(params)
    print()
    body = lambda_body(lambda_e)
    pprint(body)
    print()
    ans = m_eval(lambda_e, env)                    # ----> (PROCEDURE (x y) (+ x y) env)
    ans = m_eval(e, env)
    print(ans)
    print()
          
                   

    