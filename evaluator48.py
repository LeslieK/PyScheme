"""
Exercise 4.8.  ``Named let'' is a variant of let that has the form

(let <var> <bindings> <body>)

The <bindings> and <body> are just as in ordinary let, except that <var> is bound within <body> to a procedure whose body is <body> and whose parameters are the variables in the <bindings>. Thus, one can repeatedly execute the <body> by invoking the procedure named <var>. For example, the iterative Fibonacci procedure (section 1.2.2) can be rewritten using named let as follows:

(define (fib n)
  (let fib-iter ((a 1)
                 (b 0)
                 (count n))
    (if (= count 0)
        b
        (fib-iter (+ a b) a (- count 1)))))

Modify let->combination of exercise 4.6 to also support named let.
"""

from evaluatorLibrary import *
from consLibrary import *

LET = Symbol("let")

def make_bindings(*args):
    """
    args: [x, 1], [y, 2]
    """
    bindings = EmptyList
    if len(args) == 0:
        return List()
    for pair in reversed(args):
        name, e = pair
        bindings = cons(List(name, e), bindings)
    return bindings


def make_named_let(label, bindings, *exps):
    """
    label: a Symbol bound to a procedure
    args: [var1, e1]  ...  [varn, en]
    exps: python tuple (e1, e2, ... )
    """
    seq = list(reversed(exps))
    body = EmptyList
    for e in seq:
        body = cons(e, body)
    e = cons(LET, cons(label, cons(bindings, body)))
    return e

def make_let(bindings, *exps):
    seq = list(reversed(exps))
    body = EmptyList
    for e in seq:
        body = cons(e, body)
    e = cons(LET, cons(bindings, body))
    return e

def isLet(exp):
    return isTaggedList(exp, LET)

def isNamedLet(exp):
    return isSymbol(let_label(exp))

def let_names(exp):
    """
    input: (LETNAME label ((var1 exp1) ... (varn expn)) <body>)
    output: (var1 ... varn)
    """
    if isNamedLet(exp):
        return mapp(car, caddr(exp))
    else:
        return mapp(car, cadr(exp))

def let_body(exp):
    """
    input: (LET label ((var1 exp1) ... (varn expn)) <body>)
    cdddr(exp) ---> (<body>)
    output: ( e1 e2 )
    """
    if isNamedLet(exp):
        return cdddr(exp)
    else:
        return cddr(exp)

def let_exps(exp):
    """
    caddr(exp) ----> ( (var1 e1)  (var2 e2) ... (varn en) )
    output: (e1 e2 ... en)
    """
    if isNamedLet(exp): 
        return mapp(cadr, caddr(exp))
    else:
        return mapp(cadr, cadr(exp))

def let_label(exp):
    return cadr(exp)

def let2combination(exp):
    """
    (let foo ((x 1) (y 2)) <body>)
    (let ((x 1) (y 2)) <body>)
    output: ( (LAMBDA (var1 ... varn) <body>) e1 e2 ... en )
    """
    names = let_names(exp)
    body = let_body(exp)
    lambda_e = make_lambda(names, body)
    if isNamedLet(exp):
        label = let_label(exp)
        define_variable(label, m_eval(lambda_e, env), env)  # bind label to PROCEDURE in env
    return cons(lambda_e, let_exps(exp))

def let2combination2(exp, env):  # env is used for named_let
    """
    input: env is passed in since it is reqd for named_let
    (let foo ((x 1) (y 2)) <body>)
    (let ((x 1) (y 2)) <body>)
    output: ( (LAMBDA (var1 ... varn) <body>) e1 e2 ... en )
    """
    names = let_names(exp)
    body = let_body(exp)
    lambda_e = make_lambda(names, body)
    if isNamedLet(exp):
        label = let_label(exp)
        define_variable(label, m_eval(lambda_e, env), env)  # bind label to PROCEDURE in env
    return cons(lambda_e, let_exps(exp))

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
        pprint(exp)
        return make_procedure(lambda_params(exp), lambda_body(exp), env)
    elif isBegin(exp):
        return eval_sequence(begin_actions(exp), env)
    elif isCond(exp):
        return m_eval(cond2if(exp), env)
    elif isLet(exp):
        #pprint(let2combination(exp))
        #return m_eval(let2combination(exp), env)
        pprint(let2combination2(exp, env))
        return m_eval(let2combination2(exp, env), env)
    elif isApplication(exp):
        return m_apply(m_eval(operator(exp), env), list_of_values(operands(exp), env))
    else:
        raise TypeError("Unknown expression type -- EVAL")

#######################
if __name__ == "__main__":
    #env = setup_environment()
    #foo = Symbol("foo")
    #x = Symbol("x")
    #y = Symbol("y")

    #bindings = make_bindings([x, 2], [y, 3])

    #e = make_named_let(foo, bindings, List(PLUS, x, y), List(MULT, x, y))
    ##e = make_let(bindings, List(PLUS, x, y), List(MULT, x, y))
    #pprint(e)
    #b = let_body(e)
    #pprint(b)
    #n = let_names(e)
    #pprint(n)
    #exps = let_exps(e)
    #pprint(exps)
    #if isNamedLet(e):
    #    label = let_label(e)
    #    print(label)
    #ans = m_eval(e, env)
    #print(ans)

    ###
    env = setup_environment()
    a = Symbol("a")
    b = Symbol("b")
    count = Symbol("count")
    fib_iter = Symbol("fib_iter")

    bindings = make_bindings([a, 1], [b, 0], [count, 7])
    e = make_named_let(fib_iter, bindings, make_if(List(EQUALTO, count, 0), b, List(fib_iter, List(PLUS, a, b), a, List(MINUS, count, 1))))
    pprint(e)
    b = let_body(e)
    pprint(b)
    n = let_names(e)
    pprint(n)
    exps = let_exps(e)
    pprint(exps)
    if isNamedLet(e):
        label = let_label(e)
        print(label)
        print()

    let2combination(e)          # --> ( (lambda (a b count) (if e1 e2 (fib_iter (+ a b) a (- count 1)))) 1 0 1)
    print()

    #pprint(car(car(car(env))))  # fib_iter symbol
    #pprint(cadr(car(env)))      # fib_iter procedure
    ans = m_eval(e, env)
    print(ans)

