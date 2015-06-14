from consLibrary import *

def map_for_scheme(proc_obj, args):
    """
    proc_obj: ('procedure (param1 param2) (op1 param1 param2))
    proc_obj: ('primitive car) ('primitive cons)
    """
    if isPrimitiveProcedure(proc_obj):
        return mapp(primitive_implementation(proc_obj), args)
    else:
        if isNull(args):
            return EmptyList
        return cons(eval_sequence(procedure_body(proc_obj),
                        extend_environment(procedure_params(proc_obj),
                                            List(car(args)),
                                            procedure_environment(proc_obj))),
                    map_for_scheme(proc_obj, cdr(args)))

DEFINE = Symbol("define")
IF = Symbol("if")
BEGIN = Symbol("begin")
PROCEDURE = Symbol("procedure")
SET_BANG = Symbol("set!")
LAMBDA = Symbol("lambda")
COND = Symbol("cond")
ELSE = Symbol("else")
LET = Symbol("let")

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
        e = make_procedure(lambda_params(exp), lambda_body(exp), env)
        return e
    elif isBegin(exp):
        return eval_sequence(begin_actions(exp), env)
    elif isCond(exp):
        return m_eval(cond2if(exp), env)
    elif isLet(exp):
        e = let2combination(exp)
        pprint(e)
        return m_eval(e, env)
    elif is_let_star(exp):
        return m_eval(letstar_to_nested_let(exp), env)
    elif isApplication(exp):
        return m_apply(m_eval(operator(exp), env), list_of_values(operands(exp), env))
    else:
        raise TypeError("Unknown expression type -- EVAL")

##### New functions for nested let
LETSTAR = Symbol("let*")

def isLet(exp):
    return isTaggedList(exp, LET)

def is_let_star(exp):
    return isTaggedList(exp, LETSTAR)

def make_bindings(*args):
    """
    args: [x, 1], [y, List(PLUS, 1, 2)]
    """
    bindings = EmptyList
    for pair in reversed(args):
        name, e = pair
        bindings = cons(List(name, e), bindings)
    return bindings

def make_let_star(bindings, *exps):
    seq = list(reversed(exps))
    body = EmptyList
    for e in seq:
        body = cons(e, body)
    e = cons(LETSTAR, cons(bindings, body))
    return e

def make_let(bindings, *exps):
    seq = list(reversed(exps))
    body = EmptyList
    for e in seq:
        body = cons(e, body)
    e = cons(LET, cons(bindings, body))
    return e

def let_names(exp):
    """
    input: (LET ((var1 exp1) ... (varn expn)) e1 e2)
    output: (var1 ... varn)
    """
    return mapp(car, cadr(exp))

def let_body(exp):
    """
    cddr(exp) ---> (e1 e2)
    output: (e1 e2)
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
    input: (let ((x 1) (y 2) (z e1)) e2 e3 e4) 
    output: ( (LAMBDA (x y z) e2 e3 e4) 1 2 e1)
    """
    names = let_names(exp)    # (x y z)
    body = let_body(exp)      # (e2 e3 e4)
    lambda_e = make_lambda(names, body)
    return cons(lambda_e, let_exps(exp))

def letstar_to_nested_let(exp):
    bindings = cadr(exp) # ((x 1) (y 2))
    body = cddr(exp)     # (e1 e2 e3)

    def helper(bindings, nested_body):
        if isLastExp(bindings):
            return cons(LET, cons(bindings, body))
        else:
            abinding = List(car(bindings))
            return make_let(abinding, helper(cdr(bindings), body))
    return helper(bindings, body)

def eval_assignment(exp, env):
    """
    re-bind the value of an exisiting variable in env
    """
    set_variable_value(assignment_variable(exp), m_eval(assignment_value(exp), env), env)
def eval_definition(exp, env):
    """
    create and bind a variable in the current frame
    re-bind a variable in the current frame
    """
    return define_variable(definition_variable(exp), m_eval(definition_value(exp), env), env)

def eval_if(exp, env):
    v = if_predicate(exp)
    print("if_predicate")
    #if isPair(v):
    #    pprint(v)
    #else:
    #    print(v)
    if m_eval(if_predicate(exp), env) == FALSE:
        # pred is FALSE
        return m_eval(if_alternative(exp), env)
    else:
        v = if_consequent(exp)
        print("if_consequent")
        #if isPair(v):
        #    pprint(v)
        #else:
        #    print(v)
        return m_eval(if_consequent(exp), env)
def eval_sequence(exps, env):
    """
    (e1 e2 e3) => (v1 v2 v3)
    returns the value of the last exp
    """
    if isLastExp(exps):
        return m_eval(first_exp(exps), env)
    else:
        m_eval(first_exp(exps), env)
        return eval_sequence(rest_exps(exps), env)
def list_of_values(exps, env):
    if isNoOperands(exps):
        return EmptyList
    else:
        return cons(m_eval(first_operand(exps), env),
                    list_of_values(rest_operands(exps), env))
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
            return FALSE  # 4-15
        first = first_clause(list_of_clauses)
        rest = rest_clauses(list_of_clauses)
        if isElseClause(first):
            if isNull(rest):
                return seq2exp(cond_actions(first)) 
            else:
                raise ValueError("ELSE clause is not last -- cond2if")
        else:
            return make_if(
                    cond_predicate(first),
                    seq2exp(cond_actions(first)), # make a single "'begin" expression
                    expand_clauses(rest))
    return expand_clauses(cond_clauses(cond_exp)) # 4-15 changed exp to cond_exp

def m_apply(proc, args):
    if isPrimitiveProcedure(proc):
        return apply_primitive_procedure(proc, args)
    elif isCompoundProcedure(proc):
        return eval_sequence(procedure_body(proc), extend_environment(procedure_params(proc), args, 
                                                                      procedure_environment(proc)))
    else:
        raise TypeError("Unknown procedure type -- APPLY")


### Representing Expressions

def isTaggedList(exp, tag):
    """returns True if exp is tagged with tag"""
    return isPair(exp) and isinstance(car(exp), Symbol) and (car(exp).name == tag.name)

def isSelfEvaluating(exp):
    return isNumber(exp) or isString(exp) or isBool(exp)

def isQuoted(exp):
    return isTaggedList(exp, QUOTE)

def text_of_quotation(exp):
    """
    returns datum as a List
    (QUOTE datum)
    """
    return cadr(exp)

# (SET_BANG var value)
def isVariable(exp):
    return isSymbol(exp)
def isAssignment(exp):
    return isTaggedList(exp, SET_BANG)
def assignment_variable(exp):
    return cadr(exp)
def assignment_value(exp):
    return caddr(exp)

# (DEFINE var value)
# (DEFINE (foo x y) body)
def isDefinition(exp):
    return isTaggedList(exp, DEFINE)
def definition_variable(exp):
    if isSymbol(cadr(exp)):
        return cadr(exp)
    else:
        return caadr(exp)
def definition_value(exp):
    if isSymbol(cadr(exp)):
        return caddr(exp)
    else:
        params = cdadr(exp)
        body = cddr(exp)
        make_lambda(params, body)

# (LAMBDA (p1 p2) e1 e2)
def isLambda(exp):
    return isTaggedList(exp, LAMBDA)
def lambda_params(lambda_exp):
    """
    (x y z)
    """
    return cadr(lambda_exp)
def lambda_body(lambda_exp):
    """
    input: (LAMBDA (x y) e1 e2 e3 )
    returns ( e1 e2 e3 )
    """
    return cddr(lambda_exp)
def make_lambda(params, body):
    """
    params: (p1 p2 p3 ...)
    body: ( exp1 exp2 )
    (LAMBDA params exp1 exp2)
    """
    return cons(LAMBDA, cons(params, body))

# ('if e1 e2 e3)
def isIf(exp):
    return isTaggedList(exp, IF)
def if_predicate(exp):
    return cadr(exp)
def if_consequent(exp):
    return caddr(exp)
def if_alternative(exp):
    if not isNull(cdddr(exp)):
        return cadddr(exp)
    else:
        return False
def make_if(pred, conseq, alt):
    return List(IF, pred, conseq, alt)

# ('cond  (c1 n1) (c2 n2) (else n3) )
def isCond(exp):
    return isTaggedList(exp, COND)
def cond_clauses(cond_exp):
    return cdr(cond_exp) # 4-15 changed exp to cond_exp
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


# (begin e1 e2 e3)
def isBegin(exp):
    return isTaggedList(exp, BEGIN)
def begin_actions(beg_exp):
    return cdr(beg_exp)
def isLastExp(seq):
    """
    seq: (e1)
    """
    return isNull(cdr(seq))
def first_exp(seq):
    """
    seq: (e1 e2 ... )
    output: e1
    """
    return car(seq)
def rest_exps(seq):
    """
    seq: (e1 e2 e3)
    output: (e2 e3)
    """
    return cdr(seq)
def seq2exp(seq):
    """
    produces a single exp from a seq of expressions
    """
    if isNull(seq):
        """empty sequence"""
        return seq
    elif isLastExp(seq):
        """
        seq: sequence of 1 exp
        output: 1 exp (not a list); will be cons'd with BEGIN
        """
        return first_exp(seq)
    else:
        """convert seq to a 'begin exp"""
        return make_begin(seq)
def make_begin(seq):
    return cons(BEGIN, seq)

# application of proc to args
# (operator arg1 arg2 arg3)
def isApplication(exp):
    """used after all special forms are tested"""
    return isPair(exp)
def operator(app):
    """
    returns a variable that is the name of the procedure_object
    """
    return car(app)
def operands(app):
    """
    returns a list of values
    These are the values to which the operator is applied.
    """
    return cdr(app)
def isNoOperands(args):
    return isNull(args)
def first_operand(args):
    return car(args)
def rest_operands(args):
    return cdr(args)

### Representing Procedures
# (PROCEDURE (p1 p2 p3) (exp1 exp2) env)
def make_procedure(params, body, env):
    """evaluates a procedure definition
    returns a closure
    (closure is tagged as 'procedure)"""
    return List(PROCEDURE, params, body, env)
def isCompoundProcedure(exp):
    return isTaggedList(exp, PROCEDURE)
def procedure_params(proc):
    return cadr(proc)
def procedure_body(proc):
    return caddr(proc)
def procedure_environment(proc):
    return cadddr(proc)

### Representing Environments
# frame: ( list_of_variables list_of_values )
# Implement environments as a list of frames; parent environment is
# the cdr of the list. Each frame will be implemented as a list
# of variables and a list of corresponding values.
def enclosing_env(env):
    return cdr(env)
def first_frame(env):
    return car(env)
the_empty_environment = EmptyList

def make_frame(variables, values):
    """
    variables: (var1 var2 var3)
    values: (val1 val2 val3)
    output: (variables values)
    """
    return cons(variables, values)
def frame_variables(frame):
    return car(frame)
def frame_values(frame):
    return cdr(frame)
def addBindingToFrame(var, val, frame):
    set_car(frame, cons(var, frame_variables(frame)))
    set_cdr(frame, cons(val, frame_values(frame)))
    return
def extend_environment(vars, vals, base_env):
    """
    vars: (var1 var2 var3)
    vals: (val1 val2 val3)
    base_env: env
    """
    if isNull(vars):
        return base_env
    if length(vars) == length(vals):
        return cons(make_frame(vars, vals), base_env)
    elif length(vars) > length(vals):
        raise ValueError("extend_environment -- too few args")
    else:
        raise ValueError("extend_environment -- too many args")

# lookup in the environment chain
def lookup_variable_value(var, env):
    """
    returns a value or UnboundLocalError
    """
    def env_loop(environment):
        """
        calls scan on each frame in the env list
        """
        def scan(vars, vals):
            """
            scans variables in a frame
            """
            if isNull(vars):
                return env_loop(enclosing_env(environment)) # 5-4 env -> environment
            elif isEq(var, car(vars)) == TRUE:
                return car(vals)
            else:
                return scan(cdr(vars), cdr(vals))
        if environment is the_empty_environment:  
            raise UnboundLocalError("lookup_variable")
        frame = first_frame(environment)
        return scan(frame_variables(frame), frame_values(frame))
    return env_loop(env)

def set_variable_value(var, val, env):
    """
    sets a var to a val, if var is found, else UnboundLocalError
    """
    def env_loop(environment):
        """
        calls scan on each frame in the env list
        """
        def scan(vars, vals):
            """
            scans variables in a frame
            """
            if isNull(vars):
                return env_loop(enclosing_env(environment)) # 5-4 env -> environment
                return set_car(vals, val) #4-15
            else:
                return scan(cdr(vars), cdr(vals)) # 4-15
        if environment is the_empty_environment:
            raise UnboundLocalError("lookup_variable")
        frame = first_frame(environment)
        return scan(frame_variables(frame), frame_values(frame)) # 4-15
    return env_loop(env) # 4-15

def define_variable(var, val, env):
    """
    re-binds an existing variable or creates a new binding
    in the current frame
    """
    frame = first_frame(env)
    def scan(vars, vals):
        if isNull(vars):
            return addBindingToFrame(var, val, frame)
        elif var == car(vars):
            return set_car(vals, val)
        else:
            return scan(cdr(vars), cdr(vals))
    return scan(frame_variables(frame), frame_values(frame))

### Primitive Procedures and the Initial Environment
# ('primitive primitive_name primitive_op)
# for ex: ('primitive 'car car)

primitive_procedures = List(List(CAR, car),
                            List(CDR, cdr),
                            List(CDDR, cddr),
                            List(CDDDR, cdddr),
                            List(CADDDR, cadddr),
                            List(CADR, cadr),
                            List(CADDR, caddr),
                            List(CAADR, caadr),
                            List(CDADR, cdadr),
                            List(CADR, cadr),
                            List(CONS, cons),
                            List(ISNULL, isNull),
                            List(ISPAIR, isPair),
                            List(ISSYMBOL, isSymbol),
                            List(ISNUMBER, isNumber),
                            List(ISSTRING, isString),
                            List(ISQUOTED, isQuoted),
                            List(SET_CAR, set_car),
                            List(SET_CDR, set_cdr),
                            List(PLUS, plus),
                            List(MINUS, minus),
                            List(MULT, mult),
                            List(DIV, divide),
                            List(GT, greaterthan),
                            List(LT, lessthan),
                            List(EQUALTO, equalto),
                            List(ISEQ, isEq),
                            List(ISEQUAL, isEqual),
                            List(OR, or_lispy),
                            List(AND, and_lispy),
                            List(ASSOC, assoc),
                            List(MAPP, mapp),
                            List(MAPP_S, map_for_scheme),
                            List(LIST, List))


def primitive_procedure_names():
    """
    returns list of names of primitive procedures
    """
    return mapp(car, primitive_procedures)
def primitive_procedure_objects():
    """
    returns a list of: ( (PRIMITIVE primitive_implementation) ... )
    for ex:
    ( (PRIMITIVE car) (PRIMITIVE cdr) (PRIMITIVE cons) (PRIMITIVE plus) ... )
    """
    return mapp(lambda proc: List(PRIMITIVE, primitive_implementation(proc)), primitive_procedures)

def isPrimitiveProcedure(proc_object):
    return isTaggedList(proc_object, PRIMITIVE)
def primitive_implementation(primitive_proc):
    return cadr(primitive_proc)
def apply_primitive_procedure(proc_object, args):
    """
    invokes the primitive procedure on the arg list (args)
    args is a scheme List of values
    """
    args_py = convertToPythonList(args)
    return primitive_implementation(proc_object)(*args_py)

def setup_environment():
    initial_env = extend_environment(primitive_procedure_names(),  # ("'cons", "'+", ...)
                                    primitive_procedure_objects(), # ((PRIMITIVE, cons) (PRIMITIVE, plus)...)
                                    the_empty_environment)
    define_variable(TRUE, True, initial_env)
    define_variable(FALSE, False, initial_env)
    return initial_env

#the_global_environment = setup_environment()

###################
if __name__ == "__main__":
    env = setup_environment()
    x = Symbol("x")
    y = Symbol("y")
    z = Symbol("z")

    bindings = make_bindings([x, 3], [y, List(PLUS, x, 2)], [z, List(PLUS, x, y, 5)])
    e = make_let_star(bindings, List(MULT, x, z))
    pprint(e)
    print()
    nested_e = letstar_to_nested_let(e)
    pprint(nested_e)

    #######
    #
    # Cannot add a clause to eval whose action is:
    # m_eval(let_star_to_nested(exp), env)
    # reason: m_eval extends env with only the first binding and evaluates all nested levels in this env
    # see line 128 in evaluatorLibrary
    #######
    
    ans = m_eval(e, env)
    print(ans)