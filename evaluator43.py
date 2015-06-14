# The Core Evaluator

from evaluatorLibrary import *
from table import get, put

MAKE_EXP = Symbol("make_exp")
EVAL = Symbol("EVAL")
CALL = Symbol("call")
VARIABLE = Symbol("variable")
VALUE = Symbol("value")
OPERATOR = Symbol("operator")
OPERANDS = Symbol("operands")
SYMBOL = Symbol("symbol")
DEF_LAMBDA = Symbol("def_lambda")


# get(op, type) => looks up a procedure;
# put(op, type, procedure) => inserts a procedure

def attach_tag(type_tag, contents):
    return cons(type_tag, contents)

def get_type_tag(datum):
    """
    returns the type_tag
    """
    if isPair(datum):
        return car(datum)
    else:
        raise TypeError("Bad tagged datum -- TYPE_TAG")

def contents(datum):
    """
    returns the data without the type_tag
    """
    if isPair(datum):
        return cdr(datum)
    else:
        raise TypeError("Bad tagged datum -- CONTENTS")

def apply_generic(op, *params):
    """
    1. get the op based on the param types
    2. get the param values
    3. apply op to values
    """
    args = List(params) # convert to scheme list (exp)
    type_tags = mapp(get_type_tag, args)
    proc = get(op, type_tags)  # get supports only 2 keys (op, type); lookup(op, type)
    values = mapp(contents, args)
    vals_py = convertToPythonList(values)
    return proc(*vals_py)


def m_eval_dd(exp, env):
    """
    The data-directed core evaluator.
    """
    if isSelfEvaluating(exp):
        # number, string, bool
        return exp
    elif isVariable(exp):
        # symbol
        return lookup_variable_value(exp, env)
    else:
        return get(EVAL, get_type_tag(exp))(exp, env)


def make_define_from_symb(var, val):
    return get(MAKE_EXP, SYMBOL)(var, val)

def make_define_from_lambda(var, params, body):
    # define(foo, (x y), (e1 e2 e3))
    return get(MAKE_EXP, DEF_LAMBDA)(var, params, body)

def make_proc_call(proc, *args):
    return get(MAKE_EXP, CALL)(proc, args)


def install_define_pkg():
    """
    defines internal and external procs for evaluating
    'define expressions
    """
    # internal procedures

    # isDefine
    # ('define var value)
    # ('define (foo x y) body)
    def definition_variable(exp):
        """
        return the variable part
        """
        if isSymbol(cadr(exp)):
            return cadr(exp)
        else:
            return caadr(exp)
    def definition_value(exp):
        """
        return the value part
        """
        if isSymbol(cadr(exp)):
            return caddr(exp)
        else:
            params = cdadr(exp)
            body = cddr(exp)
            return make_lambda(params, body) # 4-14

    def make_lambda(params, body):
        """
        params: (p1 p2) 
        body: ((exp1) (exp2))
        output: (LAMBDA (p1 p2) (exp1) (exp2))
        """
        return cons(LAMBDA, cons(params, body))

    def eval_definition(exp, env):
        """
        create and bind a variable in the current frame
        re-bind a variable in the current frame
        """
        return define_variable(definition_variable(exp), m_eval_dd(definition_value(exp), env), env) # 4-15

    # interface to rest of system
    type_tag = DEFINE
    def tag(x):
        attach_tag(type_tag, x)
    put(EVAL, DEFINE, eval_definition)
    put(MAKE_EXP, SYMBOL, lambda var, val: List(DEFINE, var, val))
    put(MAKE_EXP, DEF_LAMBDA,
        lambda var, params, body: List(DEFINE, var, make_lambda(params, body)))
    print('install define done')

def install_assignment_pkg():

    # internal procedures
    # isAssignment
    def eval_assignment(exp, env):
        """
        re-bind the value of an exisiting variable in env
        """
        return set_variable_value(assignment_variable(exp), m_eval(assignment_value(exp), env), env) # 4-15
    def assignment_variable(exp):
        return cadr(exp)
    def assignment_value(exp):
        return caddr(exp)

    # external procedures
    type_tag = SET_BANG
    def tag(x):
        return attach_tag(type_tag, x)
    put(EVAL, SET_BANG, eval_assignment)
    put(MAKE_EXP, SET_BANG, lambda var, val: List(SET_BANG, var, val))
    print('install set! done')

def install_isquoted_pkg():

    # internal procedures
    # isQuoted
    def text_of_quotation(exp, env):
        """
        returns datum as a List
        (QUOTE datum)
        """
        return cadr(exp)

    # external procedures
    type_tag = ISQUOTED
    def tag(x):
        return attach_tag(type_tag, x)
    put(EVAL, ISQUOTED, text_of_quotation)
    put(MAKE_EXP, ISQUOTED, lambda exp: tag(exp))
    print('install isQuoted done')

def install_if_pkg():

    # internal procs
    def eval_if(exp, env):
        if m_eval_dd(if_predicate(exp), env):
            return m_eval_dd(if_consequent(exp), env)
        else:
            return m_eval_dd(if_alternative(exp), env)
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

    # external procs
    type_tag = IF
    def tag(x):
        return attach_tag(type_tag, x)
    put(EVAL, IF, eval_if)
    put(MAKE_EXP, IF, make_if)
    print('install IF done')
    
def install_lambda_pkg():
    # internal procs
    
    def make_procedure(exp, env):
        # input: (LAMBDA (p1, p2) (e1) (e2))
        # output: (PROCEDURE (p1 p2) ((e1) (e2)) env)
        params = cadr(exp)
        body = cddr(exp)
        return List(PROCEDURE, params, body, env)

    def make_procedure2(params, body, env):
        """evaluates a procedure definition
        returns a closure
        (closure is tagged as 'procedure)"""
        return List(PROCEDURE, params, body, env)
    def procedure_params(proc):
        return list_ref(proc, 1)
    def procedure_body(proc):
        return list_ref(proc, 2)
    def procedure_environment(proc):
        return list_ref(proc, 3)
    def make_lambda(params, body):
        """
        params: (p1 p2) 
        body: ((exp1) (exp2))
        output: (LAMBDA (p1 p2) (exp1) (exp2))
        """
        return cons(LAMBDA, cons(params, body))

    # external procs
    type_tag = LAMBDA
    def tag(x):
        return attach_tag(type_tag, x)
    put(EVAL, LAMBDA, make_procedure)
    put(MAKE_EXP, LAMBDA, make_lambda)
    print('install lambda done')

def install_begin_pkg():
    
    # internal procs
    # (begin e1 e2 e3)
    def begin_actions(beg_exp):
        return cdr(beg_exp)
    def isLastExp(seq):
        return isNull(cdr(seq))
    def first_exp(seq):
        return car(seq)
    def rest_exps(seq):
        return cdr(seq)
    def seq2exp(seq):
        """
        produces a single exp from a seq of expressions
        """
        if isNull(seq):
            """empty sequence"""
            return seq
        elif isLastExp(seq):
            """sequence of 1 exp"""
            return first_exp(seq)
        else:
            """convert seq to a 'begin exp"""
            return make_begin(seq)
    def make_begin(seq):
        return cons(BEGIN, seq)
    def eval_sequence(exps, env):
        """
        (e1 e2 e3) => (v1 v2 v3)
        returns the value of the last exp
        """
        if isLastExp(exps):
            return m_eval_dd(first_exp(exps), env)
        else:
            m_eval_dd(first_exp(exps), env)
            return eval_sequence(rest_exps(exps), env)

    # external procs
    type_tag = BEGIN
    def tag(x):
        return attach_tag(type_tag, x)
    put(EVAL, BEGIN, eval_sequence)
    put(MAKE_EXP, BEGIN, make_begin)
    print('install begin done')

def install_cond_pkg():

    # internal procs
    # ('cond  (c1 n1) (c2 n2) (else n3)  )
    def cond_clauses(cond_exp):
        return cdr(exp)
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
    def seq2exp(seq):
        """
        produces a single exp from a seq of expressions
        """
        if isNull(seq):
            """empty sequence"""
            return seq
        elif isLastExp(seq):
            """sequence of 1 exp"""
            return first_exp(seq)
        else:
            """convert seq to a 'begin exp"""
            return make_begin(seq)
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
            if isNull(rest) and cond_predicate(first) == "else":
                return seq2exp(cond_actions(first)) # make a single "'begin" expression
            else:
                return make_if(
                        cond_predicate(first),
                        seq2exp(cond_actions(first)), # make a single "'begin" expression
                        expand_clauses(rest))
        return expand_clauses(cond_clauses(exp))
    def eval_cond(exp, env):
        return m_eval_dd(cond2if(exp), env)
    def make_clause(pred, list_of_actions):
        """
        (pred e1 e2 e3 e4)
        """
        return cons(pred, list_of_actions)
    def make_cond(list_of_clauses):
        return cons(COND, list_of_clauses)

    # external procs
    type_tag = COND
    def tag(x):
        return attach_tag(type_tag, x)
    put(EVAL, COND, eval_cond)
    put(MAKE_EXP, COND, make_cond)
    print('install COND done')

def install_call_pkg():
    """
    1 new procedure: isApplication
    operator, operands are defined at module level
    """

    # internal procedures
    def isApplication(exp):
        return isTaggedList(exp, CALL)

    def m_apply(proc, args):
        proc = cadr(proc)
        if isPrimitiveProcedure(proc):
            return apply_primitive_procedure(proc, args)
        elif isCompoundProcedure(proc):
            return eval_sequence(procedure_body(proc), 
                                 extend_environment(procedure_params(proc), args, 
                                                   procedure_environment(proc)))
        else:
            raise TypeError("Unknown procedure type -- APPLY")
    def isPrimitiveProcedure(proc_object):
        return isTaggedList(proc_object, PRIMITIVE)
    def isCompoundProcedure(exp):
        return isTaggedList(exp, PROCEDURE)
    def apply_primitive_procedure(proc_object, args):
        """
        invokes the primitive procedure on the arg list (args)
        args is a scheme List of values
        """
        args_py = convertToPythonList(args)
        return primitive_implementation(proc_object)(*args_py)
    def eval_sequence(exps, env):
        """
        (e1 e2 e3) => (v1 v2 v3)
        returns the value of the last exp
        """
        if isLastExp(exps):
            return m_eval_dd(first_exp(exps), env)
        else:
            m_eval_dd(first_exp(exps), env)
            return eval_sequence(rest_exps(exps), env)
    def first_exp(seq):
        return car(seq)
    def rest_exps(seq):
        return cdr(seq)

    def make_proc_call(proc, *args):
        """
        proc is the name identifying a procedure
        (CALL proc arg1 arg2)
        """
        args = List(*args)  # make args a scheme list
        return cons(CALL, cons(proc, args))

    def operands(application):
        """
        make_procedure => (PROCEDURE (x y z) ((exp1) (exp2)) env)
        input: (CALL foo arg1 arg2)
        output: (arg1 arg2)
        """
        return cddr(application)

    def operator(application):
        """
        input: (CALL foo arg1 arg2)
        output: foo
        """
        return cadr(application)

    # external procedures
    type_tag = CALL
    def tag(x):
        return attach_tag(type_tag, x)
    put(OPERATOR, CALL, operator)
    put(OPERANDS, CALL, operands)
    put(EVAL, CALL, m_apply)
    put(MAKE_EXP, CALL, make_proc_call)
    print('install CALL done')

###########################
if __name__ == "__main__":
    the_global_environment = setup_environment()
    x = Symbol("x")
    y = Symbol("y")
    foo = Symbol("foo")

    install_define_pkg()
    install_assignment_pkg()
    install_isquoted_pkg()
    install_if_pkg()
    install_lambda_pkg()
    install_begin_pkg()
    install_cond_pkg()
    install_call_pkg()

    t = get(EVAL, COND)
    print(t)

    # test define var val
    #exp = make_define_from_symb(x, 22)
    ##(exp)

    #m_eval_dd(exp, the_global_environment)    # updates env
    #v = m_eval(x, the_global_environment)  # looks up variable in env
    #print("define and lookup: ", v)

    # test define proc
    exp = make_define_from_lambda(foo, List(x, y), List(List(PLUS, x, y)))
    pprint(exp)
    m_eval_dd(exp, the_global_environment) # adds foo PROCEDURE to env
    t = m_eval_dd(foo, the_global_environment) # looks up foo in env
    display_list(t)

    # test CALL 
    exp = make_proc_call(foo, 3, 4)
    pprint(exp)
    #t = m_eval_dd(exp, the_global_environment)
    print(t)


    






   



