"""
Symbolic Derivation
"""

from consLibrary import *
from evaluatorLibrary import *

plus = Symbol("+")
mult = Symbol("*")
power = Symbol("**")

def isVariable(exp):
    return isinstance(exp, Symbol)

### CORE Deriv ###

def deriv(exp, var):
    if isNumber(exp):
        return 0
    elif isVariable(exp):
        if same_variable(exp, var):
            return 1
        else:
            return 0
    elif is_sum(exp):
        return make_sum(deriv(addend(exp), var),
                 deriv(augend(exp), var))
    elif is_product(exp):
        return make_sum(
                 make_product(multiplier(exp),
                              deriv(multiplicand(exp), var)),
                 make_product(deriv(multiplier(exp), var),
                              multiplicand(exp)))
    elif is_exponentiation(exp):
        return make_product(
                            exponent(exp),
                            make_product(
                                         make_exponentiation(base(exp), 
                                                             make_sum(exponent(exp),
                                                                      -1)),
                                         deriv(base(exp), var)))
    else:
        raise TypeError("unknown expression type -- DERIV exp")

##################

def same_variable(v1, v2):
    return isVariable(v1) and isVariable(v2) and isEq(v1, v2) == TRUE

def make_sum(a1, a2):
    if isNumber(a1) and a1==0:
        return a2
    elif isNumber(a2) and a2==0:
        return a1
    elif isNumber(a1) and isNumber(a2):
        return a1 + a2
    else:
        return List(PLUS, a1, a2)

def make_sum_2(a1, a2, *rest):
    """
    rests: a scheme list
    restp: a python list
    """
    rests = List(*rest)
    restp = list(rest)
    if isNull(rests):
        return make_sum(a1, a2)
    elif isNull(cdr(rests)):
        return make_sum_2(make_sum(a1, a2), car(rests))
    else:
        return make_sum_2(make_sum(a1, a2), car(rests), *restp[1:])

def make_product(m1, m2):
    if isNumber(m1) and m1 == 0:
        return 0
    elif isNumber(m2) and m2 == 0:
        return 0
    elif isNumber(m1) and m1 == 1:
        return m2
    elif isNumber(m2) and m2 == 1:
        return m1
    elif isNumber(m1) and isNumber(m2):
        return m1 * m2
    else:
        return List(MULT, m1, m2)

def make_product_2(m1, m2, *rest):
    """
    rests: a scheme list
    restp: a python list
    """
    rests = List(*rest)
    restp = list(rest)
    if isNull(rests):
        return make_product(m1, m2)
    elif isNull(cdr(rests)):
        return make_product_2(make_product(m1, m2), car(rests))
    else:
        return make_product_2(make_product(m1, m2), car(rests), *restp[1:])


def is_sum(x):
    """
    ('+ a1 a2)
    """
    return isTaggedList(x, PLUS)

def addend(s):
    """
    ('+ a1 rest)
    """
    return cadr(s)

def augend(s):
    """
    ('+ a1 a2)
    """
    return caddr(s)

def augend_2(exp):
    """
    ('+ a1 a2 a3 ...)
    rests: a scheme list
    restp: a python list
    """
    a2 = cddr(exp) # (a2 a3 ...)
    rests = cdr(a2) # (a3 ... )
    if isNull(rests):
        return car(a2)
    else:
        restp = convertToPythonList(cdr(rests))
        return make_sum_2(car(a2), car(rests), *restp)

def is_product(x):
    """
    ('* m1 m2)
    """
    return isTaggedList(x, MULT)

def multiplier(p):
    return cadr(p)

def multiplicand(p):
    """
    ('* m1 m2)
    """
    return caddr(p)

def multiplicand_2(p):
    """
    ('* m1 m2 m3 ... )
    rests: a scheme list
    restp: a python list
    """
    m2 = cddr(p)    # (m2 m3 ...)
    rests = cdr(m2)  # (m3...)
    if isNull(rests):
        return car(m2)
    else:
        restp = convertToPythonList(cdr(rests))
        return make_product_2(car(m2), car(rests), *restp)


def make_exponentiation(base, exponent):
    """
    ('** base exponent)
    """
    if isNumber(exponent) and exponent == 0:
        return 1
    elif isNumber(exponent) and exponent == 1:
        return base
    elif isNumber(base) and base == 0:
        return 0
    elif isNumber(base) and isNumber(exponent):
        return base ** exponent
    else:
        return List(POWER, base, exponent)

def is_exponentiation(x):
    return isTaggedList(x, POWER)

def base(exp):
    return cadr(exp)

def exponent(exp):
    return caddr(exp)


##########################
#deriv using in-fix and operation precedence

def deriv_2(exp, var):
    """
    returns the derivative of an expression
    that uses in-fix notation and precedence of operations
    """
    if isNumber(exp):
        return 0
    elif isVariable(exp):
        if same_variable(exp, var):
            return 1
        else:
            return 0
    # product?
    elif is_product_in_2(exp):
        return make_sum_in_2(
                             make_product_in_2(multiplier_in_2(exp),
                                               deriv_2(multiplicand_in_2(exp), var)),
                             make_product_in_2(deriv_2(multiplier_in_2(exp), var),
                                               multiplicand_in_2(exp)))
    # sum?
    elif is_sum_in_2(exp):
        return make_sum_in_2(deriv_2(addend_in_2(exp), var),
                      deriv_2(augend_in_2(exp), var))
    # exponentiation?
    elif is_exponentiation_in(exp):
        return make_product_in_2(exponent_in(exp),
                          make_product_in_2(make_exponentiation_in(base_in(exp), 
                                                                   make_sum_in_2(exponent_in(exp), -1)),
                                            deriv_2(base_in(exp), var)))
    else:
        raise TypeError("unknown expression type -- DERIV-2")




##########################
## in-fix notation without requiring parentheses
# sum constructor and selectors
def make_sum_in_2(a1, *rest):
    """
    constructor for sum
    in-fix
    parentheses not required
    """
    def helper(a, alist, acc):
        """
        a: first argument
        alist: scheme list; rest of arguments
        acc: sum result so far
        """
        if isNull(alist):
            if isNumber(a) and a == 0:
                return acc
            elif isNumber(acc) and acc == 0:
                return a
            elif isNumber(a) and isNumber(acc):
                return a + acc
            elif isList(acc):
                return append(acc, List(plus, a))
            else:
                return append(List(acc), List(plus, a))
        else:
            # more arguments in alist to process
            p = car(alist)
            q = cdr(alist)
            if isNumber(a) and a == 0:
                return helper(p, q, acc)
            elif isNumber(acc) and acc == 0:
                return helper(p, q, a)
            elif isNumber(a) and isNumber(acc):
                return helper(p, q, acc + a)
            else:
                if isList(acc):
                    return helper(p, q, append(acc, List(plus, a)))
                else:
                    return helper(p, q, append(List(acc), List(plus, a)))

    rests = List(*rest)
    return helper(a1, rests, 0)

def is_sum_in_2(x):
    return memq(plus, x)

def addend_in_2(s):
    """
    in-fix notation
    returns the addend
    """
    return head_of_list(plus, s)

def augend_in_2(s):
    """
    in-fix notation
    returns the augend
    """
    return tail_of_list(plus, s)

# product constructor and selectors
def make_product_in_2(m1, *rest):
    def helper(a, alist, acc):
        if isNumber(a) and a == 0:
            return 0
        elif isNull(alist):
            if isNumber(a) and a == 1:
                return acc
            elif isNumber(acc) and acc == 1:
                return a
            elif isNumber(a) and isNumber(acc):
                return a * acc
            elif isList(acc):
                return append(acc, List(mult, a))
            else:
                return append(List(acc), List(mult, a))
        else:
            # more terms to process
            p = car(alist)
            q = cdr(alist)
            if isNumber(a) and a == 1:
                return helper(p, q, acc)
            elif isNumber(acc) and acc == 1:
                return helper(p, q, a)
            elif isNumber(a) and isNumber(acc):
                return helper(p, q, a*acc)
            else:
                if isList(acc):
                    return helper(p, q, append(acc, List(mult, a)))
                else:
                    return helper(p, q, append(List(acc), List(mult, a)))

    rests = List(*rest)
    return helper(m1, rests, 1)

def is_product_in_2(exp):
    if memq(plus, exp):
        return False
    else:
        return memq(mult, exp)

def multiplier_in_2(p):
    return head_of_list(mult, p)

def multiplicand_in_2(p):
    return tail_of_list(mult, p)

def is_exponentiation_in(x):
    if memq(plus, x):
        return False
    elif memq(mult, x):
        return False
    else:
        return memq(power, x)

def make_exponentiation_in(b, exponent):
    if isNumber(exponent) and exponent == 0:
        return 1
    elif isNumber(exponent) and exponent == 1:
        return b
    elif isNumber(b) and b == 0:
        return 0
    elif isNumber(b) and isNumber(exponent):
        return b ** exponent
    else:
        return List(b, power, exponent)

def base_in(expr):
    return car(expr)

def exponent_in(expr):
    return caddr(expr)

### some useful procedures

def tail_of_list(sep, s):
    """
    sep: separator (i.e., '+, '*)
    returns all symbols after the separator, in a list
    """
    # a = (+ x ... ) or False
    a = memq(sep, s)
    #if not a:
    #    return EmptyList
    #if isNull(cdr(a)):
    #    # list ends in separator
    #    return EmptyList
    if not isPair(cddr(a)):
        # return number or symbol
        return cadr(a)
    else:
        # return expression as a list
        return cdr(a)

def head_of_list(sep, s):
    """
    sep: separator
    returns all symbols before the separator, in a list
    """
    def helper(sep, alist, s):
        """
        s: the input list
        alist: accumulates all the symbols before the separator
        """
        if isNull(s) or not isList(s):
            return EmptyList
        elif isEq(car(s), sep) == TRUE:
            if isNull(cdr(alist)):
                # return a number or symbol
                return car(alist)
            else:
                # return a list expression
                return alist
        else:
            a = car(s)
            if isList(a):
                return helper(sep, append(alist, a), cdr(s))
            else:
                return helper(sep, append(alist, List(a)), cdr(s))

    return helper(sep, EmptyList, s)

###########################
if __name__ == "__main__":

    x = Symbol("x")
    y = Symbol("y")


    assert(is_sum_in_2(multiplicand_in_2(make_product_in_2(2, make_sum_in_2(x, y, 3)))))
    assert(is_exponentiation_in(make_exponentiation_in(x, 5)))
    #s = make_exponentiation_in(make_sum_in_2(x, y, 3), 2)
    #s = make_product_in_2(x, 2)
    #s = make_sum_in_2(make_sum_in_2(x, y, 3), make_sum_in_2(x, make_sum_in_2(x, y)))
    #s = make_product_in_2(make_sum_in_2(x, y, 3), 2)
    s = make_product_in_2(make_product_in_2(x, 5), make_product_in_2(x, 5), make_product_in_2(x, 7))


    #s = make_sum_2(1, 2, 3) # 0
    s = make_sum_2(0, make_sum_in_2(x, y, 5, 2)) # 1
    #s = augend_2(s)
    #s = addend(s)
    
    #s = make_product_in_2(1, 2, 3, 4, make_sum_in_2(Symbol("a"), 5)) # 0

    #s = multiplicand_2(s)
    #s = multiplier(s)

    d = deriv_2(s, x)
    try:
        pprint(d)
    except:
        print(d)
