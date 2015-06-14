"""
This module defines Scheme primitives.
"""
from functools import reduce

class Symbol(object):
    def __init__(self, name):
        self.name = name

    def __repr__(self):
        return self.name

#Symbol = namedtuple('Symbol', 'name')
PRIMITIVE = Symbol("primitive")
TRUE = Symbol("#t")
FALSE = Symbol("#f")
QUOTE = Symbol("QUOTE")
CAR = Symbol("car")
CDR = Symbol("cdr")
CDDR = Symbol("cddr")
CDDDR = Symbol("cdddr")
CADDDR = Symbol("cadddr")
CADR = Symbol("cadr")
CADDR = Symbol("caddr")
CAADR = Symbol("caadr")
CDADR = Symbol("cdadr")
CADR = Symbol("cadr")
CONS = Symbol("cons")
ISNULL = Symbol("isNull")
ISPAIR = Symbol("pair?")
ISSYMBOL = Symbol("symbol?")
ISNUMBER = Symbol("number?")
ISSTRING = Symbol("string?")
ISQUOTED = Symbol("quoted?")
SET_CAR = Symbol("set_car")
SET_CDR = Symbol("set_cdr")
LIST = Symbol("List")
PLUS = Symbol("+")
MINUS = Symbol("-")
MULT = Symbol("*")
DIV = Symbol("/")
GT = Symbol(">")
LT = Symbol("<")
LE = Symbol("<=")
EQUALTO = Symbol("=")
ISEQ = Symbol("eq?")
ISEQUAL = Symbol("equal?")
OR = Symbol("or")
AND = Symbol("and")
MAPP = Symbol("mapp")
MAPP_S = Symbol("map_for_scheme")
LOGNOT = Symbol("logical_not")
ASSOC = Symbol("assoc")


EmptyList = "'nil"
isNull = lambda x: x is EmptyList
quote = lambda x: List(QUOTE, x)



def isNumber(x):
    return type(x) in (int, float)

def isString(x):
    return isinstance(x, str)

def isBool(x):
    #return type(x) is type(True)
    return x == FALSE or x == TRUE

def isSymbol(x):
    """
    represented in python as a quoted str
    namedtuple Symbol: (QUOTE symbol_name)
    x = Symbol("x") => x is a Symbol
    """
    return isinstance(x, Symbol)

def memq(x, alist):
    if isNull(alist):
        return False
    if id(x) == id(car(alist)):
        return alist
    else:
        return memq(x, cdr(alist))

def cons(x, y):
    first = x
    second = y

    def set_x(v):
        nonlocal first
        first = v

    def set_y(v):
        nonlocal second
        second = v

    def pprint():
        if isPair(first):
            print("(", end=" ")
            first('pprint')     
        else:
            print(first, end=" ")
        if isPair(second):
            second('pprint')
        else:
            if not isNull(second):
                print(second, ")", end=" ")
            else:
                print(")", end=" ")
            #print() # need this line for output to print; but I don't want a new line!

    def dispatch(m):
        if m == 'car':
            return first
        elif m == 'cdr':
            return second
        elif m == 'set_car':
            return set_x
        elif m == 'set_cdr':
            return set_y
        elif m == 'pair?':
            return True
        elif m == 'pprint':
            return pprint()
        else:
            raise ValueError("pair cannot " + str(m) + " -- cons")

    return dispatch

def List(*args):
    if len(args) == 0:
        return EmptyList
    else:
        pair = EmptyList
        for i in args[::-1]:
            pair = cons(i, pair)
        return pair

def isPair(p):
    try:
        return p('pair?')
    except:
        return False

def isList(x):
    if isNull(x):
        return True
    elif not isPair(x):
        return False
    else: 
        try:
            #return isPair(cdr(x))
            return isList(cdr(x))
        except:
            return False

def car(p):
    try:
        return p('car')
    except TypeError:
        raise TypeError("pair has no car -- car")

def cdr(p):
    try:
        return p('cdr')
    except TypeError:
        raise TypeError("pair has no cdr -- cdr")

def cddr(p):
    return cdr(cdr(p))

def cdddr(p):
    return cdr(cdr(cdr(p)))

def caar(p):
    return car(car(p))

def cadddr(p):
    return car(cdr(cdr(cdr(p))))

def cadr(p):
    return car(cdr(p))

def caddr(p):
    return car(cdr(cdr(p)))

def caadr(p):
    return car(car(cdr(p)))

def cdadr(p):
    return cdr(car(cdr(p)))

def list_ref(items, n):
    """
    return nth item in list
    """
    if n == 0:
        return car(items)
    else:
        return list_ref(cdr(items), n - 1)


def append(list1, list2):
    """
    return list1 + list2
    """
    if isNull(list1):
        return list2
    return cons(car(list1), append(cdr(list1), list2))

def set_car(pair, value):
    """
    set the car of the pair to value
    """
    return pair('set_car')(value)

def set_cdr(pair, value):
    """
    set the cdr of the pair to value
    """
    return pair('set_cdr')(value)

def set_append(list1, list2):
    """
    mutate list1 to point to list2
    """
    set_cdr(last_pair(list1), list2)
    return list1

def pprint(exp):
    if not isPair(exp):  # 5/3/2015 added this if statement
        print(exp)
        return quote("ok")
    try:
        print("(", end=" ")
        exp('pprint')
        print()
        return quote("ok")
    except:
        raise TypeError("unknown type for pprint")
    

###################
# Useful functions

def length(items):
    """
    returns the length of list
    """
    def len_iter(a, count):
        if isNull(a):
            return count
        else:
            return len_iter(cdr(a), count + 1)
    if isList(items):
        return len_iter(items, 0)
    elif isPair(items):
        return 2
    else:
        raise ValueError("input must be a list -- length")

def last_pair(alist):
    """
    returns a list that contains only the last element
    """
    if isNull(alist):
        return EmptyList
    elif isNull(cdr(alist)):
        return alist
    else:
        return last_pair(cdr(alist))

def reverse(alist):
    """
    returns a list in reverse order
    """
    def rev_iter(alist, acc):
        if isNull(alist):
            return acc
        else:
            return rev_iter(cdr(alist), cons(car(alist), acc))
    return rev_iter(alist, EmptyList)

def mapp(proc, alist):
    """
    alist: a scheme list
    returns a new list, after applying proc to each item in input list
    works for proc written as a scheme primitive or as a python lambda proc
    does not work for proc written as a compound scheme proc
    """
    if isNull(alist):
        return EmptyList
    else:
        return cons(proc(car(alist)), mapp(proc, cdr(alist)))

def scale_list(alist, factor):
    return mapp(lambda x: x * factor, alist)

def convertToPythonList(cons_list):
    pylist = []
    n = length(cons_list)
    for i in range(n):
        pylist.append(list_ref(cons_list, i))
    return pylist

def accumulate(op, initial, seq):
    if isNull(seq):
        return initial
    else:
        return op(car(seq), accumulate(op, initial, cdr(seq)))

def flatmap(proc, seq):
    return accumulate(append, EmptyList, mapp(proc, seq))


def for_each(proc, alist):
    if isNull(alist):
        return EmptyList
    else:
        return cons(proc(car(alist)),
                    for_each(proc, cdr(alist)))

def display_list(alist):
    print("(", end=" ")
    for_each(lambda x: print(x, end=" "), alist)
    print(")")
    return

def make_cycle(x):
    """
    set the last pair to point to the first pair
    """
    return set_cdr(last_pair(x), x)

def count_pairs(x):
    """
    x: a pair
    returns number of unique pairs
    """
    def helper(x, marked):
        if not isPair(x):
            return 0
        elif memq(x, marked):
            return 0
        else:
            if isNull(marked):
                marked = List(x)
            else:
                marked = set_append(List(x), marked)
            return helper(car(x), marked) + helper(cdr(x), marked) + 1
    return helper(x, EmptyList)

def copy(alist):
    """
    returns a copy of the input list
    """
    if isNull(alist):
        return EmptyList
    else:
        return cons(car(alist), copy(cdr(alist)))

# Primitive Operators

def plus(*args):
    """
    args: arbitrarily long list of args
    note: reduce requires an iterable (i.e., args)
    """
    if len(args) < 2:
        raise ValueError("too few args -- plus")
    return reduce(lambda x, y: x + y, args, 0)

#def minus(*args):
#    if len(args) < 2:
#        raise ValueError("too few args -- minus")
#    return reduce(lambda x, y: x - y, args, 0)

def minus(x, y):
    return x - y

def mult(*args):
    if len(args) < 2:
        raise ValueError("too few args -- mult")
    return reduce(lambda x, y: x * y, args, 1)

def divide(*args):
    if len(args) < 2:
        raise ValueError("too few args -- divide")
    try:
        #reduce(lambda x, y: x / y, args, 1)
        return args[0] / args[1]
    except ZeroDivisionError("divide"):
        raise ZeroDivisionError

def greaterthan(*args):
    if len(args) < 2:
        raise ValueError("too few args -- greaterthan")
    #return reduce(lambda x, y: x > y, args)
    if args[0] > args[1]:
        return TRUE
    return FALSE

def lessthan(*args):
    if len(args) < 2:
        raise ValueError("too few args -- lessthan")
    #return reduce(lambda x, y: x < y, args)
    if args[0] < args[1]:
        return TRUE
    return FALSE

def lessthanequalto(x, y):
    if x > y:
        return FALSE
    return TRUE

def equalto(*args):
    """
    tests equality of numbers
    =
    """
    if len(args) < 2:
        raise ValueError("too few args -- equalto")
    #return reduce(lambda x, y: x == y, args)
    if args[0] == args[1]:
        return TRUE
    return FALSE

def isEq(x, y):
    """
    tests equality of 2 symbols
    eq?
    """
    if isList(x) or isList(y):
        return isEqual(x, y)
    if isSymbol(x) and isSymbol(y):
        if x.name == y.name:
            return TRUE
        return FALSE
    if isString(x) and isString(y):
        if x == y:
            return TRUE
        return FALSE
    if isNumber(x) and isNumber(y):
        if x == y:
            return TRUE
        return FALSE
    else:
        return FALSE
    
def isEqual(x, y):
    """
    tests equality of a list of symbols
    same elements in same order
    equal?
    """
    if isNull(x) and isNull(y):
        return TRUE
    elif not (isList(x) and isList(y)):
        return FALSE
    elif length(x) != length(y):
        return FALSE
    else:
        # car(x) or car(y) might be a list 
        # isEq must check for this
        if isEq(car(x), car(y)) == TRUE and isEqual(cdr(x), cdr(y)) == TRUE:
            return TRUE
        return FALSE

def or_lispy(*args):
    if len(args) < 2:
        raise ValueError("Too few args -- or_lispy")
    return reduce(lambda x, y: x or y, args, 0)

def and_lispy(*args):
    if len(args) < 2:
        raise ValueError("Too few args -- and_lispy")
    return reduce(lambda x, y: x or y, args, 1)

def logical_not(exp):
    if isEq(exp, FALSE):
        return TRUE
    else:
        return FALSE

def assoc(key, records):
    """
    records: a list of pairs
    """
    if isNull(records):
        return FALSE
    if isEq(key, caar(records)) == TRUE:
        return car(records)
    else:
        return assoc(key, cdr(records))
########################
if __name__ == "__main__":
    k = Symbol("k")
    x = cadr(List(k, 2))
    x

    a = List(1, 2, List(3, 4), List(6, 7), 5)
    a = List(List(1, 2), 3, List(4, 5))
    pprint(a)

    

    assert(not isList(1))
    a = append(EmptyList, List(1))
    display_list(a)

    p = cons(1, 2)
    b = isPair(p)
    assert(b)
    b1 = isPair(1)
    b2 = isPair(EmptyList)
    p1 = car(p)
    p2 = cdr(p)
    c = isList(p)
    # assert(not c) # should fail
    c
    d = isList(cons(1, cons(2, EmptyList)))
    assert(d)
    d
    d = List()
    assert(isList(d))
    assert(not isPair(d))
    assert(not isPair(EmptyList))
    assert(not isPair(cdr(List(2))))

    # memq tests
    u = List(2, 3)
    v = List(4, 5, List(99))
    a = List(1, u, v, 6, 7)
    assert(memq(6, a))

    # count_pairs
    y = List(1)
    print(count_pairs(y)) # => 1

    y = List(1)
    x = List(y, y)
    print(count_pairs(x)) # => 4

    x = List(y, 1)
    print(count_pairs(x)) # => 3

    y = List(1)
    z = cons(y, y)
    x = cons(z, z)
    print(count_pairs(x)) # => 7

    x = make_cycle(List(1, 2, 3))
    #count_pairs(x) # => infinite

    def set_to_wow(x):
        set_car(car(x), 'wow')
        return x
    









