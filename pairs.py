"""
This module contains the code to build streams.
It also contains solutions to Exercises 3.66 - 3.72 in SICP
"""
#def scale_stream(s, factor):
#    """
#    produces a stream with each item in s scaled by factor
#    """
#    return stream_map(lambda x: x * factor, s)

def memo_proc(proc):
    already_run = False
    result = False

    def helper():
        nonlocal already_run
        nonlocal result
        if not already_run:
            result = proc()
            already_run = True
            return result
        else:
            return result

    return helper

def force(proc):
    return proc()

def cons_stream(a, b):
    return (a, memo_proc(b))

def stream_car(stream):
    return stream[0]

def stream_cdr(stream):
    return force(stream[1])

the_empty_stream = []
ones = cons_stream(1, lambda: ones)

def integers_starting_from(n):
    return cons_stream(n, lambda: integers_starting_from(n + 1))

def stream_map(proc, *argstreams):
    if argstreams is None:
        return the_empty_stream
    else:
        return cons_stream(proc(*list(map(stream_car, argstreams))), 
                           lambda: stream_map(proc, *list(map(stream_cdr, argstreams))))


def add_streams(s1, s2):
    return stream_map(lambda x, y: x + y, s1, s2)

def stream_ref(s, n):
    if n == 0:
        return stream_car(s)
    else:
        return stream_ref(stream_cdr(s), n - 1)

def stream_for_each(proc, s, count, max_count):
    if stream_car(s) is None:
        print('done')
    elif count < max_count:
        print(count, end=" ")
        proc(stream_car(s))
        stream_for_each(proc, stream_cdr(s), count + 1, max_count)
    else:
        return

def stream_filter_pairs(pred, stream, count):
    if stream == the_empty_stream:
        return [None]

    if pred(stream_car(stream)):
        print(count)
        return cons_stream(stream_car(stream),
                           lambda: stream_filter_pairs(pred, stream_cdr(stream), count + 1))
    else:
        return stream_filter_pairs(pred, stream_cdr(stream), count + 1)

def stream_filter(pred, stream):
    if stream == the_empty_stream:
        return [None]

    if pred(stream_car(stream)):
        return cons_stream(stream_car(stream),
                           lambda: stream_filter(pred, stream_cdr(stream)))
    else:
        return stream_filter(pred, stream_cdr(stream))

def display_stream(s, max_count):
    return stream_for_each(print, s, 0, max_count)

def stream_enum_interval(low, high):
    if low > high:
        return the_empty_stream
    else:
        return cons_stream(low, lambda: stream_enum_interval(low + 1, high))

def interleave(p1, p2):
    """
    return a stream of interleaved pairs
    pair = (p1[i], p2[j])
    index i <= index j 
    """
    if p1 == the_empty_stream:
        return p2
    else:
        return cons_stream(stream_car(p1),
                               lambda: interleave(p2, stream_cdr(p1)))


def pairs(s, t):
    """
    produces pairs (si, tj)
    i <= j
    """
    return cons_stream([stream_car(s), stream_car(t)],
                       lambda: interleave(stream_map(lambda x: [stream_car(s), x], stream_cdr(t)), 
                                          pairs(stream_cdr(s), stream_cdr(t))))



def pairs_by_sum(s, t):
    """
    produces a stream of pairs
    ordered by increasing sum
    """
    return weighted_pairs(s, t, lambda p: p[0] + p[1])

def triples(s, t, u):
    """
    produces a stream of triples (si, tj, uk)
    i <= j <= k
    """
    p = pairs(t, u)
    return cons_stream([stream_car(s)] + stream_car(p),
                       lambda: interleave(stream_map(lambda x: [stream_car(s)] + x, stream_cdr(p)),
                                          stream_map(lambda x: [stream_car(stream_cdr(s))] + x, pairs(stream_cdr(t), stream_cdr(u)))))

def pairsless(s, t):
    """
    produces pairs (si, tj)
    i < j (strictly less than)
    """
    return cons_stream([stream_car(s), stream_car(stream_cdr(t))],
                       lambda: interleave(stream_map(lambda x: [stream_car(s), x], stream_cdr(stream_cdr(t))),
                                          pairsless(stream_cdr(s), stream_cdr(t))))

def pairs3(s, t):
    """
    produces a stream of pairs (si, tj)
    for all i, j (incl i > j)
    """
    return interleave(pairs(s, t),
                       interleave(stream_map(lambda x: [x, stream_car(t)], stream_cdr(s)),
                                          pairs(stream_cdr(stream_cdr(s)), stream_cdr(t))))

def bad_pairs(s, t):
    """
    infinite loop since bad_pairs is not thunked
    """
    return interleave(stream_map(lambda x: [stream_car(s), x], t),
               bad_pairs(stream_cdr(s), stream_cdr(t)))


def triangles(s, t, u):
    """
    generates a stream of pythagorean triples
    """
    return stream_filter(lambda x: (x[0] < x[1]) and (x[0]**2 + x[1]**2 == x[2]**2), triples(s, t, u))

def isDivisible(x, n):
    return x % n == 0

def sieve235(s):
    """
    produces a stream not divisible by 2, 3, or 5
    """
    s235 = stream_filter(lambda x: not isDivisible(x, 2) and not isDivisible(x, 3) and not isDivisible(x, 5), s)
    return cons_stream(1, lambda: s235)

def not_divisible_by_2_3_5(s, t):
    """
    produces a stream not divisible by 2, 3, 5
    ordered by sum 2i + 3j + 5ij
    """
    return weighted_pairs(sieve235(s), sieve235(t), lambda p: 2*p[0] + 3*p[1] + 5*p[0]*p[1])

# Ex. 3.70
 
def weighted_pairs(s, t, weight):
    """
    generates a stream of pairs, ordered by weight
    s: an ordered stream
    t: an ordered stream
    """
    return cons_stream([stream_car(s), stream_car(t)],
                                   lambda: merge_weighted(stream_map(lambda x: [stream_car(s), x], stream_cdr(t)),
                                                          weighted_pairs(stream_cdr(s), stream_cdr(t), weight),
                                                          weight))
  

def merge_weighted(s1, s2, weight):
    """
    produces a stream merged from ordered streams s1, s2
    weight: a procedure
    """
    if s1 == the_empty_stream:
        return s2
    elif s2 == the_empty_stream:
        return s1
    else:
        s1car = stream_car(s1)
        s2car = stream_car(s2)
        w1 = weight(s1car) # s1car is s1car_th item in s1
        w2 = weight(s2car)
        if w1 < w2:
            return cons_stream(s1car, lambda: merge_weighted(stream_cdr(s1), s2, weight))
        elif w2 < w1:
            return cons_stream(s2car, lambda: merge_weighted(s1, stream_cdr(s2), weight))
        else:
            return cons_stream(s1car, lambda: merge_weighted(stream_cdr(s1), s2, weight))


    
# Ex. 3.71 a.
def ramanujan_nums(s, t):
    """
    produces a stream of ramanujan numbers
    i**3 + j**3 is such a number iff (m, n) exists s.t.
    i**3 + j**3 == m**3 + n**3
    """
    weight = lambda p: p[0]**3 + p[1]**3
    wp = weighted_pairs(s, t, weight)
    
    def helper(wp):
        """
        produces the ram numbers
        """
        s0 = stream_car(wp)
        s1 = stream_ref(wp, 1)
        w0 = weight(s0)
        w1 = weight(s1)

        if w0 == w1:
            print([s0, s1])
            return cons_stream(w0, lambda: helper(stream_cdr(stream_cdr(wp))))
        else:
            return helper(stream_cdr(wp))

    return helper(wp)

# Ex. 3.70 b.
def sum_of_squares_3ways(s, t):
    """
    produces a stream of numbers xs s.t.
    x can be written as the sum of 2 squares
    3 different ways: (i, j), (m, n), (q, r)
    """
    weight = lambda p: p[0]**2 + p[1]**2
    wp = weighted_pairs(s, t, weight)

    def helper(wp):
        s0 = stream_car(wp)
        s1 = stream_ref(wp, 1)
        s2 = stream_ref(wp, 2)
        w0 = weight(s0)
        w1 = weight(s1)
        w2 = weight(s2)

        if w0 == w1 == w2:
            print([s0, s1, s2])
            return cons_stream(w0, lambda: helper(stream_cdr(stream_cdr(stream_cdr(wp)))))
        else:
            return helper(stream_cdr(wp))

    return helper(wp)

integers = integers_starting_from(1)

if __name__ == "__main__":

    x = integers
    y = integers
    z = integers
    #y3 = stream_ref(y, 3)
    #y1 = stream_ref(y, 1)
    #z = add_streams(x, y)
    #z3 = stream_ref(z, 3)
    #z3
    #stream_ref(z, 1)
    #stream_ref(ones, 5)
    #stream_ref(ones, 1)

    #display_stream(x)
    #display_stream(interleave(x, y))

    #p = pairs_by_sum(x, y)
    #t = triples(x, y, z)
    #tri = triangles(x, y, z)

    #p = ramanujan_nums(x, y)
    p = sum_of_squares_3ways(x, y)
    display_stream(p, 10)
    #the_pair = stream_filter_pairs(lambda x: [99, 100] == x, y, 0) # count = 0
    #the_pair




