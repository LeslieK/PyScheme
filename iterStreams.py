"""
This module has code for 3.59 and 3.60
3.60 uses the method of lining up streams in a grid and noticing the pattern in each row
"""


from pairs import stream_car, stream_cdr, cons_stream, stream_map, \
    stream_filter, stream_ref, display_stream, add_streams, \
    integers_starting_from, isDivisible, the_empty_stream, integers

def scale_stream(s, factor):
    """
    produces a stream with each item in s scaled by factor
    """
    return stream_map(lambda x: x * factor, s)

def mul_streams(s1, s2):
    """
    produces the element-wise product of s1 and s2
    """
    return stream_map(lambda x, y: x * y, s1, s2)

primes = cons_stream(2, lambda:stream_filter(isPrime, integers_starting_from(3)))

def isPrime(n):
    """
    returns True if n is prime
    false otherwise
    is n divisible by a prime <= sqrt(n)?
    """
    def iter(ps):
        """
        iterates over a stream of primes to determine 
        if n is prime
        """
        if stream_car(ps) ** 2 > n:
            return True
        elif isDivisible(n, stream_car(ps)):
            return False
        else:
            return iter(stream_cdr(ps))

    return iter(primes)

def partial_sums(s):
    """
    produces a stream of partial sums
    """
    return cons_stream(stream_car(s), lambda: add_streams(partial_sums(s), stream_cdr(s)))

def merge(s1, s2):
    """
    produces a merged stream
    s1: ordered stream
    s2: ordered stream
    """
    if s1 == the_empty_stream:
        return s2
    elif s2 == the_empty_stream:
        return s1
    else:
        i = stream_car(s1)
        j = stream_car(s2)
        if i < j:
            return cons_stream(i, lambda: merge(stream_cdr(s1), s2))
        elif j < i:
            return cons_stream(j, lambda: merge(s1, stream_cdr(s2)))
        else:
            return cons_stream(i, lambda: merge(stream_cdr(s1), stream_cdr(s2)))

def expand(num, den, radix):
    """
    produces a stream of integers
    """
    q, r = divmod(num * radix, den)
    return cons_stream(q, lambda: expand(r, den, radix))

### Ex. 3.59 (a) ################
def integrate_series(s):
    """
    produces a stream of coeff in the integral of the power series
    input: a0 + a1(x) + a2(x**2) + a3(x**3) ...
    output: a0/1, (a1)/2, (a2)/3, ... (does not incl leading constant)
    """
    return stream_map(lambda x, y: x / y, s, integers)

### Ex. 3.59 (b) ################
exp_series = cons_stream(1, lambda: integrate_series(exp_series))
cosine_series = cons_stream(1, lambda: stream_map(lambda x: -x, integrate_series(sine_series)))
sine_series = cons_stream(0, lambda: integrate_series(cosine_series))

def add_series(s1, s2):
    """
    produce a power series that is the sum of s1 and s2
    """
    return add_streams(s1, s2)

######### Ex. 3.60 ##############
def mul_series(s1, s2):
    """
    produces the product of 2 power series
    """
    a0 = stream_car(s1)
    a1 = stream_ref(s1, 1)
    b0 = stream_car(s2)
    b1 = stream_ref(s2, 1)
    return cons_stream(a0 * b0, 
                       lambda: add_streams(
                                           add_streams(
                                                       scale_stream(stream_cdr(s1), b0), 
                                                       scale_stream(stream_cdr(s2), a0)),
                                           cons_stream(0, lambda: mul_series(stream_cdr(s1), stream_cdr(s2)))))

if __name__ == "__main__":

    

    factorials =  cons_stream(1, lambda: mul_streams(factorials, integers_starting_from(2)))
    double = cons_stream(1, lambda: scale_stream(double, 2))

    S = cons_stream(1, lambda: merge(merge(S2, S3), S5))
    S2 = scale_stream(S, 2)
    S3 = scale_stream(S, 3)
    S5 = scale_stream(S, 5)

    fibs = cons_stream(0, lambda: cons_stream(1, lambda: add_streams(stream_cdr(fibs), fibs)))
    partials = partial_sums(integers)
    digits = expand(4, 5, 10)
    m = merge(digits, fibs)

    one = add_streams(mul_series(sine_series, sine_series), mul_series(cosine_series, cosine_series))

    display_stream(one, 15)
    one



