"""
This module solves problems 3.73 through 3.80.
"""


from pairs import stream_car, stream_cdr, cons_stream, stream_map, \
    stream_filter, stream_ref, display_stream, add_streams, \
    integers_starting_from, isDivisible, the_empty_stream, integers, force

from iterStreams import scale_stream, mul_streams, partial_sums
from consLibrary import cons, car, cdr, append, set_car, set_cdr, set_append

"""
Unfortunately, including delays in procedure calls wreaks havoc 
with our ability to design programs that depend on the order of 
events, such as programs that use assignment, mutate data, or 
perform input or output.

As far as anyone knows, mutability and delayed evaluation do not 
mix well in programming languages, and devising ways to deal with 
both of these at once is an active area of research.
"""

def stream_for_each_pair(proc, ps, count, max_count):
    pair = list(map(stream_car, ps)) # [10, 0]
    if pair == the_empty_stream:
        print('done')
    elif count < max_count:
        print(count, end=" ")
        proc(pair[0], pair[1])
        stream_for_each_pair(proc, list(map(stream_cdr, ps)), count + 1, max_count)
    else:
        return

def display_stream_pairs(s, max_count):
    return stream_for_each_pair(print, s, 0, max_count)


def integral(integrand, initial_val, dt):
    int = cons_stream(initial_val, lambda: add_streams(int, scale_stream(integrand, dt)))
    return int

# Ex. 3.73
def RC(R, C, dt):
    """
    returns a procedure that takes a stream of current (i) values
    and produces a stream of voltage (v) values
    """
    def current_to_voltage(i, v0):
        return add_streams(scale_stream(i, R),
                          integral(scale_stream(i, 1/C), v0, dt))
    return current_to_voltage


# Ex. 3.74
def sign_change_detector(v1, v2):
    if v1 * v2 > 0:
        # same sign
        return 0
    elif v1 < 0:
        # neg to pos
        return 1
    elif v1 > 0:
        # pos to neg
        return -1
    elif v2 >= 0:
        # same sign
        return 0
    else:
        # v1 == 0, v2 < 0
        return -1

sense_data = cons_stream(1, lambda: cons_stream(-2, 
                                                lambda: cons_stream(0, 
                                                                    lambda: cons_stream(0, 
                                                                                        lambda: cons_stream(2,
                                                                                                       lambda: sense_data)))))

zero_crossings = stream_map(sign_change_detector, sense_data, stream_cdr(sense_data))

# Ex. 3.75
# average each value of sense_data with previous value before computing zero_crossings

def make_zero_crossings(input_stream, last_value, last_avpt):
    # sign_change_detector input: takes 2 avpt values
    avpt = (stream_car(input_stream) + last_value) / 2
    return cons_stream(sign_change_detector(avpt, last_avpt),
                       lambda: make_zero_crossings (stream_cdr(input_stream),
                                                    stream_car(input_stream),
                                                    avpt)) 

# Ex. 3.76
def smooth(s):
    return scale_stream(add_streams(s, stream_cdr(s)), 0.5)

def make_zero_crossings(input_stream, smooth):
    s = smooth(input_stream)
    def helper(s1):
        return cons_stream(sign_change_detector(stream_ref(s1, 0), stream_ref(s1, 1)),
                    lambda: helper(stream_cdr(s1)))
    return helper(s)

zero_crossings = make_zero_crossings(sense_data, smooth)

def solve(f, y0, dt):
    """uses the non-delayed integrand version of integral"""
    y = cons_stream(y0, lambda: integral(stream_map(f, y), y0, dt))
    return y

def integral_d(delayed_integrand, initial_value, dt):
    """delayed integrand version"""
    int = cons_stream(initial_value,
                      lambda: add_streams(scale_stream(force(delayed_integrand), dt),
                                          int))
    return int

def solve_d(f, y0, dt):
    """uses delayed_integrand version of integral"""
    y = integral_d(lambda: dy, y0, dt)
    dy = stream_map(f, y)
    return y

# Ex. 3.77

def integral_d2(delayed_integrand, initial_val, dt):
    def helper(delayed_stream):
        s = force(delayed_stream)  # added this
        if s == []:
            return the_empty_stream
        else:
            return integral_d2(lambda: stream_cdr(s),  # added lambda to support feedback
                            stream_car(s) * dt + initial_val,
                            dt)
    return cons_stream(initial_val, lambda: helper(delayed_integrand))

def solve_d2(f, y0, dt):
    """uses delayed_integrand version of integral"""
    y = integral_d2(lambda: dy, y0, dt)
    dy = stream_map(f, y)
    return y

# Ex. 3.78
def solve_2nd(a, b, dy0, y0, dt):
    """
    produces a stream y of solutions to a 2nd order diff eq over time
    """
    y = integral_d(lambda: dy, y0, dt)
    dy = integral_d(lambda: ddy, dy0, dt)
    ddy = add_streams(scale_stream(dy, a), scale_stream(y, b))
    return y

# Ex. 3.79
def solve_2nd_g(f, dy0, y0, dt):
    """
    produces a stream y of solutions to a 2nd order diff eq over time, given f
    y" = f(y', t)
    """
    y = integral_d(lambda: dy, y0, dt)
    dy = integral_d(lambda: ddy, dy0, dt)
    ddy = stream_map(f, dy, y) # first 2 lines primed the pipeline; now we start to run
    return y

# Ex. 3.80
def RLC(R, L, C, dt):
    def helper(iL0, vC0):
        """
        produces a stream of pairs (vC, iL)
        that models the behavior of a RLC circuit
        """
        vC = integral_d(lambda: dvC, vC0, dt)
        iL = integral_d(lambda: diL, iL0, dt)
        dvC = scale_stream(iL, -1/C)
        diL = add_streams(scale_stream(vC, 1/C), scale_stream(iL, -R/L))
        return (vC, iL)
    return helper


#######################


if __name__ == "__main__":

    #RC1 = RC(5, 1, 0.5)


    x = solve_d(lambda y: y, 1, 0.001) # => 969 iterations: 2.62 (1000 iterations: 2.716924)
    x = solve_d2(lambda y: y, 1, 0.001) # => 974 iterations: 2.64 (1000 iterations: 2.716924)
    x = solve_2nd(.5, .5, 1, 1, .001)
    y = solve_2nd_g(lambda x, y: .5*x + .5*y, 1, 1, .001)
    RLCResp = RLC(1, .2, 1, 0.1)
    p = RLCResp(0, 10) # [vC, iL)

    display_stream_pairs(p, 10) # => 961 iterations
    display_stream(x, 950) # => 2.58448255
    display_stream(y, 950) # => 2.58448255
    s = zero_crossings
    display_stream(s, 10)




