"""
using streams to modularize a program without storing state
each stream value represents a state
successive values represents values over time
streams allow you to go back in time; i.e. roll back state!

Ex. 3.81, 3.82
"""
from pairs import integers_starting_from, integers
from iterStreams import stream_map, stream_ref, cons_stream, stream_car, stream_cdr, \
    partial_sums, mul_streams, scale_stream, add_streams
from fractions import gcd
from mutation312 import cesaro_test, rand_update, rand_init, rand
import math, random

ones = cons_stream(1, lambda: ones)

random_numbers = cons_stream(rand_init, lambda: stream_map(rand_update, random_numbers))

cesaro_stream = cons_stream(gcd(int(rand_init), int(stream_ref(random_numbers, 1))) == 1, 
                                lambda: map_successive_pairs(lambda x, y: gcd(x, y) == 1, random_numbers))

def map_successive_pairs(f, s):
    """
    f: function
    s: stream
    """
    return cons_stream(f(stream_car(s), stream_ref(s, 1)), 
                       lambda: map_successive_pairs(f, stream_cdr(stream_cdr(s))))

def monte_carlo(experiment_stream, passed, failed):
    def next(passed, failed):
        """
        generate next probability
        passed, failed: accumulators of 1s and 0s
        """
        return cons_stream(passed / (passed + failed),
                           lambda: monte_carlo(stream_cdr(experiment_stream), 
                                               passed, failed))
    if stream_car(experiment_stream):
        return next(passed + 1, failed)
    else:
        return next(passed, failed + 1)

pi = stream_map(lambda x: math.sqrt(6 / x), monte_carlo(cesaro_stream, 0, 0))

# Ex. 3.81
# stream of requests: generate, reset, reset, generate, ...
def rand2(s):
    """
    s: a stream of requests
    produces a stream of random numbers
    """

    rs = cons_stream(rand_init,
                       lambda:
                       stream_map(lambda x: stream_car(stream_map(rand_update, rs)) if x == 'generate' else rand_init,
                                  s))
    return rs

# Ex. 3.5 Monte Carlo Integration
def monte_carlo(trials, experiment):
    """
    experiment: returns 0 or 1
    produces a fraction: number 1s / number trials (prob of getting a 1)
    """
    def iter(trials_remaining, trials_passed):
        if trials_remaining == 0:
            return trials_passed / trials
        else:
            return iter(trials_remaining - 1, trials_passed + experiment())
    return iter(trials, 0)

def random_in_range(low, high):
    return random.uniform(low, high)

# the experiment

def estimate_integral(P, x1, x2, y1, y2, trials):
    """
    returns estimate of area of circle defined by P(x, y)
    """
    def in_region_test():
        return P(random_in_range(x1, x2), random_in_range(y1, y2))
    return monte_carlo(trials, in_region_test) * (x2 - x1) * (y2 - y1)

def estimate_pi(trials):
    # area = pi * r**2; use unit circle => area = pi
    return estimate_integral(lambda x, y: (x - 1)**2 + (y - 1)**2 <= 1, 0, 2, 0, 2, trials)

# Ex. 3.82
# Redo 3.5 on monte carlo integraion in terms of streams. The stream version of estimate-integral
# will not have an argument trials. Instead, it will produce a stream of estimates based on successively
# more trials.

def monte_carlo(experiment_stream):
    """
    produces a stream of estimates of circle area / rectangle area
    """
    return mul_streams(partial_sums(experiment_stream), stream_map(lambda x: 1 / x, integers))


def estimate_integral(P, x1, x2, y1, y2):
    """
    produces a stream of estimates of area of circle defined by P(x, y)
    each estimate corresponds to an additional trial
    """
    def in_region_test():
        return P(random_in_range(x1, x2), random_in_range(y1, y2))
    def outcome_stream(test):
        return cons_stream(test(), lambda: outcome_stream(test))

    es = outcome_stream(in_region_test)
    return scale_stream(monte_carlo(es), (x2 - x1) * (y2 - y1))

def pi_estimate():
    """
    produce stream of estimates: (unit circle area / area of rectangle) * area of rectangle
    """
    return estimate_integral(lambda x, y: (x - 1)**2 + (y - 1)**2 <= 1, 0, 2, 0, 2)

# functional programming view of time

def stream_withdraw(balance, amount_stream):
    """
    produces a stream of balances
    """
    return cons_stream(balance, 
                       lambda: stream_withdraw(balance - stream_car(amount_stream), stream_cdr(amount_stream)))

#######################
random.seed(rand_init)

if __name__ == "__main__":
    s = cons_stream('generate', 
                    lambda: cons_stream('reset', 
                                        lambda: cons_stream('generate', 
                                                            lambda: cons_stream('generate', 
                                                                                lambda: cons_stream('reset',
                                                                                                    lambda: 'generate')))))

    ### Ex. 3.81
    rs = rand2(s)    
    for i in range(6):
        x = stream_ref(rs, i)
        print(x)
        #x = random.uniform(0, 1) 

    #P = lambda x, y: (x - 1)**2 + (y - 1)**2 <= 1
    #def in_region_test():
    #    return P(random_in_range(0, 2), random_in_range(0, 2))
    #def outcome_stream(test):
    #    return cons_stream(test(), lambda: outcome_stream(test))

    #es = outcome_stream(in_region_test)
    #ps = partial_sums(es)
    #fs = mul_streams(partial_sums(es), stream_map(lambda x: 1 / x, integers))


    #for i in range(100):
    #    print(i + 1, stream_ref(fs, i))

    ### Ex. 3.82
    #pi = estimate_pi(50)
    #pi
    pi = pi_estimate()
    for i in range(60):
       print(i + 1, stream_ref(pi, i))
    pi


