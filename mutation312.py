import random
from fractions import gcd
import math

rand_init = 1
random.seed(rand_init)

def rand(seed=rand_init):
    x = seed

    def helper():
        nonlocal x
        x = rand_update(x)
        return x
    return helper


def rand_update(x):
    return random.uniform(0, 1)

rand = rand()

def estimate_pi(trials):
    return math.sqrt(6 / monte_carlo(trials, cesaro_test))

def cesaro_test():
    return gcd(int(rand()), int(rand())) == 1

def monte_carlo(trials, experiment):

    def iter(trials_remaining, trials_passed):
        if trials_remaining == 0:
            return trials_passed / trials
        else:
            if experiment():
                return iter(trials_remaining - 1, trials_passed + 1)
            else:
                return iter(trials_remaining - 1, trials_passed)
    return iter(trials, 0)

def make_accumulator(init_val):
    """
    an accumulator factory
    """
    sum = init_val
    def helper(a):
        nonlocal sum
        sum = sum + a
        return sum
    return helper

def make_monitored(f):
    count = 0
    def mf(x):
        nonlocal count
        if x == 'how_many_calls':
            return count
        elif x == 'reset_count':
            count = 0
            return count
        else:
            count += 1
            return f(x)
    return mf

def make_account(balance):
    balance = balance

    def withdraw(amount):
        nonlocal balance
        if balance >= amount:
            balance -= amount
            return balance
        else:
            return "Insufficient funds"

    def deposit(amount):
        nonlocal balance
        balance += amount
        return balance

    def dispatch(m):
        if m == 'withdraw':
            return withdraw
        elif m == 'deposit':
            return deposit
        else:
            raise("ValueError")
    return dispatch

# Ex. 3.7
def make_joint(account, password, joint_password):
    """
    returns account with secret=joint_password
    """
    def helper(secret, message):
        if secret == joint_password:
            return account(password, message)

    return helper

# Ex. 3.4
def make_account(balance, password):
    retry_count = 0

    def withdraw(amount):
        nonlocal balance
        if balance >= amount:
            balance -= amount
            return balance
        else:
            return "Insufficient funds"

    def deposit(amount):
        nonlocal balance
        balance += amount
        return balance

    def display_error(amount):
        return "Incorrect password"

    def call_the_cops(amount):
        return "call the cops"

    def dispatch(secret, m):
        nonlocal retry_count
        if secret == password:
            retry_count = 0     # reset retry_count
            if m == 'withdraw':
                return withdraw
            elif m == 'deposit':
                return deposit
            else:
                raise(ValueError)
        else:
            retry_count += 1
            if retry_count > 7:
                return call_the_cops
            return display_error
    return dispatch

# Ex. 3.8
def f(x):
    print(1/x)
    return

#f(0) + f(1) => exception without printing
#f(1) + f(0) => This prints 1.0 before exception
######################################

if __name__ == "__main__":
    #p = estimate_pi(950)

    A = make_accumulator(5)
    s = make_monitored(math.sqrt)

    peter_acc = make_account(100, 'open-sesame')   
    paul_acc = make_joint(peter_acc, 'open-sesame', 'rosebud')
    peter_acc('open-sesame', 'withdraw')(10)
    paul_acc('rosebud', 'deposit')(200)




    

