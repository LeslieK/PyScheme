These files implement SCHEME primitives and SCHEME procedures defined in SICP.
My goal was to implement 
symbolic differentiation (SICP 2.3.2), 
streams (SICP 3.5), 
metacircular evaluator (SICP 4.1)

in Scheme, but I was having trouble with MIT Scheme on Windows. So... I decided to implement the primitives in python.

I achieved my goal:
I learned the material in SICP at a deeper level by actually running (and debugging) my solutions to problems. (Learned a lot by debugging!)

consLibrary.py: python primitives
evaluatorLibrary.py: implements the code in SICP 4.1
evaluator4x.py: solutions to problems 4.1, ..., 4.9
iterstreams.py: SICP 3.59, 3.60 (import from pairs.py)
signals.py: SICP 3.73 through 3.80 (imports from pairs.py, iterStreams.py, consLibrary.py)
modularity.py: SICP 3.81, 3.82 (import from pairs.py, iterStreams.py, mutation312.py)
