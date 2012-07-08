Minimizing Delta Debugging Algorithm
====================================

Erlang implementation of the minimizing delta debugging algorithm (ddmin) 
as described in [Simplifying and Isolating Failure-Inducing Input](http://www.st.cs.uni-saarland.de/papers/tse2002/tse2002.pdf)
from the fine folks of the software engineering chair of [Saarland University](http://www.st.cs.uni-saarland.de/).

No worries, you don't have to read the paper to understand what's going on.

What does it do?
----------------

In a nutshell, the delta debugging algorithm is supposed to find the minimal difference 
between a passing and a failing test case for a given input. That means having a failing test for a given input 
the DD algorithm will produce a *minimal* test case to reproduce the error.


How does it do it?
------------------

The DD algorithm uses a devide an conquer approach of splitting the input data into smaller chunks and checks if 
a small reproduces the error the same way. Ultimately DD is supposed to find the minimal input to reproduce the error.

Let's look at an example:

    foo([7|_]) -> throw(expected_error);
    foo([_|T]) -> foo(T);
    foo([]) -> done.

This inherently useless function shall serve us for demonstartion purposes. For the input `[1,2,3,4,5,6,7,8]` 
the DD algorithm proceeds the following way by applying `foo` on different input sets:

      step | delta | test case                | test
    ---------------|--------------------------------
       1   |   1   | [1, 2, 3, 4] .  .  .  .  | pass
       2   |   2   |  .  .  .  . [5, 6, 7, 8] | fail
    ---------------|--------------------------------
       3   |   1   |  .  .  .  . [5, 6] .  .  | pass
       4   |   2   |  .  .  .  .  .  . [7, 8] | fail
    ---------------|--------------------------------
       5   |   1   |  .  .  .  .  .  . [7] .  | fail  (minimal input)
    ------------------------------------------------
    result |                           [7] 


How is that different from what QuickCheck/PropEr do?
-----------------------------------------------------

It's not that different. The quickcheck approach lets you write generators for your input data and 
automatically reduces this generated data to find minimal counterexamples that fail your properties. 
But what if your generators were not strong enough to generate a failing case that occurred in production? 

You can use the delta debugging approach on input data that was not generated, e.g. collected from a trace or 
input data you received from a third party source. 

Imagine you implemented a parser and the parser fails on a large input file making it hard to find the root cause of the problem. 
Applying the delta debugging approach lets you find a minimal test case that makes your parser fail, 
hence simplifying the debugging work required to fix the cause of the error. 
Furthermore, the minimal test case can help to improve the quickcheck generators and properties you have defined so far.


How can I use it?
-----------------

When you observe an error 
