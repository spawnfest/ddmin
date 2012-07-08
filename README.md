Minimizing Delta Debugging Algorithm
====================================

Erlang implementation of the minimizing delta debugging algorithm (ddmin) 
as described in [Simplifying and Isolating Failure-Inducing Input](http://www.st.cs.uni-saarland.de/papers/tse2002/tse2002.pdf) (PDF)
from the fine folks of the software engineering chair of [Saarland University](http://www.st.cs.uni-saarland.de/).

No worries, you don't have to read the paper to understand what's going on.

What does it do?
----------------

In a nutshell, the delta debugging algorithm is supposed to find the minimal difference 
between a passing and a failing test case for a given input. That means having a failing test for a given input 
the ddmin will produce a *minimal* test case to reproduce the error.


How does it do it?
------------------

The ddmin uses a devide and conquer approach of splitting input data into smaller chunks and checks if 
a smaller input reproduces the error the same way as a larger does. Ultimately ddmin is supposed to find the minimal input to reproduce the error.

Let's look at an example:

    foo([7|_]) -> throw(expected_error);
    foo([_|T]) -> foo(T);
    foo([]) -> done.

This inherently useless function shall serve us for demonstartion purposes. For the input `[1,2,3,4,5,6,7,8]` 
ddmin proceeds the following way by applying `foo` on different input sets:

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


In order to run ddmin you only have to implement a test function. The test function for the `foo` case could look like:

    TestFun = fun(Circumstances) ->
                try
                  foo(Circumstances),
                  pass
                catch _:expected_error -> fail;
                      _:_ -> unresolved
                end
              end.

During execution ddmin applies `TestFun` to each delta and checks for the smallest input. 
Furthermore, ddmin resizes the chunks in case it cannot find a smaller failing test case.
In the worst case almost all combination of chunks will be exercised but in the best case the 
overall complexity is that of a binary search.

Run ddmin like this:

    > ddmin:ddmin(TestFun, [1,2,3,4,5,6,7,8]).

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

