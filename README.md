ReverseDiffSource.jl
====================

Expression based Reverse automated differentiation

!! Package under development, expect minor API changes !!



Currently



## Future improvements

- I am trying a different underlying representation of the model, going from the current AST manipulation to a custom graph type. This should make the code easier to debug and improve. This is where most of my eforts are directed rigth now.
- I want to add for loops, comprehension would be next. This has become necessary as the differentiation rules definitions are now parsed to graphs too and they make use of loops.
- Higher order derivatives are also a goal, at least up to 2. This is in fact mostly difficult when the function being derived depends on more than one parameter. I have made small prototypes
