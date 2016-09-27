The main function : ``rdiff()``
*******************************

The differentiation function is called ``rdiff()`` and is called with the following parameters::

    rdiff( ex::Expr; outsym::Symbol; order::Int, init... )

Arguments
^^^^^^^^^

:ex: is a Julia Expression containing the code to derive

:outsym: (default = nothing) is the symbol of the variable within ``ex`` containing the expression output (the result whose derivatives are needed). This variable must evaluate to a ``Real``. If not specified, ``outsym`` defaults to ``nothing`` which signals to ``rdiff`` that the last statement is the result of interest for derivation.

:order: (default = 1) is an integer indicating the derivation order (1 for 1st order, etc.). Order 0 is allowed and will produce an expression that is a processed version of ``ex`` with some variables names rewritten and possibly some optimizations.

:init: (multiple keyword arguments) is one or several symbol / DataType pairs used to indicate for which variable a derivative is needed and how they should be interpreted. By default the generated expression will yield the derivative for each variable given unless the variable is listed in the ``ignore`` argument.

:evalmod: (default=Main) module where the expression is meant to be evaluated. External variables and functions should be evaluable in this module.

:debug: (default=false) indicates if ``rdiff`` should dump the graph of the generating expression, instead of returning the expression itself.

:allorders: (default=true) indicates whether to generate the code for all orders up to ``order`` (true) or only the last order.

:ignore: (default=[]) do not differentiate against the listed variables, useful if you are not interested in having the derivative of one of several variables in ``init``.

Output
^^^^^^

An expression which, when evaluated, will return a tuple containing the expression value and the derivative at first, second , etc.. order.


Usage
^^^^^

``rdiff`` takes an expression consisting of a subset of Julia statements ( assignments, getindex, setindex!, for loops, function calls ) and transforms it into a new expression whose evaluation will provide the derivatives at all orders between 0 and the order specified (unless ``allorders`` is false).

The generated expression will attempt to remove all unneeded calculations (e.g.  x + 0) and factorize repeated function calls as much as possible.

All the variables appearing in the ``init`` argument are considered as the expression's arguments and a derivative is calculated for it (and cross derivatives if order is >= 2), *unless they are listed in the ``ignore`` argument*. The other variables, if not defined by the expression, are expected to be top level variables in ``evalmod``. If they are not defined there an error will be thrown.

For orders >= 2 *only a single variable, of type Real or Vector, is allowed*. For orders 0 and 1 variables can be of type Real, Vector or Matrix and can be in an unlimited number::

    julia> rdiff( :(x^3) , x=Float64)  # first order
    :(begin
        (x^3,3 * x^2.0)
        end)

    julia> rdiff( :(x^3) , order=3, x=Float64)  # orders up to 3
    :(begin
            (x^3,3 * x^2.0,2.0 * (x * 3),6.0)
        end)

``rdiff`` runs several simplification heuristics on the generated code to remove neutral statements and factorize repeated calculations. For instance calculating the derivatives of ``sin(x)`` for large orders will reduce to the calculations of ``sin(x)`` and ``cos(x)``::

    julia> rdiff( :(sin(x)) , order=10, x=Float64)  # derivatives up to order 10
    :(begin
            _tmp1 = sin(x)
            _tmp2 = cos(x)
            _tmp3 = -_tmp1
            _tmp4 = -_tmp2
            _tmp5 = -_tmp3
            (_tmp1,_tmp2,_tmp3,_tmp4,_tmp5,_tmp2,_tmp3,_tmp4,_tmp5,_tmp2,_tmp3)
        end)

The expression produced can easily be turned into a function with the ``@eval`` macro::

    julia> res = rdiff( :(sin(x)) , order=10, x=Float64)
    julia> @eval foo(x) = $res
    julia> foo(2.)
    (0.9092974268256817,-0.4161468365471424,-0.9092974268256817,0.4161468365471424,0.9092974268256817,-0.4161468365471424,-0.9092974268256817,0.4161468365471424,0.9092974268256817,-0.4161468365471424,-0.9092974268256817)

When a second derivative expression is needed, only a single derivation variable is allowed. If you are dealing with a function of several (scalar) variables you will have you aggregate them into a vector::

    julia> ex = :( (1 - x[1])^2 + 100(x[2] - x[1]^2)^2 )  # the rosenbrock function
    julia> res = rdiff(ex, x=Vector{Float64}, order=2)
    :(begin
        _tmp1 = 1
        _tmp2 = 2
        _tmp3 = 100.0
        _tmp4 = _tmp1 - x[_tmp1]
        _tmp5 = length(x)
        _tmp6 = zeros(size(x))
        _tmp7 = x[_tmp2] - x[_tmp1] ^ _tmp2
        _tmp8 = zeros((_tmp5,_tmp5))
        _tmp9 = _tmp2 * (_tmp7 * _tmp3)
        _tmp10 = -_tmp9
        _tmp6[_tmp1] = _tmp6[_tmp1] + (_tmp2 * (x[_tmp1] * _tmp10) + -(_tmp2 * _tmp4))
        _tmp6[_tmp2] = _tmp6[_tmp2] + _tmp9
        for _idx1 = _tmp1:_tmp5
            _tmp11 = zeros(size(_tmp6))
            _tmp12 = zeros(size(x))
            _tmp11[_idx1] = _tmp11[_idx1] + 1.0
            _tmp13 = _tmp11[_tmp2]
            _tmp11[_tmp2] = 0.0
            _tmp11[_tmp2] = _tmp11[_tmp2] + _tmp13
            _tmp14 = _tmp2 * _tmp11[_tmp1]
            _tmp15 = _tmp3 * (_tmp2 * (_tmp13 + -(x[_tmp1] * _tmp14)))
            _tmp12[_tmp1] = _tmp12[_tmp1] + ((_tmp10 * _tmp14 + _tmp2 * (x[_tmp1] * -_tmp15)) + -(_tmp2 * -(_tmp11[_tmp1])))
            _tmp12[_tmp2] = _tmp12[_tmp2] + _tmp15
            _tmp8[(_idx1 - 1) * _tmp5 + 1:_idx1 * _tmp5] = _tmp12
        end
        (_tmp4 ^ _tmp2 + 100 * _tmp7 ^ _tmp2,_tmp6,_tmp8)
        end)
    julia> @eval foo(x) = $res
    julia> foo([0.5, 2.])
        (306.5,[-351.0,350.0],
        2x2 Array{Float64,2}:
         -498.0  -200.0
         -200.0   200.0)

``foo(x)`` returns a tuple containing respectively the value of the expression at ``x``, the gradient (a 2-vector) and the hessian (a 2x2 matrix)

Limitations
^^^^^^^^^^^

* The canonical implementation of ``for`` loops derivation in reverse accumulation requires the caching of the complete state of each iteration which makes the generated code complex and memory intensive. The current algorithm uses a simpler approach that limits the kind of loops that can be correctly derived : in short, loops should not have any kind of recursivity in them (the calculations of each iteration should not depend on the calculations of previous iterations)::

    # will work
    for i in 1:n
        a = f(x[i])
        b = a + g(y[i])
        c[i] = b
    end

    # will (probably) not work
    for i in 1:n
        c[i] = f( c[i-1] )
    end

However simple accumulations are an instance of recursive calculations that will work::

        # will work
        for i in 1:n
            a += b[i]    # new a value depends on previous a
        end

* ``for`` loops are limited to a single index. If you have a ``for i,j in 1:10, 1:10`` in your expression you will have to translate it to nested loops as a workaround

* All variables should be type-stable (not change from a scalar to a vector for example).

* Only a limited set of Julia semantics are supported at this stage. Some frequently used statements such as comprehensions, ``if else``, ``while`` loops cannot be used in the expression.

* Mutating functions cannot be used (with the exception of ``setindex!`` and ``setfield!``).
