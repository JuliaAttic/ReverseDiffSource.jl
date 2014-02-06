############################################################################
#  Proto
############################################################################


	import Base.show


	# where the derived functions are to be evaluated : the parent module of Autodiff
    # const parent_mod = Base.module_parent(current_module())
    const parent_mod = Main  # temporarily, for prototyping

	# naming conventions
	const TEMP_NAME = "_tmp"     # prefix of new variables
	const DERIV_PREFIX = "d"   # prefix of gradient variables

	# ## variable symbol sampling functions
	# getSymbols(ex::Any)    = Set{Symbol}()
	# getSymbols(ex::Symbol) = Set{Symbol}(ex)
	# getSymbols(ex::Array)  = mapreduce(getSymbols, union, ex)
	# getSymbols(ex::Expr)   = getSymbols(toExH(ex))
	# getSymbols(ex::ExH)    = mapreduce(getSymbols, union, ex.args)
	# getSymbols(ex::ExCall) = mapreduce(getSymbols, union, ex.args[2:end])  # skip function name
	# getSymbols(ex::ExRef)  = setdiff(mapreduce(getSymbols, union, ex.args), Set(:(:), symbol("end")) )# ':'' and 'end' do not count
	# getSymbols(ex::ExDot)  = Set{Symbol}(ex.args[1])  # return variable, not fields
	# getSymbols(ex::ExComp) = setdiff(mapreduce(getSymbols, union, ex.args), 
	# 	Set(:(>), :(<), :(>=), :(<=), :(.>), :(.<), :(.<=), :(.>=), :(==)) )


	# ## variable symbol subsitution functions
	# substSymbols(ex::Any, smap::Dict)     = ex
	# substSymbols(ex::Expr, smap::Dict)    = substSymbols(toExH(ex), smap::Dict)
	# substSymbols(ex::Vector, smap::Dict)  = map(e -> substSymbols(e, smap), ex)
	# substSymbols(ex::ExH, smap::Dict)     = Expr(ex.head, map(e -> substSymbols(e, smap), ex.args)...)
	# substSymbols(ex::ExCall, smap::Dict)  = Expr(:call, ex.args[1], map(e -> substSymbols(e, smap), ex.args[2:end])...)
	# substSymbols(ex::ExDot, smap::Dict)   = (ex = toExpr(ex) ; ex.args[1] = substSymbols(ex.args[1], smap) ; ex)
	# substSymbols(ex::Symbol, smap::Dict)  = get(smap, ex, ex)

	
	## misc functions
	dprefix(v::Union(Symbol, String, Char)) = symbol("$DERIV_PREFIX$v")
	# dprefix(v::Expr)                        = dprefix(toExH(v))
	# dprefix(v::ExRef)                       = Expr(:ref, dprefix(v.args[1]), v.args[2:end]...)
	# dprefix(v::ExDot)                       = Expr(:., dprefix(v.args[1]), v.args[2:end]...)

	isSymbol(ex)   = isa(ex, Symbol)
	isDot(ex)      = isa(ex, Expr) && ex.head == :.   && isa(ex.args[1], Symbol)
	isRef(ex)      = isa(ex, Expr) && ex.head == :ref && isa(ex.args[1], Symbol)

	## var name generator
	let
		vcount = Dict()
		global newvar
		function newvar(radix::Union(String, Symbol)=TEMP_NAME)
			vcount[radix] = haskey(vcount, radix) ? vcount[radix]+1 : 1
			return symbol("$(radix)$(vcount[radix])")
		end

		global resetvar
		function resetvar()
			vcount = Dict()
		end
	end


##############  ExNode type  #############################

	type ExNode
		nodetype::Symbol
		name
		parents::Vector
		value
	end

	ExNode(typ::Symbol, name) = ExNode(typ, name, ExNode[], NaN)
	ExNode(typ::Symbol, name, parents) = ExNode(typ, name, parents, NaN)

  	function show(io::IO, res::ExNode)
  		pl = join( map(x->string(x.name), res.parents) , " / ")
  		# cl = join( map(x->string(x.name), res.children) , " / ")
        print(io, "[$(res.nodetype)] $(res.name) ($(res.value))")
        length(pl) > 0 && print(io, ", in = $pl")
        # length(cl) > 0 && print(io, ", out = $cl")
	end

	typealias ExNodes Vector{ExNode}

##############  ExGraph type  #############################

	type ExGraph
		nodes::ExNodes
		exitnodes::Dict
	end

	ExGraph() = ExGraph(ExNode[], Dict{Symbol, ExNode}())

##############  Graph functions  ##########################

	function add_node(g::ExGraph, nargs...)
		v = ExNode(nargs...)
		push!(g.nodes, v)
		v
	end

	# add_link(n1, n2) = (push!(n2.parents, n1); nothing )



