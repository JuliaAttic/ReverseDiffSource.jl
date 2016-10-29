#########################################################################
#
#   Bi-directional Dict()
#
#     - enforces unicity of keys and values
#     - provides lookup by values
#
#########################################################################


import Base: setindex!, getindex, haskey, delete!,
  keys, values, start, next, done


type BiDict{K,V}
    kv::Dict{K,V}
    vk::Dict{V,K}

    BiDict() = new(Dict{K,V}(), Dict{V,K}())

    function BiDict(ks, vs)
        n = length(ks)
        length(unique(ks)) != n && error("Duplicate keys")
        length(unique(vs)) != n && error("Duplicate values")
        h = BiDict{K,V}()
        for i=1:n
              h.kv[ks[i]] = vs[i]
              h.vk[vs[i]] = ks[i]
        end
        return h
    end

    function BiDict(d)
        n = length(d)
        vs = values(d)
        length(unique(vs)) != n && error("Duplicate values")
        h = BiDict{K,V}()
        for (k,v) in d
              h.kv[k] = v
              h.vk[v] = k
        end
        return h
    end
end

# BiDict()                                                = BiDict{Any,Any}()
# BiDict{K,V}(ks::AbstractArray{K}, vs::AbstractArray{V}) = BiDict{K,V}(ks,vs)
# BiDict(ks, vs)                                          = BiDict{Any,Any}(ks, vs)
# BiDict{K,V}(d::Dict{K,V})                               = BiDict{K,V}(d)
# BiDict(d)                                               = BiDict{Any,Any}(d)

# use first dict (kv) by default for base functions
function setindex!(bd::BiDict, v, k)
    k2 = get(bd.vk, v, nothing)  # existing key for v ?
    if k2 != nothing
        delete!(bd.kv, k2)
        delete!(bd.vk, v)
    end

    v2 = get(bd.kv, k, nothing)  # existing value for k ?
    if v2 != nothing
        delete!(bd.kv, k)
        delete!(bd.vk, v2)
    end

    bd.kv[k] = v
    bd.vk[v] = k
end

function delete!(bd::BiDict, k)
    !haskey(bd.kv, k) && error("unknown key $k")
    v = bd.kv[k]
    delete!(bd.kv, k)
    delete!(bd.vk, v)
end

length(bd::BiDict)      = length(bd.kv)
haskey(bd::BiDict, k)   = haskey(bd.kv, k)
keys(bd::BiDict)        = keys(bd.kv)
values(bd::BiDict)      = values(bd.kv)

start(bd::BiDict)       = start(bd.kv)
next(bd::BiDict, i)     = next(bd.kv, i)
done(bd::BiDict, i)     = done(bd.kv, i)

getindex(bd::BiDict, k) = bd.kv[k]
