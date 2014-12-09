#########################################################################
#
#   Generates a GraphViz compatible graph definition
#
#########################################################################
  
function plot(g::ExGraph)

  gshow(n::NConst) = 
    "$(nn[n]) [label=\"$(n.main)\", shape=\"square\", style=filled, fillcolor=\"lightgreen\"];"
  gshow(n::NExt)   = 
    "$(nn[n]) [label=\"$(n.main)\", shape=\"circle\", style=filled, fillcolor=\"orange\"];"
  gshow(n::NCall)  = 
    "$(nn[n]) [label=\"$(n.main)\", shape=\"box\", style=filled, fillcolor=\"lightblue\"];"
  gshow(n::NComp)  = 
    "$(nn[n]) [label=\"$(n.main)\", shape=\"box\", style=filled, fillcolor=\"lightblue\"];"
  gshow(n::NRef)   = 
    "$(nn[n]) [label=\"$(n.main)\", shape=\"rarrow\", style=filled, fillcolor=\"lightblue\"];"
  gshow(n::NDot)   = 
    "$(nn[n]) [label=\"$(n.main)\", shape=\"rarrow\", style=filled, fillcolor=\"lightblue\"];"
  gshow(n::NSRef)  = 
    "$(nn[n]) [label=\"$(n.main)\", shape=\"larrow\", style=filled, fillcolor=\"lightblue\"];"
  gshow(n::NSDot)  = 
    "$(nn[n]) [label=\"$(n.main)\", shape=\"larrow\", style=filled, fillcolor=\"lightblue\"];"
  gshow(n::NIn)    = 
    "$(nn[n]) [label=\"in\", shape=\"box3d\", style=filled, fillcolor=\"pink\"];"

  nn = Dict() # node names for GraphViz
  i = 1
  out = ""
  for n in g.nodes
    if isa(n, NFor)  # FIXME : will fail for nested for loops
      nn[n] = "cluster_$i"
      i += 1
      out = out * """
          subgraph $(nn[n]) { label=\"for $(n.main[1])\" ; 
          color=pink;
        """

      # for n2 in filter(n -> !isa(n, NExt), n.main[2].nodes)
      for n2 in n.main[2].nodes
          nn[n2] = "n$i"
        i += 1
        out = out * gshow(n2)
      end

      out = out * "};"
    else
      nn[n] = "n$i"
      i += 1
      out = out * gshow(n)
    end 
  end

  for n in g.nodes 
    if isa(n, NFor)  # FIXME : will fail for nested for loops
      g2 = n.main[2]
      for n2 in g2.nodes
        if hasnode(g2.exti, n2)
          sym = g2.exti[n2]
          if haskey(g2.exto.vk, sym)
            p = getnode(g2.exto, sym)
            out = out * "$(nn[p]) -> $(nn[n2]) [style=dashed];"
          end
        else  
            for p in n2.parents
              out = out * "$(nn[p]) -> $(nn[n2]);"
            end
        end

        if hasnode(g2.seti, n2)
          sym = g2.seti[n2]
          if hassym(g2.seto, sym)
            p = getnode(g2.seto, sym)
            out = out * "$(nn[p]) -> $(nn[n2]) [style=dashed];"
          end          
        end
      end

    else
        for p in filter(n -> !isa(n, NFor), n.parents)
            out = out * "$(nn[p]) -> $(nn[n]);"
        end
    end 
  end

  for (el, en) in g.seti.vk
      out = out * "n$el [label=\"$el\", shape=\"note\", stype=filled, fillcolor=\"lightgrey\"];"
      out = out * "$(nn[en]) -> n$el [ style=dotted];"
  end

  "digraph gp {layout=dot; $out }"
end