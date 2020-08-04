make.citnet = function(M) {
  cat("Building edges ...\n")
  histResults = make.hist.citation.net(M, min.citations = 1, sep = ";")
  cat("Making graph ...\n")
  ADJ = as.matrix(histResults$NetMatrix)
  NET = graph_from_adjacency_matrix(ADJ, mode = "directed", diag = F)
  cat("Simplifying network ...\n")
  NET = igraph::simplify(NET, remove.multiple = T, remove.loops = T)
  NET
}

make.net.for.pajek = function (NET) {
  cat("Simplifying network ...\n")
  NET = simplify(NET, remove.multiple = T, remove.loops = T)
  # browser()
  tries = 0
  while (!is_dag(NET) & tries < 10) {
    removed = F
    for (size in 2:4) {
      w = rep(0, 4)
      m = as_adjacency_matrix(NET, sparse = T)
      mm = m
      for (xp in 2:4) {
        mm = mm %*% m
        w[xp] = sum(diag(mm))
      }

      if (w[size] > 0) {
        cat(paste0("Finding citation loops of size ", size," ...\n"))
        loops = find_cycles2(NET, size)

        cat(paste0("Found ", length(loops), " loops.\n"))
        if (length(loops) > 0) {
          cat(paste0("Removing citation loops of size ", size," ...\n"))
          NET = make_loops_into_families(loops, NET)

          NET = igraph::simplify(NET, remove.multiple = T, remove.loops = T)
          NET = delete.vertices(NET, degree(NET) == 0)
          removed = T
          break
        }
      }
    }
    if (!removed) {
      tries = tries + 1
    }
  }

  if (!is_dag(NET)) {
    warning("Warning: network still contains loops (not a DAG).")
  }

  cat("Saving network ...\n")
  write_graph(NET, "./Historical Citation Net.gml", format = "GML")
  write_graph(NET, "./Historical Citation Net.net", format = "pajek")

  NET
}

find_cycles = function(g, size) {
  Cycles = NULL
  if (size == 2) {
    for(v1 in V(g)) {
      if (v1 %% 100 == 0) cat(paste0("Articles analyzed ", v1,"\n"))
      nei1 = neighbors(g, v1, mode="out")
      nei1 = nei1[nei1 > v1]
      for(v2 in nei1) {
        nei2 = neighbors(g, v2, mode="out")
        if (v1 %in% nei2) {
          Cycles = c(Cycles, list(c(v1,v2)))
        }
      }
    }
    Cycles = unique(
      lapply(Cycles, function (x) {
        unique(sort(unlist(x)))
      })
    )
  } else if (size == 3) {
    for(v1 in V(g)) {
      if (v1 %% 100 == 0) cat(paste0("Articles analyzed ", v1,"\n"))
      nei1 = neighbors(g, v1, mode="out")
      nei1 = nei1[nei1 > v1]
      for(v2 in nei1) {
        nei2 = neighbors(g, v2, mode="out")
        nei2 = nei2[nei2 > v2]
        for(v3 in nei2) {
          nei3 = neighbors(g, v3, mode="out")
          if (v1 %in% nei3) {
            Cycles = c(Cycles, list(c(v1,v2,v3)))
          }
        }
      }
    }
    Cycles = unique(
      lapply(Cycles, function (x) {
        unique(sort(unlist(x)))
      })
    )
  } else {
    for(v1 in V(g)) {
      if (v1 %% 100 == 0) cat(paste0("Articles analyzed ", v1,"\n"))
      nei1 = neighbors(g, v1, mode="out")
      nei1 = nei1[nei1 > v1]
      for(v2 in nei1) {
        nei2 = neighbors(g, v2, mode="out")
        nei2 = nei2[nei2 > v2]
        for(v3 in nei2) {
          nei3 = neighbors(g, v3, mode="out")
          nei3 = nei3[nei3 > v3]
          for(v4 in nei3) {
            nei4 = neighbors(g, v4, mode="out")
            if (v1 %in% nei4) {
              Cycles = c(Cycles, list(c(v1,v2,v3,v4)))
            }
          }
        }
      }
    }
    Cycles = unique(
      lapply(Cycles, function (x) {
        unique(sort(unlist(x)))
      })
    )
  }
  Cycles
}

family_attr_comb = function(x) {
  paste(x, collapse = "---")
}

make_loops_into_families = function (loops, g) {
  contraction = 1:vcount(g)
  contracted = numeric(0)

  for (loop in loops) {
    contraction[loop] = loop[1]
    contracted = c(contracted, loop[2:length(loop)])
  }
  g = contract(g, mapping = contraction, vertex.attr.comb = family_attr_comb)
  contracted = contracted[contracted %in% V(g)]
  g = delete.vertices(g, V(g)$name[contracted])

  g
}
