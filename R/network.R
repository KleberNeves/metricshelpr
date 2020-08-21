make.hist.citation.net = function (M, min.citations = 1, sep = ";") {
  min.citations = max(c(1, min.citations))
  M$TC = as.numeric(M$TC)
  M = M[!is.na(M$TC), ]
  if (!("SR_FULL" %in% names(M))) {
    M = bibliometrix::metaTagExtraction(M, Field = "SR")
  }
  M = M[order(M$PY), ]
  M2 = M[M$TC >= min.citations, ]
  if (dim(M2)[1] == 0) {
    cat("\nNo document has a number of citations above the fixed threshold\n")
    return(NULL)
  }
  N = dim(M2)[1]
  N2 = dim(M)[1]
  rows = c(1:N2)
  lCit = Matrix(0, N, N2)

  for (i in 1:N) {
    if (i %% 10 == 0 | i == N) cat("Articles analysed  ", i, "\n")

    # Searches by name
    x = M2$SR_FULL[i]
    Year = M2$PY[i]
    pos = stringr::str_which(M$CR[M$PY >= Year], x)
    pos = rows[M$PY >= Year][pos]

    # Searches by DOI
    # if (!is.na(M2$DI[i])) {
    #   pos2 = stringr::str_which(fixed(M$CR[M$PY >= Year]), M2$DI[i])
    #   pos2 = rows[M$PY >= Year][pos2]
    #   p1 = pos
    #   pos = unique(pos, pos2)
    #   if (length(p1) > 0 & length(pos) > 0) {
    #     if (p1 != pos) { print("AAAAAAAAA")}
    #   }
    # }

    if (length(pos) > 0) {
      lCit[i, pos] = 1
    }
  }

  LCS = rowSums(lCit)
  ind = which(LCS > M2$TC)
  LCS[ind] = M2$TC[ind]
  M2$LCS = LCS
  row.names(lCit) = M2$SR
  colnames(lCit) = M$SR
  lCit = lCit[, (M$SR %in% M2$SR)]
  if (!("DI" %in% names(M2))) {
    M2$DI = NA
  }
  df = data.frame(Paper = M2$SR, DOI = M2$DI, Year = M2$PY,
                  LCS = LCS, GCS = M2$TC, stringsAsFactors = F)
  df = df[order(df$Year), ]
  row.names(df) = paste(df$Year, rep("-", dim(df)[1]), 1:dim(df)[1])
  results = list(NetMatrix = t(lCit), histData = df, M = M2,
                 LCS = LCS)
  return(results)
}

make.citnet = function(M) {
  cat("Building edges ...\n")
  histResults = make.hist.citation.net(M, min.citations = 1, sep = ";")
  cat("Making graph ...\n")
  ADJ = as.matrix(histResults$NetMatrix)
  NET = igraph::graph_from_adjacency_matrix(ADJ, mode = "directed", diag = F)
  cat("Simplifying network ...\n")
  NET = igraph::simplify(NET, remove.multiple = T, remove.loops = T)
  NET
}

make.net.for.pajek = function (NET) {
  cat("Simplifying network ...\n")
  NET = igraph::simplify(NET, remove.multiple = T, remove.loops = T)
  # browser()
  tries = 0
  while (!igraph::is_dag(NET) & tries < 10) {
    removed = F
    for (size in 2:4) {
      w = rep(0, 4)
      m = igraph::as_adjacency_matrix(NET, sparse = T)
      mm = m
      for (xp in 2:4) {
        mm = mm %*% m
        w[xp] = sum(diag(mm))
      }

      if (w[size] > 0) {
        cat(paste0("Finding citation loops of size ", size," ...\n"))
        loops = find_cycles(NET, size)

        cat(paste0("Found ", length(loops), " loops.\n"))
        if (length(loops) > 0) {
          cat(paste0("Removing citation loops of size ", size," ...\n"))
          NET = make_loops_into_families(loops, NET)

          NET = igraph::simplify(NET, remove.multiple = T, remove.loops = T)
          NET = igraph::delete.vertices(NET, igraph::degree(NET) == 0)
          removed = T
          break
        }
      }
    }
    if (!removed) {
      tries = tries + 1
    }
  }

  if (!igraph::is_dag(NET)) {
    warning("Warning: network still contains loops (not a DAG).")
  }

  cat("Saving network ...\n")
  igraph::write_graph(NET, "./Historical Citation Net.gml", format = "GML")
  igraph::write_graph(NET, "./Historical Citation Net.net", format = "pajek")

  NET
}

find_cycles = function(g, size) {
  Cycles = NULL
  if (size == 2) {
    for(v1 in igraph::V(g)) {
      if (v1 %% 100 == 0) cat(paste0("Articles analyzed ", v1,"\n"))
      nei1 = igraph::neighbors(g, v1, mode="out")
      nei1 = nei1[nei1 > v1]
      for(v2 in nei1) {
        nei2 = igraph::neighbors(g, v2, mode="out")
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
    for(v1 in igraph::V(g)) {
      if (v1 %% 100 == 0) cat(paste0("Articles analyzed ", v1,"\n"))
      nei1 = igraph::neighbors(g, v1, mode="out")
      nei1 = nei1[nei1 > v1]
      for(v2 in nei1) {
        nei2 = igraph::neighbors(g, v2, mode="out")
        nei2 = nei2[nei2 > v2]
        for(v3 in nei2) {
          nei3 = igraph::neighbors(g, v3, mode="out")
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
    for(v1 in igraph::V(g)) {
      if (v1 %% 100 == 0) cat(paste0("Articles analyzed ", v1,"\n"))
      nei1 = igraph::neighbors(g, v1, mode="out")
      nei1 = nei1[nei1 > v1]
      for(v2 in nei1) {
        nei2 = igraph::neighbors(g, v2, mode="out")
        nei2 = nei2[nei2 > v2]
        for(v3 in nei2) {
          nei3 = igraph::neighbors(g, v3, mode="out")
          nei3 = nei3[nei3 > v3]
          for(v4 in nei3) {
            nei4 = igraph::neighbors(g, v4, mode="out")
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
  contraction = 1:igraph::vcount(g)
  contracted = numeric(0)

  for (loop in loops) {
    contraction[loop] = loop[1]
    contracted = c(contracted, loop[2:length(loop)])
  }
  g = igraph::contract(g, mapping = contraction, vertex.attr.comb = family_attr_comb)
  contracted = contracted[contracted %in% igraph::V(g)]
  g = igraph::delete.vertices(g, igraph::V(g)$name[contracted])

  g
}
