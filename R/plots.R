#' Plots the cognitive career of a researcher
#'
#' The cognitive career plot (REF) is a bibliographic coupling graph of the papers of a single author over the years. This function receives a bibliometrix dataset with the papers of single author and makes such plot.
#'
#' Periods can be identified (e.g. "before post-doc", "after tenure"). Years which divide periods can be given as a vector. Period names is another vector, with length equal that of the divisions plus one (i.e. if there's one division - length(periods) == 1 - then there's two periods - length(period.names) must be == 2).
#'
#' @param M A bibliometrix dataset
#' @param base.size A base size for the nodes in the plot
#' @param n The maximum number of papers to include
#' @param periods A numeric vector of years to add vertical lines and divide periods
#' @param period.names A vector of names for the periods
#' @return A plot of the cogntive career.
#' @importFrom magrittr %>%
#' @export
cognitiveCareerPlot = function(M, base.size = 10, n = 20, periods = NULL, period.names = NULL) {
  # Filter the papers that will appear, if not showing all of them.
  if (nrow(M) > n) {
    # Possibilidade: incluir todas as revisões, porque elas são marcos

    # Picks the most cited for each year.
    topyear = (M %>% dplyr::group_by(PY) %>% dplyr::top_n(1,TC))$DI
    k = min(n, nrow(M)) - length(topyear)

    # Picks the most cited overall
    topall = (M %>% dplyr::filter(!(DI %in% topyear)) %>% dplyr::top_n(k,TC))$DI

    # Filters
    toinclude = unique(c(topyear, topall))
    M = M[M$DI %in% toinclude, ]
  }

  # Makes the adjancency matrix and graph for the bibliographic coupling network
  ADJ = as.matrix(bibliometrix::biblioNetwork(M, analysis = "coupling", network = "references", sep = ";", shortlabel = F))
  bsk = igraph::graph_from_adjacency_matrix(ADJ)

  # Creates the layout from graphs, to be able to specify positioning of nodes
  bsk = igraph::simplify(bsk, remove.multiple = T, remove.loops = T)
  layout_m = igraph::create_layout(bsk, layout = "auto")

  # Extract numeric years
  layout_m$Year = as.numeric(str_extract(V(bsk)$name, "[0-9]{4,}"))
  Years = layout_m$Year

  # Defines size of nodes
  # layout_m$node.size = 10 * (base.size + 2 * base.size * M$TC / max(M$TC))
  # TODO for some reason, setting node.size via aes(size = node.size) from layout_m does not work
  V(bsk)$node.size = (base.size / 2 + 2 * base.size * M$TC / max(M$TC))

  # Name of nodes is the paper title, truncated at 50 characters
  layout_m$id = stringr::str_to_upper(M$TI)
  layout_m$name.truncated = stringr::str_sub(M$TI, 1, 60)

  # Breaks the label in three lines for longer titles and sets node properties
  layout_m$name.label = ifelse(V(bsk)$node.size < 0.8 * base.size,
                               "", paste0(
                                 stringr::str_sub(M$TI, 1, 20),"\n",
                                 stringr::str_sub(M$TI, 21, 40),"\n",
                                 stringr::str_sub(M$TI, 41, 60),"\n"
                               )
  )

  # Separates papers in clusters and add cluster data to layout, to use for y pos
  V(bsk)$id = layout_m$id # will use to check for cluster belonging in the loop below
  dg = igraph::decompose.graph(bsk, mode = "weak")
  layout_m$cluster = 0
  for (k in 1:length(dg)) {
    dec = dg[[k]]
    layout_m$cluster = ifelse(layout_m$id %in% V(dec)$id, k, layout_m$cluster)
  }

  # Join isolated nodes in a single cluster
  d = layout_m %>% group_by(cluster) %>% summarise(n = n())
  isolated = (d %>% filter(n == 1))$cluster
  layout_m$cluster = ifelse(layout_m$cluster %in% isolated, 0, layout_m$cluster)

  # Sets position of nodes based on cluster and year
  layout_m$x = layout_m$Year
  posy = layout_m %>% dplyr::group_by(Year) %>% dplyr::mutate(py = 1:n()) %>% dplyr::mutate(maxpy = max(py))
  layout_m$py = (posy$py) / (posy$maxpy + 1)
  layout_m$py = layout_m$py + ifelse(layout_m$Year %% 2 == 0, 0.5, 0)
  pheight = max(posy$maxpy) * base.size / 2
  layout_m$y = layout_m$py * pheight + rnorm(nrow(layout_m), 0, layout_m$py * 1.5)
  layout_m$y = layout_m$y - min(layout_m$y)

  # Builds ggplot
  g = ggraph::ggraph(layout_m)

  if (!is.null(periods)) {
    g = g + ggplot2::geom_vline(xintercept = periods, linetype = "dashed", color = "black")
  }
  # browser()
  g = g +
    ggraph::geom_edge_link(width = 1, check_overlap = T, edge_alpha = 0.8, color = "grey") +
    ggraph::geom_node_point(aes(color = as.factor(cluster)), size = V(bsk)$node.size, alpha = 0.8) +
    ggraph::geom_node_text(aes(label = name.label), size = 2.2,
                   repel = F, color = "black", alpha = 1) +
    ggplot2::scale_x_continuous(labels = as.character(seq(min(Years), max(Years))), breaks = seq(min(Years), max(Years)), expand = c(.1, .1)) +
    ggplot2::labs(title = "Cognitive Career Plot (Reference Coupling)", x = "", y = "")

  # Adicionar retângulos e nomes de períodos
  if (!is.null(periods)) {
    periods = c(min(Years), periods, c(max(Years)))
    tly = (max(layout_m$y) * 1.15)
    for (i in 1:(length(periods)-1)) {
      g = g +
        ggplot2::annotate("rect", xmin = periods[i], ymin = (tly),
                 xmax = periods[i+1], ymax = (tly*0.98), fill = "grey80", color = "black") +
        ggplot2::annotate("text", label = period.names[i], x = (periods[i] + periods[i+1]) / 2,
                 y = (tly * 0.95), size = 3)
    }
  }

  g + ggplot2::theme_minimal() + ggplot2::theme(
    legend.position = "none",

    panel.background = element_rect(fill = "grey97", color = "grey97"),
    panel.grid.minor.y = element_blank(), panel.grid.major.y = element_blank(),
    panel.grid.minor.x = element_blank(),

    axis.line.y = element_blank(), axis.text.y = element_blank(),
    axis.ticks.y = element_blank(), axis.title.y = element_blank(),

    axis.title.x = element_blank(), axis.line.x = element_blank(),
    axis.text.x = element_text(face = "bold")
  )
}
