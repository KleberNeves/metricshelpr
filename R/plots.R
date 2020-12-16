#' Plots the cognitive career of a researcher
#'
#' The cognitive career plot (see "A Bibliometric Reconstruction of Research Trails for Qualitative Investigations of Scientific Innovations") is a bibliographic coupling graph of the papers of a single author over the years. This function receives a bibliometrix dataset with the papers of single author and makes such plot.
#'
#' Periods can be identified (e.g. "before post-doc", "after tenure"). Years which divide periods can be given as a vector. Period names is another vector, with length equal that of the divisions plus one (i.e. if there's one division - length(periods) == 1 - then there's two periods - length(period.names) must be == 2).
#'
#' @param M A bibliometrix dataset
#' @param base.size A base size for the nodes in the plot
#' @param n The maximum number of papers to include
#' @param periods A numeric vector of years to add vertical lines and divide periods
#' @param period.names A vector of names for the periods
#' @param special_papers The name of a logical column in M. If provided, the papers which are TRUE for this column will be highlighted in red.
#' @return A plot of the cogntive career.
#' @importFrom magrittr %>%
#' @export
cognitiveCareerPlot = function(M, base.size = 10, n = 20, periods = NULL, period.names = NULL, special_papers = NULL) {
  # Filter the papers that will appear, if not showing all of them.
  if (nrow(M) > n) {
    # Possibilidade: incluir todas as revisões, porque elas são marcos

    # Picks the most cited for each year.
    topyear = (M %>% dplyr::group_by(PY) %>% dplyr::top_n(1,TC))$DI
    k = min(n, nrow(M)) - length(topyear)

    # Picks the most cited overall
    topall = (M %>% dplyr::filter(!(DI %in% topyear)) %>% dplyr::top_n(k,TC))$DI

    # Picks the special ones
    special_dois = c()
    if (!is.null(special_papers)) {
      special_ones = M[M[[special_papers]],]
      special_dois = special_ones$DI
      special_names = stringr::str_to_upper(special_ones$TI)
    }

    # Filters
    toinclude = unique(c(topyear, topall, special_dois))
    M = M[M$DI %in% toinclude, ]
  }

  # Makes the adjancency matrix and graph for the bibliographic coupling network
  ADJ = as.matrix(bibliometrix::biblioNetwork(M, analysis = "coupling", network = "references", sep = ";", shortlabel = F))
  bsk = igraph::graph_from_adjacency_matrix(ADJ)

  # Creates the layout from graphs, to be able to specify positioning of nodes
  igraph::E(bsk)$weight = igraph::count_multiple(bsk)
  bsk = igraph::simplify(bsk, remove.multiple = T, remove.loops = T)
  layout_m = ggraph::create_layout(bsk, layout = "auto")

  # Extract numeric years
  layout_m$Year = as.numeric(stringr::str_extract(igraph::V(bsk)$name, "[0-9]{4,}"))
  Years = layout_m$Year

  # Defines size of nodes
  # TODO for some reason, setting node.size via aes(size = node.size) from layout_m does not work
  igraph::V(bsk)$node.size = (base.size / 2 + 2 * base.size * M$TC / max(M$TC))

  # Name of nodes is the paper title, truncated at 50 characters
  layout_m$id = stringr::str_to_upper(M$TI)
  layout_m$name.truncated = stringr::str_sub(M$TI, 1, 60)

  # Breaks the label in three lines for longer titles and sets node properties
  layout_m$name.label = ifelse(igraph::V(bsk)$node.size < 0.8 * base.size,
                               "", paste0(
                                 stringr::str_sub(M$TI, 1, 20),"\n",
                                 stringr::str_sub(M$TI, 21, 40),"\n",
                                 stringr::str_sub(M$TI, 41, 60),"\n"
                               )
  )

  # Separates papers in clusters and add cluster data to layout, to use for y pos
  igraph::V(bsk)$id = layout_m$id # will use to check for cluster belonging in the loop below
  dg = igraph::decompose.graph(bsk, mode = "weak")
  layout_m$cluster = 0
  for (k in 1:length(dg)) {
    dec = dg[[k]]
    layout_m$cluster = ifelse(layout_m$id %in% igraph::V(dec)$id, k, layout_m$cluster)
  }

  # Join isolated nodes in a single cluster
  d = layout_m %>% dplyr::group_by(cluster) %>% dplyr::summarise(n = n())
  isolated = (d %>% dplyr::filter(n == 1))$cluster
  layout_m$cluster = ifelse(layout_m$cluster %in% isolated, 0, layout_m$cluster)

  # Sets position of nodes based on cluster and year
  layout_m$x = layout_m$Year
  posy = layout_m %>% dplyr::group_by(Year) %>% dplyr::mutate(py = 1:n()) %>% dplyr::mutate(maxpy = max(py))
  layout_m$py = (posy$py) / (posy$maxpy + 1)
  layout_m$py = layout_m$py + ifelse(layout_m$Year %% 2 == 0, 0.5, 0)
  pheight = max(posy$maxpy) * base.size / 2
  layout_m$y = layout_m$py * pheight + stats::rnorm(nrow(layout_m), 0, layout_m$py * 1.5)
  layout_m$y = layout_m$y - min(layout_m$y)

  if (!is.null(special_papers)) {
    layout_m$contour = ifelse(layout_m$id %in% special_names, 1.5, 0.5)
  }

  # Builds ggplot
  g = ggraph::ggraph(layout_m)

  if (!is.null(periods)) {
    g = g + ggplot2::geom_vline(xintercept = periods, linetype = "dashed", color = "black")
  }

  g = g +
    ggraph::geom_edge_link(aes(edge_alpha = igraph::E(bsk)$weight, width = igraph::E(bsk)$weight,), check_overlap = T, color = "grey") +
    ggraph::geom_node_point(shape = 21, ggplot2::aes(fill = as.factor(cluster), color = as.factor(contour)), size = igraph::V(bsk)$node.size, alpha = 0.8, stroke = layout_m$contour) +
    ggraph::geom_node_text(ggplot2::aes(label = name.label), size = 2,
                           repel = F, color = "black", alpha = 1) +
    ggplot2::scale_x_continuous(breaks = scales::pretty_breaks(n = (max(Years) - min(Years))/2), expand = c(.1, .1)) +
    ggplot2::scale_color_manual(values = c("black", "red")) +
    ggraph::scale_edge_alpha_continuous(range = c(0.2, 1)) +
    ggraph::scale_edge_width_continuous(range = c(0.5, 2.5)) +
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

    panel.background = ggplot2::element_rect(fill = "grey97", color = "grey97"),
    panel.grid.minor.y = ggplot2::element_blank(), panel.grid.major.y = ggplot2::element_blank(),
    panel.grid.minor.x = ggplot2::element_blank(),

    axis.line.y = ggplot2::element_blank(), axis.text.y = ggplot2::element_blank(),
    axis.ticks.y = ggplot2::element_blank(), axis.title.y = ggplot2::element_blank(),

    axis.title.x = ggplot2::element_blank(), axis.line.x = ggplot2::element_blank(),
    axis.text.x = ggplot2::element_text(face = "bold")
  )
}

#' Plots a bibliographic network
#'
#' Wrapper around bibliometrix::biblioNetwork and bibliometrix::networkPlot, to be able to change the appearance of the network.
#'
#' @param M A bibliometrix M dataframe.
#' @param edge_type The type of entity that edges represent (passed to biblioNetwork). Can be "collaboration", "coupling", "co-occurrences" or "co-citation".
#' @param node_type The type of entity that nodes represent (passed to biblioNetwork). Can be "authors", "references", "sources", "countries","keywords", "author_keywords", "titles", or "abstracts".
#' @param n Number of nodes to be included in the plot (the ones with the highest degrees are selected).
#' @param title The title for the plot.
#' @param layout The layout type for the network (passed to ggraph::create_layout). Can be "auto" (default), "dendrogram", "manual", "linear", "matrix", "treemap", "circlepack", "partition", "hive".
#' @param cluster_method The method for clustering (passed to bibliometrix::networkPlot). Can be "none" (default), optimal", "louvain","infomap","edge_betweenness","walktrap", "spinglass", "leading_eigen" or "fast_greedy".
#' @param label_size A numeric vector with the range (min, max) of size for labels, passed to the ggplot2::scale_* function.
#' @param edge_size A numeric vector with the range (min, max) of size for edges, passed to the ggplot2::scale_* function.
#' @param node_size A numeric vector with the range (min, max) of size for nodes, passed to the ggplot2::scale_* function.
#' @param min_degree Minimum number of edges a node must have to be included.
#' @param get_network Whether to return the network as well as the plot. Default is FALSE.
#' @return A ggplot object.
#' @export
plot_biblio_network = function (M, edge_type, node_type, n = 10, title = "", layout = "auto", cluster_method = "none", label_size = 4, edge_size = c(0.2, 1), node_size = c(2,10), min_degree = 1, get_network = FALSE) {
  if (node_type == "countries" & is.null(M$AU_CO)) {
    stop('Missing column AU_CO:\nHINT: Run M = metaTagExtraction(M, Field = "AU_CO", sep = ";")')
  }

  delim = ";"
  if (node_type %in% c("references") & edge_type %in% c("co-citation","coupling")) { delim = ".  " }

  NetMatrix = bibliometrix::biblioNetwork(M, analysis = edge_type, network = node_type, sep = delim, n = n, shortlabel = F)

  # The temp file thing is to suppress the plot printed within networkPlot
  # I want just the network, I'll plot it myself below
  ff = tempfile()
  grDevices::png(filename = ff)
  NetGraph = (bibliometrix::networkPlot(NetMatrix, n = n, degree = min_degree, Title = title, type = layout, size = T, remove.multiple = F, labelsize = label_size, cluster = cluster_method, edgesize = edge_size))$graph
  grDevices::dev.off()
  unlink(ff)

  layout_m = ggraph::create_layout(NetGraph, layout = layout)

  layout_m$label = stringr::str_to_title(layout_m$label)
  layout_m$Cluster = as.factor(layout_m$community)

  p = ggraph::ggraph(layout_m) +
    ggraph::geom_edge_link(ggplot2::aes(width = width, alpha = width), check_overlap = T, color = "grey")

  if (is.null(aes_color)) {
    p = p +
      ggraph::geom_node_point(ggplot2::aes(color = Cluster, size = size), alpha = 0.8)
  } else {
    p = p +
      ggraph::geom_node_point(ggplot2::aes(color = node_color, size = size), alpha = 0.8)
  }

  p = p +
    ggraph::geom_node_text(ggplot2::aes(label = label), size = label_size,
                           repel = F, color = "black", alpha = 1) +
    ggplot2::labs(title = title) +
    ggplot2::scale_size(range = node_size) +
    ggraph::scale_edge_alpha(range = c(0.3, 1)) +
    ggraph::scale_edge_width(range = edge_size) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      panel.grid = ggplot2::element_blank(),
      axis.line = ggplot2::element_blank(),
      axis.title = ggplot2::element_blank(),
      axis.text = ggplot2::element_blank(),
      legend.position = "none"
    )

  if (get_network) {
    list(plot = p, net = NetGraph)
  } else {
    p
  }
}

#' Plots a triangle plot
#'
#' The biomedical triangle plot (see "Identifying translational science within the triangle of biomedicine") is a ternary plot where each corner represents Animal research, Cellular/Molecular research and Human research. This is a way of visualizing translation.
#'
#' This function is generic. The *biomed_triangle_plot* is closer to the plot in the original paper.
#'
#' Before calling this function, you can generate the appropriately formatted data with *codify_triangle_categories* and *count_data_for_triangle*.
#'
#' @param tri_data A dataset with the values for each of the three categories.
#' @param tri_cols The name of the columns in the dataset which represent the categories.
#' @param tri_labs The name of the axes (if NULL, defaults to the same name as the columns).
#' @param plot_translation_axis Whether to plot the line of translation, from AC to H.
#' @param add_density Whether to add a 2d density to the plot, in addition to the points.
#' @return The triangle plot.
#' @export
triangle_plot = function (tri_data, tri_cols, tri_labs = NULL, plot_translation_axis = F, add_density = T) {
  if (is.null(tri_labs)) { tri_labs =  tri_cols }
  p = ggtern::ggtern(data = tri_data) +
    ggplot2::aes_string(x = tri_cols[2], y = tri_cols[1], z = tri_cols[3]) +
    ggplot2::geom_point(size = 2) +
    ggplot2::labs(x = tri_labs[2], y = tri_labs[1], z = tri_labs[3]) +
    ggtern::theme_nogrid_minor()

  if (add_density) {
    p = p + ggtern::geom_density_tern()
  }

  if (plot_translation_axis) {
    p = p + ggtern::geom_Risoprop(value = 0.5, linetype = "dashed", color = "blue")
  }

  p
}

#' Plots a triangle plot
#'
#' Same as *triangle_plot*, except that it assumes the categories are ACH (Animal, Cellular/Molecular and Human) and automatically adds the translation axis.
#'
#' @param tri_data A dataset with the values for each of the three categories.
#' @return The triangle plot.
#' @export
biomed_triangle_plot = function (tri_data) {
  triangle_plot(tri_data, c("A","C","H"), c("Animal","Mol./Cell.","Human"), T, F)
}

#' Generates codes based on MeSH terms
#'
#' Compares a vector of terms (multiple terms separated by semicolons) with a reference table and returns the categories to which the elements belong. The reference table must have two columns, one named Term and another named Category. Categories must be represented by a single letter and each term must belong to a single category.
#'
#' This is intended as a support function for triangle plots, but can be used for any arbitrary classification based on MeSH terms (or other terms).
#'
#' Example:
#' ref_table = data.frame(Term = c("Letter A", "Letter B"), Category = c("A", "B"))
#' codify_triangle_categories(terms = c("Letter B", "Letter A", "Letter A;Letter B"), ref_table)
#' Returns: "B"      "A"         "AB"
#'
#' @param terms A character vector with terms (multiple terms in the same element separated by semicolon).
#' @param ref_table A reference table with terms and their categories.
#' @return A vector with the categories of the terms in each element.
#' @export
codify_triangle_categories = function (terms, ref_table) {
    term_categories = purrr::map_chr(terms, function (x) {
      terms_df = data.frame(Term = unlist(stringr::str_split(x, ";")))
      terms_df = merge(terms_df, ref_table, by = "Term")
      categories = paste0(sort(unique(terms_df$Category)), collapse = "")
      categories
    })
    term_categories
}

#' Counts data for ternary categories
#'
#' Counts the ocurrences of each category (represented by single letters) in a vector of categories (as returned by *codify_triangle_categories*).
#'
#' This is intended as a support function for triangle plots. To plot multiple points on the same triangle plot, run this function on grouped/split datasets
#'
#' @param mesh_categories A vector with categories, as letters (see *codify_triangle_categories*).
#' @param ref_categories A character vector with three elements, the three categories.
#' @return A data frame that can be used for triangle plots.
#' @export
count_data_for_triangle = function (mesh_categories, ref_categories) {
  DF = data.frame(Category = mesh_categories)
  cat_cols = purrr::map_dfc(ref_categories, function (tri_cat) {
    df = data.frame(x = sum(stringr::str_detect(DF$Category, tri_cat)))
    colnames(df) = tri_cat
    df
  })
  cat_cols
}

#' Plots counts as a barplot
#'
#' Plots a table as a bar plot. The first column contains the names, the second contains the counts.
#'
#' @param df A data frame with two columns.
#' @param bar_color The color of the bars.
#' @param ylab The label for the count axis.
#' @param title The title for the plot.
#' @return A ggplot object.
#' @export
plot_counts = function (df, bar_color, ylab, title) {
  colnames(df) = c("CAT","COUNT")
  ggplot2::ggplot(df) +
    ggplot2::aes(x = stats::reorder(CAT, COUNT), y = COUNT) +
    ggplot2::geom_col(fill = bar_color) +
    ggplot2::labs(x = "", y = ylab, title = title) +
    ggplot2::coord_flip()
}

#' Plots RPYS
#'
#' RPYS is the Referenced Publication Years Spectroscopy (see https://doi.org/10.1002/asi.23089).
#' It is a histogram of the years of the cited references for a literature corpus.
#'
#' @param df A bibliometrix M dataset.
#' @param bar_color The color of the bars.
#' @param title The title for the plot.
#' @return A ggplot object.
#' @export
plot_rpys = function (df, bar_color, title) {
  refs = unlist(df$CR %>% stringr::str_split(";"))

  ref_years = data.frame(Year = as.numeric(refs %>% stringr::str_extract(" [0-9]{4}"))) %>%
    dplyr::group_by(Year) %>%
    dplyr::summarise(N = n())

  ggplot2::ggplot(ref_years) +
    ggplot2::aes(x = Year, y = N) +
    ggplot2::geom_col(fill = bar_color) +
    ggplot2::scale_x_continuous(breaks = scales::pretty_breaks(10)) +
    ggplot2::ylim(c(0,1+max(ref_years$N))) +
    ggplot2::labs(x = "Year", y = "Number of articles", title = title)
}

#' Plots a wordcloud of keywords
#'
#' Uses ggwordcloud to plot a word cloud from the ID field from Web of Science (Keywords Plus).
#'
#' @param M A bibliometrix M data frame.
#' @param title The plot title.
#' @param n_words Number of words to include (most frequent ones).
#' @param type Which keywords to use. Can be "keywords" to use the ID field or "mesh" to use the MeshHeadings field (obtained from PubMed beforehand).
#' @param excluded_terms Terms to exclude from the word cloud. Will be converted to uppercase. Default is empty. If you pass "mesh*", it will exclude c("Humans","Male","Female","Adult").
#' @return A ggplot object.
#' @export
plot_keywordcloud = function (M, title, n_words = 70, type = "keywords", excluded_terms = c()) {
  if (type == "keywords") {
    target_col = M$ID
  } else if (type == "mesh") {
    target_col = M$MeshHeadings[M$MeshHeadings != "No Mesh Terms"] %>%
      stringr::str_to_upper()
  }

  if (excluded_terms == "mesh*") {
    excluded_terms = c("Humans", "Male", "Female", "Adult")
  }
  target_col = target_col[!(target_col %in% stringr::str_to_upper(excluded_terms))]

  words = target_col %>%
    stringr::str_split(";") %>%
    unlist() %>%
    data.frame(word = .) %>%
    dplyr::group_by(word) %>%
    dplyr::summarise(freq = n())

  cloud_words = words %>% dplyr::slice_max(freq, n = n_words) %>%
    dplyr::mutate(angle = 90 * sample(c(0, 1), n(), replace = TRUE, prob = c(75, 25)))

  ggplot2::ggplot(cloud_words) +
    ggplot2::aes(label = word, size = freq, angle = angle,
        color = factor(sample.int(10, nrow(cloud_words), replace = TRUE))) +
    ggplot2::scale_size_area(max_size = 20) +
    ggwordcloud::geom_text_wordcloud(eccentricity = 1, shape = "circle", rm_outside = T) +
    ggplot2::theme_minimal() + ggplot2::labs(title = title)
}
