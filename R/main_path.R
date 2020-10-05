#' Pajek file reader: obtains the main path saved from Pajek
#'
#' The main path file will be exported from Pajek. This function
#' reads it back into R as an igraph network built by subsetting
#' the full network.
#'
#' Code is adapted from function *read_net*, from Jonathan H. Morgan (2019)
#' http://mrvar.fdv.uni-lj.si/pajek/R/RMorgan.htm
#'
#' @param fname The path to the Pajek exported file with the main path network.
#' @param net The igraph full network.
#' @return An igraph network with only the main path nodes.
#' @export
read_main_path = function (fname, net) {
  read_net <- function(net_file) {
    net <- readLines(net_file)

    trim <- function (x) gsub("^\\s+|\\s+$", "", x)

    #Determining the length of the resulting nodes file
    vertices <- net[[1]]
    vertices <- strsplit(as.character(vertices),' ')
    vertices <- lapply(vertices, function(x) x[x != ""])
    vertices <- as.numeric(vertices[[1]][[2]])

    #Need to account for the fact that meta-information is in the file
    v_num <- vertices + 1

    #Extracting the nodes list from the network file
    nodes <- net[2:v_num]
    nodes <- trim(nodes)

    nodes <- strsplit(as.character(nodes),' ')
    nodes <- lapply(nodes, function(x) x[x != ""])

    shapes <- nodes
    for (i in seq_along(shapes)){
      shapes[[i]] <- nodes[[i]][-c(1:5)]
    }

    shapes <- lapply(shapes, function(x) paste(x,collapse=" "))

    for (i in seq_along(nodes)){
      nodes[[i]] <- nodes[[i]][1:5]
    }

    nodes <- as.data.frame(matrix(unlist(nodes), nrow = length(nodes), byrow = TRUE), stringsAsFactors = FALSE)
    colnames(nodes) <- c('ID', 'Label', 'x-coord', 'y-coord', 'z-coord')

    shapes <-  as.data.frame(matrix(unlist(shapes), nrow = length(shapes), byrow = TRUE),  stringsAsFactors = FALSE)
    colnames(shapes) <- c('shapes information')

    nodes <- cbind(nodes, shapes)

    #Removing shape information if there no information
    nodes[nodes==""] <- NA
    nodes <- nodes[, colSums(is.na(nodes)) == 0]

    rm(shapes, i)

    #Creating Edges File
    vertices <- (vertices + 1)

    edges <- net[-c(1:vertices)]

    `Tie Type` <- edges[[1]]

    edges <- edges[-c(1)]
    edges <- trim(edges)

    edges <- strsplit(as.character(edges),' ')
    edges <- lapply(edges, function(x) x[x != ""])

    edges <-  as.data.frame(matrix(unlist(edges), nrow = length(edges), byrow = TRUE), stringsAsFactors = FALSE)
    colnames(edges) <- c('Person i', 'Person j', 'Weight')

    #Removing edge color information as it adds little value when importing data
    edges <- edges[-c(4:5)]

    edges$`Tie Type` <- `Tie Type`

    rm(net, `Tie Type`)

    #writing objects to the Global Environment
    return (list(nodes = nodes, edges = edges))
  }

  x = read_net(fname)
  vertices = x$nodes
  ties = x$edges
  rm(x)

  vertices$Node = as.numeric(gsub('v|\"', "", vertices$Label))

  ties$Source = vertices$Node[as.numeric(ties$`Person i`)]
  ties$Target = vertices$Node[as.numeric(ties$`Person j`)]
  main_ties = c(rbind(ties$Source,ties$Target))

  main_path_nodes = igraph::V(net)[vertices$Node]
  main_path_edges = igraph::get.edge.ids(net, vp = main_ties, directed = T)

  main_path = igraph::subgraph.edges(net, main_path_edges, delete.vertices = T)
  main_path
}

#' Build, prepare and save citation network
#'
#' Helper function: it will call other functions to (1) build the citation net,
#' (2) simplify it into an acyclic graph and (3) save it in Pajek format (side effect).
#'
#' @param M A bibliometrix M data frame.
#' @return An igraph citation network.
#' @export
load_citation_net = function (M) {
  cit.net = make_citnet(M)
  pajek.net = make_net_for_pajek(cit.net)
  pajek.net
}

#' Read, convert and save main path
#'
#' Helper function: it will call other functions to (1) read the
#' Pajek exported main path network file (2) save it as a GML
#' network file and (3) save the list of papers in the main path,
#' as well as the full Web of Science record for the papers in
#' the main path.
#'
#' @param pajekfile The path to the Pajek exported main path file (.net).
#' @param M A bibliometrix M data frame.
#' @param NET The full igraph network.
#' @param output_path Path to a directory to save the main path files.
#' @return An bibliometrix data frame with only the main path nodes.
#' @importFrom magrittr %>%
#' @importFrom utils write.table
#' @export
load_main_path = function(pajekfile, M, NET, output_path = ".") {
  main_path = read_main_path(pajekfile, NET)
  igraph::V(main_path)$Year = as.numeric(
    stringr::str_extract(igraph::V(main_path)$name, "[0-9]+")
  )

  igraph::write_graph(main_path,
                      paste0(output_path, "/Citation Main Path.gml"),
                      format = "GML")

  mpPapers = igraph::V(main_path)$name
  mpPapers = c(unlist(purrr::map(mpPapers, stringr::str_split, "---")))
  mpPapers = mpPapers[!duplicated(mpPapers)]

  mpM = M[rownames(M) %in% mpPapers,]

  write.table(mpM %>% dplyr::select(SR_FULL, TI, PY, SO, DI, TC),
              paste0(output_path, "/Main Path Papers List.csv"),
              sep = "\t", row.names = F)

  write.table(mpM,
              paste0(output_path,"/Main Path Papers List Full Records.csv"),
              sep = "\t", row.names = F)

  mpM
}
