# Pajek file reader: obtains the main path saved from Pajek
read_main_path = function (fname, net) {
  # Adapted from function *read_net*, from Jonathan H. Morgan (2019) - http://mrvar.fdv.uni-lj.si/pajek/R/RMorgan.htm
  require("igraph")
  read_net <- function(net_file) {
    net <- readLines(net_file)

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

    nodes <-  as.data.frame(matrix(unlist(nodes), nrow = length(nodes), byrow = TRUE), stringsAsFactors = FALSE)
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

  main_path_nodes = V(net)[vertices$Node]
  main_path_edges = get.edge.ids(net, vp = main_ties, directed = T)

  main_path = subgraph.edges(net, main_path_edges, delete.vertices = T)
  main_path
}
