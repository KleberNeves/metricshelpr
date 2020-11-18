#' Reads a GML file exported from VOSViewer
#'
#' This is a haphazard way of bringing VOSViewer clustering data back into R. This is helpful to make networks in VOSViewer and bring it back into R (I'm pretty sure that's what bibliometrix does, but I want to do it visually).
#'
#' It assumes the network is bibliographic coupling of papers.
#'
#' @param gml_file A GML network file, as exported by VOSViewer.
#' @return A data frame with the labels, DOIs and cluster number.
#' @export
read_vos_cluster_gml = function (gml_file) {

  vosnet = igraph::read_graph("net1.gml", format = "gml")

  f = file("net1.gml")
  content = readr::read_lines(f)

  node_lines = stringr::str_which(content, "node")

  labels = content[node_lines+3] %>%
    stringr::str_trim() %>%
    stringr::str_remove("label ") %>%
    stringr::str_remove_all('\"')

  dois = content[node_lines+5] %>%
    stringr::str_trim() %>%
    stringr::str_remove_all('\"') %>%
    stringr::str_extract("doi.org/.+") %>%
    stringr::str_remove("doi.org/")

  clusters = content[node_lines+8] %>%
    stringr::str_trim() %>%
    stringr::str_extract("cluster [0-9]+") %>%
    stringr::str_remove("cluster") %>%
    as.numeric()

  cluster_data = data.frame(
    label = labels,
    DOI = dois,
    voscluster = clusters
  )

  cluster_data
}

#' Writes a Web of Science record file after excluding DOIs
#'
#' This function filters the plain text files with Web of Science records by excluding a designated list of DOIs. It saves the filtered records in the same folder as the original ones, under a subfolder /filtered.
#'
#' This is helpful for when you want to load the records into R, filter, then have them read by another software (like VOSViewer) that can read the WoS format.
#'
#' It will read all .txt files in the folder, remove the records associated with the designated DOIs, as well as remove any records without a DOI field (DI).
#'
#'
#' @param folder The folder with the WoS records, in plain text format. Every .txt file will be read.
#' @param dois_to_exclude The DOIs to be filtered out.
#' @return Saves the filtered files in a subfolder as a side effect. Returns 0.
#' @export
filter_wos_records = function (folder, dois_to_exclude) {
  dois_to_exclude = stringr::str_to_lower(dois_to_exclude)
  wos_files = paste0(folder,"/",list.files(folder, ".txt$"))
  dir.create(paste0(folder,"/filtered"))

  purrr::walk(wos_files, function (fn) {
    f = file(fn)
    lines_to_exclude = c()
    content = readr::read_lines(fn)
    entries = stringr::str_which(content, "^PT")
    for (i in 1:length(entries)) {
      start_i = entries[i]
      start_line = content[start_i]
      j = 0; line = start_line
      found_doi = NA
      while (line != "ER") {
        j = j + 1; line = content[start_i + j]
        if (stringr::str_detect(line, "^DI")) {
          found_doi = stringr::str_extract(line, "10.+")
        }
      }
      end_i = start_i + j + 1
      if (is.na(found_doi) | stringr::str_to_lower(found_doi) %in% dois_to_exclude) {
        lines_to_exclude = c(lines_to_exclude, (start_i:end_i))
      }
    }
    new_content = content[-lines_to_exclude]
    records_left = length(stringr::str_which(new_content, "^PT"))
    if (records_left > 0) {
      print(glue::glue("Filtered '{basename(fn)}'!"))
      readr::write_lines(new_content, path = paste0(folder,"/filtered/", basename(fn)))
    } else {
      print(glue::glue("No records left after filtering in file: '{basename(fn)}'"))
    }
  })

  return (0)
}
