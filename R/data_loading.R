#' Load a M data frame
#'
#' Obtains the "M" data frame using bibliometrix. See get.biblio.data
#' and get.biblio.data.multiple for loading files using generations.
#'
#' @param filenames Vector of filenames
#' @return A data frame, as loaded and coverted through bibliometrix
#' @export
get.bibliometrix = function(filenames) {
  M = convert2df(filenames, dbsource = "isi", format = "plaintext")
  M
}
