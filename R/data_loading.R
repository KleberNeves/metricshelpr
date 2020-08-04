#' Load a M data frame
#'
#' Obtains the "M" data frame using bibliometrix to read all
#' text files in the given folder.
#'
#' @param folder Full path to the folder containing the bibliometric data.
#' @param dbsource Passed on to convert2df. Defaults to "isi" (Web of Science).
#' @param format Passed on to convert2df. Defaults to "plaintext" (Web of Science).
#' @return An "M" data frame, as loaded and converted through bibliometrix.
#' @export
get.bibliometrix.M = function(folder, dbsource = "isi", format = "plaintext") {
  filenames = paste0(folder, "/", list.files(folder, ".txt$"))
  M = convert2df(filenames, dbsource = dbsource, format = format)
  M
}
