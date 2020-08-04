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
  M = bibliometrix::convert2df(filenames, dbsource = dbsource, format = format)
  M
}

#' Obtains the "M" data frame for a generation
#'
#' Obtains the "M" data frame using bibliometrix to read all
#' text files in the folder for the generations specified.
#' Assumes generations are structured in subfolders named G0, G1, G2 and so on.
#'
#' @param generations Integer vector indicating the generation(s) to be loaded.
#' @param data_folder Full path to the folder where the data is stored.
#' @return An "M" data frame, as loaded and converted through bibliometrix.
#' @export
get.biblio.data = function (generations, data_folder) {
  if (length(generations) > 1) {
    filenames = character(0)
    for (gen in generations) {
      datapath = paste0(data_folder, "/G", gen)
      filenames = c(filenames, paste0(datapath,"/",list.files(datapath, pattern = ".txt$")))
    }
  } else {
    datapath = paste0(data_folder, "/G", generations)
    filenames = paste0(datapath,"/",list.files(datapath, pattern = ".txt$"))
  }
  M = get.bibliometrix.M(filenames)
  M = M[!duplicated(M$TI),]
  M
}
