#' Queries PubMed to obtain MeSH codes
#'
#' Given a list of PubMed IDs, this function queries the PubMed API
#' (in blocks of 100 PMIDs, with 15 second intervals) for their MeSH
#' codes.
#'
#' Codes are returned in a two-column data frame, with the MeSH
#' headings and complete MeSH terms, separated by semicolons.
#'
#' @param pmids A numeric vector with PubMed IDs.
#' @return A datta frame with the MeSH headings and terms.
#' @export
get.full.mesh = function(pmids) {
  parts = split(pmids, ceiling(seq_along(pmids)/100))

  done = 0
  get.part = function (part) {
    p.rec = RISmed::EUtilsGet(part)
    done <<- done + length(part)
    cat(paste0(done,"/",length(pmids),"\n"))
    Sys.sleep(15)
    p.rec
  }

  print("Downloading PubMed records ...")
  record.parts = lapply(parts, get.part)

  print("Matching MeSH terms ...")
  D = plyr::ldply(record.parts, function(record.part) {
    D2 = plyr::ldply(record.part@Mesh, function (x) {
      if (all(is.na(x))) {
        return (data.frame(MeshFullTerms = "No Mesh Terms",
                           MeshHeadings = "No Mesh Terms"))
      }

      x$I = 1:nrow(x)
      for (i in 2:nrow(x)) {
        if (!is.na(x[i, "Type"])) {
          if (x[i, "Type"] == "Qualifier") {
            x[i, "I"] = x[i - 1, "I"]
          }
        }
      }
      x$Qualifier = ifelse(x$Type == "Qualifier", paste0(":", as.character(x$Heading)), "")
      x$Heading2 = x$Heading[x$I]
      x$FullTerm = paste0(x$Heading2, x$Qualifier)

      MeshFullTerms = paste(x$FullTerm, collapse = ";")
      MeshHeadings = paste(unique(x$Heading2), collapse = ";")

      data.frame(MeshFullTerms, MeshHeadings)
    })
    D2$pmid = unlist(record.part@PMID)
    D2
  })

  D
}
