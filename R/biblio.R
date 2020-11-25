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

#' Extracts author order and country
#'
#' From a M bibliometrix data frame, extracts the author order and affiliation country
#' from the AU and C1 fields. Lots of ad hoc work arounds to handle abbreviated names and such.
#' Author order comes in two columns, as counted from first to last (positive) and from last to first (negative, i.e. -1 is the last author).
#'
#' @param M A bibliometrix dataset.
#' @return A data frame with the author order and country data.
#' @export
extract_author_country_order = function (M) {
  author_country_data = plyr::ldply(1:nrow(M), function (i) {
    title = M[i, "TI"]
    affil = M[i, "C1"]
    author_field = M[i, "AU"]

    if (is.na(affil) | !stringr::str_detect(affil, "\\[")) {
      return (
        data.frame(Title = title,
                   Author = NA,
                   Position = NA,
                   Country = NA,
                   stringsAsFactors = F)
      )
    }

    # Extract author list and positions to be merged later
    author_list = unlist(stringr::str_split(author_field, ";"))
    author_list = data.frame(Author = author_list, Position = 1:length(author_list))
    author_list$NegPosition = author_list$Position - nrow(author_list) - 1

    # Extracts author list from affiliation text
    authors = unlist(stringr::str_extract_all(affil, "(\\[.+?\\])"))
    authors = stringr::str_replace_all(authors, "[.];", ".")
    nauthors_per_group = stringr::str_count(authors, ";") + 1
    authors = stringr::str_remove_all(authors, "\\[")
    authors = stringr::str_remove_all(authors, "\\]")
    authors = stringr::str_remove_all(authors, ",")
    authors = stringr::str_remove_all(authors, "[.]")
    authors = unlist(stringr::str_split(authors, "; "))

    # Extracts affiliations per group of authors
    affil = stringr::str_replace_all(affil, "; \\[", " [")
    affil = stringr::str_replace_all(affil, "(\\[.+?\\])", "]")
    affil = unlist(stringr::str_split(affil, "\\] "))
    affil = affil[2:length(affil)]
    affil = affil[affil != ""]

    # Extract countries from affiliations
    countries = unlist(plyr::llply(affil, function (x) {
      x = unlist(stringr::str_split(x, "; "))
      x = stringr::str_trim(x)
      x = stringr::str_remove_all(x, "[.]")
      x = stringr::str_remove(x, ".+, ")
      x[stringr::str_which(x, " USA$")] = "USA"
      x = paste(x, collapse = "; ")
    }))
    countries = rep(countries, nauthors_per_group)

    # Building results data frame
    R = data.frame(Title = title,
                   Author = authors,
                   Country = countries,
                   stringsAsFactors = F)

    RR = merge(R, author_list, by = "Author")

    # If merge fails, see if author list is using initials and remerge
    if (nrow(RR) == 0) {
      author_names = unlist(lapply(authors, function (x) {
        x = unlist(stringr::str_split(x, " "))
        first_name = x[1]
        if (first_name %in% c("DE","DOS","DAS","DA")) {
          first_name = paste(x[1:2], collapse = " ")
          x = x[3:length(x)]
        } else {
          x = x[2:length(x)]
        }

        if (any(x %in% c("DE","DOS","DAS","DA"))) {
          dosdas = which(x %in% c("DE","DOS","DAS","DA")) + 1
          x = x[-dosdas]
        }
        x = stringr::str_extract(x, "[A-Z]")
        # } else { x = c() }
        fullname = paste(c(first_name, " ", x), collapse = "")
        fullname
      }))

      R = data.frame(Title = title,
                     Author = author_names,
                     Country = countries,
                     stringsAsFactors = F)
      RR = merge(R, author_list, by = "Author")
    }

    RR
  })

  author_country_data
}

#' Extracts author countries for each paper
#'
#' From a M bibliometrix data frame, extracts the author's affiliation countries
#' from the C1 field.
#'
#' @param M A bibliometrix dataset.
#' @param count_unique Flag to indicate whether to return an unique list or to include repeated.
#' @return A vector with the countries.
#' @export
extract_countries = function (M, count_unique = F) {
  purrr::map_chr(M$C1, function (affil) {
    # Extracts affiliations per group of authors
    affil = stringr::str_replace_all(affil, "; \\[", " [")
    affil = stringr::str_replace_all(affil, "(\\[.+?\\])", "]")
    affil = unlist(stringr::str_split(affil, "\\] "))
    affil = affil[2:length(affil)]
    affil = affil[affil != ""]

    # Extract countries
    countries = unlist(purrr::map(affil, function (x) {
      x = unlist(stringr::str_split(x, "; "))
      x = stringr::str_trim(x)
      x = stringr::str_remove_all(x, "[.]")
      x = stringr::str_remove(x, ".+, ")
      x = stringr::str_remove_all(x, "[;]")
      x[stringr::str_which(x, " USA$")] = "USA"
      x = paste(unique(x), collapse = ";")
      x
    }))

    if (count_unique) {
      countries = paste(unique(countries), collapse = ";")
    } else {
      countries = paste(countries, collapse = ";")
    }
    countries
  })
}

#' Extracts affiliation of individual authors
#'
#' From a M bibliometrix data frame, extracts the author affiliation and country
#' from the AU and C1 fields. Lots of ad hoc work arounds to handle abbreviated names and such.
#' Author order comes in two columns, as counted from first to last (positive) and from last to first (negative, i.e. -1 is the last author).
#'
#' @param M A bibliometrix dataset.
#' @return A data frame with the author order and country data.
#' @export
extract_author_affiliations = function (M) {
  author_country_data = plyr::ldply(1:nrow(M), function (i) {
    title = M[i, "TI"]
    affil = M[i, "C1"]
    author_field = M[i, "AU"]

    if (is.na(affil) | !stringr::str_detect(affil, "\\[")) {
      return (
        data.frame(Title = title,
                   Author = NA,
                   Position = NA,
                   Country = NA,
                   stringsAsFactors = F)
      )
    }

    # Extract author list and positions to be merged later
    author_list = unlist(stringr::str_split(author_field, ";"))
    author_list = data.frame(Author = author_list, Position = 1:length(author_list))
    author_list$NegPosition = author_list$Position - nrow(author_list) - 1

    # Extracts author list from affiliation text
    authors = unlist(stringr::str_extract_all(affil, "(\\[.+?\\])"))
    authors = stringr::str_replace_all(authors, "[.];", ".")
    nauthors_per_group = stringr::str_count(authors, ";") + 1
    authors = stringr::str_remove_all(authors, "\\[")
    authors = stringr::str_remove_all(authors, "\\]")
    authors = stringr::str_remove_all(authors, ",")
    authors = stringr::str_remove_all(authors, "[.]")
    authors = unlist(stringr::str_split(authors, "; "))

    # Extracts affiliations per group of authors
    affil = stringr::str_replace_all(affil, "; \\[", " [")
    affil = stringr::str_replace_all(affil, "(\\[.+?\\])", "]")
    affil = unlist(stringr::str_split(affil, "\\] "))
    affil = affil[2:length(affil)]
    affil = affil[affil != ""]

    affils = rep(affil, nauthors_per_group) %>%
      stringr::str_remove_all("(;$)|([.] $)")

    # Extract countries from affiliations
    countries = unlist(plyr::llply(affil, function (x) {
      x = unlist(stringr::str_split(x, "; "))
      x = stringr::str_trim(x)
      x = stringr::str_remove_all(x, "[.]")
      x = stringr::str_remove(x, ".+, ")
      x[stringr::str_which(x, " USA$")] = "USA"
      x = paste(x, collapse = "; ")
    }))
    countries = rep(countries, nauthors_per_group)

    # Building results data frame
    R = data.frame(Title = title,
                   Author = authors,
                   Affiliation = affils,
                   Country = countries,
                   stringsAsFactors = F)

    RR = merge(R, author_list, by = "Author")

    # If merge fails, see if author list is using initials and remerge
    if (nrow(RR) == 0) {
      author_names = unlist(lapply(authors, function (x) {
        x = unlist(stringr::str_split(x, " "))
        first_name = x[1]
        if (first_name %in% c("DE","DOS","DAS","DA")) {
          first_name = paste(x[1:2], collapse = " ")
          x = x[3:length(x)]
        } else {
          x = x[2:length(x)]
        }

        if (any(x %in% c("DE","DOS","DAS","DA"))) {
          dosdas = which(x %in% c("DE","DOS","DAS","DA")) + 1
          x = x[-dosdas]
        }
        x = stringr::str_extract(x, "[A-Z]")
        # } else { x = c() }
        fullname = paste(c(first_name, " ", x), collapse = "")
        fullname
      }))

      R = data.frame(Title = title,
                     Author = author_names,
                     Affiliation = affils,
                     Country = countries,
                     stringsAsFactors = F)
      RR = merge(R, author_list, by = "Author")
    }

    RR
  })

  author_country_data$Institution = stringr::str_extract(author_country_data$Affiliation, ".+?,") %>%
    stringr::str_remove(",")

  author_country_data
}



#' Collapses multiple author names into a single one
#'
#' When authors have multiple names, you can use this function to search and
#' replace through the AU field and return a version that collapses all occurrences
#' into a single one.
#'
#' @param M A bibliometrix dataset.
#' @param alternatives A character vector with the multiple names.
#' @param unique_name The preferred name into which all other will be turned.
#' @return A character vector, same as AU, but with the names replaced.
#' @export
collapse_author_names <- function(M, alternatives, unique_name) {
  collapsed_author_names = purrr::map_chr(M$AU, function (author_list) {
    purrr::walk(alternatives, function (alternative_name) {
      author_list <<- stringr::str_replace_all(author_list, alternative_name, unique_name)
    })
    author_list
  })
  collapsed_author_names
}
