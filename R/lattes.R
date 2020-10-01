#' Cleans the advisor field from Lattes
#'
#' Removes common words added to the advisor field on CV Lattes (e.g. "Dr" or "Professor"). Meant for internal use.
#'
#' @param s The string extracted from the advisor field.
#' @return The string with the titles/words removed.
#' @importFrom magrittr %>%
str_clean_from_titles = function (s) {
  s = s %>%
    stringr::str_remove_all("Dra[.]") %>%
    stringr::str_remove_all("Dra ") %>%
    stringr::str_remove_all("Dr ") %>%
    stringr::str_remove_all("Dr[.]") %>%
    stringr::str_remove_all("Prof[.]") %>%
    stringr::str_remove_all("Profa[.]") %>%
    stringr::str_remove_all("Doutora") %>%
    stringr::str_remove_all("Doutores") %>%
    stringr::str_remove_all("Professora") %>%
    stringr::str_remove_all("Professores") %>%
    stringr::str_remove_all("Doutor") %>%
    stringr::str_remove_all("Professor")
  return (s)
}

#' Flattens the name, removing diacritics
#'
#' @param s The string to be flattened.
#' @return The string with the non-marked versions of the vowels.
#' @importFrom magrittr %>%
str_flatten_name = function (s) {
  s = s %>% stringr::str_to_lower() %>%
    stringr::str_replace_all("[áãâàä]","a") %>%
    stringr::str_replace_all("[éêë]","e") %>%
    stringr::str_replace_all("[íîï]","i") %>%
    stringr::str_replace_all("[óôõòö]","o") %>%
    stringr::str_replace_all("[úûü]","u") %>%
    stringr::str_replace_all("[ç]","c") %>%
    stringr::str_replace_all("[-`'´]","")
  s
}

#' Reverses a string
#'
#' @param s The character object to be reversed.
#' @return The string, backwards.
str_rev = function (s) { paste(rev(unlist(stringr::str_split(s,""))), collapse = "") }

#' Trims and makes Title Case
#'
#' @param s The character string to make look like a name.
#' @return The string, in Title Case and trimmed.
#' @importFrom magrittr %>%
str_namify = function (s) {
  s = s %>% stringr::str_trim() %>% stringr::str_to_title()
  s
}

#' Scrapes data from the Formação section of a Lattes CV
#'
#' Reads the XML from a Lattes CV (as downloaded by ScriptLattes) and extracts the data in the Formacao section - advisor, institution, year and so on.
#'
#' @param fn The path to the XML CV file.
#' @return A data frame with the information extracted.
#' #' @importFrom magrittr %>%
#' @export
scrape_formacao_from_lattes =  function (fn) {
  page = xml2::read_html(fn)

  add_register = function (advisor, coadvisor, inst, year, title, sw = F) {
    advisors <<- c(advisors, advisor %>% stringr::str_remove_all("Orientador:") %>%
                     stringr::str_trim() %>% stringr::str_remove_all("[.)(]"))
    if (sw) {
      insts <<- c(insts, inst %>% stringr::str_sub(15,-3))
    } else {
      insts <<- c(insts, inst %>% stringr::str_sub(1,-2))
    }
    coadvisors <<- c(coadvisors, coadvisor %>% stringr::str_sub(15,-3))
    years <<- c(years, as.numeric(year %>% stringr::str_sub(-4,-1)))
    titles <<- c(titles, title)
  }

  years = numeric(0)
  titles = character(0)
  advisors = character(0)
  coadvisors = character(0)
  insts = character(0)

  person = (page %>% rvest::html_nodes("h2.nome") %>% rvest::html_text())[1]
  print(paste0(basename(fn), ": ", person))

  divs = page %>% rvest::html_nodes(".layout-cell-pad-5")

  ### Doutorado
  divindex = stringr::str_which(divs %>% rvest::html_text(), "^Doutorado")

  if (length(divindex) > 0) {
    divindex = divindex[1]
    divTitle = divs[divindex] %>% rvest::html_nodes(xpath = "text()[preceding-sibling::br]") %>% rvest::html_text()
    divTitle = str_clean_from_titles(divTitle)

    # Testa se teve sanduíche ou não
    divtext = divs[divindex] %>% rvest::html_text()
    sandwich = stringr::str_detect(divtext, "período sanduíche")

    if (any(sandwich)) {
      i = stringr::str_which(divTitle, "^Título: ")
      year = divTitle[i] %>% stringr::str_extract("btenção: [0-9]{4}")

      inst = stringr::str_extract(divtext, "sanduíche .+? \\(")
      inst = inst[!is.na(inst)]
      advisor = stringr::str_extract(divtext, "\\(Orientador: .+?\\)")
      advisor = advisor[!is.na(advisor)]
      if (length(year) > 0) {
        add_register(advisor, "", inst, year, "Período Sanduíche", T)
      }

      i = stringr::str_which(divTitle, "^Orientador: ")
      if (length(i) > 0) { advisor = divTitle[i] } else { advisor = paste("Orientador:", divTitle[3]) }
      if (is.na(stringr::str_extract(advisor, ": .+"))) {
        advisor = stringr::str_trim(
          stringr::str_extract(paste(divTitle, collapse = " "), " Orientador: .+?[.]")
        )
      }
      i = stringr::str_which(divTitle, "^Coorientador: "); coadvisor = divTitle[i]
      i = 1; inst = divTitle[i]
      if (length(year) > 0) {
        add_register(advisor, coadvisor, inst, year, "Doutorado")
      }

    } else {
      i = stringr::str_which(divTitle, "^Orientador: ")
      if (length(i) > 0) { advisor = divTitle[i] } else { advisor = paste("Orientador:", divTitle[3]) }
      if (is.na(stringr::str_extract(advisor, ": .+"))) {
        advisor = stringr::str_trim(
          stringr::str_extract(paste(divTitle, collapse = " "), " Orientador: .+?[.]")
        )
      }
      i = stringr::str_which(divTitle, "^Coorientador: "); coadvisor = divTitle[i]
      i = 1; inst = divTitle[i]
      i = stringr::str_which(divTitle, "^Título: "); year = divTitle[i] %>% stringr::str_extract("btenção: [0-9]{4}")
      if (length(year) > 0) {
        add_register(advisor, coadvisor, inst, year, "Doutorado")
      }
    }
  }

  # Mestrado
  divindex = stringr::str_which(divs %>% rvest::html_text(), "^Mestrado")

  if (length(divindex) > 0) {
    divindex = divindex[1]
    divTitle = divs[divindex] %>% rvest::html_nodes(xpath = "text()[preceding-sibling::br]") %>% rvest::html_text()
    divTitle = str_clean_from_titles(divTitle)

    i = stringr::str_which(divTitle, "^Orientador: ")
    if (length(i) > 0) { advisor = divTitle[i] } else { advisor = paste("Orientador:", divTitle[3]) }
    if (is.na(stringr::str_extract(advisor, ": .+"))) {
      advisor = stringr::str_trim(
        stringr::str_extract(paste(divTitle, collapse = " "), " Orientador: .+?[.]")
      )
    }
    i = stringr::str_which(divTitle, "^Coorientador: "); coadvisor = divTitle[i]
    i = 1; inst = divTitle[i]
    i = stringr::str_which(divTitle, "^Título: ")
    year = divTitle[i] %>% stringr::str_extract("btenção: [0-9]{4}")
    if (length(year) > 0) {
      add_register(advisor, coadvisor, inst, year, "Mestrado")
    }
  }

  # Pós-Doutorado
  divindex = stringr::str_which(divs %>% rvest::html_text(), "^Pós-Doutorado")

  if (length(divindex) > 0) {
    divsPD = page %>% rvest::html_nodes("a[name='FormacaoAcademicaPosDoutorado'] ~ div > div > div")
    if (length(divsPD) == 0) {
      divsPD = page %>% rvest::html_nodes("a[name='FormacaoAcademicaPosDoutoradoLivreDocencia'] ~ div > div > div")
    }

    for (i in seq(1, length(divsPD), 2)) {
      divTitle = divsPD[i+1] %>% rvest::html_nodes(xpath = "text()[preceding-sibling::br]") %>% rvest::html_text()
      divTitle = str_clean_from_titles(divTitle)
      inst = divTitle[1]
      advisor = NA
      coadvisor = NA

      title = ifelse(
        divsPD[i+1] %>% rvest::html_text() %>% stringr::str_to_lower() %>%
          stringr::str_detect("pós-doutorado"),
        "Pós-Doutorado", "Livre-docência")

      year = divsPD[i] %>% rvest::html_text() %>%
        stringr::str_squish() %>% stringr::str_remove_all(" ")
      add_register(advisor, coadvisor, inst, year, title)
    }
  }

  if (length(years) == 0) {
    D = data.frame(IDLattes = basename(fn), Nome = str_namify(person), Titulacao = NA,
                   Orientacao = NA, Instituicao = NA, Pais = NA, Ano = NA, stringsAsFactors = F)
    return (D)
  }

  # Extract countries from the institution column
  invs = sapply(insts, str_rev)
  countries = sapply(stringr::str_extract(invs, ".+? ,") %>% stringr::str_sub(2,-3), str_rev)

  # Preparing data frame to return, removing blank ones
  D = data.frame(IDLattes = basename(fn), Nome = str_namify(person), Titulacao = str_namify(titles),
                 Orientacao = str_namify(advisors), Instituicao = stringr::str_trim(insts),
                 Pais = str_namify(countries), Ano = years, stringsAsFactors = F) %>%
    dplyr::filter(!is.na(Titulacao))

  return (D)
}

#' Reads the CSV snapshot from Lattes
#'
#' The resulting data frame is used by other functions (e.g. find_lattes_ID).
#' It uses data.table::fread to read the large file. The current snapshots can be obtained at http://vision.ime.usp.br/~jmena/base-2020-marco/
#' It returns a list of data tables, obtained by splitting the dataset by the first two letters of
#' the names. This is intended to make finding IDs faster, as an improvised dictionary structure.
#'
#' @param fn The path to the CSV snapshot.
#' @return A list of data table with the IDs and full names.
#' @export
read_lattes_snapshot = function (fn) {
  D = data.table::fread(fn, fill = T, quote = F, header = T, showProgress = T, select = c(1,2))
  D$SearchName = str_flatten_name(D$Nome)
  D$Initials = stringr::str_sub(D$SearchName, 1, 2)
  DS = split(D %>% dplyr::select(`ID-Lattes`, SearchName, Initials), D$Initials)
  DS
}

#' Finds the matching Lattes ID for a given name
#'
#' Searches for a given name on the loaded Lattes CSV snapshot. To use this function, you must first call read_lattes_snapshot.
#' Matches are found by calculating Levenshtein distances against the names in the snapshot.
#' It does not accept the snapshot as an argument: it assumes it exists in the global environment.
#'
#' @param query_name The name to be searched.
#' @param D The snapshot data table, loaded with read_lattes_snapshot().
#' @return A data table with the matching ID.
#' @export
find_lattes_ID = function (query_name, D) {
  print("Running search ...")
  initials = stringr::str_sub(query_name, 1, 2) %>% stringr::str_to_lower()
  LD = D[[initials]]
  dists = utils::adist(LD$SearchName, str_flatten_name(query_name))
  shortests = apply(dists, 2, function (x) { which(x == min(x)) })
  found_rows = LD[shortests, .(`ID-Lattes`, SearchName)]
  return (found_rows)
}

#' @importFrom glue glue
#' @export
run_scriptLattes = function (SL_path, ID_list, save_xml = "~") {
  # browser()
  # Check if cache has files, if so, warns and stops
  cache_path = paste0(dirname(SL_path),"/cache")
  cache_files = list.files(cache_path)
  if (length(cache_files) > 0) {
    stop("ScriptLattes cache is not empty.")
    return (1)
  }

  tmp = tempdir()

  # Save the list of lattes IDs
  lattesList = file(paste0(tmp,"/lattes.list"))
  writeLines(as.character(ID_list), con = lattesList)
  close(lattesList)

  # Copy the default config file to the same folder
  config_path = paste0(tmp,"/lattes.config")
  file.copy("./lattes.config", config_path, overwrite = T)

  # Runs scriptLattes
  cdline = glue('cd "{dirname(SL_path)}"')
  cmdline = glue('python "{basename(SL_path)}" "{config_path}"')
  slrun = system(command = paste(cdline, cmdline, sep = ";"), wait = T)

  # Returns if script doesn't run
  if (slrun != 0) {
    stop("Failed to run scriptLattes.")
    return (slrun)
  }

  # If it ran successfully, just copy the cache folder contents to the output folder
  cache_files = paste0(cache_path, "/", list.files(cache_path))
  file.copy(from = cache_files, to = save_xml)

  # Compare contents of folder (list.files) with lattes ID list, build log table indicating which ones were downloaded
  xml_list = list.files(save_xml)
  failed = ID_list[!(ID_list %in% basename(xml_list))]

  # Write the txt file with the ones that were not downloaded
  failedList = file(paste0(save_xml,"/failed.list"))
  writeLines(as.character(failed), con = failedList)
  close(failedList)

  print("Success! See downloaded XML CVs and a log of which ones failed.")
  return (0)
}
