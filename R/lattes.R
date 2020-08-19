str_clean_from_titles = function (s) {
  s = s %>%
    stringr::str_remove_all("Dra[.]") %>%
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

str_rev = function (s) { paste(rev(unlist(stringr::str_split(s,""))), collapse = "") }

str_namify = function (s) {
  s = s %>% stringr::str_trim() %>% stringr::str_to_title()
  s
}

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

  # if (basename(fn) == "0079842946439755") { browser() }

  divs = page %>% rvest::html_nodes(".layout-cell-pad-5")

  ### Doutorado
  divindex = stringr::str_which(divs %>% rvest::html_text(), "^Doutorado")

  if (length(divindex) > 0) {
    divindex = divindex[1]
    divTitle = divs[divindex] %>% rvest::html_nodes(xpath = "text()[preceding-sibling::br]") %>% rvest::html_text()
    divTitle = str_clean_from_titles(divTitle)

    # Testa se teve sanduíche ou não
    divtext = divs[divindex] %>% rvest::html_text()
    sandwich = stringr::str_detect(divtext, "sanduíche")

    if (any(sandwich)) {

      i = stringr::str_which(divTitle, "^Título: ")
      year = divTitle[i] %>% stringr::str_extract("btenção: [0-9]{4}")

      inst = stringr::str_extract(divtext, "sanduíche .+? \\(")
      inst = inst[!is.na(inst)]
      advisor = stringr::str_extract(divtext, "\\(Orientador: .+?\\)")
      advisor = advisor[!is.na(advisor)]

      add_register(advisor, "", inst, year, "Período Sanduíche", T)

      i = stringr::str_which(divTitle, "^Orientador: "); advisor = divTitle[i]
      if (is.na(stringr::str_extract(advisor, ": .+"))) {
        advisor = stringr::str_trim(
          stringr::str_extract(paste(divTitle, collapse = " "), " Orientador: .+?[.]")
        )
      }
      i = stringr::str_which(divTitle, "^Coorientador: "); coadvisor = divTitle[i]
      i = 1; inst = divTitle[i]
      add_register(advisor, coadvisor, inst, year, "Doutorado")

    } else {
      i = stringr::str_which(divTitle, "^Orientador: "); advisor = divTitle[i]
      if (is.na(stringr::str_extract(advisor, ": .+"))) {
        advisor = stringr::str_trim(
          stringr::str_extract(paste(divTitle, collapse = " "), " Orientador: .+?[.]")
        )
      }
      i = stringr::str_which(divTitle, "^Coorientador: "); coadvisor = divTitle[i]
      i = 1; inst = divTitle[i]
      i = stringr::str_which(divTitle, "^Título: "); year = divTitle[i] %>% stringr::str_extract("btenção: [0-9]{4}")
      add_register(advisor, coadvisor, inst, year, "Doutorado")
    }
  }

  # Mestrado
  divindex = stringr::str_which(divs %>% rvest::html_text(), "^Mestrado")

  if (length(divindex) > 0) {
    divindex = divindex[1]
    divTitle = divs[divindex] %>% rvest::html_nodes(xpath = "text()[preceding-sibling::br]") %>% rvest::html_text()
    divTitle = str_clean_from_titles(divTitle)

    i = stringr::str_which(divTitle, "^Orientador: "); advisor = divTitle[i]
    if (is.na(stringr::str_extract(advisor, ": .+"))) {
      advisor = stringr::str_trim(
        stringr::str_extract(paste(divTitle, collapse = " "), " Orientador: .+?[.]")
      )
    }
    i = stringr::str_which(divTitle, "^Coorientador: "); coadvisor = divTitle[i]
    i = 1; inst = divTitle[i]
    i = stringr::str_which(divTitle, "^Título: ")
    year = divTitle[i] %>% stringr::str_extract("btenção: [0-9]{4}")
    add_register(advisor, coadvisor, inst, year, "Mestrado")
  }

  # Pós-Doutorado
  divindex = stringr::str_which(divs %>% rvest::html_text(), "^Pós-Doutorado")

  if (length(divindex) > 0) {
    divsPD = page %>% rvest::html_nodes("a[name='FormacaoAcademicaPosDoutorado'] ~ div > div > div")
    divTitle = divsPD[2] %>% rvest::html_nodes(xpath = "text()[preceding-sibling::br]") %>% rvest::html_text()
    divTitle = str_clean_from_titles(divTitle)
    inst = divTitle[1]
    advisor = NA
    coadvisor = NA

    # browser()
    year = divsPD[1] %>% rvest::html_text() %>%
      stringr::str_squish() %>% stringr::str_remove_all(" ")
    add_register(advisor, coadvisor, inst, year, "Pós-Doutorado")
  }

  # Extract countries from the institution column
  invs = sapply(insts, str_rev)
  countries = sapply(stringr::str_extract(invs, ".+? ,") %>% stringr::str_sub(2,-3), str_rev)

  # Preparing data frame to return, removing blank ones
  D = data.frame(Nome = str_namify(person), Titulacao = str_namify(titles),
                 Orientacao = str_namify(advisors), Instituicao = stringr::str_trim(insts),
                 Pais = str_namify(countries), Ano = years, stringsAsFactors = F) %>%
    dplyr::filter(!is.na(Titulacao))

  return (D)
}

