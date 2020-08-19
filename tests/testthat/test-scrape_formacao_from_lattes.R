test_that("Scraping from Lattes gets the correct information", {
  D = scrape_formacao_from_lattes("9646923479627457")


  expect_equal(D[D$Nome == "Fernanda Guarino De Felice" & D$Titulacao == "Mestrado", "Orientacao"],
                   "Sérgio Teixeira Ferreira")
  expect_equal(D[D$Nome == "Fernanda Guarino De Felice" & D$Titulacao == "Mestrado", "Instituicao"],
                   "Universidade Federal do Rio de Janeiro, UFRJ, Brasil.")
  expect_equal(D[D$Nome == "Fernanda Guarino De Felice" & D$Titulacao == "Mestrado", "Ano"],
                   "1997")

  # expect_equal(D[D$Nome == "Fernanda Guarino De Felice" & D$Titulacao == "Pós-Doutorado", "Orientacao"],
  #                  "Sérgio Teixeira Ferreira")
  # expect_equal(D[D$Nome == "Fernanda Guarino De Felice" & D$Titulacao == "Pós-Doutorado", "Instituicao"],
  #                  "Northwestern University, NORTHWESTERN, Estados Unidos.")
  # expect_equal(D[D$Nome == "Fernanda Guarino De Felice" & D$Titulacao == "Pós-Doutorado", "Ano"],
  #                  "2005 - 2008")
})

test_that("Scraping from Lattes can export table", {
  cv_folder = "/home/kleber/Dropbox/Scientific Research/Scientometrics/ScriptLattes/scriptLattesV8.11q/cache"
  fns = paste0(cv_folder, "/", list.files(cv_folder))
  df = plyr::ldply(fns, scrape_formacao_from_lattes)

  write.table(df, "teste orientacao.csv", sep = ";", row.names = F)

  expect_equal(0,0)
})
