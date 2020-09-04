# test_that("ScriptLattes call saves the files correctly", {
#   testIDs = c(1417327467050274)
#   scriptLattesPath = "/home/kleber/Dropbox/Scientific Research/Scientometrics/ScriptLattes/scriptLattesV8.11q/scriptLattes.py"
#   # browser()
#   saveto = "./teste"
#   dir.create(saveto)
#   run_scriptLattes(scriptLattesPath, testIDs, saveto)
#   x = as.numeric(basename(list.files(saveto)))
#   print(x)
#   expect_equal(x, testIDs)
# })
