test_that("ScriptLattes call saves the files correctly", {
  testIDs = c(1417327467050274, 910507988777644, 9584217233879031)
  scriptLattesPath = "/home/kleber/Dropbox/Scientific Research/Scientometrics/ScriptLattes/scriptLattesV8.11q/scriptLattes.py"
  saveto = tempdir()
  dir.create(saveto)
  run_scriptLattes(scriptLattesPath, testIDs, saveto)
  x = basename(list.files(saveto))
  print(x)
  expect_equal(x, testIDs)
})
