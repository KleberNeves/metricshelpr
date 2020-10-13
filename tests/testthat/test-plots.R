test_that("can make triangle data", {
  M = readr::read_tsv("M.csv")
  MESH_REF = readr::read_delim("REF_ACH.csv", ";")
  M$ACH = codify_triangle_categories(M$Mesh, MESH_REF)
  triangle_data = count_data_for_triangle(M$ACH, ref_categories = c("A","C","H"))
  expect_equal(triangle_data$A, 2)
  expect_equal(triangle_data$C, 3)
  expect_equal(triangle_data$H, 3)
})
