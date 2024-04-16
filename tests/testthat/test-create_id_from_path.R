test_that("create_id_from_path creates correct ID at data level", {
  # Test the function at data level
  path <- "blabla/project1/region1/location1/deployment1/some_directory/data.csv"
  id <- create_id_from_path(path, level = "data")
  expect_equal(id, "f3fb934f4107b3f99ee3eaf384d1d8b2")
})

test_that("create_id_from_path creates correct ID at deployment_id level", {
  # Test the function at deployment_id level
  path <- "blabla/blabla/project1/region1/location1/deployment1/summary.txt"
  path <- dirname(path)
  id <- create_id_from_path(path, level = "deployment_id")
  expect_equal(id, "5106fac0fecd29dc9c9c4fa433d30b2a")
})
