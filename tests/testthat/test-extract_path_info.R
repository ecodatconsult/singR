
test_that("extract_path_info extracts correct path information at data level", {
  # Test the function at data level
  path <- "/project1/region1/location1/deployment1/deployment1/data.csv"
  info <- extract_path_info(path, level = "data")
  expect_equal(info$data, "data.csv")
  expect_equal(info$data_dir, "deployment1")
  expect_equal(info$deployment_id, "deployment1")
  expect_equal(info$location_id, "location1")
  expect_equal(info$region_id, "region1")
  expect_equal(info$project_id, "project1")
})

test_that("extract_path_info extracts correct path information at deployment_id level", {
  # Test the function at deployment_id level
  path <- "/project1/region1/location1/deployment1/data.csv"
  path <- dirname(path)
  info <- extract_path_info(path, level = "deployment_id")
  expect_equal(info$deployment_id, "deployment1")
  expect_equal(info$location_id, "location1")
  expect_equal(info$region_id, "region1")
  expect_equal(info$project_id, "project1")
})
