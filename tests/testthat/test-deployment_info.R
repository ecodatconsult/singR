test_that("deployment_info returns correct deployment information",{
  # Create a temporary directory with test files
  temp_dir <- tempdir()
  dir.create(temp_dir)

  # Create a sample _Summary.txt file
  tmpfile <- file.path(temp_dir, "example_Summary.txt")
  writeLines("DATE,TIME,LAT,,LON,,POWER(V),#FILES
             2024-Mar-08,07:34:00,50.72196,N,7.50443,E,5.0,0
             2024-Mar-12,12:00:00,50.72196,N,7.50443,E,4.9,0
             2024-Mar-12,12:01:00,50.72196,N,7.50443,E,4.9,1", tmpfile)

  # Mock the format_deployment function to avoid external dependencies
  format_deployment_mock <- function(file, device) {
    data <- read.csv(file)
    data <- data.frame(
      start_datetime = as.POSIXct(data$TIME, format = "%H:%M:%S", tz = "UTC"),
      end_datetime = as.POSIXct(data$TIME, format = "%H:%M:%S", tz = "UTC") + 3600,
      deployment_path = file
    )
    return(data)
  }

  # Replace format_deployment with the mocked version
  assignInNamespace("format_deployment", format_deployment_mock, ns = asNamespace("singR"))

  # Test the deployment_info function
  deployments <- deployment_info(temp_dir)

  # Check if the output has the correct structure and content
  expect_s3_class(deployments, "data.frame")
  expect_true(all(c("start_datetime", "end_datetime", "deployment_path", "deployment_id") %in% names(deployments)))
  expect_equal(nrow(deployments), 3)
  expect_equal(ncol(deployments), 4)
})

# Clean up
test_that("deployment_info throws an error for non-existent directory", {
  # Test if the function throws an error for non-existent directory
  expect_error(deployment_info("non_existent_directory"))
})
