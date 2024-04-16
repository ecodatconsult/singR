test_that("format_deployment returns the correct formatted deployment data for songmeter device", {
  # Create a temporary TXT file for testing
  tmpfile <- tempfile(fileext = ".txt")
  writeLines("DATE,TIME,LAT,,LON,,POWER(V),#FILES
             2024-Mar-08,07:34:00,50.72196,N,7.50443,E,5.0,0
             2024-Mar-12,12:00:00,50.72196,N,7.50443,E,4.9,0
             2024-Mar-12,12:01:00,50.72196,N,7.50443,E,4.9,1
             2024-Mar-12,12:05:00,50.72196,N,7.50443,E,4.9,0
             2024-Mar-12,12:06:00,50.72196,N,7.50443,E,4.9,1
             2024-Mar-12,12:10:00,50.72196,N,7.50443,E,4.9,0
             2024-Mar-12,12:11:00,50.72196,N,7.50443,E,4.9,1
             2024-Mar-12,12:15:00,50.72196,N,7.50443,E,4.9,0
             2024-Mar-12,12:16:00,50.72196,N,7.50443,E,4.9,1
             2024-Mar-12,12:20:00,50.72196,N,7.50443,E,4.9,0
             2024-Mar-12,12:21:00,50.72196,N,7.50443,E,4.9,1", tmpfile)

  # Test the function
  formatted_data <- format_deployment(tmpfile, device = "songmeter")

  # Check if the output has the correct structure and content
  expect_s3_class(formatted_data, "sf")
  expect_true(all(c("start_datetime", "end_datetime", "deployment_path") %in% names(formatted_data)))
  expect_equal(nrow(formatted_data), 1)
  expect_equal(ncol(formatted_data), 4)
})

test_that("format_deployment throws an error for unsupported devices", {
  # Test if the function throws an error for unsupported device
  expect_error(format_deployment("dummy.txt", device = "other_device"), "no other devices than songmeter supported")
})
