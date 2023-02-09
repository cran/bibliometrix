# Unit test with testthat package
test_that("missingData function works correctly", {
  # Create example data
  data(management, package="bibliometrixData")
  M <- management
  
  # Check that missingData function returns a list
  expect_type(missingData(M), "list")
  
  # Check that df_all has 3 columns
  expect_equal(ncol(missingData(M)$allTags), 4)
  
  # Check that df_tags has 5 columns
  expect_equal(ncol(missingData(M)$mandatoryTags), 5)
  
  # Check that the status of col1 is "Excellent"
  expect_equal(missingData(M)$allTags$status[1], "Excellent")
  
  # Check that the status of col2 is "Good"
  expect_equal(missingData(M)$allTags$status[3], "Good")
  
  # Check that the status of col3 is "Acceptable"
  expect_equal(missingData(M)$allTags$status[27], "Acceptable")
  
  # Check that the status of col3 is "Poor"
  expect_equal(missingData(M)$allTags$status[34], "Poor")
  
  # Check that the status of col3 is "Critical
  expect_equal(missingData(M)$allTags$status[26], "Critical")
  
  # Check that the status of col3 is "Completely missing"
  expect_equal(missingData(M)$allTags$status[65], "Completely missing")

})
