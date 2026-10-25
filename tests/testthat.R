# Run all tests from the repository root: Rscript tests/testthat.R
testthat::test_dir("tests/testthat", stop_on_failure = TRUE)
