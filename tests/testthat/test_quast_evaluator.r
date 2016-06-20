library(testthat)

context("Quast Evaluator")

test_that("Script loads without an Error.", {
  expect_true(TRUE)
})

cleanup_output_dir <- function(dir){
    unlink(dir, recursive = TRUE)
}

create_output_dir <- function(dirPath){
    dir.create(dirPath)
}

output = "tests/testthat/output"
setwd("../..")

outputDirs = c("coverage_log", "coverage_no_points_log", "coverage-facet_log", "boxplots_groups_facet", "boxplots_groups" , "coverage-facet_multiple_smooth_log", "coverage-facet_multiple_smooth")

cli_test_that <- function(description, test_code) {

    create_output_dir(output)

    test_that(description, test_code)

    cleanup_output_dir(output)
}

cli_test_that("Script produces expected output files.(new mode)", {

  cmd = paste("Rscript", "quast_evaluator_cli.r", "new","tests/testthat/data/assemblers.tsv"," tests/testthat/data/info.tsv","-o", output)

  if(system(cmd) != 0){
      expect_true(FALSE)
  } 

  lapply(outputDirs, function(outputDir) expect_true(file.exists(normalizePath(file.path(output, outputDir)))))
})


cli_test_that("Run scripts with plot configuration", {
  cmd = paste("Rscript", "quast_evaluator_cli.r","new",
	      "tests/testthat/data/assemblers.tsv",
	      " tests/testthat/data/info.tsv",
	      "-o", output,
	      "-c", "tests/testthat/data/info.tsv"
	      )

  if(system(cmd) != 0){
      expect_true(FALSE)
  } 

  lapply(outputDirs, function(outputDir) expect_true(file.exists(normalizePath(file.path(output, outputDir)))))
})
