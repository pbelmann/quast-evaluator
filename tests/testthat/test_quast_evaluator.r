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

outputDir = "tests/testthat/output"
setwd("../..")

cli_test_that <- function(description, test_code) {

    create_output_dir(outputDir)

    test_that(description, test_code)

    cleanup_output_dir(outputDir)
}

cli_test_that("Script produces expected output files.", {

  cmd = paste("Rscript", "quast_evaluator_cli.r","tests/testthat/data/assemblers.tsv"," tests/testthat/data/info.tsv","-o", outputDir)

  if(system(cmd) != 0){
      expect_true(FALSE)
  } 
  
  outputFiles = c("abundance.pdf", "abundance_no_points.pdf", "abundance-facet.pdf", "gc.pdf", "gc_no_points.pdf", "gc-facet.pdf")  

  lapply(outputFiles, function(outputFile) expect_true(file.exists(normalizePath(file.path(outputDir, outputFile)))))
})


cli_test_that("Run scripts with plot configuration", {

  cmd = paste("Rscript", "quast_evaluator_cli.r",
	      "tests/testthat/data/assemblers.tsv",
	      " tests/testthat/data/info.tsv",
	      "-o", outputDir,
	      "-c", "tests/testthat/data/info.tsv"
	      )

  if(system(cmd) != 0){
      expect_true(FALSE)
  } 
  
  outputFiles = c("abundance.pdf", "abundance_no_points.pdf", "abundance-facet.pdf", "gc.pdf", "gc_no_points.pdf", "gc-facet.pdf")  

  lapply(outputFiles, function(outputFile) expect_true(file.exists(normalizePath(file.path(outputDir, outputFile)))))

})
