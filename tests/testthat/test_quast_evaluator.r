library(testthat)

context("Quast Evaluator")

test_that("Script loads without an Error.", {
  expect_true(TRUE)
})

test_that("Script produces expected output files.", {
  setwd("../..")
  outputDir = "tests/testthat/data/output"
  dir.create(outputDir)
  cmd = paste("Rscript", "quast_evaluator_cli.r","tests/testthat/data/assemblers.tsv"," tests/testthat/data/info.tsv","-o", outputDir)
  system(cmd)
  
  outputFiles = c("references.html", "abundance.pdf", "abundance_no_points.pdf", "abundance-facet.pdf", "gc.pdf", "gc_no_points.pdf", "gc-facet.pdf")  

  lapply(outputFiles, function(outputFile) expect_true(file.exists(normalizePath(file.path(outputDir, outputFile)))))
   
})
