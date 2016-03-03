#!/usr/bin/Rscript

library(docopt)

source("quast_evaluator.r")

'Usage: quast_evaluator.R  <assemblers.tsv>  <info.tsv>  -o DIRECTORY [-h] 
-h --help    
-o DIRECTORY      specify optional output directory [default: ./]
' -> doc

options <- docopt(doc)

assemblersPath = options$assemblers.tsv
infoPaths = options$info.tsv
outputPath = options[["-o"]]

init(assemblersPath, infoPaths)
#init("/home/belmann/projects/quast-evaluator/tests/testthat/data/assemblers.tsv", "/home/belmann/projects/quast-evaluator/tests/testthat/data/info.tsv")

printToLog("----SCRIPT START----") 
printToLog(format(Sys.time(), "%a %b %d %X %Y")) 

prepareData()
buildPlots(outputPath)
writeTables(outputPath)

printToLog("----SCRIPT END----")
