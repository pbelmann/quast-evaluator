#!/usr/bin/Rscript

library(docopt)

source("quast_evaluator.r")

'Usage: quast_summarizer.R  <assemblers.tsv>  <info.tsv>  [-ho DIRECTORY] 
-h --help    
-o DIRECTORY      specify optional output directory [default: ./]
' -> doc

options <- docopt(doc)

assemblers_path = options$assemblers.tsv 
info_paths = options$info.tsv

init(assemblers_path, info_paths)
#init("/home/belmann/projects/quast-evaluator/tests/testthat/data/assemblers.tsv", "/home/belmann/projects/quast-evaluator/tests/testthat/data/info.tsv")

printToLog("----SCRIPT START----") 
printToLog(format(Sys.time(), "%a %b %d %X %Y")) 

prepareData()
buildPlots()

printToLog("----SCRIPT END----")
