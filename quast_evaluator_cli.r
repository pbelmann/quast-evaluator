#!/usr/bin/Rscript

library(docopt)

source("quast_evaluator.r")

'Usage: quast_evaluator.r  <assemblers.tsv>  <info.tsv>  -o DIRECTORY [-h] 
-h --help    
-o DIRECTORY      specify optional output directory [default: ./]
' -> doc

options <- docopt(doc)

assemblersPath = normalizePath(options$assemblers.tsv)
infoPaths = normalizePath(options$info.tsv)
outputPath = normalizePath(options[["-o"]])

init(assemblersPath, infoPaths)

printToLog("----SCRIPT START----") 
printToLog(format(Sys.time(), "%a %b %d %X %Y")) 

prepareData()
buildPlots(outputPath)
writeTables(outputPath)

printToLog("----SCRIPT END----")
