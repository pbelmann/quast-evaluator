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

printToLog("----SCRIPT START----") 
printToLog(format(Sys.time(), "%a %b %d %X %Y")) 

init(assemblers_path, info_paths)
init("/home/belmann/projects/quast-evaluator/assemblers.tsv", "/home/belmann/projects/cami_plots/info2.tsv")

prepareData()
buildPlots()

printToLog("----SCRIPT END----")