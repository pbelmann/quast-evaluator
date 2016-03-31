#!/usr/bin/Rscript

library(docopt)

source("quast_evaluator.r")

'Usage: quast_evaluator.r  <assemblers.tsv>  <info.tsv>  -o DIRECTORY [-c FILE] [-h] 
-h --help    
-o DIRECTORY      specify optional output directory [default: ./]
-c FILE           Plot configuration tsv file. The following columns are allowed: plot (name of the plot), min (Minimum plot scale) and max (Maximum plot scale).
' -> doc

options <- docopt(doc)

assemblersPath = normalizePath(options$assemblers.tsv)
infoPaths = normalizePath(options$info.tsv)
outputPath = normalizePath(options[["-o"]])

plotConfPath = options[["-c"]]

if(!is.null(plotConfPath)){
  plotConfPath = normalizePath(plotConfPath)
}

init(assemblersPath, infoPaths, plotConfPath) 

printToLog("----SCRIPT START----") 
printToLog(format(Sys.time(), "%a %b %d %X %Y")) 

prepareData()
buildPlots(outputPath)
writeTables(outputPath)

printToLog("----SCRIPT END----")
