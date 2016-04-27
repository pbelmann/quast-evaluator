#!/usr/bin/Rscript

library(docopt)

source("quast_evaluator.r")

'Usage: 
  quast_evaluator.r new <assemblers.tsv>  <info.tsv> -o DIRECTORY [-c FILE] [-h] 
  quast_evaluator.r reuse <combinedRef.tsv> <ref.tsv> -o DIRECTORY [-c FILE] [-h] 

-h --help    
-o DIRECTORY      specify optional output directory [default: ./]
-c FILE           Plot configuration tsv file. The following columns are allowed: plot (name of the plot), min (Minimum plot scale) and max (Maximum plot scale).
' -> doc

options <- docopt(doc)

outputPath = normalizePath(options[["-o"]])
plotConfPath = options[["-c"]]

newModus = options[["new"]]

if(newModus){
 assemblersPath = normalizePath(options$assemblers.tsv)
 infoPaths = normalizePath(options$info.tsv)

 if(!is.null(plotConfPath)){
   plotConfPath = normalizePath(plotConfPath)
 }

 init(assemblersPath, infoPaths) 

 printToLog("----SCRIPT START----") 
 printToLog(format(Sys.time(), "%a %b %d %X %Y")) 

 prepareData(plotConfPath = plotConfPath)
 buildPlots(outputPath)
 writeTables(outputPath)

 printToLog("----SCRIPT END----")
}

reuseModus = options[["reuse"]]

if(reuseModus){
  combinedRefPath = normalizePath(options$combinedRef.tsv)
  refPath = normalizePath(options$ref.tsv)
  
  if(!is.null(plotConfPath)){
    plotConfPath = normalizePath(plotConfPath)
  }
  
  printToLog("----SCRIPT START----") 
  printToLog(format(Sys.time(), "%a %b %d %X %Y")) 
  
  prepareData(existingCombinedRefPath = combinedRefPath, existingRefPath = refPath, plotConfPath = plotConfPath)
  buildPlots(outputPath)
  writeTables(outputPath)
  
  printToLog("----SCRIPT END----")
}