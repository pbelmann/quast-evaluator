#!/usr/bin/Rscript

library(docopt)
options(warn=1)

source("quast_evaluator.r")

'Usage:
  quast_evaluator_cli.r new <assemblers.tsv>  <info.tsv> -o DIRECTORY [-c FILE] [-h]
  quast_evaluator_cli.r reuse <combinedRef.tsv> <ref.tsv> -o DIRECTORY [-c FILE] [-h]

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

 # initialize dataframes
 init(assemblersPath, infoPaths)

 # read in additional the data
 prepareData(plotConfPath = plotConfPath)

 # build the plots
 buildPlots(outputPath)

 # export the tables
 writeTables(outputPath)
}

reuseModus = options[["reuse"]]

if(reuseModus){
  combinedRefPath = normalizePath(options$combinedRef.tsv)
  refPath = normalizePath(options$ref.tsv)

  if(!is.null(plotConfPath)){
    plotConfPath = normalizePath(plotConfPath)
  }
  # prepare the data
  prepareData(existingCombinedRefPath = combinedRefPath, existingRefPath = refPath, plotConfPath = plotConfPath)

  # build the plots
  buildPlots(outputPath)

  # export the tables
  writeTables(outputPath)
}
