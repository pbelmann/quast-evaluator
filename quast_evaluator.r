#!/usr/bin/Rscript

library(grid)
library(ggplot2)
library(plyr)
library(ggplus)
options(warn=1)

toPlot=c("N50","NG50","X..contigs","Total.length","X..misassemblies","Genome.fraction....","Duplication.ratio","GC....","Reference.GC....","X..mismatches.per.100.kbp","misassemblies.per.MB")
toPlotNames=c("N50","NG50","# contigs","Total length","# misassemblies","Genome fraction (%)","Duplication ratio","GC","ref GC","# mismatches per 100 kbp","Misassemblies per MB")

init <- function(assemblers_path, info_paths){
  assemblers <<- read.delim(assemblers_path, header=TRUE, stringsAsFactors=FALSE)
  infos <<- read.delim(info_paths, header=TRUE, stringsAsFactors=FALSE)
  assemblers <<- cbind.data.frame(assemblers)
  infos <<- cbind.data.frame(infos)
  logPath <<- "out.log" 
}

printToLog <- function(str=""){
  cat(str,file=logPath, append=TRUE, "\n") 
}

prepareData <- function(){
  fileReport <<- NULL
  iterInfos <- function(ref, assemblerPath, assemblerName) {
    refPath = ref["path"]
    reportPath = file.path(assemblerPath, refPath, "transposed_report.tsv")
    if(file.exists(reportPath)){
        report = read.delim(reportPath, stringsAsFactors=FALSE)
        report = cbind.data.frame(report, gID=as.factor(ref["ID"]), cov=as.double(ref["Cov"]), gc=as.double(ref["GC"]))
        report = report[report[, "Assembly"] == assemblerName, ]
        if (exists("fileReport")){
          fileReport <<- rbind.fill(fileReport, report)
        } else {
          fileReport <<- report
        }
    }
  }

  iterAssemblers  <- function(assemblerRow){
    assemblerPath = file.path(assemblerRow["path"])
    if(file.exists(assemblerPath)){
      apply(infos,1,function(ref) iterInfos(ref, assemblerPath=assemblerRow["path"], assemblerName=assemblerRow["assembler"]))
    } else {
      printToLog(sprintf("Directory %s does not exist", assemblerPath)) 
    }
  }
  apply(assemblers, 1, iterAssemblers)
  fileReport <<- cbind.data.frame(fileReport, misassemblies.per.MB=fileReport$X..misassemblies/(fileReport$Total.length/1000000))
}

referencePlot <- function(cov, reportName){
  cov$ID <- factor(cov$ID, levels = cov$ID[order(cov$Cov)])
  pdf(reportName, width=11, height=7.6)
  p = ggplot(cov, aes(x=ID, y=Cov)) +
    geom_point() +
    theme_bw() + theme(panel.grid.major.x = element_blank(),panel.grid.minor.x = element_blank()) +
    theme(axis.text.x = element_text(angle = 90, vjust = 1, hjust=1, size=2)) +
    ylab("Coverage")
  print(p)
  dev.off()
}

assemblyPlot <- function(toPlot, toPlotNames, fileReport, reportName, facet=FALSE, height=8, sortBy="cov"){
  fileReport$gID <- factor(fileReport$gID, levels = fileReport$gID[order(fileReport$cov)])
  pdf(reportName, width=11, height=height)
  for (n in 1:length(toPlot)){
    p = ggplot(fileReport, aes_string(x="gID", color="Assembly", y=toPlot[n]))
    p = p + stat_smooth(method=loess, span=0.25, aes(fill=Assembly,group=Assembly))
    p = p + theme(axis.text.x = element_text(angle = 90, vjust = 1, hjust=1, size=2))
    p = p + geom_point(aes(colour=factor(Assembly)))
    if(facet){
      p = facet_multiple(plot = p, facets ="Assembly",  ncol = 1, nrow = 6)
    }
    print(p)
  }
  dev.off()
}

buildPlots <- function(){
  referencePlot(infos, "references.pdf")
  assemblyPlot(toPlot, toPlotNames , fileReport, "coverage.pdf", FALSE)
  assemblyPlot(toPlot, toPlotNames , fileReport, "coverage-facet.pdf", TRUE, height=12)
  assemblyPlot(toPlot, toPlotNames , fileReport, "gc.pdf", facet=FALSE, sortBy="gc")
  assemblyPlot(toPlot, toPlotNames , fileReport, "gc-facet.pdf", facet=TRUE, sortBy="gc", height=12)
}