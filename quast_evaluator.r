#!/usr/bin/Rscript

library(grid)
library(ggplot2)
library(plyr)
library(plotly)
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
  referenceReport <<- NULL
  combinedRefReport <<- NULL
  
  getInfos <- function(ref, assemblerPath, assemblerName) {
    refPath = ref["path"]
    reportPath = file.path(assemblerPath, "runs_per_reference", refPath, "transposed_report.tsv")
    if(file.exists(reportPath)){
        report = read.delim(reportPath, stringsAsFactors=FALSE)
        report = cbind.data.frame(report, gID=as.factor(ref["ID"]), label=as.character(ref["label"]), cov=as.double(ref["Cov"]), gc=as.double(ref["GC"]))
        report = report[report[, "Assembly"] == assemblerName, ]
        if (exists("referenceReport")){
          referenceReport <<- rbind.fill(referenceReport, report)
        } else {
          referenceReport <<- report
        }
    }
  }

  iterInfos <- function(assemblerRow){
    assemblerPath = file.path(assemblerRow["path"])
    if(file.exists(assemblerPath)){
      apply(infos,1,function(ref) getInfos(ref, assemblerPath=assemblerRow["path"], assemblerName=assemblerRow["assembler"]))
    } else {
      printToLog(sprintf("Directory %s does not exist", assemblerPath)) 
    }
  }
  
  combinedFileReport <- function(assemblerRow){
    assemblerPath = file.path(assemblerRow["path"])
    assemblerName = assemblerRow["assembler"]
    reportPath = file.path(assemblerPath, "combined_reference" , "transposed_report.tsv")
    if(file.exists(reportPath)){
      report = read.delim(reportPath, stringsAsFactors=FALSE)
      report = cbind.data.frame(report)
      report = report[report[, "Assembly"] == assemblerName, ]
      if (exists("combinedRefReport")){
        combinedRefReport <<- rbind.fill(combinedRefReport, report)
      } else {
        combinedRefReport <<- report
      }
    }
  }
  
  iterAssemblers  <- function(assemblerRow){
    iterInfos(assemblerRow)
    combinedFileReport(assemblerRow)
  }
  
  apply(assemblers, 1, iterAssemblers)
  referenceReport <<- cbind.data.frame(referenceReport, misassemblies.per.MB=referenceReport$X..misassemblies/(referenceReport$Total.length/1000000))
}

referencePlot <- function(cov, reportName){
  cov$ID <- factor(cov$ID, levels = cov$ID[order(cov$Cov)])
  p = ggplot(cov, aes(x=ID, y=Cov)) +
    geom_point() +
    theme_bw() + theme(panel.grid.major.x = element_blank(),panel.grid.minor.x = element_blank()) +
    theme(axis.text.x = element_text(angle = 90, vjust = 1, hjust=1, size=2)) +
    ylab("Abundance")
  p <-ggplotly(p)
  htmlwidgets::saveWidget(as.widget(p), reportName)
}

assemblyPlot <- function(toPlot, toPlotNames, fileReport, reportName, facet=FALSE, height=8, sortBy="cov"){
  fileReport$gID <- factor(fileReport$gID, levels = fileReport$gID[order(fileReport[sortBy])])
  pdf(reportName, width=11, height=height)
  for (n in 1:length(toPlot)){
    #localRep = remove_missing(referenceReport, vars = toPlot[n], finite = TRUE)
    p = ggplot(fileReport, aes_string(x="gID", color="Assembly", y=toPlot[n]))
    p = p + stat_smooth(method=loess, span=0.25, aes(fill=Assembly,group=Assembly))
    p = p + theme(axis.text.x = element_text(angle = 90, vjust = 1, hjust=1, size=2))
#    p = p + scale_x_discrete(labels=fileReport$label)
    p = p + geom_point(aes(colour=factor(Assembly)))
    if(facet){
      p = p + facet_grid(Assembly ~ .)
 #     p = facet_multiple(plot = p, facets ="Assembly",  ncol = 1, nrow = 6, scales = "free_y")
    }
    print(p)
  }
  dev.off()
}

parallelCoordinatesPlot <- function(outputPath, combinedRefReport){
  write.table(combinedRefReport, file.path(outputPath, "combined_ref_data.tsv"), sep="\t", row.names = FALSE)
}

buildPlots <- function(outputPath){
   referencePlot(infos, file.path(outputPath, "references.html"))
   assemblyPlot(toPlot, toPlotNames, referenceReport, file.path(outputPath, "coverage.pdf" ), FALSE)
   assemblyPlot(toPlot, toPlotNames, referenceReport, file.path(outputPath,"coverage-facet.pdf" ), TRUE, height=40)
   assemblyPlot(toPlot, toPlotNames, referenceReport, file.path(outputPath, "gc.pdf"), facet=FALSE, sortBy="gc")
   assemblyPlot(toPlot, toPlotNames, referenceReport, file.path(outputPath, "gc-facet.pdf"), facet=TRUE, sortBy="gc", height=40)
   parallelCoordinatesPlot(outputPath, combinedRefReport)
}

writeTables <- function(outputPath){
   write.table(combinedRefReport, file.path(outputPath, "combined_ref_data.tsv"), sep="\t", row.names = FALSE)
   write.table(referenceReport, file.path(outputPath, "ref_data.tsv"), sep="\t", row.names = FALSE)
}