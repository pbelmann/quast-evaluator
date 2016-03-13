#!/usr/bin/Rscript

library(grid)
library(ggplot2)
library(plyr)
library(plotly)
library(RColorBrewer)
options(warn=1)

toPlot=c("N50","NG50","X..contigs","Total.length","X..misassemblies","Genome.fraction....","Duplication.ratio","GC....","Reference.GC....","X..mismatches.per.100.kbp","normalized.misassemblies.per.MB","normalized.mismatches.per.100.kbp")
toPlotNames=c("N50","NG50","# contigs","Total length","# misassemblies","Genome fraction (%)","Duplication ratio","GC","ref GC","# mismatches per 100 kbp","normalized misassemblies per MB","normalized mismatches per 100 kbp")

logPath <<- "out.log" 

init <- function(assemblers_path, info_paths){
  assemblers <<- read.delim(assemblers_path, header=TRUE, stringsAsFactors=FALSE)
  infos <<- read.delim(info_paths, header=TRUE, stringsAsFactors=FALSE)
  assemblers <<- cbind.data.frame(assemblers)
  infos <<- cbind.data.frame(infos)
}

printToLog <- function(str=""){
  cat(str,file=logPath, append=TRUE, "\n") 
}

prepareData <- function(existingCombinedRefPath, existingRefPath){
  referenceReport <<- NULL
  combinedRefReport <<- NULL
  
  getInfos <- function(ref, assemblerPath, assemblerName) {
    refPath = ref["path"]
    reportPath = file.path(assemblerPath, "runs_per_reference", refPath, "transposed_report.tsv")
    report = NULL
    if(file.exists(reportPath)){
        report = read.delim(reportPath, stringsAsFactors=FALSE)
        report = cbind.data.frame(report, gID=as.factor(ref["ID"]), label=as.character(ref["label"]), refOrder=as.double(ref["order"]), gc=as.double(ref["GC"]))
        report = report[report[, "Assembly"] == assemblerName, ]
    } else {
        report = data.frame(Assembly=assemblerName, gID=as.factor(ref["ID"]), Genome.fraction....=0, 
                            N50=0, 
                            NG50=0, 
                            X..contigs=0, 
                            Total.length=0, 
                            GC....=0, 
                            Reference.GC....=0,
                            label=as.character(ref["label"]), 
                            refOrder=as.double(ref["order"]), gc=as.double(ref["GC"]))
#        Duplication.ratio=0,
#        X..misassemblies=0, 
#        X..mismatches.per.100.kbp=0, 
#        normalized.misassemblies.per.MB=0, 
#        normalized.mismatches.per.100.kbp=0,
        
    }
    if (exists("referenceReport")){
      referenceReport <<- rbind.fill(referenceReport, report)
    } else {
      referenceReport <<- report
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
  
  
  if(!missing(existingRefPath)){
    refPath = file.path(existingRefPath)
    refReport = read.delim(refPath, stringsAsFactors=FALSE)
    referenceReport <<- cbind.data.frame(refReport)
  }
  
  if(!missing(existingCombinedRefPath)){
    combRefPath = file.path(existingCombinedRefPath)
    combRefReport = read.delim(combRefPath, stringsAsFactors=FALSE)
    combinedRefReport <<- cbind.data.frame(combRefReport)
  } else {
      apply(assemblers, 1, iterAssemblers)
      referenceReport <<- cbind.data.frame(referenceReport, normalized.misassemblies.per.MB=referenceReport$X..misassemblies/(referenceReport$Total.length/1000000))
      referenceReport <<- cbind.data.frame(referenceReport, normalized.mismatches.per.100.kbp=referenceReport$X..mismatches.per.100.kbp/referenceReport$Total.length)
  }
}

referencePlot <- function(refInfos, reportName){
  refInfos$ID <- factor(refInfos$ID, levels = refInfos$ID[order(refInfos$refOrder)])
  p = ggplot(refInfos, aes(x=ID, y=refOrder)) +
    geom_point() +
    theme_bw() + theme(panel.grid.major.x = element_blank(),panel.grid.minor.x = element_blank()) +
    theme(axis.text.x = element_text(angle = 90, vjust = 1, hjust=1, size=2)) +
    ylab("Abundance")
  p <-ggplotly(p)
  htmlwidgets::saveWidget(as.widget(p), reportName)
}

assemblyPlot <- function(toPlot, toPlotNames, fileReport, reportName, facet=FALSE, height=8, sortBy="refOrder", se=FALSE, points=TRUE, lineTypes=c(rep("solid",40)), manualColor=NULL){
  fileReport$gID <- factor(fileReport$gID, levels = fileReport$gID[order(fileReport[sortBy])])
  pdf(reportName, width=11, height=height)
  for (n in 1:length(toPlot)){
#    fileReport = remove_missing(fileReport, vars = toPlot[n], finite = TRUE)
    maxCol = max(fileReport[toPlot[n]], na.rm = TRUE)
    minCol = min(fileReport[toPlot[n]], na.rm = TRUE)
    
    p = ggplot(fileReport, aes_string(x="gID", color="Assembly", y=toPlot[n]))
    p = p + ylim(c(minCol,maxCol))
    p = p + stat_smooth(method=loess, span=0.25, aes(fill=Assembly,group=Assembly, linetype = Assembly), se=FALSE)
    p = p + scale_linetype_manual(values = lineTypes)
    p = p + theme(axis.text.x = element_text(angle = 90, vjust = 1, hjust=1, size=2))
    if(!missing(manualColor)){
      p = p + scale_color_manual(values=manualColor)
    }
    if(points){
      p = p + geom_point(aes(colour=factor(Assembly)))
    }
    if(facet){
      p = p + facet_grid(Assembly ~ .)
    }
    print(p)
  }
  dev.off()
}

parallelCoordinatesPlot <- function(outputPath, combinedRefReport){
  write.table(combinedRefReport, file.path(outputPath, "combined_ref_data.tsv"), sep="\t", row.names = FALSE)
}

buildPlots <- function(outputPath){
  ownColor <- sort(rep(brewer.pal(n=6, name="Set1"),3))
  customLines <- c(rbind(rep("solid", 30), rep("dashed", 30), rep("dotted", 30)))
  referencePlot(infos, file.path(outputPath, "references.html"))
  assemblyPlot(toPlot, toPlotNames, referenceReport, file.path(outputPath, "abundance.pdf" ), FALSE)
  assemblyPlot(toPlot, toPlotNames, referenceReport, file.path(outputPath, "abundance_no_points.pdf" ), FALSE, se=FALSE, points=FALSE, lineTypes=customLines, manualColor=ownColor)
  assemblyPlot(toPlot, toPlotNames, referenceReport, file.path(outputPath,"abundance-facet.pdf" ), TRUE, height=40)
  assemblyPlot(toPlot, toPlotNames, referenceReport, file.path(outputPath, "gc.pdf"), facet=FALSE, sortBy="gc")
  assemblyPlot(toPlot, toPlotNames, referenceReport, file.path(outputPath, "gc_no_points.pdf"), facet=FALSE, sortBy="gc", se=FALSE, points=FALSE, lineTypes=customLines, manualColor=ownColor)
  assemblyPlot(toPlot, toPlotNames, referenceReport, file.path(outputPath, "gc-facet.pdf"), facet=TRUE, sortBy="gc", height=40)
  parallelCoordinatesPlot(outputPath, combinedRefReport)
}

writeTables <- function(outputPath){
   write.table(combinedRefReport, file.path(outputPath, "combined_ref_data.tsv"), sep="\t", row.names = FALSE)
   write.table(referenceReport, file.path(outputPath, "ref_data.tsv"), sep="\t", row.names = FALSE)
}