#!/usr/bin/Rscript

library(grid)
library(ggplot2)
library(plyr)
library(plotly)
library(RColorBrewer)
library(mgcv)
options(warn=1)

toPlot=c("N50","NGA50","X..contigs","Total.length","X..misassemblies","Genome.fraction....","Duplication.ratio","GC....","Reference.GC....","X..mismatches.per.100.kbp","normalized.misassemblies.per.MB","normalized.mismatches.per.100.kbp")
toPlotNames=c("N50","NGA50","# contigs","Total length","# misassemblies","Genome fraction (%)","Duplication ratio","GC","ref GC","# mismatches per 100 kbp","normalized misassemblies per MB","normalized mismatches per 100 kbp")

logPath <<- "out.log" 

maxScale = list()
maxScale$N50 = 700000
maxScale$NGA50 = 800000
maxScale$X..contigs = 3000
maxScale$Total.length = 6000000
maxScale$X..misassemblies = 100
maxScale$Duplication.ratio = 1.5
maxScale$X..mismatches.per.100.kbp = 3000
maxScale$normalized.misassemblies.per.MB = 25
maxScale$normalized.mismatches.per.100.kbp = 1

minScale = list()
minScale$N50 = 0
minScale$NGA50 = 0
minScale$X..contigs = 0
minScale$Total.length = 0
minScale$X..misassemblies = 0
minScale$Duplication.ratio = 1
minScale$X..mismatches.per.100.kbp = 0
minScale$normalized.misassemblies.per.MB = 0
minScale$normalized.mismatches.per.100.kbp = 0

init <- function(assemblersPath, infoPaths, plotConfPath){
  assemblers <<- read.delim(assemblersPath, header=TRUE, stringsAsFactors=FALSE)
  assemblers <<- cbind.data.frame(assemblers)
  infos <<- read.delim(infoPaths, header=TRUE, stringsAsFactors=FALSE)
  infos <<- cbind.data.frame(infos)
  if(!missing(plotConfPath)){
    plotConf <<- read.delim(plotConfPath, header=TRUE, stringsAsFactors=FALSE)
    plotConf <<- cbind.data.frame(plotConf)
  }
}

printToLog <- function(str=""){
  cat(str,file=logPath, append=TRUE, "\n") 
}

prepareData <- function(existingCombinedRefPath, existingRefPath){
  referenceReport <<- NULL
  combinedRefReport <<- NULL
  
  getInfos <- function(ref, assemblerPath, assemblerName, assemblerGroup) {
    refPath = ref["path"]
    reportPath = file.path(assemblerPath, "runs_per_reference", refPath, "transposed_report.tsv")
    report = NULL
    if(file.exists(reportPath)){
        report = read.delim(reportPath, stringsAsFactors=FALSE)
        report = cbind.data.frame(report, gid=as.factor(ref["id"]), label=as.character(ref["label"]), 
                                  refOrder=as.double(ref["order"]), gc=as.double(ref["gc"]), group=as.character(ref["group"]), assemblerGroup = assemblerGroup)
        report = report[report[, "Assembly"] == assemblerName, ]
        report$NGA50[report$NGA50 == "-"]  <- 0
        report$NGA50 <- as.numeric(as.character(report$NGA50))
    } else {
        report = data.frame(Assembly=assemblerName, gid=as.factor(ref["id"]), Genome.fraction....=0, 
                            N50=0, 
                            NGA50=0, 
                            X..contigs=0, 
                            Total.length=0, 
                            GC....=0, 
                            Reference.GC....=0,
                            label=as.character(ref["label"]), 
                            group=as.character(ref["group"]),
                            refOrder=as.double(ref["order"]), 
                            gc=as.double(ref["gc"]),
                            assemblerGroup = assemblerGroup)
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
      apply(infos,1,function(ref) getInfos(ref, assemblerPath=normalizePath(assemblerRow["path"]), assemblerName=assemblerRow["assembler"], assemblerGroup=assemblerRow["group"]))
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
  refInfos$id <- factor(refInfos$id, levels = refInfos$id[order(refInfos$order)])
  p = ggplot(refInfos, aes(x=id, y=order)) +
    geom_point() +
    theme_bw() + theme(panel.grid.major.x = element_blank(),panel.grid.minor.x = element_blank()) +
    theme(axis.text.x = element_text(angle = 90, vjust = 1, hjust=1, size=2)) +
    ylab("Abundance")
  p <-ggplotly(p)
  htmlwidgets::saveWidget(as.widget(p), reportName)
}

assemblyPlot <- function(toPlot, toPlotNames, fileReport, reportName, facet=FALSE, height=8, sortBy="refOrder", se=FALSE, points=TRUE, lineTypes=c(rep("solid",40)), manualColor=NULL){
  fileReport$gid <- factor(fileReport$gid, levels = fileReport$gid[order(fileReport[sortBy])])

  pdf(reportName, width=11, height=height)
  for (n in 1:length(toPlot)){
#    fileReport = remove_missing(fileReport, vars = toPlot[n], finite = TRUE)
    maxCol = max(fileReport[toPlot[n]], na.rm = TRUE)
    minCol = min(fileReport[toPlot[n]], na.rm = TRUE)
    
    p = ggplot(fileReport, aes_string(x="gid", color="Assembly", y=toPlot[n]))
    p = p + ylim(c(minCol,maxCol))
    
    minPlotScale = minScale[[toPlot[n]]]
    maxPlotScale = maxScale[[toPlot[n]]]
    
    if(!is.null(minPlotScale) && !is.null(maxPlotScale)){
      p = p + coord_cartesian(ylim = c(minPlotScale, maxPlotScale))
    }
    
    p = p + stat_smooth(method = "gam", formula = y ~ s(x), aes(fill=Assembly, group=Assembly, linetype = Assembly), se=FALSE)
    p = p + scale_linetype_manual(values = lineTypes)
    p = p + theme(axis.text.x = element_text(angle = 90, vjust = 1, hjust=1, size=2))
    p = p + ylab(toPlotNames[n])
    if(!missing(manualColor)){
      p = p + scale_color_manual(values=manualColor)
    }
    if(points){
      p = p + scale_shape_manual(values = c(0, 1, 2, 3, 4, 7, 8, 9, 10, 16, 17, 18,97, 98, 99, 100, 101, 102, 103, 104, 105, 106))
      p = p + geom_point(aes(colour = factor(Assembly), shape = factor(group)))
    }
    if(facet){
      p = p + facet_grid(Assembly ~ .)
    #  p <-ggplotly(p)
    #  htmlwidgets::saveWidget(as.widget(p), reportName)
    }
    
    print(p)
  }
  dev.off()
}

boxPlot <- function(toPlot, toPlotNames, fileReport, reportName, height=8, flip=FALSE, category = "group"){
  
  pdf(reportName, width=11, height=height)
  for (n in 1:length(toPlot)){
    p = ggplot(fileReport, aes_string(x=category, fill="group", y=toPlot[n]))
    p = p + geom_jitter() + geom_boxplot()
    p = p + theme(axis.text.x = element_text(angle = 90, hjust=1))
    if(flip){
      p = p + coord_flip()
    }
    print(p)
  }
  dev.off()
  
}

violinPlot <- function(toPlot, toPlotNames, fileReport, reportName, height=8){
  
  pdf(reportName, width=11, height=height)
  for (n in 1:length(toPlot)){
    p = ggplot(fileReport, aes_string(x="group", fill="group", y=toPlot[n]))
    p = p + geom_jitter() +  geom_violin()
    p = p + theme(axis.text.x = element_text(angle = 90, hjust=1))
    print(p)
  }
  dev.off()
  
}

parallelCoordinatesPlot <- function(outputPath, combinedRefReport){
  write.table(combinedRefReport, file.path(outputPath, "combined_ref_data.tsv"), sep="\t", row.names = FALSE)
}

buildPlots <- function(outputPath){
  #  fileReport$Assembly <- factor(fileReport$Assembly, levels = c(""))
  
  ## must be refactored
  assemblersTable <- table(assemblers$group)
  lines <- c()
  colors <- c()
  levels <- c()
  assemblerLevels <- c()
  linetypes <- c("solid", "dashed", "dotted", "dotdash", "longdash", "twodash")
  linecolors <- brewer.pal(n = length(names(assemblersTable)), name="Set1")
  index <- 1
  sapply(names(assemblersTable), function(name){
    group <- assemblersTable[name]        
    colors <<- append(colors, rep(linecolors[index], group))
    lines <<- append(lines, linetypes[1:group])
    levels <<- append(levels, name)
    assemblerLevels <<- append(assemblerLevels, assemblers$assembler[assemblers$group==name])
    index <<- index + 1 
  })
  
  referenceReport$Assembly <- factor(referenceReport$Assembly, levels = assemblerLevels)
  
  customLines <- lines
  ownColor <- colors
  
#  ownColor <- sort(rep(brewer.pal(n=6, name="Set1"),3))
#  customLines <- c(rbind(rep("solid", 30), rep("dashed", 30), rep("dotted", 30)))
#  referencePlot(infos, file.path(outputPath, "references.html"))
  boxPlot(toPlot, toPlotNames, referenceReport, file.path(outputPath, "boxplots_groups.pdf" ))
  violinPlot(toPlot, toPlotNames, referenceReport, file.path(outputPath, "violinplots.pdf" ))
  boxPlot(toPlot, toPlotNames, referenceReport, file.path(outputPath, "boxplots_refs.pdf" ), height=20, flip=TRUE, category="gid")
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
