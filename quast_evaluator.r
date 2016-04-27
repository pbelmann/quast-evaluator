#!/usr/bin/Rscript

library(grid)
library(ggplot2)
library(plyr)
library(plotly)
library(RColorBrewer)
library(mgcv)
library(functional)
options(warn=1)

toPlot=c("genomeFractionNormalized","X..predicted.genes..unique.","N50","NGA50","X..contigs","Total.length","X..misassemblies","Genome.fraction....","Duplication.ratio","GC....","Reference.GC....","X..mismatches.per.100.kbp","normalized.misassemblies.per.MB","normalized.mismatches.per.100.kbp")
toPlotNames=c("FractionNormalized","# predicted genes (unique)","N50","NGA50","# contigs","Total length","# misassemblies","Genome fraction (%)","Duplication ratio","GC","ref GC","# mismatches per 100 kbp","normalized misassemblies per MB","normalized mismatches per 100 kbp")
logPath <<- "out.log" 

init <- function(assemblersPath, infoPaths){
  assemblers <<- read.delim(assemblersPath, header=TRUE, stringsAsFactors=FALSE)
  assemblers <<- cbind.data.frame(assemblers)
  infos <<- read.delim(infoPaths, header=TRUE, stringsAsFactors=FALSE)
  infos <<- cbind.data.frame(infos)
}

printToLog <- function(str=""){
  cat(str,file=logPath, append=TRUE, "\n") 
}

calculateLook <- function(assemblers, assemblerNameColumn = "assembler", groupColumn = "group"){
  assemblersTable <- table(assemblers[groupColumn])
  customLines <<- c()
  ownColor <<- c()
  assemblerLevels <<- c()
  linetypes <<- c("solid", "dashed", "dotted", "dotdash", "longdash", "twodash")
  linecolors <<- brewer.pal(n = length(names(assemblersTable)), name="Set1")
  index <- 1
  
  sapply(names(assemblersTable), function(name){
    group <- assemblersTable[name]        
    ownColor <<- append(ownColor, rep(linecolors[index], group))
    customLines <<- append(customLines, linetypes[1:group])
    assemblerLevels <<- append(assemblerLevels, assemblers[assemblerNameColumn][assemblers[groupColumn]==name])
    index <<- index + 1 
  })
  
  customShapes <<- c(0, 1, 2, 3, 4, 7, 8, 9, 10, 16, 17, 18,97, 98, 99, 100, 101, 102, 103, 104, 105, 106)
}

prepareData <- function(existingCombinedRefPath, existingRefPath, plotConfPath=NULL){
  referenceReport <<- NULL
  combinedRefReport <<- NULL
  if(!is.null(plotConfPath)){
    plotConf <<- read.delim(plotConfPath, header=TRUE, stringsAsFactors=FALSE)
    plotConf <<- cbind.data.frame(plotConf)
  }
  
  
  getInfos <- function(ref, assemblerPath, assemblerName, assemblerGroup) {
    refPath = ref["path"]
    reportPath = file.path(assemblerPath, "runs_per_reference", refPath, "transposed_report.tsv")
    report = NULL
    if(file.exists(reportPath)){
        report = read.delim(reportPath, stringsAsFactors=FALSE)
        report = cbind.data.frame(report, gid=as.factor(ref["id"]), label=as.character(ref["label"]), 
				  refLength = as.numeric(ref["length"]),
				  refMapping = as.numeric(ref["mapping"]),
                                  abundance=as.double(ref["abundance"]), 
				                          gc=as.double(ref["gc"]), group=as.character(ref["group"]), 
				  refGenes=as.character(ref["genes"]),
				  assemblerGroup = assemblerGroup)
        report = report[report[, "Assembly"] == assemblerName, ]
        report$NGA50[report$NGA50 == "-"]  <- 0
      #  report$X..predicted.genes..unique.[report$X..predicted.genes..unique. == "NA"]  <- 0
        report$NGA50 <- as.numeric(as.character(report$NGA50))
    } else {
        report = data.frame(Assembly=assemblerName, gid=as.factor(ref["id"]), Genome.fraction....=0, 
			    X..predicted.genes..unique.=0,
                            N50=0, 
                            NGA50=0, 
                            X..contigs=0, 
                            Total.length=0, 
                            GC....=0, 
			    refMapping = as.numeric(ref["mapping"]),
			    refLength = as.numeric(ref["length"]),
                            Reference.GC....=0,
                            label=as.character(ref["label"]), 
                            group=as.character(ref["group"]),
                            abundance=as.double(ref["abundance"]), 
                            gc=as.double(ref["gc"]),
                            assemblerGroup = assemblerGroup, row.names = NULL)
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
    refReport = read.table(refPath, stringsAsFactors=TRUE, header = TRUE)
    refReport$X..predicted.genes..unique. = as.numeric(as.character(refReport$X..predicted.genes..unique.))
    referenceReport <<- cbind.data.frame(refReport)

    if(!exists("infos")){
      infos <- ddply(cbind(referenceReport), c("gid","group", "label", "refMapping"), head, 1)  
      assign("infos", infos, envir = .GlobalEnv)
    }
    extractedAssemblers = ddply(cbind(referenceReport), c("Assembly","assemblerGroup"), head, 1)  
    calculateLook(assemblers = extractedAssemblers, assemblerNameColumn = "Assembly", groupColumn = "assemblerGroup")
  }
  
  if(!missing(existingCombinedRefPath)){
    combRefPath = file.path(existingCombinedRefPath)
    combRefReport <<- read.table(combRefPath, stringsAsFactors=TRUE, header = TRUE)
    combinedRefReport <<- cbind.data.frame(combRefReport)
  } else {
      apply(assemblers, 1, iterAssemblers)
      referenceReport <<- cbind.data.frame(referenceReport, normalized.misassemblies.per.MB=referenceReport$X..misassemblies/(referenceReport$Total.length/1000000))
      referenceReport <<- cbind.data.frame(referenceReport, normalized.mismatches.per.100.kbp=referenceReport$X..mismatches.per.100.kbp/referenceReport$Total.length)
      referenceReport <<- cbind.data.frame(referenceReport, cov=(150*referenceReport$refMapping)/(referenceReport$refLength))
      referenceReport <<- cbind.data.frame(referenceReport, genomeFractionNormalized=(((referenceReport$Genome.fraction.... * referenceReport$Genome.fraction....)/(referenceReport$X..contigs))/10000))
      
      calculateLook(assemblers = assemblers)
  }
}


referencePlot <- function(refInfos, reportName){
  refInfos$id <- factor(refInfos$id, levels = refInfos$id[order(refInfos$abundance)])
  p = ggplot(refInfos, aes(x=id, y=abundance)) +
    geom_point() +
    theme_bw() + theme(panel.grid.major.x = element_blank(),panel.grid.minor.x = element_blank()) +
    theme(axis.text.x = element_text(angle = 90, vjust = 1, hjust=1, size=2)) +
    ylab("Abundance")
  p <-ggplotly(p)
  htmlwidgets::saveWidget(as.widget(p), reportName)
}

assemblyPlot <- function(toPlot, toPlotNames, fileReport, reportPath, subsets=c(), ggplotColor="Assembly", 
			    customShapes = c(),
                            xAxis = theme(axis.text.x = element_text(angle = 90, vjust = 1, hjust=1)),
                            x="gid" ,facet=FALSE, height=8, sortBy="abundance", log=FALSE, se=FALSE, 
                            points=TRUE, lineTypes=c(rep("solid",40)), manualColor=NULL){
  
  fileReport$gid <- factor(fileReport$gid, levels = fileReport$gid[order(fileReport[sortBy])])
  
  if(length(subsets) == 0 ){
	  subsets = c(function(data){ return(data)})
  }

  dir.create(reportPath)

  for (n in 1:length(toPlot)){
#    fileReport = remove_missing(fileReport, vars = toPlot[n], finite = TRUE)
    
    if(!(toPlot[n] %in% names(fileReport))){
      next
    }
    maxCol = max(fileReport[toPlot[n]], na.rm = TRUE)
    minCol = min(fileReport[toPlot[n]], na.rm = TRUE)
    
    p = ggplot(fileReport, aes_string(x=x, color=ggplotColor, y=toPlot[n]))
    p = p + ylim(c(minCol,maxCol))
    
    title = c(toPlotNames[n], " with references sorted by ", sortBy)
    p = p + ggtitle(paste(title, collapse = '')) +
    theme(plot.title = element_text(lineheight=.8))
    
    if(exists("plotConf") && toPlot[n] %in% plotConf$plot){
      minPlotScale = plotConf$min[which(plotConf$plot == toPlot[n])]
      maxPlotScale = plotConf$max[which(plotConf$plot == toPlot[n])]
      if(!is.null(minPlotScale) && !is.null(maxPlotScale)){
          p = p + coord_cartesian(ylim = c(minPlotScale, maxPlotScale))
      }
    }

    for (subsetIndex in 1:length(subsets)){
       p = p + stat_smooth(data=subsets[[subsetIndex]](fileReport), span = 0.25, aes(fill=Assembly, group=Assembly, linetype = Assembly), se=FALSE)
    }

    p = p + scale_linetype_manual(values = lineTypes)
    p = p + xAxis 
    p = p + ylab(toPlotNames[n])
    if(!missing(manualColor)){
      p = p + scale_color_manual(values=manualColor)
    }
    if(log){
      p = p + scale_x_log10()
    }

    if(length(customShapes) > 0){
        p = p + scale_shape_manual(values = customShapes)
    }

    if(facet){
      p = p + facet_grid(Assembly ~ .)
      p = p + guides(linetype=FALSE, fill=FALSE)
      p = p + geom_point(aes(colour = factor(group), shape = factor(group)))
    } else {
       if(points){
           legend = guide_legend(nrow = 4, title.position = "top", title.hjust=0.5)
	         p = p + geom_point(aes(colour = factor(Assembly), shape = group))
      	   p = p + guides(shape = legend , colour = legend, fill = legend, group = legend, linetype = legend)
       }
    }
    fileName = file.path(reportPath, paste(gsub("\\)","", gsub("\\(","", gsub("#", "", gsub("%", "", gsub(" ", "_",toPlotNames[n]))))), ".png", sep = ""))
    ggsave(fileName, p, width=11, height=height, device = "png")
  }
}

reorder_size <- function(x) {
  factor(x, levels = names(sort(table(x))))
}

boxPlot <- function(toPlot, toPlotNames, fileReport, reportPath, height=8, facet=FALSE, flip=FALSE, manualColor=NULL, data = function(data){ return(data)}, fill=NULL, category = "group"){
  fileReport$group <- factor(fileReport$group, levels =  names(sort(table(infos$group))))

  dir.create(reportPath)
  
  print(paste("Building plots in ", reportPath))
  
  for (n in 1:length(toPlot)){
    
    if(!(toPlot[n] %in% names(fileReport))){
      next
    }
    y=toPlot[n]
    x=category
    if(!missing(fill)){
      aes <- aes_string(x=x, fill=fill, y=y)
    } else {
      aes <- aes_string(x=x, y=y)
    }

    p = ggplot(data(fileReport), aes)
    p = p + geom_jitter() + geom_boxplot()
    p = p + theme(axis.text.x = element_text(angle = 90, hjust=1), plot.title = element_text(size=7))

    title = c(toPlotNames[n], " with references grouped by ANI score ")
    p = p + ggtitle(paste(title, collapse = ''))
    
    if(!missing(fill)){
      p = p + scale_fill_manual(values = manualColor, guide = FALSE)
    }

    if(flip){
      p = p + coord_flip()
    }

    if(facet){
      p = p + facet_grid(Assembly ~ .)
    }
    fileName = file.path(reportPath, paste(gsub("\\)","", gsub("\\(","", gsub("#", "", gsub("%", "", gsub(" ", "_",toPlotNames[n]))))), ".png", sep = ""))
    ggsave(fileName, p, device = "png")
  }
}

barPlot <- function(fileReport, reportName, height=8){

  pdf(reportName, width=11, height=height)
  p = ggplot(fileReport, aes(x=reorder_size(group)))
  p = p + geom_bar()
  p = p + theme(axis.text.x = element_text(angle = 90))
  print(p)
  dev.off()
}

violinPlot <- function(toPlot, toPlotNames, fileReport, reportName, height=8){
  
  pdf(reportName, width=11, height=height)
  for (n in 1:length(toPlot)){
    if(!(toPlot[n] %in% names(fileReport))){
      next
    }
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
  referenceReport$Assembly <- factor(referenceReport$Assembly, levels = assemblerLevels)
#  referencePlot(infos, file.path(outputPath, "references.html"))
  
  barPlot(infos, file.path(outputPath, "barplots_groups" ))
  boxPlot(toPlot, toPlotNames, referenceReport, file.path(outputPath, "boxplots_groups" ))
  boxPlot(toPlot, toPlotNames, referenceReport, file.path(outputPath, "boxplots_groups_facet" ), height=60, facet=TRUE)
#  boxPlot(toPlot, toPlotNames, referenceReport, file.path(outputPath, "boxplots_refs.pdf" ), height=20, fill="group", flip=TRUE, category="gid")

  xAxis = theme(legend.position="bottom", legend.box = "horizontal", axis.text.x = element_text(angle = 90, vjust = 1, hjust=1, size=1))

  xAxisFacet = theme(axis.text.x = element_text(angle = 90, vjust = 1, hjust=1, size=1))

  assemblyPlot(toPlot, toPlotNames, referenceReport, file.path(outputPath, "abundance" ), customShapes=customShapes, facet=FALSE, xAxis = xAxis)
  assemblyPlot(toPlot, toPlotNames, referenceReport, file.path(outputPath, "abundance_no_points" ), xAxis = xAxis, facet=FALSE, se=FALSE, points=FALSE, lineTypes=customLines, manualColor=ownColor)
  assemblyPlot(toPlot, toPlotNames, referenceReport, file.path(outputPath, "abundance-facet" ),customShapes=customShapes, xAxis = xAxisFacet, ggplotColor="group" ,facet=TRUE, height=40)

  assemblyPlot(toPlot, toPlotNames, referenceReport, file.path(outputPath, "abundance_log" ),customShapes=customShapes, x="abundance" , facet=FALSE, log=TRUE)
  assemblyPlot(toPlot, toPlotNames, referenceReport, file.path(outputPath, "abundance_no_points_log" ),customShapes=customShapes, x="abundance", facet=FALSE, se=FALSE, points=FALSE, log=TRUE, lineTypes=customLines, manualColor=ownColor)
  assemblyPlot(toPlot, toPlotNames, referenceReport, file.path(outputPath, "abundance-facet_log" ),customShapes=customShapes, xAxis = xAxisFacet, ggplotColor="group", x="abundance", facet=TRUE, log=TRUE, height=40)

  assemblyPlot(toPlot, toPlotNames, referenceReport, file.path(outputPath, "coverage" ), xAxis = xAxis, customShapes=customShapes, sortBy="cov", facet=FALSE)
  assemblyPlot(toPlot, toPlotNames, referenceReport, file.path(outputPath, "coverage_no_points" ), xAxis = xAxis, customShapes=customShapes, facet=FALSE, se=FALSE, sortBy="cov", points=FALSE, lineTypes=customLines, manualColor=ownColor)
  assemblyPlot(toPlot, toPlotNames, referenceReport, file.path(outputPath, "coverage-facet" ), xAxis = xAxisFacet, customShapes=customShapes, ggplotColor="group", facet=TRUE, sortBy="cov", height=40)

  assemblyPlot(toPlot, toPlotNames, referenceReport, file.path(outputPath, "coverage_log" ),customShapes=customShapes, x="cov", log=TRUE, sortBy="cov", facet=FALSE)
  assemblyPlot(toPlot, toPlotNames, referenceReport, file.path(outputPath, "coverage_no_points_log" ),customShapes=customShapes, facet=FALSE, x="cov", se=FALSE, log=TRUE, sortBy="cov", points=FALSE, lineTypes=customLines, manualColor=ownColor)
  assemblyPlot(toPlot, toPlotNames, referenceReport, file.path(outputPath,"coverage-facet_log" ), customShapes=customShapes, xAxis = xAxisFacet, ggplotColor="group", facet=TRUE, x="cov", log=TRUE, sortBy="cov", height=40)

  refGroups <- names(table(referenceReport$group))

  subsetsList <- unlist(lapply(refGroups, function(name){
	groupFun <- function(data,name){ return(subset(data, group==name));};
        return(Curry(groupFun, name=name)); 
  }))
 
  assemblyPlot(toPlot, toPlotNames, referenceReport, file.path(outputPath, "abundance-facet_multiple_smooth" ), customShapes=customShapes, xAxis = xAxisFacet, subsets=subsetsList, ggplotColor="group" ,facet=TRUE, height=40)
  assemblyPlot(toPlot, toPlotNames, referenceReport, file.path(outputPath, "abundance-facet_multiple_smooth_log" ), xAxis = xAxisFacet, customShapes=customShapes, subsets=subsetsList, ggplotColor="group", x="abundance", facet=TRUE, log=TRUE, height=40)
  assemblyPlot(toPlot, toPlotNames, referenceReport, file.path(outputPath, "coverage-facet_multiple_smooth" ), xAxis = xAxisFacet, customShapes=customShapes, subsets=subsetsList, ggplotColor="group", facet=TRUE, sortBy="cov", height=40)
  assemblyPlot(toPlot, toPlotNames, referenceReport, file.path(outputPath, "coverage-facet_multiple_smooth_log" ), xAxis = xAxisFacet, customShapes=customShapes, subsets=subsetsList, ggplotColor="group", facet=TRUE, x="cov", log=TRUE, sortBy="cov", height=40)
  mapply(function(subsetFunc,i){ 
     pdfName <- paste(paste("boxplots_assemblers", as.character(i), sep="_"), "pdf", sep=".")
     boxPlot(toPlot, toPlotNames, referenceReport, file.path(outputPath, pdfName), data = subsetFunc, flip = TRUE, fill = "Assembly", 
             manualColor = ownColor, category = "Assembly") 
  }, 
  subsetsList, refGroups)
  boxPlot(toPlot, toPlotNames, referenceReport, file.path(outputPath, "boxplots_groups_facet" ), height=60, facet=TRUE)

  assemblyPlot(toPlot, toPlotNames, referenceReport, file.path(outputPath, "gc"), facet=FALSE, sortBy="gc",  customShapes=customShapes, xAxis = xAxis)
  assemblyPlot(toPlot, toPlotNames, referenceReport, file.path(outputPath, "gc_no_points"), sortBy="gc", xAxis = xAxis, facet=FALSE, se=FALSE, points=FALSE, lineTypes=customLines, manualColor=ownColor)
  assemblyPlot(toPlot, toPlotNames, referenceReport, file.path(outputPath, "gc-facet"), sortBy="gc", customShapes=customShapes, xAxis = xAxisFacet, ggplotColor="group" ,facet=TRUE, height=40)
  
  assemblyPlot(toPlot, toPlotNames, referenceReport, file.path(outputPath, "genes"), facet=FALSE, sortBy="refGenes",  customShapes=customShapes, xAxis = xAxis)
  assemblyPlot(toPlot, toPlotNames, referenceReport, file.path(outputPath, "genes_no_points"), sortBy="refGenes", xAxis = xAxis, facet=FALSE, se=FALSE, points=FALSE, lineTypes=customLines, manualColor=ownColor)
  assemblyPlot(toPlot, toPlotNames, referenceReport, file.path(outputPath, "genes-facet"), sortBy="refGenes", customShapes=customShapes, xAxis = xAxisFacet, ggplotColor="group" ,facet=TRUE, height=40)
  
  parallelCoordinatesPlot(outputPath, combinedRefReport)
}

writeTables <- function(outputPath){
   write.table(combinedRefReport, file.path(outputPath, "combined_ref_data.tsv"), sep="\t", row.names = FALSE)
   write.table(referenceReport, file.path(outputPath, "ref_data.tsv"), sep="\t", row.names = FALSE)
}
