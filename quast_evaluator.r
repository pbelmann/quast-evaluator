# !/usr/bin/Rscript

library(grid)
library(ggplot2)
library(plyr)
library(plotly)
library(RColorBrewer)
library(mgcv)
library(MASS)
library(functional)
library(htmlwidgets)
library(parcoords)
options(warn=1)

# Categories for which the plots are build
toPlot=c("Genome.fraction....")
toPlotNames=c("Genome fraction (%)")

logPath <<- "out.log"

init <- function(assemblersPath, infoPaths){
  # Initializes dataframes used in other functions
  #
  # Args:
  #  assemblerPath: path to a file listing assemblers that were provided to QUAST
  #  infoPaths: path to a file listing references that were provided to QUAST
  #
  assemblers <<- cbind.data.frame(read.delim(assemblersPath, header=TRUE, stringsAsFactors=FALSE))
  infos <<- cbind.data.frame(read.delim(infoPaths, header=TRUE, stringsAsFactors=FALSE))
}

printToLog <- function(str=""){
  cat(str,file=logPath, append=TRUE, "\n")
}

calculateLook <- function(assemblers, assemblerNameColumn = "name", groupColumn = "group"){
  # Computes the colors and line shapes of lines for consistent use in all boxplots and line plots
  #
  # Args:
  #  assemblers: assemblers dataframe
  #  assemblerNameColumn: column with name of the assemblers
  #  groupColumn: column with the group identifier of an assembler
  #
  assemblersTable <- table(assemblers[groupColumn])
  customLines <<- c()
  ownColor <<- c()
  assemblerLevels <<- c()
  linetypes <<- c("solid", "dashed", "dotted", "dotdash", "longdash", "twodash", "F1")
  linecolors <<- c("#66C2A5", "#FC8D62", "#8DA0CB", "#A6D854", "#FFD92F", "#E5C494", "#B3B3B3")
  index <- 1

  sapply(names(assemblersTable), function(name){
    group <- assemblersTable[name]
    ownColor <<- append(ownColor, rep(linecolors[index], group))
    customLines <<- append(customLines, linetypes[1:group])
    assemblerLevels <<- append(assemblerLevels, assemblers[assemblerNameColumn][assemblers[groupColumn]==name])
    index <<- index + 1
  })

  customShapes <<- c(0, 1, 2, 3, 4, 7, 8, 9, 10, 16, 17, 18, 97, 98, 99, 100, 101, 102, 103, 104, 105, 106)
}

prepareData <- function(existingCombinedRefPath, existingRefPath, plotConfPath=NULL){
  # This function prepares the dataframes used in all plot functions below.
  # It prepares two dataframes:
  # - combinedRef dataframe with combined (general assembly) metrics for each assembler
  # - refPath dataframe with metrics for each reference independently
  # If these dataframes were already computed and exported to a file then they can be provided to this function.
  #
  # Args:
  #  existingCombinedRefPath: path to an exported combined references dataframe
  #  existingRefPath: path to an exported references dataframe
  #  plotConfPath: path to a file describing the configuration of a plot
  #
  referenceReport <<- NULL
  combinedRefReport <<- NULL
  contigsReport <<- NULL
  if(!is.null(plotConfPath)){
    plotConf <<- read.delim(plotConfPath, header=TRUE, stringsAsFactors=FALSE)
    plotConf <<- cbind.data.frame(plotConf)
  }

  getInfos <- function(ref, assemblerPath, assemblerName, assemblerGroup, assemblerRealName) {
    refPath = ref["path"]
    reportPath = file.path(assemblerPath, "runs_per_reference", refPath, "transposed_report.tsv")
    report = NULL

    #if quast reference folder exists, it will be parsed otherwise default values will be set
    if(file.exists(reportPath)){

        report = read.delim(reportPath, stringsAsFactors=FALSE)
        report = cbind.data.frame(report, gid=as.factor(ref["id"]),
				  refLength = as.numeric(ref["length"]),
				  refMapping = as.numeric(ref["mapping"]),
                                  group=as.character(ref["group"]),
				  assemblerGroup = assemblerGroup, row.names=NULL)

        report = report[report[, "Assembly"] == assemblerName, ]
	      report["assemblerRealName"] <- rep(assemblerRealName,nrow(report))
        report$NGA50[report$NGA50 == "-"]  <- 0
        report$NGA50 <- as.numeric(as.character(report$NGA50))
    } else {
        report = data.frame(Assembly=assemblerName, gid=as.factor(ref["id"]), Genome.fraction....=0,
			    assemblerRealName=assemblerRealName,
			    X..predicted.genes..unique.=0,
                            N50=0,
                            NGA50=0,
                            X..contigs=0,
                            Total.length=0,
                            GC....=0,
			    refMapping = as.numeric(ref["mapping"]),
			    refLength = as.numeric(ref["length"]),
                            Reference.GC....=0,
                            group=as.character(ref["group"]),
                            assemblerGroup = assemblerGroup, row.names = NULL)
    }
    if (exists("referenceReport")){
      referenceReport <<- rbind.fill(referenceReport, report)
    } else {
      referenceReport <<- report
    }
  }

  iterInfos <- function(assemblerRow){
    assemblerPath = file.path(assemblerRow["path"])
    contigsReport <<- NULL
    if(file.exists(assemblerPath)){
      apply(infos,1,function(ref) getInfos(ref, assemblerPath=normalizePath(assemblerRow["path"]), assemblerName=assemblerRow["assembler"], assemblerGroup=assemblerRow["group"], assemblerRealName=assemblerRow["name"]))
    } else {
      printToLog(sprintf("Directory %s does not exist", assemblerPath))
    }
  }

  combinedFileReport <- function(assemblerRow){
    assemblerPath = file.path(assemblerRow["path"])
    assemblerName = assemblerRow["assembler"]
    reportPath = file.path(assemblerPath, "combined_reference" , "transposed_report.tsv")

    print(assemblerName)
    print(reportPath)
    if(file.exists(reportPath)){
    print("exists")
      report = read.delim(reportPath, stringsAsFactors=FALSE)
      report = cbind.data.frame(report)
      report = report[report[, "Assembly"] == assemblerName, ]
      report["name"] <- rep(assemblerRow["name"], nrow(report))
      if (exists("combinedRefReport")){
        combinedRefReport <<- rbind.fill(combinedRefReport, report)
      } else {
        combinedRefReport <<- report
      }
    }
  }

  iterAssemblers  <- function(assemblerRow){
    print(assemblerRow)
    iterInfos(assemblerRow)
    combinedFileReport(assemblerRow)
  }

  if(!missing(existingRefPath)){
    refPath = file.path(existingRefPath)
    refReport = read.table(refPath, stringsAsFactors=TRUE, header = TRUE)
    referenceReport <<- cbind.data.frame(refReport)

    if(!exists("infos")){
      infos <- ddply(cbind(referenceReport), c("gid","group", "refMapping"), head, 1)
      assign("infos", infos, envir = .GlobalEnv)
    }
    extractedAssemblers = ddply(cbind(referenceReport), c("Assembly","assemblerGroup"), head, 1)
    calculateLook(assemblers = extractedAssemblers, assemblerNameColumn = "assemblerRealName", groupColumn = "assemblerGroup")
  }

  if(!missing(existingCombinedRefPath)){
    combRefPath = file.path(existingCombinedRefPath)
    combRefReport <<- read.table(combRefPath, stringsAsFactors=TRUE, header = TRUE)
    combinedRefReport <<- cbind.data.frame(combRefReport)
  } else {
    print(assemblers)
    apply(assemblers, 1, iterAssemblers)
    referenceReport <<- cbind.data.frame(referenceReport, cov=(150*referenceReport$refMapping)/(referenceReport$refLength))
   calculateLook(assemblers = assemblers)
  }
}

linePlot <- function(toPlot, toPlotNames, fileReport, reportOutputPath, subsets=c(), ggplotColor="assemblerRealName",
			    customShapes = c(),
          xAxis = theme(axis.text.x = element_text(angle = 90, vjust = 1, hjust=1)),
          x="gid" ,facet=FALSE, height=35, log=FALSE, se=FALSE,
          points=TRUE, lineTypes=c(rep("solid",40)), manualColor=NULL){
  # This function builds a line plot for each assembler.
  #
  # Args:
  #  toPlot: list of categories to plot (e.g: genome fraction)
  #  toPlotName: names of the categories to show on the plot
  #  fileReport: dataframe with assemblers and the categories specified in 'toPlot'
  #  reportOutputPath: output path
  #  subsets: Array of functions that compute the subset of data
  #  ggplotColor: Column that is used to compute the colors
  #  customShapes: Custom shapes that will be used for the points
  #  xAxis: Configuration for the axis
  #  x: Dataframe column that is used for the xAxis
  #  facet: Plot will be build with facets
  #  height: Height of the plot
  #  log: Build axis in log scale
  #  se: Show confidence interval
  #  points: Show points.
  #  lineTypes: Linetypes that are used for the lines
  #  manualColor: Manual color specification for points and lines.

  if(length(subsets) == 0 ){
	  subsets = c(function(data){ return(data)})
  }

  dir.create(reportOutputPath)

  for (n in 1:length(toPlot)){
    if(!(toPlot[n] %in% names(fileReport))){
      next
    }

    print("before")
    maxCol = max(fileReport[toPlot[n]], na.rm = TRUE)
    minCol = min(fileReport[toPlot[n]], na.rm = TRUE)


    p = ggplot(fileReport, aes_string(x=x, color=ggplotColor, y=toPlot[n]))
    p = p + ylim(c(minCol,maxCol))

    title = c(toPlotNames[n])
    theme(plot.title = element_text(lineheight=.8))

    if(exists("plotConf") && toPlot[n] %in% plotConf$plot){
      minPlotScale = plotConf$min[which(plotConf$plot == toPlot[n])]
      maxPlotScale = plotConf$max[which(plotConf$plot == toPlot[n])]
      if(!is.null(minPlotScale) && !is.null(maxPlotScale)){
          p = p + coord_cartesian(ylim = c(minPlotScale, maxPlotScale))
      }
    }

    for (subsetIndex in 1:length(subsets)){
       p = p + stat_smooth(data=subsets[[subsetIndex]](fileReport), span = 0.25, aes(fill=assemblerRealName, group=assemblerRealName, linetype = assemblerRealName), se=FALSE)
    }

    p = p + scale_linetype_manual(values = lineTypes)
    p = p + xAxis
    if(!missing(manualColor)){
      p = p + scale_color_manual(values=manualColor)
    }
    if(log){
      p = p + scale_x_log10()
    }

    if(length(customShapes) > 0){
        p = p + scale_shape_manual(values = customShapes)
    }

    p = p + ylab(toPlotNames[n])
    p = p + xlab(x)

    if(facet){
      p = p + facet_grid(assemblerRealName ~ .)
      p = p + guides(linetype=FALSE, fill=FALSE, shape=FALSE, colour = FALSE)
      p = p + geom_point(aes(colour = factor(group), shape = factor(group)), alpha=0.80)
      p = p + scale_colour_manual(values = c("#7FC97F", "#386CB0", "#BF5B17"))
      p = p + theme_bw()
      p = p + theme(axis.text.x = element_text(color="black", size=20), text=element_text(family="Helvetica", size = 22), axis.title = element_text(size=22), axis.text.y = element_text(color="black", size=20), plot.title = element_text(size=22), strip.text = element_text(size=12))
    } else {
       if(points){
           legend = guide_legend(nrow = 4, title.position = "top", title.hjust=0.5)
	   p = p + geom_point(aes(colour = factor(assemblerRealName), shape = group))
      	   p = p + guides(shape = legend , colour = legend, fill = legend, group = legend, linetype = legend)
       }
       p = p + theme(legend.position="bottom", legend.direction = "horizontal", legend.box = "horizontal")
    }

    fileName = file.path(reportOutputPath, paste(gsub("\\)","", gsub("\\(","", gsub("#", "", gsub("%", "", gsub(" ", "_",toPlotNames[n]))))), ".pdf", sep = ""))
    ggsave(fileName, p, height=height, width=21, device = "pdf")
  }
}

boxPlot <- function(toPlot, toPlotNames, fileReport, reportOutputPath,
                    height=8, facet=FALSE, flip=FALSE, manualColor=NULL,
                    data = function(data){ return(data)}, xlabel = "Assemblers", fill=NULL ,category = "group", title = ""){
  # This function builds a boxplot plot for each assembler.
  #
  # Args:
  #  toPlot: list of categories to plot (e.g: genome fraction)
  #  toPlotName: names of the categories to show on the plot
  #  fileReport: Dataframe with assemblers and the categories specified in 'toPlot'
  #  reportOutputPath: output path
  #  height: Height of the plot
  #  facet: Plot will be build with facets
  #  flip: Flip x and y axis
  #  manualColor: Manual color specification for the bars.
  #  data: Function that is used to get the subset of the dataframe for the boxplots.
  #  fill: Dataframe column that is used for the color of the bars
  #  category: Category that is used for the barplot.
  #  title: Title of the plot

  fileReport$group <- factor(fileReport$group, levels =  names(sort(table(infos$group))))
  dir.create(reportOutputPath)

  print(paste("Building plots in ", reportOutputPath))
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
    p = p + geom_boxplot()
    p = p + theme_bw()
    p = p + theme(axis.text.x = element_text(color="black", size=17), text=element_text(family="Helvetica"), axis.title = element_text(size=17), axis.title.y=element_blank(), axis.text.y = element_text(color="black", size=17), plot.title = element_text(size=17))
    name = "Genome Fraction [%]"

    p = p + ggtitle(title)
    if(!missing(fill)){
      p = p + scale_fill_manual(values = manualColor, guide = FALSE)
    }

    if(flip){
      p = p + coord_flip()
    }

    p = p + ylab(name)
    p = p + xlab(name)

    if(facet){
      p = p + facet_grid(assemblerRealName ~ .)
    }
    fileName = file.path(reportOutputPath, paste(gsub("\\)","", gsub("\\(","", gsub("#", "", gsub("%", "", gsub(" ", "_",toPlotNames[n]))))), ".pdf", sep = ""))
    ggsave(fileName, p, height=height, device = "pdf")
  }
}

parallelCoordinatesPlot <- function(outputPath, combinedRefReport){
  # This function builds and exports a parallel coordinates html plot.
  #
  # Args:
  #   outputPath: Path to a directory where the files will be stored
  #   combinedRefReport: Combined ref dataframe.

  plot <- parcoords(combinedRefReport[,c("Assembly",
                                 "N50","X..contigs.....0.bp.",
                                 "Total.length",
                                 "X..misassemblies",
                                 "Unaligned.length",
                                 "Genome.fraction....",
                                 "Duplication.ratio",
                                 "X..mismatches.per.100.kbp",
                                 "X..predicted.genes..unique.",
                                 "NA50")],
            color=list(colorBy="Assembly", colorScale=htmlwidgets::JS('d3.scale.category10()')),
            width = 1700,
            rownames=F)

  fileName <- file.path(outputPath, paste("parallel", ".html", sep = ""))
  saveWidget(plot, file = fileName)
}


buildPlots <- function(outputPath){
  # This is a wrapper function that contains different configurations for line, box and paralle coordinates plots
  # Args:
  #  outputPath: Path to an output directory.
  #

  print("Starting with Plots")
  referenceReport$assemblerRealName <- factor(referenceReport$assemblerRealName, levels = assemblerLevels)
  xAxis = theme(legend.position="bottom", legend.box = "horizontal", axis.text.x = element_text(angle = 90, vjust = 1, hjust=1, size=10))

  xAxisFacet = theme(axis.text.x = element_text(angle = 90, vjust = 1, hjust=1, size=10))

  refGroups <- names(table(referenceReport$group))

  subsetsList <- unlist(lapply(refGroups, function(name){
    groupFun <- function(data,name){ return(subset(data, group==name));};
    return(Curry(groupFun, name=name));
  }))
  linePlot(toPlot, toPlotNames, referenceReport, file.path(outputPath,"coverage-facet-subset_log"), subsets=subsetsList , customShapes=customShapes, xAxis = xAxisFacet, ggplotColor="group", facet=TRUE, x="cov", log=TRUE)

  allRefGroups <- c("all_references")
  allSubsetsList <- unlist(lapply(allRefGroups, function(name){
    groupFun <- function(data,name){ return(data);};
    return(Curry(groupFun, name=name));
  }))

  mapply(function(subsetFunc,i){
    pdfName <- paste("boxplots_assemblers", as.character(i), sep="_")
    boxPlot(toPlot, toPlotNames, referenceReport, file.path(outputPath, pdfName), data = subsetFunc, flip = TRUE, fill = "assemblerRealName",
            manualColor = ownColor, category = "assemblerRealName", title = as.character(i))
  },
  subsetsList, refGroups)

  mapply(function(subsetFunc,i){
    pdfName <- paste("boxplots_assemblers", as.character(i), sep="_")
    boxPlot(toPlot, toPlotNames, referenceReport, file.path(outputPath, pdfName), data = subsetFunc, flip = TRUE, fill = "assemblerRealName",
            manualColor = ownColor, category = "assemblerRealName", title = as.character(i))
  },
  allSubsetsList, allRefGroups)
  parallelCoordinatesPlot(outputPath, combinedRefReport)
}

writeTables <- function(outputPath){
  # Exports the references and combined references plot
  # Args:
  #   - outputPath: Path to the output directory

   write.table(combinedRefReport, file.path(outputPath, "combined_ref_data.tsv"), sep="\t", row.names = FALSE)
   write.table(referenceReport, file.path(outputPath, "ref_data.tsv"), sep="\t", row.names = FALSE)
}
