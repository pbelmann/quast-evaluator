#!/usr/bin/Rscript

library(jsonlite)
library(dplyr)
args = commandArgs(trailingOnly=TRUE)
path = args[1] #commits_info.tsv
prefix = args[2] #/cami/workspace...
postfix = args[3]
output  = "/web/src/data.js"

inputDataframe = cbind.data.frame(read.delim(path, header = TRUE, stringsAsFactors =
                                               FALSE))
inputDataframe$path <- file.path(prefix, inputDataframe$path, postfix)

fillDataset <- function(paths) {
  for (file in paths) {

      if (!exists("dataset")) {
        print(file)
        dataset <- cbind.data.frame(read.delim(file, header=TRUE, stringsAsFactors=FALSE))
       # dataset <- read.table(file, header = TRUE, sep = "\t")
        dataset$path = file
      } else {
        temp_dataset <- cbind.data.frame(read.delim(file, header=TRUE, stringsAsFactors=FALSE))
        temp_dataset$path = file
        dataset <- rbind(dataset, temp_dataset)
        rm(temp_dataset)
      }
  }
  dataset
}

total <- merge(inputDataframe, fillDataset(inputDataframe$path), by = c("path"))

total[is.na(total)] <- 0
write(toJSON(total), file.path(output), append = TRUE)
system("npm --prefix /web  install  /web")
system("npm run --prefix /web  build ")
system("cp /web/build/index.html /biobox.yaml /output")
