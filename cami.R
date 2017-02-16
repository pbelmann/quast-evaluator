library(jsonlite)
args = commandArgs(trailingOnly=TRUE)
path = args[1] #commits_info.tsv
prefix = args[2] #/cami/workspace...
postfix = args[3]
output  = "/web/src/data.js"

#path = "/home/belmann/projects/quast-evaluator/test.tsv"
#prefix = "/home/belmann/projects/quast-evaluator/framework"
#postfix = "test.tsv"
#output = "/home/belmann/projects/quast-evaluator/web/src/data.js"

inputDataframe = cbind.data.frame(read.delim(path, header = TRUE, stringsAsFactors =
                                               FALSE))
inputDataframe$path <- file.path(prefix, inputDataframe$path, postfix)

fillDataset <- function(paths) {
  for (file in paths) {
    if (file.exists(file)) {
      if (!exists("dataset")) {
        dataset <- read.table(file, header = TRUE, sep = "\t")
        dataset$path = file
      } else {
        temp_dataset <- read.table(file, header = TRUE, sep = "\t")
        temp_dataset$path = file
        dataset <- rbind(dataset, temp_dataset)
        rm(temp_dataset)
      }
    }
  }
  dataset
}

total <- merge(inputDataframe, fillDataset(inputDataframe$path), by = c("path"))
write(toJSON(total), file.path(output), append = TRUE)
system("npm --prefix /web  install  /web")
system("cp /web/build/index.html /biobox.yaml /output")