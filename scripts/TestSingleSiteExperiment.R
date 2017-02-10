library("RJSONIO")
inputFile <- "data/TestInput_SingleSite.json"
input <- fromJSON(paste(readLines(inputFile), collapse=""));
Rruntime = system.time(source("SingleSiteExperiment.R"))
print(paste('Time to run:',Rruntime[3]))
output <- toJSON(output)
fileConn<-file("data/output_SingleSite.json")
writeLines(output, fileConn)
close(fileConn)
