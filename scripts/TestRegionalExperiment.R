library("RJSONIO")
inputFile <- "data/TestInput_Regional.json"
input <- fromJSON(paste(readLines(inputFile), collapse=""));
Rruntime = system.time(source("RegionalAus0.25Experiment.R"))
print(paste('Time to run:',Rruntime[3]))
output <- toJSON(output)
fileConn<-file("data/output_Regional.json")
writeLines(output, fileConn)
close(fileConn)
