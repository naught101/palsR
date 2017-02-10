library("RJSONIO")
#inputFile <- "data/TestInput_Global_MPI.json"
#inputFile <- "data/TestInput_Global_NEE_MPI.json"
inputFile <- "Tdata/estInput_Global_MPI_uncertainty.json"
#inputFile <- "data/TestInput_Global_MODIS.json"
#inputFile <- "data/TestInput_Global_MODIS_VISalbedo.json"
#inputFile <- "data/TestInput_Global_MODIS_NIRalbedo.json"
#inputFile <- "data/TestInput_Global.json"
#inputFile <- "data/TestInput_Global_GLEAM_v2A.json"
#inputFile <- "data/TestInput_Global_GLEAM_v2B.json"
#inputFile <- "data/TestInput_Global_LandFlux.json"
input <- fromJSON(paste(readLines(inputFile), collapse=""));
Rruntime = system.time(source("GlobalGSWP30.5Experiment_MultiVar.R"))
print(paste('Time to run:',Rruntime[3]))
output <- toJSON(output)
fileConn<-file("data/output_Global.json")
writeLines(output, fileConn)
close(fileConn)
