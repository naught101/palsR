# setwd("/media/nadja/Documents/CCRC/palsR/scripts")
library("RJSONIO")
#inputFile <- "data/TestInput_Regional_05deg_short_MultiVar_MPI.json"
#inputFile <- "data/TestInput_Regional_05deg_short_Anna_MPI.json"
inputFile <- "data/TestInput_Regional_05deg_short_Anna_MPI_NEE.json"
#inputFile <- "data/TestInput_Regional_05deg_short_MPI_uncertainty.json"
#inputFile <- "data/TestInput_Regional_05deg_short_MultiObs_ETuncertainty_long.json"
#inputFile <- "data/TestInput_Regional_05deg_short_Anna_MultiObs_ETuncertainty_long.json"
#inputFile <- "data/TestInput_Regional_05deg_short_Anna_MultiObs_ETuncertainty_longMap.json"
#inputFile <- "data/TestInput_Regional_05deg_short_GLEAM.json"
#inputFile <- "data/TestInput_Regional_05deg_short_GLEAM_v2A.json"
#inputFile <- "data/TestInput_Regional_05deg_short_Anna_GLEAM_v2A.json"
#inputFile <- "data/TestInput_Regional_05deg_short_GLEAM_v2B.json"
#inputFile <- "data/TestInput_Regional_05deg_short_Anna_GLEAM_v2B.json"
#inputFile <- "data/TestInput_Regional_05deg_short_MODIS.json"
#inputFile <- "data/TestInput_Regional_05deg_short_Anna_MODIS.json"
#inputFile <- "data/TestInput_Regional_05deg_short_MODIS_VISalbedo.json"
#inputFile <- "data/TestInput_Regional_05deg_short_MODIS_VISalbedo_QA.json"
#inputFile <- "data/TestInput_Regional_05deg_short_Anna_MODIS_VISalbedo_QA.json"
#inputFile <- "data/TestInput_Regional_05deg_short_MODIS_NIRalbedo.json"
#inputFile <- "data/TestInput_Regional_05deg_short_MODIS_NIRalbedo_QA.json"
inputFile <- "data/TestInput_Regional_05deg_short_Anna_MODIS_NIRalbedo_QA.json"
#inputFile <- "data/TestInput_Regional_05deg_short_LandFlux.json"
#inputFile <- "data/TestInput_Regional_05deg_short_Anna_LandFlux.json"
#inputFile <- "data/TestInput_Regional_05deg_short_Anna_GSCD_Qs.json"
#inputFile <- "data/TestInput_Regional_05deg_short_Anna_GSCD_RC.json"
#inputFile <- "data/TestInput_Regional_05deg_short_Anna_GSCD_BFI4.json"
input <- fromJSON(paste(readLines(inputFile), collapse=""));
Rruntime = system.time(source("RegionalAus0.5Experiment_MultiVar.R"))
print(paste('Time to run:',Rruntime[3]))
output <- toJSON(output)
fileConn<-file("data/output_Regional.json")
writeLines(output, fileConn)
close(fileConn)
