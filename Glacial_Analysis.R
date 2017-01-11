library(rhdf5) #Library for reading in HDF5 files such as matlab v.7.3
library(abind) #Library for combining arrays of multiple dimensions.
library(doParallel) #Library for parallel processing on for loops.
path_CRU = "/Users/kylechezik/Documents/Simon_Fraser_University/PhD_Research/Projects/Data/Original Data/GIS Data/Clark et al 2015 Glacier Data/CRU 1902-1999" #Path to CRU files.
path_NARR = "/Users/kylechezik/Documents/Simon_Fraser_University/PhD_Research/Projects/Data/Original Data/GIS Data/Clark et al 2015 Glacier Data/NARR 1980-2008" #Path to NARR files.
file_CRU = "CRU_HIST_R07_1902-1999.mat" #File name in CRU database.
file_NARR = "NARR_R07_1980-2008.mat" #File name in NARR database.
location_CRU = file.path(path_CRU,file_CRU) #Create the entire file path.
location_NARR = file.path(path_NARR,file_NARR) #Create the entire file path.

#List subfiles in the hdf5 files.
h5ls(location_NARR)
h5ls(location_CRU)

ice_data = abind(h5read(location_CRU, "/S")[c(68:78),,], h5read(location_NARR, "/S")[-29,,], along = 1) #Combine CRU and NARR datasets so 1969-2007 are present.
topo_data = h5read(location_NARR, "/B") #Get elevation data without ice.

registerDoParallel(cores=7) #Set number of cores for parallel processing.
#Determine the amount of ice lost from 
#dim(ice_data)[1]
ice_flow = foreach(i = c(2:5)) %dopar% {
	#browser()
	ice_year1 = ice_data[i-1,,]-topo_data
	ice_year2 = ice_data[i,,]-topo_data
	pmax(ice_year1 - ice_year2, 0)
}