#downloading snodas data for contiguous us by day
#based on a shape file, crop data to given area
#combines into SWE matrix with dimensions as (ongitude, latitude, time)


#=-=----- LIBRARY FUNCTIONS --------
# SPECIFCY_SNODAS_LOCATIONFMT(directory, naming_scheme, etc)
# DOWNLOAD_SNOW_DATA(start_day, end_day, directory)
# download snodas data for each and every day in given range and output to specified directory
# 
# DOWNLOAD_SNOW_DATA_ENUM(list_of_days, directory)
#download snow data for each day in list and output to specified directory

datesWanted <- ""
#specify the dates wanted
SPECIFY_SNODAS_DATES <- function(start_date, end_date) {
  #do error checking???
  datesWanted<-seq.Date(as.Date(start_date), as.Date(end_date), by="day")
}


#set output directory - this is where the downloaded snodas files will save
outDir<- "" 

DOWNLOAD_SNODAS <- function(outDir) {
  #use default output directory specified relative to the running directory??
  for (i in 1:length(datesWanted)) {
      #download daily snodas for one day
      #print("Downloaded {i} of length(datesWanted)")
      GetSnodasDepthSweDate(datesWanted[i], outputDir = outDir, overwrite = T, quiet=FALSE)
  }
  #close connections to avoid errors
  closeAllConnections()
}

# CROP_SNODAS_DATA(shapefile_location, start_day, end_day)
#crop data between given dates based on shapefile
# if data is missing???? error? download it? 
# input (shapefile)
# output (a single matrix)
CROP_SNODAS_SHP <- function(shapefile_location) {
  #allow specification of different start/end days for crop vs download?? 
  #currently doable by recalling SPECIFY_SNODAS_DATES

  #this is the beginning of all swe files, can be changed depending of parameter of interest
  datafile_prefix<-"us_ssmv11034tS__T0001TTNATS"

  #make list of all files
  swe_files<-list.files(path=outDir, pattern = datafile_prefix)

  #read in shapefile
  #check that the file exists and is a valid shapefile
  shapefile_area<-readOGR(shapefile_location)

  #scaling factors for matrix reformatting
  #columns and rows are defined by the data matrix size downloaded from SNODAS
  nCol <- 6935
  nRow <- 3351 #columns and rows number: masked version of contiguous US
  dataScaleFactor  <-  1000  #multiply the data this amount, both depth and SWE

  #the coordianate system used in SNODAS is the WGS84 system, so we use that
  #set coordinate system
  wgs84 <- CRS("+proj=longlat +datum=WGS84")

  #declare lists for loop
  #we need to define two lists to store the dates and the data matrix
  swe_matrix_mm<-list()
  dateList<-list()


  for (i in 1:length(datesWanted)) {

    #format the string using swe code and date
    working_date <-gsub("-", "", datesWanted[i])
    
    #search for file name
    file_name<-str_subset(swe_files, working_date)
    
    #extract SWE file
    file_location  <- paste0(outDir, "/", file_name)
    
    #extract from dat.gz file
    swe_data_binary  <- gzcon(file(file_location, "rb"))
    
    #read binary data from file above
    sweData <- readBin(swe_data_binary, integer(), n=nRow*nCol, size=2, signed=TRUE, endian='big')/dataScaleFactor
    
    #remove error values
    sweData[which(sweData==-9.999)]<-NA
    
    #convert binary file to matrix
    sweDataMatrix<-matrix(sweData, ncol=nCol, nrow=nRow, byrow = T)
    
    #convert units to mm
    sweDataMatrix_mm<-sweDataMatrix*dataScaleFactor
    
    #georeference matrix to turn into raster
    sweRaster <- raster(sweDataMatrix_mm, ymn=24.9504, ymx=52.8754, xmn=-124.7337, xmx=-66.9421, crs=wgs84)
    
    #extract data from raster for given watershed
    swe_mm<-crop(sweRaster, shapefile_area)
    
    #convert back to matrix and add to list - each item in list is one 2D matrix for one day
    swe_matrix_mm[[i]]<-as.matrix(swe_mm)
    
    #write date to file - this way if date is skipped you will have record of which is downloaded
    dateList[[i]]<-datesWanted[i]
    
  }
    
  #turn into array (3D matrix - [lat, long, date])
  swe_array<-array(as.numeric(unlist(swe_matrix_mm)), dim=c(8, 16, length(swe_matrix_mm)))


}

#if want to plot matrix data back convert into raster after cropping using "raster" function and extent
#of shapefile
#have to do for each individual level of matrix

default_start_date <-"2003-10-01"
default_end_date <- "2020-09-30"
defaultOutdir <-c("/Users/keirajohnson/Documents/SNODAS")
default_shapefile_location = "/Users/keirajohnson/Box Sync/Keira_Johnson/CoalCshapefile/layers/globalwatershed.shp"
TEST_SNODAS_CROP <- (start_date, end_date, output_dir, shapefile_location) {
  SPECIFY_SNODAS_DATES(start_date, end_date)
  DOWNLOAD_SNODAS(output_dir)
  CROP_SNODAS_SHP(shapefile_location)
}

TEST_SNODAS_CROP(default_start_date, default_end_date, defaultOutdir, default_shapefile_location)
