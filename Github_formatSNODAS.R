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

#specify the dates wanted
datesWanted<-seq.Date(as.Date("2003-10-01"), as.Date("2020-09-30"), by="day")

#set output directory - this is where the downloaded snodas files will save
outDir<-c("/Users/keirajohnson/Documents/SNODAS")

for (i in 1:length(datesWanted)) {
    
    #download daily snodas for one day
    GetSnodasDepthSweDate(datesWanted[i], outputDir = outDir, overwrite = T, quiet=FALSE)
  
}

# CROP_SNODAS_DATA(shapefile_location, start_day, end_day)
#crop data between given dates based on shapefile
# if data is missing???? error? download it? 
# input (shapefile)
# output (a single matrix)

#this is the beginning of all swe files, can be changed depending of parameter of interest
beg<-"us_ssmv11034tS__T0001TTNATS"

#make list of all files
swe_files<-list.files(path=outDir, pattern = beg)

#read in shapefile
shapefile_area<-readOGR("/Users/keirajohnson/Box Sync/Keira_Johnson/CoalCshapefile/layers/globalwatershed.shp")

#scaling factors for matrix reformatting
nCol <- 6935
nRow <- 3351 #columns and rows number: masked version of contiguous US
dataScaleFactor  <-  1000  #multiply the data this amount, both depth and SWE

#set coordinate system
wgs84 <- CRS("+proj=longlat +datum=WGS84")

#open lists for loop
swe_matrix_mm<-list()
dateList<-list()


for (i in 1:length(datesWanted)) {

  #get correct URL based on swe code and date
  #extract date from datesWanted list
  date<-gsub("-", "", datesWanted[i])
  
  #search for file name
  file_name<-str_subset(swe_files, date)
  
  #extract SWE file
  sweFile0  <- paste0(outDir, "/", file_name)
  
  #extract from dat.gz file
  sweCon  <- gzcon(file(sweFile0, "rb"))
  
  #read binary data from file above
  sweData <- readBin(sweCon, integer(), n=nRow*nCol, size=2, signed=TRUE, endian='big')/dataScaleFactor
  
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
  
  #close connections to avoid errors
  closeAllConnections()
  
  #write date to file - this way if date is skipped you will have record of which is downloaded
  dateList[[i]]<-datesWanted[i]
  
}
  
#turn into array (3D matrix - [lat, long, date])
swe_array<-array(as.numeric(unlist(swe_matrix_mm)), dim=c(8, 16, length(swe_matrix_mm)))

#if want to plot matrix data back convert into raster after cropping using "raster" function and extent
#of shapefile
#have to do for each individual level of matrix



