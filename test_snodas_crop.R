source(snodas_crop_library.R)

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