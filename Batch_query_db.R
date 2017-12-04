# This is the R script for batch query
# Designed for command line use, given the required packages are installed
# 
# Example usage:
# Rscript Batch_query_db.R ./GeoDB ./example/input.csv ./example/output.csv
#
# ./GeoDB the folder containing the query database generated in previous step
#         see: Gen_data_proc.R
# ./example/input.csv the input spreadsheet given coordinates
# ./example/output.csv the output destination of the queried results
#

# Loading dependencies
cat('Loading dependencies: raster, rgeos, rgdal, maptools, dplyr \n')
suppressMessages(library(raster))
suppressMessages(library(rgeos))
suppressMessages(library(rgdal))
suppressMessages(library(maptools))
suppressMessages(library(dplyr))

# Variable command line
args = commandArgs(trailingOnly=TRUE)

# Assign variables
dpath <- args[1]
inputcsv <- args[2]
outputcsv <- args[3]
cat(paste0('Loading geo database from ',dpath, '\n'))


# Checking and loading database
cat('Checking curated polygon database')
flist <- dir(dpath, pattern = 'Query_geo_db')
nfile <- length(flist)
if (nfile == 1){
  cat(paste0('  -- ', flist, '\n'))
} else {
  cat(paste0('  -- Error: Found ', nfile, ' of curated polygon database, please check\n'))
  stop("", call.=FALSE)
}
load(paste0(dpath, '/', flist))

cat('Checking database of raster population density')
flist <- dir(dpath, pattern = 'density')
nfile <- length(flist)
if (nfile == 1){
  cat(paste0('  -- ', flist, '\n'))
} else {
  cat(paste0('  -- Error: Found ', nfile, ' of density GeoTiff, please check\n'))
  stop("", call.=FALSE)
}
draster <- raster(paste0(dpath, '/', flist),  values=TRUE)
draster <- crop(draster, extent(-180, -40, 10,72))

cat('Checking database of raster PM2.5')
flist <- dir(dpath, pattern = 'PM')
nfile <- length(flist)
if (nfile == 1){
  cat(paste0('  -- ', flist, '\n'))
} else {
  cat(paste0('  -- Error: Found ', nfile, ' of PM2.5 GeoTiff, please check\n'))
  stop("", call.=FALSE)
}
pm25 <- crop(raster(paste0(dpath, '/', flist)), extent(-180, -40, 0,72))

cat('Checking database of raster NO2')
flist <- dir(dpath, pattern = 'NO2')
nfile <- length(flist)
if (nfile == 1){
  cat(paste0('  -- ', flist, '\n'))
} else {
  cat(paste0('  -- Error: Found ', nfile, ' of NO2 GeoTiff, please check\n'))
  stop("", call.=FALSE)
}
no2 <- crop(raster(paste0(dpath, '/', flist)), extent(-180, -40, 0,72))

# Input spreadsheet
# Current version is using a csv spreadsheet
# First column is ID, second column is Longitude, and the third is the latitude
# The naming convention is as mentioned above
#
if (!file.exists(inputcsv)){
cat(paste0('Cannot locate ', inputcsv,', please check \n'))
stop("", call.=FALSE)
}

flist <- dir(inputcsv)
cat(paste0('Reading ', inputcsv, '\n'))
bq <- read.csv(inputcsv, header=TRUE, as.is=TRUE)

coordinates(bq) <- ~ Longitude + Latitude
crs(bq) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"

bq_fc <- over(bq, fc_clean)
bq_fc <- bq_fc[,5:dim(bq_fc)[2]]
PopDensity <- extract(draster, bq)
NO2 <- extract(no2, bq)
PM25 <- extract(pm25, bq)
utmStr <- "+proj=utm +zone=%d +datum=NAD83 +units=m +no_defs +ellps=GRS80 +towgs84=0,0,0"
pUTM <- spTransform(bq, CRS(sprintf(utmStr, 10))) # UTM in hard code number, might need to fix in the future

# Need to check this part
proxRd <- gDistance(pUTM, rd_trans)
ID <- bq$ID

bq_df <- data.frame(ID, bq_fc, PopDensity, NO2, PM25, proxRd)

# Writing output
cat(paste0('Writing ', outputcsv, '\n'))
write.csv(bq_df, file=outputcsv, quote=FALSE, row.names=FALSE)

cat('Done! \n')

