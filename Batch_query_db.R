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
suppressMessages(library(FNN))

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
flist <- dir(dpath, pattern = 'PM25_2010')
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

# Getting data of Lead risks based on VOX model
load(paste0(dpath, '/VOXLeadRisk.rda'))

# Legality as additional module
# Currently not integrated in the data batch until the final part is verified
law <- read.table(paste0(dpath, '/State_cannabis_use_laws.csv'), header=TRUE, sep='\t')
names(law)[1] <- 'Name'
lawfips <- read.csv(paste0(dpath, '/stat_SFIPS.csv'), header=TRUE)
law <- left_join(lawfips, law)
law <- law[,c(3,5:dim(law)[2])]
law$SFIPS <- sprintf('%02d',law$SFIPS)

# Input spreadsheet
# Current version is using a csv spreadsheet
# First column is ID, second column is Longitude, and the third is the latitude
# The naming convention is as mentioned above
#
if (!file.exists(inputcsv)){
cat(paste0('Cannot locate ', inputcsv,', please check \n'))
stop("", call.=FALSE)
}

# REading new pollution data
cat(paste0('Reading supplement pollution data \n'))
gridsite <- readRDS(paste0(dpath, "/USGridSite.Rds"))
avg_pm2.5<- readRDS(paste0(dpath, "/PredictionStep2_Annual_PM25_USGrid_20160101_20161231.rds"))


#############################################################
###  Begin to query
############################################################
 
flist <- dir(inputcsv)
cat(paste0('Reading ', inputcsv, '\n'))
bq <- read.csv(inputcsv, header=TRUE, as.is=TRUE)
dist <- get.knnx(gridsite[,c("Lat","Lon")], bq[,c("Latitude","Longitude")], k=1) ## calculate distance matrix
grid_id<-dist$nn.index   ###extract grid id of 21 sites ---id-s are identical of the avg_pm2.5 

coordinates(bq) <- ~ Longitude + Latitude
crs(bq) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"

print('Step 1: query SES maps')
bq_fc <- over(bq, fc_clean)
bq_fc <- left_join(bq_fc, law)
bq_fc <- bq_fc[,5:dim(bq_fc)[2]]

PopDensity <- extract(draster, bq)
print('Step 2: extract pollutant maps')
NO2 <- extract(no2, bq)
PM25 <- extract(pm25, bq)
PM25_supp <- avg_pm2.5[grid_id]  ###extract PM2.5 concentrations of the 21 sites according to their grid id-s and assign them to the original dataset with the addresses

utmStr <- "+proj=utm +zone=%d +datum=NAD83 +units=m +no_defs +ellps=GRS80 +towgs84=0,0,0"
pUTM <- spTransform(bq, CRS(sprintf(utmStr, 10))) # UTM in hard code number, might need to fix in the future

# Need to check this part
print('Step 3: project distance')
proxRd <- apply(gDistance(pUTM, rd_trans, byid=TRUE), 2, min)
ID <- bq$ID

# Get the lead risks
Pb = fc_clean['GEOID10']
Pb@data$FIPS <- substr(Pb@data$GEOID10,1,11)
Pb@data <- left_join(Pb@data, df2)
bq_pb <- over(bq, Pb)


# bq_df <- data.frame(ID, bq_fc, PopDensity, NO2, PM25, proxRd)
bq_df <- data.frame(ID, bq_fc, PopDensity, NO2, PM25, proxRd, PM25_supp, leadrisk_poverty=bq_pb$PovertyRisk, leadrisk_housing=bq_pb$HousingRisk, leadrisk=bq_pb$LeadRisk)

# Writing output
cat(paste0('Writing ', outputcsv, '\n'))
write.csv(bq_df, file=outputcsv, quote=FALSE, row.names=FALSE)

cat('Done! \n')

