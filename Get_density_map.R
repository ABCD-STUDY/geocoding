# This is the R script for generating sampling density map
# Designed for command line use, given the required packages are installed
# 
# Example usage:
# Rscript Get_density_map.R ./GeoDB ./example/input.csv ./example/output.csv titlename
#
# ./GeoDB the folder containing the query database generated in previous step
#         see: Gen_data_proc.R
# ./example/input.csv the input spreadsheet given coordinates
# ./example/output.csv the output destination of the queried results
# titlename -- this is for the figure titling

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
tnames <- args[4]
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

# define region first
bq_fc <- over(bq, fc_clean)
t <- data.frame(table(as.character(bq_fc$CBSA_Name)))

for (geo_id in t$Var1){

fc_grep <- fc_clean[which(is.element(fc_clean@data$CBSA_Name, as.character(geo_id))), ]
ext <- extent(fc_grep)
r <- raster(fc_grep, nrow=round(abs(ext[1] - ext[2])/0.03),
            ncol=round(abs(ext[3] - ext[4])/0.03))
nc <- rasterize(coordinates(bq), r, fun='count')
rbPal <- colorRampPalette(c('yellow','red'),alpha=TRUE)
png(paste0(outputcsv,'.',gsub(" |,", ".", geo_id),'.sampling.density.png'),
	width = 7, height = 7, units = 'in',res = 300)
plot(fc_grep, border=rgb(0.5,0.5,0.5,0.2))
plot(nc, col=adjustcolor(rbPal(5),alpha.f = 0.7), add=TRUE)
title(paste0(tnames, '\r\n', 
             as.character(geo_id), '\r\n Sampling Density',' ',
             'n = ', t$Freq[grep(as.character(geo_id), t$Var1)]))
dev.off()
}
# Truncated whole map
fc_grep <- fc_clean[which(is.element(fc_clean@data$CBSA_Name, 
		    as.character(t$Var1[t$Freq > 5]))), ]
ext <- extent(fc_grep)
r <- raster(fc_grep, nrow=round(abs(ext[1] - ext[2])/0.03),
            ncol=round(abs(ext[3] - ext[4])/0.03))
nc <- rasterize(coordinates(bq), r, fun='count')
rbPal <- colorRampPalette(c('yellow','red'),alpha=TRUE)
tiff(paste0(outputcsv,'.whole.sampling.density.tif'),
	width = 7, height = 7, units = 'in',res = 600)
plot(fc_grep, border=rgb(0.5,0.5,0.5,0.2))
plot(nc, col=adjustcolor(rbPal(5),alpha.f = 0.7), add=TRUE)
title(paste0(tnames, '\r\n', 
             'Sampling Density',' ',
             'n = ', sum(t$Freq[t$Freq > 5])))
dev.off()

# Table output
write.table(t, file=paste0(outputcsv,'.sampling.counts'), quote=FALSE, row.names=FALSE)
GEO10 <- data.frame(table(substr(as.character(bq_fc$GEOID10), 1,10)))
write.table(GEO10, file=paste0(outputcsv,'.geo10.counts'), quote=FALSE, row.names=FALSE)



