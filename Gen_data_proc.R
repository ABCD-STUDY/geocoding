# This is the script for generating the query database
# All required files are annotated
# Can search through variables with suffix 'path' to modify the input files
# The downloading source can be found in the annotations

require(raster)
require(rgeos)
require(rgdal)
require(maps)
require(maptools)
require(dplyr)

# The geo tif is a better way to store raster data
# Here is example to load them and resample them
# However, the query function will load them directly

############################################################################################
# UN adjusted population density
# http://dx.doi.org/10.7927/H4HX19NJ
# Grid version from SEDAC
# 2010 estimates
dpath <- 'gpw-v4-population-density-adjusted-to-2015-unwpp-country-totals_2010.tif'
draster <- raster(dpath,  values=TRUE)
draster <- crop(draster, extent(-180, -40, 10,72))

##############################################################################################
# PM 2.5
# Original resolution in 100 km^2
# Global Annual PM2.5 Grids from MODIS, MISR and SeaWiFS Aerosol Optical Depth (AOD), v1 (1998 – 2012)
# http://sedac.ciesin.columbia.edu/data/set/sdei-global-annual-avg-pm2-5-modis-misr-seawifs-aod-1998-2012/data-download
ppath <- 'PM25_201001_201212.tif'
pm25 <- crop(raster(ppath), extent(-180, -40, 0,72))

################################################################################################
# NO 2
# Global 3-Year Running Mean Ground-Level Nitrogen Dioxide (NO2) Grids from GOME, SCIAMACHY and GOME-2
# The same resolution as PM2.5
# http://sedac.ciesin.columbia.edu/data/set/sdei-global-3-year-running-mean-no2-gome-sciamachy-gome2
nopath <- 'SURFACE_NO2_010x010_2011.tif'
no2 <- crop(raster(nopath), extent(-180, -40, 0,72)) 

# Those files are not actually saved in the curation. The tif map will be stored separately

################################################################################################
# Walkability index and smart location databse
# Original resolution in Census Block
#
# From EPA: https://www.epa.gov/smartgrowth/smart-location-mapping#walkability
# Variable names: https://www.epa.gov/sites/production/files/2014-03/documents/sld_userguide.pdf
# Estiamted based on 2010 census 

dbpath <- "Natl_WI.gdb"
fc <- readOGR(dsn=dbpath, layer='CompleteSLD_WI_join')

##################################################################################################
# Uniform Crime Report from FBI
# http://www.icpsr.umich.edu/icpsrweb/NACJD/studies/33523
# Vendors do provide a more detailed regional info, yet it is not uniformed across USA
# Original resolution in County level
# Mapping between census County code in "national_county.txt"
# Current version, I use three years average

crimepaths <- c('33523-0001-Data.rda', '34582-0001-Data.rda', '35019-0001-Data.rda')
load(crimepaths[[1]])
load(crimepaths[[2]])
load(crimepaths[[3]])
dbcrime <- rbind(da33523.0001[,c(5:56)], da34582.0001[,c(5:56)], da34582.0001[,c(5:56)])
by_FIPS <- dbcrime %>% group_by(FIPS_ST, FIPS_CTY) %>% summarise_all(mean)
by_FIPS <- as.data.frame(by_FIPS)

# Merge with Walkability indx spreadsheet, as easy for usage
by_FIPS$CFIPS_1 <- as.integer(by_FIPS$FIPS_CTY)
by_FIPS$SFIPS_1 <- as.integer(by_FIPS$FIPS_ST)
fc@data <- left_join(fc@data, by_FIPS[,c(3:54)])


###################################################################################################
#
# Clean up the fc data to pick only those of interested
#
# For smart location database:
#   D1A - gross residential density
#   WalkIndex - national walkability index
# For Uniform Crime:
#   GRNDTOT - grand total
#   P1TOT - total adult offenses 
#   P1VLNT - adult violent crimes
#   DRUGTOT - durg abose violations total
#   DRGSALE - drug sale total
#   MJSALE - Marijuana sale 
#   DRGPOSS - drug possession total
#   DUI - DUI
#   
      
fc_clean <- fc
fc_clean@data <- fc@data[,c('GEOID10', 'CFIPS', 'SFIPS', 'CBSA_Name', 'D1A', 'WalkIndex', 'GRNDTOT', 'P1TOT', 'P1VLNT', 'DRUGTOT', 'DRGSALE', 'MJSALE', 'DRGPOSS', 'DUI')]

####################################################################################################
# Proximity to major roads and highways
# The North America Atlas for roads til 2013
#
# https://nationalmap.gov/small_scale/mld/1roadsl.html
# Currently, there is no AADT incorporated, need to do in the future

rd <- readOGR(dsn='roadtrl010g.gdb', layer='roadtrl010g')
utmStr <- "+proj=utm +zone=%d +datum=NAD83 +units=m +no_defs +ellps=GRS80 +towgs84=0,0,0"
rd_trans <- spTransform(rd,CRS(sprintf(utmStr, 10)))

####################################################################################################
# 2011 - 2015 American Community Survey 5-year summary
# From Census Bureau
# Resolution in Census Tract
# https://www.census.gov/programs-surveys/acs/data/summary-file.html
# ADI - area deprivation index
#
# Including:
# ADI_edu_l - Percentage of population aged >=25 y with <9 y of education
# ADI_edu_h - Percentage of population aged >=25 y with at least a high school diploma
# ADI_work_c - Percentage of employed persons aged >=16 y in white collar occupations
# ADI_income - Median family income
# ADI_in_dis - Income disparity defined by Singh as the log of 100 x ratio of the number of households with <10000 annual income to the number of households with >50000 annual income.
# ADI_home_v - Median home value
# ADI_rent - Median gross rent
# ADI_mortg - Median monthly mortgage
# ADI_home_o - Percentage of owner-occupied housing units (home ownership rate)
# ADI_crowd - Percentage of occupied housing units with >1 person per room (crowding)
# ADI_unemp - Percentage of civilian labor force population aged >=16 y unemployed (unemployment rate)
# ADI_pov - Percentage of families below the poverty level
# ADI_b138 - Percentage of population below 138% of the poverty threshold
# ADI_sp - Percentage of single-parent households with children aged < 18 y
# ADI_ncar - Percentage of occupied housing units without a motor vehicle
# ADI_ntel - Percentage of occupied housing units without a telephone
# ADI_nplumb - Percentage of occupied housing units without complete plumbing (log)
#
# Final score:
# ADI_wSUM - scaled weighted sum based on Kind et al., Annals of Internal Medicine, 2014
# ADI_perc - national percentiles, higher means higher value of ADI

e_path <- 'ACS/tab4/sumfile/prod/2010thru2014/group2/'
g_path <- 'ACS/geography/'
s_path <- 'ACS/seq/'

# Loop over stats
statvec <- c('ak','al','ar','az','ca','co','ct','dc','de','fl','ga','hi','ia','id','il','in','ks','ky','la','ma','md','me','mi','mn','mo','ms','mt','nc','nd','ne','nh','nj','nm','nv','ny','oh','ok','or','pa','pr','ri','sc','sd','tn','tx','ut','va','vt','wa','wi','wv','wy')

adi_df <- data.frame()

for (si in statvec){
print(paste0('Processing stat: ', si))
geo <- read.csv(paste0(g_path,'g20145',si,'.csv'), header=FALSE, as.is=TRUE)
geo_mat <- data.frame(geo$V5, geo$V49)
names(geo_mat) <- c('LOGRECNO','GEOID')

# 1) Percentage of population aged >=25 y with <9 y of education
#    Seq42, 100*sum(V126:V136)/V125
# 2) Percentage of population aged >=25 y with at least a high school diploma
#    Seq42, sum(V141:149)/V125

ftmp <- paste0(e_path,'e20145',si,'0',sprintf('%.3d', 42),'000.txt')
acs <- read.csv(ftmp, header=FALSE, as.is=TRUE)
adi_var <- data.frame(acs$V6, 
                       100*rowSums(acs[,126:136])/acs[,125],
                       100*rowSums(acs[,141:149])/acs[,125])
names(adi_var) <- c('LOGRECNO', 'ADI_edu_l','ADI_edu_h')
geo_mat <- left_join(geo_mat, adi_var)

# 3) Percentage of employed persons aged >=16 y in white collar occupations
#    This is a relatively outdated item, since labor statistics no longer define the 
#    blue collar or white collar after 2006
#    here, we coded using the definition by Census Bureau 2000
#    https://www.census.gov/prod/2003pubs/c2kbr-25.pdf
#    i.e. agriculture, forestry, fishing, hunting, mining, and construction
#    Seq82, sum(V144:V145)/V143

ftmp <- paste0(e_path,'e20145',si,'0',sprintf('%.3d', 82),'000.txt')
acs <- read.csv(ftmp, header=FALSE, as.is=TRUE)
adi_var <- data.frame(acs$V6,
                       100*(1 - rowSums(acs[,144:145])/acs[,143]))
names(adi_var) <- c('LOGRECNO', 'ADI_work_c')
geo_mat <- left_join(geo_mat, adi_var)

# 4) Median family income
#    Seq63, V75 
ftmp <- paste0(e_path,'e20145',si,'0',sprintf('%.3d', 63),'000.txt')
acs <- read.csv(ftmp, header=FALSE, as.is=TRUE)
adi_var <- data.frame(acs$V6,
                       as.numeric(as.character(acs[,75])))
names(adi_var) <- c('LOGRECNO', 'ADI_income')
geo_mat <- left_join(geo_mat, adi_var)

# 5) Income disparity
#    Income disparity defined by Singh as the log of 100 x ratio of the number of households with <10000 annual income to the number of households with >50000 annual income.
#    This is the value bin defined by Census Bureau
#    Seq58, 100*log(V8/sum(V17:V23))
#    i.e. log(100*acs[,8]/(rowSums(acs[,17:23]))) - need to be careful about true 0!
ftmp <- paste0(e_path,'e20145',si,'0',sprintf('%.3d', 58),'000.txt')
acs <- read.csv(ftmp, header=FALSE, as.is=TRUE)
adi_var <- data.frame(acs$V6,
                       log(100*acs[,8]/(rowSums(acs[,17:23]))))
names(adi_var) <- c('LOGRECNO', 'ADI_in_dis')
adi_var[which(!is.finite(adi_var$ADI_in_dis)),2] <- 0
geo_mat <- left_join(geo_mat, adi_var)

# 6) Median home value
#    Seq105, V238
# 7) Median gross rent
#    Seq105, V54
ftmp <- paste0(e_path,'e20145',si,'0',sprintf('%.3d', 105),'000.txt')
acs <- read.csv(ftmp, header=FALSE, as.is=TRUE)
adi_var <- data.frame(acs$V6,
                       as.numeric(as.character(acs$V238)), 
                       as.numeric(as.character(acs$V54)))
names(adi_var) <- c('LOGRECNO', 'ADI_home_v', 'ADI_rent')
geo_mat <- left_join(geo_mat, adi_var)

# 8) Median monthly mortgage
#    Seq106, V82 
ftmp <- paste0(e_path,'e20145',si,'0',sprintf('%.3d', 106),'000.txt')
acs <- read.csv(ftmp, header=FALSE, as.is=TRUE)
adi_var <- data.frame(acs$V6,
                       as.numeric(as.character(acs$V82)))
names(adi_var) <- c('LOGRECNO', 'ADI_mortg')
geo_mat <- left_join(geo_mat, adi_var)

# 9) Percentage of owner-occupied housing units (home ownership rate)
#    Seq102, 100*V84/V83
# 17)Percentage of occupied housing units with >1 person per room (crowding) 
#    Seq102, 100*(sum(V187:V189) + sum(V193:195))/V183
ftmp <- paste0(e_path,'e20145',si,'0',sprintf('%.3d', 102),'000.txt')
acs <- read.csv(ftmp, header=FALSE, as.is=TRUE)
adi_var <- data.frame(acs$V6,
                       100*acs$V84/acs$V83,
                       100*rowSums(acs[,c(187:189, 193:195)])/acs$V183)
names(adi_var) <- c('LOGRECNO', 'ADI_home_o', 'ADI_crowd')
geo_mat <- left_join(geo_mat, adi_var)

# 10) Percentage of civilian labor force population aged >=16 y unemployed (unemployment rate)
#     Seq78, 100*V42/V40
ftmp <- paste0(e_path,'e20145',si,'0',sprintf('%.3d', 78),'000.txt')
acs <- read.csv(ftmp, header=FALSE, as.is=TRUE)
adi_var <- data.frame(acs$V6,
                       100*acs$V42/acs$V40)
names(adi_var) <- c('LOGRECNO', 'ADI_unemp')
geo_mat <- left_join(geo_mat, adi_var)

# 11) Percentage of families below the poverty level
#     Seq50, 100*V75/V74
ftmp <- paste0(e_path,'e20145',si,'0',sprintf('%.3d', 50),'000.txt')
acs <- read.csv(ftmp, header=FALSE, as.is=TRUE)
adi_var <- data.frame(acs$V6,
                       100*acs$V75/acs$V74)
names(adi_var) <- c('LOGRECNO', 'ADI_pov')
geo_mat <- left_join(geo_mat, adi_var)

# 12) Percentage of population below 150% of the poverty threshold
#     The 2014 release of ACS don't have this cut point anymore 
#     It can be categorized in the health insurance part, defined by 1.38 threshold
#     Seq116, 100*V98/V97 
ftmp <- paste0(e_path,'e20145',si,'0',sprintf('%.3d', 116),'000.txt')
acs <- read.csv(ftmp, header=FALSE, as.is=TRUE)
adi_var <- data.frame(acs$V6,
                       100*acs$V98/acs$V97)
names(adi_var) <- c('LOGRECNO', 'ADI_b138')
geo_mat <- left_join(geo_mat, adi_var)

# 13) Percentage of single-parent households with children aged < 18 y
#     Seq36, 100*(V16 + V22)/V7
ftmp <- paste0(e_path,'e20145',si,'0',sprintf('%.3d', 36),'000.txt')
acs <- read.csv(ftmp, header=FALSE, as.is=TRUE)
adi_var <- data.frame(acs$V6,
                       100*(acs$V16 + acs$V22)/acs$V7)
names(adi_var) <- c('LOGRECNO', 'ADI_sp')
geo_mat <- left_join(geo_mat, adi_var)

# 14) Percentage of occupied housing units without a motor vehicle
#      Seq104, 100*(V120 + V113)/V111
# 15) Percentage of occupied housing units without a telephone
#     Seq104, 100*((V98 + V107)/V92
# 16) Percentage of occupied housing units without complete plumbing (log)
#     Seq104, 100*V153/V148
ftmp <- paste0(e_path,'e20145',si,'0',sprintf('%.3d', 104),'000.txt')
acs <- read.csv(ftmp, header=FALSE, as.is=TRUE)
adi_var <- data.frame(acs$V6,
                       100*(acs$V120 + acs$V113)/acs$V111,
                       100*(acs$V98 + acs$V107)/acs$V92,
                       100*acs$V153/acs$V148)
names(adi_var) <- c('LOGRECNO', 'ADI_ncar','ADI_ntel','ADI_nplumb')

geo_mat <- left_join(geo_mat, adi_var)

# weighted summary score
geo_mat$ADI_wSUM <- 0.0849*geo_mat$ADI_edu_l - 0.0970*geo_mat$ADI_edu_h - 0.0874*geo_mat$ADI_work_c - 
               0.0977*geo_mat$ADI_income + 0.0936*geo_mat$ADI_in_dis - 0.0688*geo_mat$ADI_home_v -  
               0.0781*geo_mat$ADI_rent - 0.0770*geo_mat$ADI_mortg - 0.0615*geo_mat$ADI_home_o + 
               0.0556*geo_mat$ADI_crowd + 0.0806*geo_mat$ADI_unemp + 0.0977*geo_mat$ADI_pov +
               0.1037*geo_mat$ADI_b138 + 0.0719*geo_mat$ADI_sp + 0.0694*geo_mat$ADI_ncar + 
               0.0877*geo_mat$ADI_ntel + 0.0510*geo_mat$ADI_nplumb
geo_mat$GEOID_NUM <- sapply(geo_mat$GEOID,function(x) strsplit(as.character(x), 'US')[[1]][2])
adi_df <- rbind(adi_df, geo_mat)

}
partial_join <- function(x, y, by_x, pattern_y) {
 idx_x <- sapply(y[[pattern_y]], grep, x[[by_x]])
 idx_y <- sapply(seq_along(idx_x), function(i) rep(i, length(idx_x[[i]])))

 df <- dplyr::bind_cols(x[unlist(idx_x), , drop = F],
                        y[unlist(idx_y), , drop = F])
 return(df)
}

# Constraint ACS only in the census tract level
final <- partial_join(fc_clean@data, adi_df[which(nchar(adi_df$GEOID_NUM) == 11),], by_x="GEOID10", pattern_y="GEOID_NUM")
# ADI summary score need scaling
excludevec <- names(final) %in% c('GEOID_NUM')
final <- final[!excludevec]
final$ADI_perc <- cut(final$ADI_wSUM, 
                  breaks=quantile(final$ADI_wSUM, seq(0,1, length.out=101), na.rm=TRUE), 
                  include.lowest=T, labels=F)
final$ADI_wSUM <- 100 + 20*((final$ADI_wSUM - mean(final$ADI_wSUM, na.rm=TRUE))/sd(final$ADI_wSUM, na.rm=TRUE))

fc_clean@data <- left_join(fc_clean@data, final)
save(file = paste0('GeoDB/Query_geo_db_ver_',
            format(Sys.time(),"%y_%m_%d_%H"),'.RData'),'fc_clean','rd_trans')



# ToDo:
# Modularize to take into account of time points


