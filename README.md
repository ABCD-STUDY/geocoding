# ABCD geocoding

## Intro

The ABCD geocoding project is to use publically available geo-location database to determine individuals' residential environment. In this repository, we demonstrates the R scripts and functions to perform the query given individuals' residential history in longitude and lattitude. 

### Prerequisites

This package is dependent on R version > 3.0.0. The R packages required include raster, rgdal, rgeos, maptools, and dplyr. 

## Running

### Building the database

The **Gen_data_proc.R** is the R script to generate the query database. The path suffix variables should be changed to the corresponding data downloaded from the public domain. Each corresponding datasets can be found in the annotations of the .R script.

### Query the neightborhood measures

The **Batch_query_db.R** is a command line query function given individuals' lon/lat coordinates and the geo-database built in previous step. In the command line, run: 

```
Rscript Batch_query_db.R ./GeoDB ./example/input.csv ./example/output.csv

```

An output csv file containing ID and queried variables will be generated.

## Deployment

R with raster, rgeos, rgdal, maptools, and dplyr, is needed for deploying the R scripts provided here. We also have a [reprozip](https://reprozip.readthedocs.io/en/1.0.x/unpacking.html) deployable repository for users upon request.

## Versioning

Ver. 1.0.0

## Authors

* Chun Chieh Fan, MD, MSc, PhD. UCSD
 
## License

This project is licensed under the MIT License - see the [LICENSE.md](LICENSE.md) file for details

  



