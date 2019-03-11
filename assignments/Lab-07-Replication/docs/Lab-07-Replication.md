Lab-07-Replication
================
Branson Fox
(March 10, 2019)

Introduction
------------

This notebook replicates the results of lab 07.

Dependencies
------------

This notebook requires the following packages to load, clean and export our data.

``` r
# tidyverse packages
library(readr) # import tabular data
library(dplyr) # data manipulation
```

    ## 
    ## Attaching package: 'dplyr'

    ## The following objects are masked from 'package:stats':
    ## 
    ##     filter, lag

    ## The following objects are masked from 'package:base':
    ## 
    ##     intersect, setdiff, setequal, union

``` r
# other packages
library(here) # file path management
```

    ## here() starts at /Users/lawrence/Desktop/Lab-07-Replication

``` r
library(sf)   # methods for spatial data
```

    ## Linking to GEOS 3.6.1, GDAL 2.1.3, PROJ 4.9.3

``` r
library(measurements) # unit conversion
```

Load Data
---------

This notebook requires three pieces of raw data from the lecture 08 repository.

``` r
# spatial data
counties <- st_read(here("data", "MO_BOUNDARY_Counties", "MO_BOUNDARY_Counties.shp"), stringsAsFactors = FALSE)
```

    ## Reading layer `MO_BOUNDARY_Counties' from data source `/Users/lawrence/Desktop/Lab-07-Replication/data/MO_BOUNDARY_Counties/MO_BOUNDARY_Counties.shp' using driver `ESRI Shapefile'
    ## Simple feature collection with 115 features and 17 fields
    ## geometry type:  POLYGON
    ## dimension:      XY
    ## bbox:           xmin: -95.7747 ymin: 35.99568 xmax: -89.09897 ymax: 40.61364
    ## epsg (SRID):    4269
    ## proj4string:    +proj=longlat +datum=NAD83 +no_defs

``` r
countyPop <- st_read(here("data", "MO_DEMOS_CountyPop", "MO_DEMOS_CountyPop.shp"), stringsAsFactors = FALSE)
```

    ## Reading layer `MO_DEMOS_CountyPop' from data source `/Users/lawrence/Desktop/Lab-07-Replication/data/MO_DEMOS_CountyPop/MO_DEMOS_CountyPop.shp' using driver `ESRI Shapefile'
    ## Simple feature collection with 115 features and 3 fields
    ## geometry type:  POLYGON
    ## dimension:      XY
    ## bbox:           xmin: -95.7747 ymin: 35.99568 xmax: -89.09897 ymax: 40.61364
    ## epsg (SRID):    4269
    ## proj4string:    +proj=longlat +datum=NAD83 +no_defs

``` r
# tabular data
countyDisability <- read_csv(here("data", "MO_DEMOS_CountyDisability/MO_DEMOS_CountyDisability.csv"))
```

    ## Parsed with column specification:
    ## cols(
    ##   GEOID = col_double(),
    ##   under50 = col_double(),
    ##   under50_dis = col_double(),
    ##   under50_non = col_double(),
    ##   btwn50_99 = col_double(),
    ##   btwn50_99_dis = col_double(),
    ##   btwn50_99_non = col_double(),
    ##   btwn100_150 = col_double(),
    ##   btwn100_150_dis = col_double(),
    ##   btwn100_150_non = col_double()
    ## )

Part 1: Data Wrangling
----------------------

### Question 1

We'll begin by creating a new variable that adds together rates of disability by poverty to find the total rate of disability between 0 and 150% of the poverty line.

``` r
countyDisability %>%
  mutate(
    TotalDisability =  under50_dis + btwn50_99_dis + btwn100_150_dis
  ) %>%
  select(GEOID, TotalDisability) -> countyDisability
```

### Question 2

Now, we'll remove the `NAME` column from the county population data and convert it to a data.frame.

``` r
countyPop <- select(countyPop, -NAME)
```

First, observe that it is of class sf:

``` r
class(countyPop)
```

    ## [1] "sf"         "data.frame"

Now, to remove the geometry:

``` r
st_geometry(countyPop) <- NULL
```

Now, see that it is no longer of class `sf`

``` r
class(countyPop)
```

    ## [1] "data.frame"

### Question 3

Next, we'll join the county population and disability data. Because of the way a shapefile and csv are read in differently, we need to change the character format of countyPop GEOID to be numeric.

``` r
# change variable class
countyPop <- mutate(countyPop, GEOID = as.numeric(GEOID))
# then to join as usual
countyFull <- left_join(countyDisability, countyPop, by = "GEOID")
```

### Question 4

Next, we'll calculate the proportion of the county that has a dissability and is between 0 and 150% of the poverty line out of the total county population.

``` r
countyFull <- mutate(countyFull,
                     disProp = TotalDisability/totalPop)
```

### Question 5

Our data is in the correct order now, but if you need to rearrange the order of variables, use the `select()` function and manually rearrange them.

``` r
countyFull <- select(countyFull, GEOID, TotalDisability, totalPop, disProp)
```

### Question 6

Now, we want to remove all of the columns from the county data except for `GEOID` variable we can join by. Remember that sf geometries are "sticky" or in other words they will remain unless explicitly dropped.

``` r
counties <- select(counties, GEOID)
```

### Question 7

Now, we will join our `countyFull` data with `counties`. Once again, we will change the GEOID from character to numeric in order to match.

``` r
# change class of GEOID
counties <- mutate(counties, GEOID = as.numeric(GEOID))
# join as usual
countyFull <- left_join(counties, countyFull, by = "GEOID")
```

### Question 8

The last modification we'll make to the data is calculating the square kilometer area per county. First we use `st_area()` to get the area in square meters. We have to force to numeric using `as.numeric()` or else there will be trouble converting later. Then, we use the `conv_unit()` function from `measurements` to convert this to square km.

``` r
# to get area from geometry
countyFull <- mutate(countyFull, area = as.numeric(st_area(geometry)))
# then to convert to square km
countyFull <- mutate(countyFull, area = conv_unit(area, "m2", "km2"))
```

### Question 9

Finally, we will write the cleaned data as a shapefile to the `data/` folder. In this example, we created a new folder called `MO_CountyDisabilityTotal` as a subfolder of `data/`.

``` r
# an example of creating a folder by R code
dir.create(here("data", "MO_CountyDisabilityTotal"))
# then regular saving of our data
st_write(countyFull, here("data", "MO_CountyDisabilityTotal", "MO_CountyDisabilityTotal.shp"))
```

    ## Warning in abbreviate_shapefile_names(obj): Field names abbreviated for
    ## ESRI Shapefile driver

    ## Writing layer `MO_CountyDisabilityTotal' to data source `/Users/lawrence/Desktop/Lab-07-Replication/data/MO_CountyDisabilityTotal/MO_CountyDisabilityTotal.shp' using driver `ESRI Shapefile'
    ## features:       115
    ## fields:         5
    ## geometry type:  Polygon

Part 2: Geodatabase Creation
----------------------------

This part of the lab is completed in ArcGIS Pro.
