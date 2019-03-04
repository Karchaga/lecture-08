Lecture-08 Examples
================
Christopher Prener, Ph.D.
(March 04, 2019)

## Introduction

This notebook provides additional examples of data cleaning plus new
material on table joins and exporting data.

## Dependencies

This notebook requires a number of packages:

``` r
# tidyverse packages
library(dplyr)       # data wrangling
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
library(readr)       # read/write tabular data
library(stringr)     # work with strings
library(tidyr)       # data wrangling

# spatial packages
library(janitor)     # data wrangling
library(sf)          # spatial data tools
```

    ## Linking to GEOS 3.6.1, GDAL 2.1.3, PROJ 4.9.3

``` r
# other packages
library(here)        # file path management
```

    ## here() starts at /Users/chris/GitHub/SOC5650/LectureRepos/lecture-08

``` r
library(measurements) # measurement conversion
```

## Load Data

This notebook requires three sets of data:

``` r
# missouri counties
st_read(here("data", "example-data", "MO_BOUNDARY_Counties", "MO_BOUNDARY_Counties.shp"), 
                    stringsAsFactors = FALSE) %>%
  st_transform(crs = 32615) -> counties
```

    ## Reading layer `MO_BOUNDARY_Counties' from data source `/Users/chris/GitHub/SOC5650/LectureRepos/lecture-08/data/example-data/MO_BOUNDARY_Counties/MO_BOUNDARY_Counties.shp' using driver `ESRI Shapefile'
    ## Simple feature collection with 115 features and 17 fields
    ## geometry type:  POLYGON
    ## dimension:      XY
    ## bbox:           xmin: -95.7747 ymin: 35.99568 xmax: -89.09897 ymax: 40.61364
    ## epsg (SRID):    4269
    ## proj4string:    +proj=longlat +datum=NAD83 +no_defs

``` r
# clean water act lakes
lakes <- read_csv(here("data", "example-data", "MO_HYDRO_ImpairedLakes.csv"))
```

    ## Parsed with column specification:
    ## cols(
    ##   .default = col_character(),
    ##   YR = col_double(),
    ##   BUSINESSID = col_double(),
    ##   WBID = col_double(),
    ##   MDNR_IMPSZ = col_double(),
    ##   SIZE_ = col_double(),
    ##   EPA_APPRSZ = col_double(),
    ##   DWN_X = col_double(),
    ##   DWN_Y = col_double(),
    ##   EVENTDAT = col_date(format = ""),
    ##   RCHSMDATE = col_logical(),
    ##   RCH_RES = col_logical(),
    ##   FEAT_URL = col_logical(),
    ##   SHAPE_Leng = col_double(),
    ##   Shape_Le_1 = col_double(),
    ##   Shape_Area = col_double()
    ## )

    ## See spec(...) for full column specifications.

``` r
# clean water act rivers/streams
rivers <- read_csv(here("data", "example-data", "MO_HYDRO_ImpairedRiversStreams.csv"))
```

    ## Parsed with column specification:
    ## cols(
    ##   .default = col_double(),
    ##   WATER_BODY = col_character(),
    ##   WB_CLS = col_character(),
    ##   UNIT = col_character(),
    ##   POLLUTANT = col_character(),
    ##   SOURCE_ = col_character(),
    ##   IU = col_character(),
    ##   OU = col_character(),
    ##   COUNTY_U_D = col_character(),
    ##   WB_EPA = col_character(),
    ##   COMMENT_ = col_character(),
    ##   PERM_ID = col_character(),
    ##   EVENTDAT = col_date(format = ""),
    ##   REACHCODE = col_character(),
    ##   RCHSMDATE = col_logical(),
    ##   RCH_RES = col_logical(),
    ##   SRC_DESC = col_character(),
    ##   FEAT_URL = col_logical()
    ## )
    ## See spec(...) for full column specifications.

## Clean Data

We’re going to extend some of the data cleaning techniques we introduced
last class, introducing them using the data on Clean Water Act listed
lakes and then applying them to the Clean Water Act listed rivers data.
Our goal is to create counts of listed bodies of water per county in
Missouri.

### Lakes Data

First, we’re going to get back to where we left off last week by
obtaining counts of pollutants per lake. We’re going to retain the
county name as well, using `first(county_u_d)` in our `summarize()` call
to get the county name from the first observation once the data are
grouped:

``` r
lakes %>%
  clean_names() %>%
  distinct(water_body, pollutant, county_u_d, .keep_all = TRUE) %>%
  group_by(water_body) %>% 
  summarize(count = n(), county = first(county_u_d)) -> lakes

lakes
```

    ## # A tibble: 55 x 3
    ##    water_body                 count county        
    ##    <chr>                      <int> <chr>         
    ##  1 August A Busch Lake No. 37     1 St. Charles   
    ##  2 Bee Tree Lake                  1 St. Louis     
    ##  3 Belcher Branch Lake            1 Buchanan      
    ##  4 Bowling Green Lake - Old       3 Pike          
    ##  5 Buffalo Bill Lake              1 DeKalb        
    ##  6 Busch W.A. No. 35 Lake         1 St. Charles   
    ##  7 Chaumiere Lake                 1 Clay          
    ##  8 Clearwater Lake                3 Wayne/Reynolds
    ##  9 Coot Lake                      1 Jackson       
    ## 10 Cottontail Lake                1 Jackson       
    ## # … with 45 more rows

Notice that `Clearwater Lake` contains two county names separated by a
forward slash (i.e. `/`). This is a problem - we need pristine county
names for our join later on. We can split these into two values in what
is known as a “list-column”. These are used by `sf`, for example, to
store the geometry data:

``` r
counties %>%
  select(geometry)
```

    ## Simple feature collection with 115 features and 0 fields
    ## geometry type:  POLYGON
    ## dimension:      XY
    ## bbox:           xmin: 265131.4 ymin: 3986650 xmax: 847373 ymax: 4496645
    ## epsg (SRID):    32615
    ## proj4string:    +proj=utm +zone=15 +datum=WGS84 +units=m +no_defs
    ## First 10 features:
    ##                          geometry
    ## 1  POLYGON ((407087.4 4258038,...
    ## 2  POLYGON ((357018.1 4149104,...
    ## 3  POLYGON ((555742.9 4441840,...
    ## 4  POLYGON ((435019.7 4406658,...
    ## 5  POLYGON ((402629.9 4295560,...
    ## 6  POLYGON ((716787.9 4155877,...
    ## 7  POLYGON ((571712.7 4223996,...
    ## 8  POLYGON ((405925.3 4103964,...
    ## 9  POLYGON ((661466.4 4142452,...
    ## 10 POLYGON ((355897 4087900, 3...

We can create a new list-col by using the `stringr` package’s
`str_split()` function, which will remove any forward slashes that are
found and convert `Wayne/Reynolds` into `c("Wayne", "Reynolds")`. This
creates, in essence, an object within an object:

``` r
mutate(lakes, county = str_split(string = county, pattern = "/"))
```

    ## # A tibble: 55 x 3
    ##    water_body                 count county   
    ##    <chr>                      <int> <list>   
    ##  1 August A Busch Lake No. 37     1 <chr [1]>
    ##  2 Bee Tree Lake                  1 <chr [1]>
    ##  3 Belcher Branch Lake            1 <chr [1]>
    ##  4 Bowling Green Lake - Old       3 <chr [1]>
    ##  5 Buffalo Bill Lake              1 <chr [1]>
    ##  6 Busch W.A. No. 35 Lake         1 <chr [1]>
    ##  7 Chaumiere Lake                 1 <chr [1]>
    ##  8 Clearwater Lake                3 <chr [2]>
    ##  9 Coot Lake                      1 <chr [1]>
    ## 10 Cottontail Lake                1 <chr [1]>
    ## # … with 45 more rows

It is better explored by viewing the data.

Once we have our list-col, we can convert each element of `county` into
its on row using the `tidyr` package’s `unnest()` function:

``` r
lakes %>%
  mutate(county = str_split(string = county, pattern = "/")) %>%
  unnest()
```

    ## # A tibble: 56 x 3
    ##    water_body                 count county     
    ##    <chr>                      <int> <chr>      
    ##  1 August A Busch Lake No. 37     1 St. Charles
    ##  2 Bee Tree Lake                  1 St. Louis  
    ##  3 Belcher Branch Lake            1 Buchanan   
    ##  4 Bowling Green Lake - Old       3 Pike       
    ##  5 Buffalo Bill Lake              1 DeKalb     
    ##  6 Busch W.A. No. 35 Lake         1 St. Charles
    ##  7 Chaumiere Lake                 1 Clay       
    ##  8 Clearwater Lake                3 Wayne      
    ##  9 Clearwater Lake                3 Reynolds   
    ## 10 Coot Lake                      1 Jackson    
    ## # … with 46 more rows

Notice how we now have two rows for `Clearwater Lake` - one for `Wayne`
and one for `Reynolds`. This is perfect - it converts our observational
unit from “lake” to “lake per county”.

Another thing to notice is that we have multiple entries for St. Charles
county. This is true for other counties as well. We can create
counts-per-county in the same way we made our initial counts per lake.
We’ll:

1.  split the data, **then**
2.  use `unnest()` to parse out our list-cols, **then**
3.  group by `county`, **then**
4.  summarize to create a new variable named `lakes` with a count of
    lakes and a new variable named `lakes_avg` that has the average
    number of pollutants per body of water, **and**
5.  assign the data to a new object named `lakesByCounty`.

<!-- end list -->

``` r
lakes %>%
  mutate(county = str_split(string = county, pattern = "/")) %>%
  unnest() %>%
  group_by(county) %>%
  summarise(
    lakes = n(), 
    lakes_avg = mean(count)) -> lakesByCounty

lakesByCounty
```

    ## # A tibble: 40 x 3
    ##    county         lakes lakes_avg
    ##    <chr>          <int>     <dbl>
    ##  1 Adair              2       3  
    ##  2 Boone              2       1.5
    ##  3 Buchanan           1       1  
    ##  4 Cape Girardeau     1       1  
    ##  5 Cass               1       1  
    ##  6 Clark              1       3  
    ##  7 Clay               1       1  
    ##  8 Cole               2       1  
    ##  9 DeKalb             1       1  
    ## 10 Douglas            1       3  
    ## # … with 30 more rows

Perfect\!

### River Data

Now, lets replicate the process on the rivers data. We need to:

1.  Clean the variable names en masse, **then**
2.  Get distinct combinations of bodies of water, pollutants, and
    counties (since rivers often cross multiple counties), **then**
3.  Group the data by body of water, **then**
4.  Calculate counts of pollutants per body of water and retain the
    county name, **then**
5.  Split counties that contain a forward slash, **then**
6.  Unnest our list-cols, **then** 7, Group by county, **then**,
7.  Calculate counts of rivers per county as well as the average number
    of pollutants per river, **and**
8.  Store the data in a new object called `riversByCounty`

<!-- end list -->

``` r
rivers %>%
  clean_names() %>%
  distinct(water_body, pollutant, county_u_d, .keep_all = TRUE) %>%
  group_by(water_body) %>% 
  summarize(
    count = n(), 
    county = first(county_u_d)) %>%
  mutate(county = str_split(string = county, pattern = "/")) %>%
  unnest() %>%
  group_by(county) %>%
  summarise(rivers = n(), rivers_avg = mean(count)) -> riversByCounty
```

## Joins

Now that we have our counts per county for both the lakes and rivers
data, we want to combine them with the `counties` data. First, we want
to subset our columns. We also want to make sure our identification
variable contains no duplicates:

``` r
# convert to data frame
counties_df <- counties
st_geometry(counties_df) <- NULL

# check for duplicates in id variable
counties_df %>%
  get_dupes(NAME)
```

    ## # A tibble: 2 x 18
    ##   NAME  dupe_count STATEFP COUNTYFP COUNTYNS GEOID NAMELSAD LSAD  CLASSFP
    ##   <chr>      <int> <chr>   <chr>    <chr>    <chr> <chr>    <chr> <chr>  
    ## 1 St. …          2 29      189      00758549 29189 St. Lou… 06    H1     
    ## 2 St. …          2 29      510      00767557 29510 St. Lou… 25    C7     
    ## # … with 9 more variables: MTFCC <chr>, CSAFP <chr>, CBSAFP <chr>,
    ## #   METDIVFP <chr>, FUNCSTAT <chr>, ALAND <chr>, AWATER <chr>,
    ## #   INTPTLAT <chr>, INTPTLON <chr>

We have two duplicate observations - the `NAME` variable contains `St.
Louis` twice, once for the city and once for the county. We’ll go ahead
and rename the city’s instance of `NAME` to `St. Louis City`, to match
how it appears in the EPA data.

``` r
counties %>% 
  select(COUNTYFP, NAME, ALAND, AWATER) %>%
  mutate(NAME = ifelse(COUNTYFP == "510", "St. Louis City", NAME)) -> counties
```

Now, we’re ready to join our data. We’ll use the `dplyr` `left_join()`
function because it ensures we only have observations for areas we have
valid geometry for. The `sf` object (in this case `counties`) should
always go in the `x` or “lefthand”
position:

``` r
counties <- left_join(counties, lakesByCounty, by = c("NAME" = "county"))
```

Once we have those data combined, we can also add our rivers data in the
same
manner.

``` r
counties <- left_join(counties, riversByCounty, by = c("NAME" = "county"))
```

## Clean-up

With our data combined, we have a little bit more cleaning to do:

1.  First, we replace zeros in `lakes` and `rivers` and add `ALAND` and
    `AWATER` together into a new variable named \`area, **then**
2.  We remove `ALAND` and `AWATER`, **then**
3.  We rename `COUNTYFP` to `county` and `NAME` to `name`, **then**
4.  We convert the units of `area` from square meters to kilometers
    squared, **and**
5.  We assign our changes to the `counties` object.

<!-- end list -->

``` r
counties %>%
  mutate(
    lakes = ifelse(is.na(lakes) == TRUE, 0, lakes),
    rivers = ifelse(is.na(rivers) == TRUE, 0, rivers),
    area = as.numeric(ALAND)+as.numeric(AWATER)
  ) %>%
  select(-ALAND, -AWATER) %>%
  rename(
    county = COUNTYFP,
    name = NAME
  ) %>%
  mutate(area = conv_unit(area, from = "m2", to = "km2")) -> counties
```

The `conv_unit()` function takes any numeric variable and will convert
it from a given unit to a given unit. Use `?conv_unit` to get a full
listing of units contained in the package. For our purposes, we’re
typically interested in the length and area measurements:

  - area - nm2, um2, mm2, cm2, m2, hectare, km2, inch2, ft2, yd2, acre,
    mi2, naut\_mi2
  - length - angstrom, nm, um, mm, cm, dm, m, km, inch, ft, yd, fathom,
    mi, naut\_mi, au, light\_yr, parsec, point

A second option for calculating area is to use the `geometry` itself:

``` r
counties %>%
  mutate(area2 = as.numeric(st_area(geometry))) %>%
  mutate(area2 = conv_unit(area2, from = "m2", to = "km2")) %>%
  select(name, area, area2)
```

    ## Simple feature collection with 115 features and 3 fields
    ## geometry type:  POLYGON
    ## dimension:      XY
    ## bbox:           xmin: 265131.4 ymin: 3986650 xmax: 847373 ymax: 4496645
    ## epsg (SRID):    32615
    ## proj4string:    +proj=utm +zone=15 +datum=WGS84 +units=m +no_defs
    ## First 10 features:
    ##          name     area    area2                       geometry
    ## 1       Henry 1896.712 1895.428 POLYGON ((407087.4 4258038,...
    ## 2      Barton 1545.503 1544.814 POLYGON ((357018.1 4149104,...
    ## 3        Knox 1312.684 1311.807 POLYGON ((555742.9 4441840,...
    ## 4  Livingston 1394.863 1393.827 POLYGON ((435019.7 4406658,...
    ## 5     Johnson 2157.957 2156.504 POLYGON ((402629.9 4295560,...
    ## 6     Madison 1288.776 1289.499 POLYGON ((716787.9 4155877,...
    ## 7      Maries 1372.506 1371.714 POLYGON ((571712.7 4223996,...
    ## 8    Lawrence 1588.641 1587.589 POLYGON ((405925.3 4103964,...
    ## 9    Reynolds 2109.320 2109.321 POLYGON ((661466.4 4142452,...
    ## 10     Newton 1622.822 1622.101 POLYGON ((355897 4087900, 3...

Notice the differences between `area` and `area2` - this could be due to
measurement error on the part of the Census Bureau, or to the fact that
we’re using a different projection system to calculate area than they
did.

## Write Data

Finally, we want to go ahead and write our data in a variety of formats.
First up, `.csv`
format:

``` r
write_csv(counties, here("data", "example-data", "MO_HYDRO_PolByCounty", "MO_HYDRO_PolByCounty.csv"))
```

Next, we can write our data to a shapefile (`.shp`)
format:

``` r
st_write(counties, here("data", "example-data", "MO_HYDRO_PolByCounty", "MO_HYDRO_PolByCounty.shp"), 
         delete_dsn = TRUE)
```

    ## Deleting source `/Users/chris/GitHub/SOC5650/LectureRepos/lecture-08/data/example-data/MO_HYDRO_PolByCounty/MO_HYDRO_PolByCounty.shp' using driver `ESRI Shapefile'
    ## Writing layer `MO_HYDRO_PolByCounty' to data source `/Users/chris/GitHub/SOC5650/LectureRepos/lecture-08/data/example-data/MO_HYDRO_PolByCounty/MO_HYDRO_PolByCounty.shp' using driver `ESRI Shapefile'
    ## features:       115
    ## fields:         7
    ## geometry type:  Polygon

Finally, we can write our data to a `.geoJSON` format if we want them to
appear in GitHub as an interactive map:

``` r
counties %>%
  st_transform(crs = 4326) %>%
  st_write(here("data", "example-data", "MO_HYDRO_PolByCounty", "MO_HYDRO_PolByCounty.geoJSON"),
           delete_dsn = TRUE)
```

    ## Deleting source `/Users/chris/GitHub/SOC5650/LectureRepos/lecture-08/data/example-data/MO_HYDRO_PolByCounty/MO_HYDRO_PolByCounty.geoJSON' using driver `GeoJSON'
    ## Writing layer `MO_HYDRO_PolByCounty' to data source `/Users/chris/GitHub/SOC5650/LectureRepos/lecture-08/data/example-data/MO_HYDRO_PolByCounty/MO_HYDRO_PolByCounty.geoJSON' using driver `GeoJSON'
    ## features:       115
    ## fields:         7
    ## geometry type:  Polygon
