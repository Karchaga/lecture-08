# dependencies
library(dplyr)
library(readr)
library(sf)
library(tidycensus)
library(tigris)

# download county boundaries
counties(state = 29, class = "sf") %>%
  mutate(ALAND = as.character(ALAND)) %>%
  mutate(AWATER = as.character(AWATER)) -> mo

# write counties to example data
st_write(mo, "data/example-data/MO_BOUNDARY_Counties/MO_BOUNDARY_Counties.shp", delete_dsn = TRUE)

# write counties to lab data
st_write(mo, "data/lab-07/MO_BOUNDARY_Counties/MO_BOUNDARY_Counties.shp", delete_dsn = TRUE)

# download county population
get_acs(state = 29, geography = "county", table = "C18131", output = "wide") %>%
  select(GEOID, C18131_002E, C18131_003E, C18131_004E,
         C18131_005E, C18131_006E, C18131_007E, C18131_008E,
         C18131_009E, C18131_010E) %>%
  rename(
    under50 = C18131_002E,
    under50_dis = C18131_003E,
    under50_non = C18131_004E,
    btwn50_99 = C18131_005E,
    btwn50_99_dis = C18131_006E,
    btwn50_99_non = C18131_007E,
    btwn100_150 = C18131_008E,
    btwn100_150_dis = C18131_009E,
    btwn100_150_non = C18131_010E,
  ) -> disability

get_acs(state = 29, geography = "county", variable = "B01003_001") %>%
  select(-moe, -variable) %>%
  rename(totalPop = estimate) -> totalPop

# write counties to example data
mo %>%
  select(GEOID) %>%
  left_join(., totalPop, by = "GEOID") %>%
  st_write("data/lab-07/MO_DEMOS_CountyPop/MO_DEMOS_CountyPop.shp", delete_dsn = TRUE)

# write counties to lab data
st_geometry(disability) <- NULL

write_csv(disability, "data/lab-07/MO_DEMOS_CountyDisability/MO_DEMOS_CountyDisability.csv")
