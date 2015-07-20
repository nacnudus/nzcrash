library(curl)
library(rvest)
library(dplyr)
library(tidyr)
library(purrr)
library(magrittr)
library(readr)
library(stringr)
library(lubridate)
library(sp)
library(devtools)

# Define constants ------------------------------------------------------------#


nztz <- "Pacific/Auckland"             # Time zone
nzorigin <- ymd(origin, tz = nztz)

wgsproj <- "+init=epsg:4326"           # Google maps projection
nztmproj <- "+init=epsg:2193"          # Source projection

download_dir <- tempdir()
# download_dir <- "../../../nzcrash_dev/downloads"  # Debug

home_url <- "http://www.nzta.govt.nz"  # Note, no trailing slash
source_url <- "http://www.nzta.govt.nz/resources/crash-analysis-system-data/"

data_dictionary_url <- "http://www.nzta.govt.nz/assets/resources/guide-to-coded-crash-reports/docs/guide-to-coded-crash-reports.pdf"
tcr_url <- "http://www.nzta.govt.nz/assets/resources/traffic-crash-reports/docs/traffic-crash-reports.pdf"

col_names <- c("TLA NAME",
               "CRASH ROAD",
               "CRASH DIST",
               "CRASH DIRN",
               "INTSN",
               "SIDE ROAD",
               "CRASH ID",
               "CRASH DATE",
               "CRASH DOW",
               "CRASH TIME",
               "MVMT",
               "VEHICLES",
               "CAUSES",
               "OBJECTS STRUCK",
               "ROAD CURVE",
               "ROAD WET",
               "LIGHT",
               "WTHRa",
               "JUNC TYPE",
               "TRAF CTRL",
               "ROAD MARK",
               "SPD LIM",
               "CRASH FATAL CNT",
               "CRASH SEV CNT",
               "CRASH MIN CNT",
               "PERS AGE1",
               "PERS AGE2",
               "EASTING",
               "NORTHING",
               "DUMMY")                 # Dummy column for trailing commas

col_names_nice <- c("region",
                    "road",
                    "distance_from_landmark",
                    "direction_from_landmark",
                    "landmark",
                    "side_road",
                    "id",
                    "date",
                    "weekday",
                    "time",
                    "movement",
                    "vehicles",
                    "causes",
                    "objects_struck",
                    "curvature",
                    "wetness",
                    "light",
                    "weather",
                    "junction_type",
                    "traffic_control",
                    "road_marking",
                    "speed_limit",
                    "fatalities",
                    "severe_injuries",
                    "minor_injuries",
                    "pedestrian_age",
                    "cyclist_age",
                    "easting",
                    "northing")

col_types <- "cciccciccccccccccccccciiiiiiic"

# Download metadata -----------------------------------------------------------#

curl_download(data_dictionary_url, destfile = "./data-dictionary.pdf")
curl_download(tcr_url, destfile = "./traffic-crash-report-guide.pdf")

# Obtain urls of csv files ----------------------------------------------------#

urls <- 
  html(source_url) %>% 
  html_nodes(".publication__sections--link") %>% # Get the links
  html_attrs %>%                       # Get the metadata
  unzip %>%                            # Convert from a list to a data.frame
  as_data_frame %>%
  select(-class) %>%                   # Drop unnecessary metadata
  rename(url = href) %>%               # Nice column names
  mutate(url = paste0(home_url, url) # Prepend parent to relative url
         , title = str_sub(title, -4)  # Only keep the year of the title
         , path = paste0(download_dir, "/", title)) # Download path

# Download csv files ----------------------------------------------------------#

urls %>%
  rowwise %>%
  do(result = curl_download(.$url, .$path))

# Read csv files --------------------------------------------------------------#

crashes <- 
  data_frame(path = paste0(download_dir, "/", list.files(download_dir))) %>%
  rowwise %>%
  do(data = read_csv(.$path
                     , skip = 1
                     , col_names = col_names
                     , col_types = col_types))

# Bind all csv datasets together
crashes  <- bind_rows %>% smash %>% invoke(crashes$data) %>% .[[1]]

# Drop dummy column
crashes %<>% select(-DUMMY)

# Make nice column names -------------------------------------------------------#

crashes %<>%
  set_colnames(col_names_nice)

# Make nice dates --------------------------------------------------------------#

crashes %<>%
  mutate(date = dmy(date, tz = nztz)
         , time = nzorigin + hm(paste(str_sub(time, , -3L), str_sub(time, 3L)))
         , datetime = date + hours(hour(time)) + minutes(minute(time))) %>%
  select(-weekday)

# Geo-locate ------------------------------------------------------------------#

crashes_geo <- 
  crashes %>%
  filter(!is.na(easting) & 
         !is.na(northing) & 
         easting !=0 &
         northing != 0) %>%
  as.data.frame

coordinates(crashes_geo) <- ~ easting + northing
proj4string(crashes_geo) <- nztmproj

crashes_geo %<>% 
  spTransform(CRS(wgsproj)) %>%
  as.data.frame %>%
  tbl_df %>%
  select(id, easting, northing)

crashes %<>% 
  select(-easting, -northing) %>%
  left_join(crashes_geo)

rm(crashes_geo)

# Normalise -------------------------------------------------------------------#

# Movements
crashes %<>%
  separate(movement, c("movement_row", "movement_column"), 1, extra = "drop")
# street and direction of vehicle 1 are added later
# Join movements to a descriptive table
movement_codes <- 
  read_csv("./movement_codes.csv") %>%
    mutate(movement_category = factor(movement_category, levels = movement_category)
           , movement_description = factor(movement_description, levels = movement_description))
crashes %<>% 
  left_join(movement_codes, by = c("movement_row", "movement_column")) %>%
  select(-movement_row, -movement_column)
# Reorder factors by count
m1 <- 
  crashes %>%
  count(movement_category, sort = TRUE) %>%
  .$movement_category
crashes %<>%
  mutate(movement_category = factor(movement_category, levels = m1))
m2 <- 
  crashes %>%
  group_by(movement_category, movement_description) %>%
  tally %>%
  arrange(movement_category, desc(n)) %>%
  mutate(combination = paste0(movement_category, movement_description))
crashes %<>%
  mutate(movement_description = factor(paste0(movement_category, movement_description)
                                , levels = m2$combination
                                , labels = m2$movement_description))
rm(m1, m2, movement_codes)

# Vehicles 
vehicles <- 
  crashes %>%
  select(id, vehicles) %>%
  separate(vehicles, c("a", "dummy1", "dummy2", "b", "c", "d"), 1:5
           , extra = "drop", remove = FALSE) %>% # Can't remove 'vehicles' until streetdirection has been obtained
  gather(key = "vehicle", value = "type", a, b, c, d) %>%
  select(id, vehicle, type) %>%
  filter(type != "") %>%
  arrange(id, vehicle) 
# Can't remove 'vehicles' column of 'crashes' until streetdirection has been obtained
vehicles %<>% 
  mutate(vehicle_id = as.character(vehicle)
         , vehicle = factor(type
                       , levels = c("C", "V", "X", "B", "L", "4", "T", "M", "P",
                                    "S", "O")
                       , labels = c("Car", "Van, ute"
                                    , "Taxi or taxi van", "Bus"
                                    , "School bus"
                                    , "SUV or 4x4 vehicle", "Truck"
                                    , "Motorcycle", "Moped", "Bicycle"
                                    , "Other or unknown"))) %>%
select(id, vehicle_id, vehicle)

# Street and direction of vehicle 1 in movement 
crashes %<>%
  separate(vehicles, c("dummy1", "direction", "street", "dummy2"), 1:3, extra = "drop") %>% 
  select(-dummy1, -dummy2)

# Causes
maxcauses <- max(str_count(crashes$causes, " ")) # space-delimited string
causes <- 
  crashes %>%
  select(id, causes) %>%
  separate(causes, 1:maxcauses, extra = "drop")
causes[causes == ""] <- NA             # trailing spaces create empty strings
causes %<>%
  gather_("index", "value", as.character(1:maxcauses)) %>%
  filter(!is.na(value)) %>%
  separate(value, c("code", "vehicle_id"), 3, extra = "drop") %>%
  mutate(index = as.integer(index)
         , code = as.integer(code)
         , vehicle_id = str_to_lower(vehicle_id))
causes[causes == ""] <- NA             # 800-900-series causes are environmental (not associated with vehicles)
crashes %<>% select(-causes)
# Join to code descriptions
causes_table <- read.csv("./causes.csv", sep = "|")
causes %<>% 
  left_join(causes_table)
rm(causes_table, maxcauses)

# Objects struck
objects_struck <- 
  crashes %>%
  select(id, objects_struck) %>%
  separate(objects_struck, 1:3, 1:2, extra = "drop") %>%
  gather_("object_order", "object", as.character(1:3))
objects_struck[objects_struck == ""] <- NA # separate fills with blanks
objects_struck %<>%
  filter(!is.na(object)) %>%
  arrange(id, object_order)
crashes %<>% select(-objects_struck)
objects_struck %<>%
  mutate(object_order = as.integer(object_order)
         , object = factor(object
                           , levels = LETTERS[c(1:14, 16:20, 22:26)]
                           , labels = c("Driven or accompanied animals, i.e. under control"
                                        , "Bridge abutment, handrail or approach, includes tunnels"
                                        , "Upright cliff or bank, retaining walls"
                                        , "Debris, boulder or object dropped from vehicle"
                                        , "Over edge of bank"
                                        , "Fence, letterbox, hoarding etc."
                                        , "Guard or guide rail (including median barriers)"
                                        , "House or building"
                                        , "Traffic island or median strip"
                                        , "Public furniture, e.g. phone boxes, bus shelters, signal controllers, etc."
                                        , "Kerb, when directly contributing to incident"
                                        , "Landslide, washout or floodwater"
                                        , "Parked motor vehicle"
                                        , "Train"
                                        , "Utility pole, includes lighting columns"
                                        , "Broken down vehicle, workmen's vehicle, taxis picking up, etc."
                                        , "Roadwork signs or drums, holes and excavations, etc."
                                        , "Traffic signs or signal bollards"
                                        , "Trees, shrubbery of a substantial nature"
                                        , "Ditch"
                                        , "Wild animal, strays, or out of control animals"
                                        , "Other"
                                        , "Objects thrown at or dropped onto vehicles"
                                        , "Into water, river or sea")))

# Light (needn't be separate from crashes)
crashes %<>%
  separate(light, c("light_ambient", "light_artificial"), 1, extra = "drop") %>%
  mutate(light_ambient = ifelse(light_ambient == " ", NA, light_ambient) # separate fills with blanks
         , light_artificial = ifelse(light_artificial == " ", NA, light_artificial))
crashes %>%
  select(light_ambient, light_artificial)

# Weather (needn't be separate from crashes)
crashes %<>%
  separate(weather, paste0("weather_", 1:2), 1, extra = "drop") %>%
  mutate(weather_1 = ifelse(weather_1 == " ", NA, weather_1) # separate fills with blanks
         , weather_2 = ifelse(weather_2 == " ", NA, weather_2))

# Severity (worst injury) of crash (needn't be separate from crashes)
crashes %<>%
  mutate(severity  = ifelse(fatalities >= 1, "fatal"
                            , ifelse(severe_injuries >= 1, "serious"
                                     , ifelse(minor_injuries >= 1, "minor", "non-injury")))
         , severity = factor(severity, levels = c("fatal"
                                                  , "serious"
                                                  , "minor"
                                                  , 'non-injury')))


# Make certain columns factors for empty levels
crashes %<>%
  mutate(direction_from_landmark = ifelse(direction_from_landmark == " "
                                          , NA
                                          , direction_from_landmark)
         , direction_from_landmark = factor(direction_from_landmark
                                            , levels = c("N", "E", "S", "W")
                                            , labels = c("North", "East"
                                                         , "South", "West"))
         , landmark = ifelse(landmark == " " , NA , landmark)
         , landmark = factor(landmark
                             , levels = c("I", "A")
                             , labels = c("Intersection", "Landmark"))
         , direction = factor(direction
                              , levels = c("N", "E", "S", "W", "U")
                              , labels = c("North", "East", "South", "West"
                                           , "Unknown"))
         , street = factor(street
                           , levels = c("1", "2", "C")
                           , labels = c("First", "Second", "C"))
         , curvature = factor(curvature
                              , levels = c("R", "E", "M", "S")
                              , labels = c("Straight", "Easy", "Medium"
                                           , "Severe"))
         , wetness = factor(wetness
                            , levels = c("D", "W", "I")
                            , labels = c("Dry", "Wet", "Ice or snow"))
         , light_ambient = factor(light_ambient
                                  , levels = c("B", "O", "T", "D")
                                  , labels = c("Bright sun", "Overcast"
                                               , "Twilight", "Dark"))
         , light_artificial = factor(light_artificial
                                     , levels = c("O", "F", "N")
                                     , labels = c("Street lights on"
                                                  , "Street lights off"
                                                  , "No street lights present"))
         , weather_1 = factor(weather_1
                              , levels = c("F", "M", "L", "H", "S")
                              , labels = c("Fine", "Mist/fog", "Light rain"
                                           , "Heavy rain", "Snow"))
         , weather_2 = factor(weather_2
                              , levels = c("F", "S")
                              , labels = c("Frost", "Strong wind"))
         , junction_type = factor(junction_type
                                  , levels = c("D", "R", "X", "T", "Y", "M")
                                  , labels = c("Driveway"
                                               , "Roundabout"
                                               , "Crossroads"
                                               , "T junction"
                                               , "Y junction"
                                               , "Multi-leg (>4)"))
         , traffic_control = factor(traffic_control
                                    , levels = c("T", "S", "G", "P", "N")
                                    , labels = c("Traffic signals"
                                                 , "Stop sign"
                                                 , "Give way sign"
                                                 , "School patrol or warden"
                                                 , "Nil"))
         , road_marking = factor(road_marking
                                 , levels = c("X", "R", "P", "L", "C", "N")
                                 , labels = c("Pedestrian crossing"
                                              , "Raised island"
                                              , "painted island"
                                              , "no passing line"
                                              , "centre line"
                                              , "Nil"))
         , speed_limit = factor(speed_limit
                                , levels = c("LSZ", "005", "010", "015", "020"
                                             , "025", "030", "040", "050"
                                             , "060", "070", "080", "090"
                                             , "100")
                                , labels = c("LSZ", "5", "10", "15", "20", "25"
                                             , "30", "40", "50", "60", "70"
                                             , "80", "90", "100")))

# Convert everything back to data.frame
crashes %<>% as.data.frame
causes %<>% as.data.frame
objects_struck %<>% as.data.frame
vehicles %<>% as.data.frame

# Save data in the package
devtools::use_data(crashes, causes, objects_struck, vehicles, overwrite = TRUE, compress = "xz")
