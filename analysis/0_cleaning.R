## this script clean and organize the datasheets 
library(readxl)
library(purrr)
library(tidyverse)
library(naniar)

## ----- function -----------------
# function to read multiple sheets as separate df
multiplesheets <- function(fname) {
  
  # getting info about all excel sheets
  sheets <- readxl::excel_sheets(fname)
  tibbles <- lapply(sheets, function(x) readxl::read_excel(fname, sheet = x))
  return(tibbles)
}

## ------ read data --------------
### mater sheet ###
# specifying the path name
path <- "./data/Google_Drive_Data_Sheets/Clean_Annotated_Master_Data_Sheet.xlsx"

# read master sheet 
data.list <- multiplesheets(path)
reference.structure = 

# combine useful columns
n <- length(data.list)
data.df <- tibble()

for ( i in 1:n) {
  # get all numeric columns and make sure they are numeric
  data.i.n <- data.list[[i]] %>% 
    select(Northing, Easting, Avg_Height,	Stdev_Height,	N_Species, Percent_Grazed,
           N_Different_Colors,	Value,	Chroma,	Frame_Height,	N_Gaps,	Avg_Gaps,	Stdev_Gaps,
           Total_Gap_Size,	Cattle,	Sheep_Goats,	Wildebeest,	Zebra,	Thompsons_Gazelle,	Impala,	Topi,	Eland,	Buffalo,
           Grants_Gazelle,	Waterbuck,	Dikdik,	Elephant,	Giraffe,	Ostrich) %>% 
    replace_with_na_all(condition = ~.x == "NA") %>%
    mutate_all(type.convert) # converts strings to numeric where it can

  # get all character columns and make sure they are character
  data.i.c <- data.list[[i]] %>% 
    select(Transect, Site, Most_Common, Most_Common_Grass_Color, Hue) %>% 
    replace_with_na_all(condition = ~.x == "NA")
  data.i.c [] <- lapply(data.i.c, as.character) 
  
  # get date column and make sure it is date 
  data.i.d <- data.list[[i]] %>% select(Date) %>% mutate( Date = lubridate::as_datetime (Date, "Africa/Nairobi"))

  # bind all cols   
  data.i <- bind_cols(data.i.n, data.i.c, data.i.d)
 
  # bind the long df  
  data.df <- data.df %>% bind_rows(data.i)
}

data.df <- data.df %>% mutate (Year = lubridate::year(Date), Month = lubridate::month(Date)) %>%
  rename (Most_Common_Grass_Spp = Most_Common)

### read forage lab results ###
Forage_lab <- read_excel( "./data/Google_Drive_Data_Sheets/Forage_Lab_Results.xlsx") %>% 
  separate(Transect_ID,  into = c("Transect", "Site"),  sep = "(?<=[A-Za-z])(?=[0-9])") %>%
  select(-Sample_Code) %>%
  rename(Forage_Sample_Condition = Sample_Condition,
         Forage_Sample_Name = Sample_Name) %>% mutate_all(type.convert) %>%
  mutate(Site = as.character(Site))

# find which months lack data 
Forage_lab %>% group_by( Year, Month, Transect) %>% summarize(n = n()) %>% filter( n != 12) 
# Year Month Transect     n
# <dbl> <dbl> <chr>    <int>
#   1  2018     7 C           11
# 2  2018     9 C            9
# 3  2018     9 D            6
# 4  2018     9 E            7
# 5  2018     9 F           11
# 6  2018    11 C           11
# 7  2019     1 E           11
# 8  2019     3 C           11
# 9  2019     3 G            8
# 10  2020     7 NA          60

### read soil lab results ###
Soil_lab <- read_excel( "./data/Google_Drive_Data_Sheets/Soil_Lab_Results.xlsx")  %>% 
  separate(Transect_ID, into = c("Transect", "Site"), sep = "(?<=[A-Za-z])(?=[0-9])") %>% 
  select(-Sample_Code) %>%
  rename(Soil_Sample_Condition = Condition) 

### read soil compaction ###
Soil_Compaction <- read_excel( "./data/Google_Drive_Data_Sheets/Soil_Compaction.xlsx") %>% 
  separate(transect, into = c("Transect", "Site"),  sep = "(?<=[A-Za-z])(?=[0-9])") %>% 
  mutate( cum_strikes = as.numeric (cum_strikes),
          cum_strikes = ifelse(is.na(cum_strikes), 0, cum_strikes), # replace NA with 0 
          depth = case_when( depth == 5 ~ "five",
                             depth == 10 ~ "ten",
                             depth == 15 ~ "fifteen",
                             depth == 20 ~ "twenty")) %>%
  spread(depth, cum_strikes) %>%
  mutate(first = five, second = ten - five, third = fifteen - ten, fourth = twenty - fifteen)  %>%
  group_by (Year, Month, Transect, Site) %>% 
  mutate (total_strikes = ifelse((five + ten + fifteen + twenty == 0) , NA, max(five, ten, fifteen, twenty)), # not all occasions all depth strikes were measured 
         max_strikes =  ifelse((five + ten + fifteen + twenty == 0) , NA, max(first, second, third, fourth)),
         ave_strikes =  ifelse((five + ten + fifteen + twenty == 0) , NA, mean (c(first, second, third, fourth), na.rm = T))) %>%
 select(Year, Month, Transect, Site, total_strikes, max_strikes, ave_strikes)

## ------ compiling all data --------------
# data.df; Soil_Compaction; Forage_Lab, Soil_Lab
master_data <- data.df %>% 
  left_join(Soil_Compaction, by = c("Year" = "Year", "Month" = "Month", "Transect" = "Transect", "Site" = "Site")) %>%
  left_join(Soil_lab, by = c("Year" = "Year", "Month" = "Month", "Transect" = "Transect", "Site" = "Site")) %>%
  left_join(Forage_lab, by = c("Year" = "Year", "Month" = "Month", "Transect" = "Transect", "Site" = "Site"))

# order columns 
master_data <- master_data %>% select (
  Year, Month, Transect, Site, Date, Northing, Easting,  #first site attributes
  Avg_Height, Stdev_Height, Frame_Height, Forage_Sample_Condition, Forage_Sample_Name, DM, Biomass,
  E,	Protein,	Fibre,	Fat,	Ash,	Starch,	ADF,	NDF,	Sugar,	NCGD, #second grass quantity and quality
  N_Species, Most_Common_Grass_Spp, N_Different_Colors, Most_Common_Grass_Color, Hue,	Value,	Chroma, # then grass composition 
  N_Gaps, Avg_Gaps, Stdev_Gaps, Total_Gap_Size, # grass structure
  total_strikes, max_strikes,	ave_strikes, # soil structure 
  Soil_Sample_Condition,	Nitrate_N,	Ammonium,	pH,	EC_Salts,	Phosphorus,	Potassium,	Calcium,	Magnesium,	Sodium,	C.E.C, OB,# soil compoition
  Percent_Grazed, Cattle, Sheep_Goats,	Wildebeest,	Zebra,	Thompsons_Gazelle,	Impala,	Topi,	Eland,	Buffalo,	Grants_Gazelle,	Waterbuck,	Dikdik,	Elephant,	Giraffe,	Ostrich  # then animal use
)

## ------ save all data --------------
write_csv(master_data, "./data/cleaned_master_data.csv")
write_csv(master_data %>% select(Year, Month, Transect, Site, Date, Northing, Easting,
                                 Avg_Height, Stdev_Height, Frame_Height, Forage_Sample_Condition, Forage_Sample_Name, DM, Biomass,
                                 E,	Protein,	Fibre,	Fat,	Ash,	Starch,	ADF,	NDF,	Sugar,	NCGD, #second grass quantity and quality
                                 N_Species, Most_Common_Grass_Spp, N_Different_Colors, Most_Common_Grass_Color, Hue,	Value,	Chroma, # then grass composition 
                                 N_Gaps, Avg_Gaps, Stdev_Gaps, Total_Gap_Size# grass structure 
                                 ),  "./data/cleaned_grass_data.csv")
write_csv(master_data %>% select(Year, Month, Transect, Site, Date, Northing, Easting,
                                 total_strikes, max_strikes,	ave_strikes, # soil structure 
                                 Soil_Sample_Condition,	Nitrate_N,	Ammonium,	pH,	EC_Salts,	Phosphorus,	Potassium,	Calcium,	Magnesium,	Sodium,	C.E.C, OB# soil compoition
),  "./data/cleaned_soil_data.csv")
write_csv(master_data %>% select(Year, Month, Transect, Site, Date, Northing, Easting,
                                 Percent_Grazed, Cattle, Sheep_Goats,	Wildebeest,	Zebra,	Thompsons_Gazelle,	Impala,	Topi,	Eland,	Buffalo,	Grants_Gazelle,	Waterbuck,	Dikdik,	Elephant,	Giraffe,	Ostrich  # then animal use
),  "./data/cleaned_animal_data.csv")
