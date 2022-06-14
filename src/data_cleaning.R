library(here)
library(rio)
library(dplyr)
library(lubridate)
library(questionr)
library(ggplot2)
library(forcats)
library(RcppRoll)
library(tidyr)

dfs <- rio::import_list(list.files(here("data", "raw"), full.names = TRUE))

# latitude and longitude column contains a trailing space 
# so imported as character.
dfs$Les_Cayes$LATITUDE <- as.numeric(dfs$Les_Cayes$LATITUDE)
dfs$Les_Cayes$LONGITUDE <- as.numeric(dfs$Les_Cayes$LONGITUDE)
dfs$Ouanaminthe$LATITUDE <- as.numeric(dfs$Ouanaminthe$LATITUDE)
dfs$Ouanaminthe$LONGITUDE <- as.numeric(dfs$Ouanaminthe$LONGITUDE)

# Confirm no missing values were introduced
summary(dfs$Les_Cayes)
summary(dfs$Ouanaminthe)

# Les Cayes LONGITUDE increments by 0.000001 each row
# likely a data entry Excel Auto Fill error.
# The first value is the correct longitude.
dfs$Les_Cayes$LONGITUDE <- dfs$Les_Cayes$LONGITUDE[1]

# EVELATION column contains "m" after number in some files so remove
# and convert to numeric.
for (i in seq_along(dfs)) {
  if (!is.numeric(dfs[[i]]$ELEVATION)) {
    df <- dfs[[i]] %>%
      mutate(
        ELEVATION = case_when(
          endsWith(ELEVATION, "m") ~ substr(ELEVATION, 1, nchar(ELEVATION) - 1)),
        ELEVATION = as.numeric(ELEVATION)
        )
    dfs[[i]] <- df
  }
}

# Combine data from all stations into a single data frame
daily <- dplyr::bind_rows(dfs)

# Convert column names to lowercase
names(daily) <- tolower(names(daily))

# Create a date column
daily$date <- as.Date(paste(daily$year, daily$month, daily$day), 
                      format = "%Y %b %d")

# Observe records on invalid dates
daily %>% filter(is.na(date))

# Remove records on invalid dates (one non-zero measurement removed)
daily <- daily %>% filter(!is.na(date))

summary(daily)

# Check for duplicate dates
daily$dup <- duplicated(daily %>% select(station, date))

daily %>% filter(dup)

daily %>% filter(station == "DAMIEN" & year == 1974 & month %in% c("FEB", "MAR"))

# Data entry error: values added for Mar 29-31 in 1974 after Feb values
daily <- daily %>% filter(!(station == "DAMIEN" & year == 1974 & 
                            month == "MAR" & day %in% 29:31 & !dup))

daily %>% filter(station == "DAMIEN" & year == 1974 & month %in% c("JUL", "AUG"))

# Data entry error: Aug 31 entered instead of July 31 in 1974
daily$month[daily$station == "DAMIEN" & daily$date == as.Date("1974-08-31") & !daily$dup] <- "JUL"

daily %>% filter(station == "LesCayes (FIC)" & year == 1966 & month %in% c("SEP", "OCT"))

# Data entry error: Sep 1 entered instead of Oct 1 in 1966
daily$month[daily$station == "LesCayes (FIC)" & daily$date == as.Date("1966-09-01") & daily$dup] <- "OCT"

# Recalculate corrected date column
daily$date <- as.Date(paste(daily$year, daily$month, daily$day), 
                      format = "%Y %b %d")
# Recheck duplicates
daily$dup <- duplicated(daily %>% select(station, date))

daily %>% filter(dup)

# Clean station names
daily <- daily %>%
  mutate(station = dplyr::recode(station,
                                 CapHaitienFIC = "Cap Haitien",
                                 DAMIEN = "Damien",
                                 `Delmas 33 (FIC)` = "Delmas 33",
                                 `Jacmel (FIC)` = "Jacmel",
                                 `Jeremie(Chateau)` = "Jeremie Chateau",
                                 `LesCayes (FIC)` = "Les Cayes",
                                 `Ouanaminthe (FIC)` = "Ouanaminthe",
                                 `Petionville(JuvenatFIC)` = "Petionville Juvenat",
                                 `Thiotte(Ville)` = "Thiotte Ville"))

# Check station metadata is consistent
daily %>%
  group_by(station) %>%
  summarise(n_lat = length(unique(latitude)),
            n_lon = length(unique(longitude)),
            n_ele = length(unique(elevation)))

# Extract station metadata
metadata <- daily %>%
  group_by(station) %>%
  summarise(latitude = first(latitude), 
            longitude = first(longitude), 
            elevation = first(elevation))

# Remove metadata columns from daily data
# and reorder columns
daily <- daily %>%
  select(station, date, year, month, day, rain)

# Convert month to numeric
daily$month <- lubridate::month(daily$date)

# Fill any date gaps
dates_list <- list()
for(s in unique(daily$station)) {
  
  dates <- seq(min((daily %>% filter(station == s))$date), 
               max((daily %>% filter(station == s))$date),
               by = 1)
  dd <- data.frame(station = s, date = dates)
  dates_list[[length(dates_list) + 1]] <- dd
}
date_df <- bind_rows(dates_list)
nr <- nrow(date_df)
if(nrow(daily) < nr) {
  print(paste("Filling", nr - nrow(daily), "rows"))
  daily <- full_join(date_df, daily, by = c("station", "date"))
  daily <- daily %>%
    mutate(year = year(date), month = month(date), day = day(date))
}

# Sort by date
daily <- daily %>%
  arrange(station, date)

# Inventory plot
ggplot(daily, aes(x = date, y = station, fill = !is.na(rain))) +
  geom_tile() +
  geom_hline(yintercept = 
               seq(0.5, by = 1, 
                   length.out = length(unique(daily$station)) + 1)) +
  scale_fill_manual(name = "", labels = c("Missing", "Present"), 
                    values = c("#F8766D", "grey")) +
  scale_x_date(breaks = seq(as.Date("1930/1/1"), as.Date("2020/1/1"), 
                            by = "10 years"),
               date_labels = "%Y") +
  theme(panel.grid.minor = element_blank(), panel.grid.major.y = element_blank()) +
  labs(x = "Date", y = NULL)

# Check for negative rainfall
daily %>% filter(rain < 0)
# no negative rainfall values

# Check large values
daily %>% filter(rain > 300)
# 9 values above 300mm but none above 500mm.
# These may be real heavy rainfall/hurricane values so not removed.

# Check consecutive non-zero values
consec_check <- daily %>% 
  group_by(station) %>%
  mutate(same = rep(rle(as.numeric(rain))$lengths, rle(as.numeric(rain))$lengths)) %>%
  filter(rain > 1.5 & same >= 2)
View(consec_check)
# No more than 3  consecutive days with the same value.
# These may be accumulated values split over 2/3 days so not removed.

# Zero rainfall in most rain months check
# Check rainy season months May and October with no rainfall.
dry_check <- daily %>%
  group_by(station, year, month) %>%
  summarise(t_rain = sum(rain)) %>%
  group_by(station) %>%
  #mutate(t_rain_2 = RcppRoll::roll_sum(t_rain, 2, align = "right", fill = NA)) %>%
  filter(month %in% c(5, 10)) %>%
  filter(t_rain == 0)
View(dry_check)

# Inspect rainfall totals in the same months at nearby stations
# and surrounding months at the same station 
# to detect missing values recorded as zeros.
month_rain <- daily %>%
  group_by(station, year, month) %>%
  summarise(t_rain = sum(rain)) %>%
  pivot_wider(id_cols = c(year, month), names_from = station, values_from = t_rain) %>%
  arrange(year, month)
View(month_rain)

# Cap Haitian, May 1988
# Jan-May 1988 are all zero. Suspected missing values.
# Replace zeros with missing values.
daily$rain[daily$station == "Cap Haitien" & daily$year == 1988 & daily$month %in% 1:5] <- NA

# Damien, May 1929
# Apr-Dec are all zero. Suspected missing values.
# Replace zeros with missing values.
daily$rain[daily$station == "Damien" & daily$year == 1929 & daily$month %in% 4:12] <- NA

# Damien, October 1953
# Oct-Nov are all zero. Suspected missing values in October.
# November values are questionable but lower rainfall expected.
# Replace zeros in October with missing values.
daily$rain[daily$station == "Damien" & daily$year == 1953 & daily$month %in% 10] <- NA

# Damien, October 1961
# Oct-Dec are all zero. Suspected missing values in October & November.
# December values are questionable but lower rainfall expected 
# and low rainfall observed in nearby station.
# Replace zeros in October & November with missing values.
daily$rain[daily$station == "Damien" & daily$year == 1961 & daily$month %in% 10:11] <- NA

# Check consecutive rain days
daily %>%
  group_by(station) %>%
  mutate(raindays = cumsum(rain > 0) - cummax(((rain > 0) == 0) * cumsum(rain > 0))) %>%
  filter(raindays > 20)
