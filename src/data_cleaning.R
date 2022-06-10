library(here)
library(rio)
library(dplyr)
library(lubridate)
library(questionr)
library(ggplot2)

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
# The first value is the correct longitude
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
daily$dup <- questionr::duplicated2(daily %>% select(station, date))

daily %>% filter(dup)

daily %>% filter(station == "DAMIEN" & year == 1974 & month %in% c("FEB", "MAR"))

# Data entry error: values added for Mar 29-31 in 1974 after Feb values
daily <- daily %>% slice(-c(45351:45353))

daily %>% filter(station == "DAMIEN" & year == 1974 & month %in% c("JUL", "AUG"))

# Data entry error: Aug 31 entered instead of July 31 in 1974
daily$month[[45503]] <- "JUL"

daily %>% filter(station == "LesCayes (FIC)" & year == 1966 & month %in% c("SEP", "OCT"))

# Data entry error: Sep 1 entered instead of Oct 1 in 1966
daily$month[[115597]] <- "OCT"

# Recalculate date column
daily$date <- as.Date(paste(daily$year, daily$month, daily$day), 
                      format = "%Y %b %d")
# Recheck duplicates
daily$dup <- questionr::duplicated2(daily %>% select(station, date))

daily %>% filter(dup)

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

daily %>% filter(rain > 300)
