###------prepare GBD Data-----
## @knitr prepare-gbd-data


# Downloading and unzipping the GBD Data
download.file("http://s3.healthdata.org/gbd-api-2017-public/94a6924295130f8efecc9f9c5185038d_files/IHME-GBD_2017_DATA-94a69242-1.zip",
                              "./Manuscript/R/DATA-RAW/IHME-GBD_2017_DATA-94a69242-1.zip")
unzip(zipfile='./Manuscript/R/DATA-RAW/IHME-GBD_2017_DATA-94a69242-1.zip', exdir = "Manuscript/R/DATA-RAW")

# Loading in the GBD Data
gbd <- read_csv("./Manuscript/R/DATA-RAW/IHME-GBD_2017_DATA-94a69242-1.csv") %>%
  filter(metric_name == "Rate") %>% # Selecting only the mortality rates by age
  group_by(location_id, location_name, age_id, age_name) %>%
  summarise(meanval = mean(val/100000), # Rates are reported per 100k persons. Converting to a strict mortality rate.
            upperval = mean(upper/100000),
            lowerval = mean(lower/100000)) %>%
  ungroup() %>%
  # Fixing the UK name to join with the HMD and other data.
  mutate(location_name = case_when(
    location_name == "United Kingdom" ~ "United Kingdom of Great Britain and Northern Ireland",
    TRUE ~ as.character(location_name)
  ),
  # Fixing Age Group codes
  Age = case_when(
    age_id == 4 ~ 0,
    age_id == 5 ~ 1,
    age_id == 6 ~ 5,
    age_id == 7 ~ 10,
    age_id == 8 ~ 15,
    age_id == 9 ~ 20,
    age_id == 10 ~ 25,
    age_id == 11 ~ 30,
    age_id == 12 ~ 35,
    age_id == 13 ~ 40,
    age_id == 14 ~ 45,
    age_id == 15 ~ 50,
    age_id == 16 ~ 55,
    age_id == 17 ~ 60,
    age_id == 18 ~ 65,
    age_id == 19 ~ 70,
    age_id == 20 ~ 75,
    age_id == 30 ~ 80,
    age_id == 31 ~ 85,
    age_id == 32 ~ 90,
    age_id == 235 ~ 95
  )
  ) %>%
  dplyr::select(COUNTRY = location_name, meanval:Age)



countrycodes <- read.csv("https://raw.githubusercontent.com/lukes/ISO-3166-Countries-with-Regional-Codes/master/all/all.csv") %>%
  rename(CNTRY = alpha.3,
         COUNTRY = name) %>%
  mutate(ISO3 = CNTRY) %>%
  dplyr::select(COUNTRY, CNTRY, ISO3, Country.code = country.code)


gbd2 <- left_join(gbd, countrycodes) %>%
  mutate(CNTRY = case_when(
     COUNTRY == "Czech Republic" ~ "CZE",
    TRUE ~ as.character(CNTRY)
  ))

write_csv(gbd2, "./Manuscript/R/DATA-PROCESSED/gbd_data.csv")