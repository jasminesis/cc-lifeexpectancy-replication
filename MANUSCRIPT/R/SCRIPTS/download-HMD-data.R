###------Download HMD data-----
## @knitr download-HMD-data

###   Inputting user info for HMD

myusername <- getPass(msg = "HMD username: ", noblank = FALSE, forcemask = FALSE)
mypassword <- getPass(msg = "HMD password: ", noblank = FALSE, forcemask = FALSE)

###   Getting a country list from the HMD
Countries <- getHMDcountries()

###   Downloading the DEATHS data from the HMD in 5-year age groups by single-year.
deaths <- rbindlist(lapply(Countries, function(CNTRY){
  Dat <- readHMDweb(CNTRY = CNTRY, item = "Deaths_5x1", fixup=TRUE, username = myusername, password = mypassword)
  Dat$CNTRY <- CNTRY
  Dat}))

###   Downloading the POPULATION data from the HMD.
pops <- rbindlist(lapply(Countries, function(CNTRY){
  Dat <- readHMDweb(CNTRY = CNTRY, item = "Population", fixup=TRUE, username = myusername, password = mypassword)
  Dat$CNTRY <- CNTRY
  Dat}))

###   Downloading the LIFE TABLE data from the HMD in 5-year age groups by single-year.
lt <- rbindlist(lapply(Countries, function(CNTRY){
  Dat <- readHMDweb(CNTRY = CNTRY, item = "bltper_5x1", fixup=TRUE, username = myusername, password = mypassword)
  Dat$CNTRY <- CNTRY
  Dat}))

###   Topcoding the age groups to 95+ of the LIFE TABLE data, and summing the ax value.
lt2 <- lt%>%
  filter(Age <= 95)
  # mutate(Age = ifelse(Age >= 95, 95, Age)) %>%
  # 
  # group_by(CNTRY, Year, Age) %>%
  # summarise(ax = sum(ax))

###   Topcoding the age groups of the DEATHS data and summing.
deaths2 <- deaths %>%
  mutate(Age = ifelse(Age >=95, 95, Age)) %>%
  group_by(CNTRY, Year, Age) %>%
  summarise(deaths = sum(Total) )

###   Recoding the Population data to single year of age
pops2 <- pops %>%
  mutate(Age = case_when(
    Age >= 95 ~ 95,
    Age >= 90 ~ 90,
    Age >= 85 ~ 85,
    Age >= 80 ~ 80,
    Age >= 75 ~ 75,
    Age >= 70 ~ 70,
    Age >= 65 ~ 65,
    Age >= 60 ~ 60,
    Age >= 55 ~ 55,
    Age >= 50 ~ 50,
    Age >= 45 ~ 45,
    Age >= 40 ~ 40,
    Age >= 35 ~ 35,
    Age >= 30 ~ 30,
    Age >= 25 ~ 25,
    Age >= 20 ~ 20,
    Age >= 15 ~ 15,
    Age >= 10 ~ 10,
    Age >= 5 ~ 5,
    Age >= 1 ~ 1,
    TRUE ~ as.numeric(Age)
  )) %>%
  group_by(CNTRY, Year, Age) %>%
  summarise(Pop= sum(Total1))

###   Joining the POPULATION, DEATHS, and LIFE TABLE data and subseting to the maximum year in the data.
a <- left_join(pops2, deaths2)
a <- left_join(a, lt2)
a <- group_by(a, CNTRY, Age) %>%
  filter(Year == max(Year)) %>%
  dplyr::select(-OpenInterval)

write_csv(a, "Manuscript/R/DATA-PROCESSED/hmd.csv")




