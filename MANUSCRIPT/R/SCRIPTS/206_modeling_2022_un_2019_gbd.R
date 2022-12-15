gbd_2019 <- read_csv("./MANUSCRIPT/R//DATA-RAW/IHME-GBD_2019_DATA.csv") %>%
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

gbd2_2019 <- left_join(gbd_2019, countrycodes) %>%
  mutate(CNTRY = case_when(
    COUNTRY == "Czech Republic" ~ "CZE",
    TRUE ~ as.character(CNTRY)
  ))


dat_UNlifetables2022 <- read.xlsx("./MANUSCRIPT/R/DATA-RAW/WPP2022_MORT_F07_1_ABRIDGED_LIFE_TABLE_BOTH_SEXES.xlsx", 
                                     sheet = 2,
                                     startRow = 17) %>%
  rename(Country.code=`Location.code`) %>% 
  filter(Year == "2080") %>%
  left_join(., countrycodes) %>%
  dplyr::select(Year, Age = `Age.(x)`,
                mx_base = `Central.death.rate.m(x,n)`, 
                ax = `Average.number.of.years.lived.a(x,n)`,
                COUNTRY, CNTRY, ISO3) %>%
  mutate(Age = as.numeric(Age),
         mx_base = as.numeric(mx_base),
         ax = as.numeric(ax))

dat_UNlifetables2022[is.na(dat_UNlifetables2022)] <- 1

# The UN pop data is in 5-year intervals, but has a separate file that contains 0-1 and 1-4 age groups.
dat_UN0004_2022 <- read.xlsx("./MANUSCRIPT/R/DATA-RAW/WPP2022_POP_F03_1_POPULATION_SELECT_AGE_GROUPS_BOTH_SEXES.xlsx",
                                sheet = 2,
                                startRow = 17) %>%
  filter(Year == "2080") %>%
  rename(Country.code=`Location.code`) %>% 
  left_join(., countrycodes) %>%
  dplyr::select(`0` = `0-1`,
                `1` = `0-4`,
                COUNTRY, CNTRY, ISO3) %>%
  gather(Age, Pop, `0`:`1`) %>%
  mutate(Age = as.numeric(Age), Pop = as.numeric(Pop))

dat_UN05plus_2022 <- read.xlsx("./MANUSCRIPT/R/DATA-RAW/WPP2022_POP_F02_1_POPULATION_5-YEAR_AGE_GROUPS_BOTH_SEXES.xlsx",
                                  sheet = 2,
                                  startRow = 17) %>%
  filter(Year == "2080") %>%
  rename(Country.code=`Location.code`) %>% 
  left_join(., countrycodes) %>%
  dplyr::select(-`0-4`) %>%
  gather(Age, Pop, `5-9`:`100+`) %>%
  dplyr::select(Period = `Year`,
                Age, Pop,
                COUNTRY, CNTRY, ISO3) %>%
  separate(Age, c("Age", NA), sep = '-' ) %>%
  mutate(Age = if_else(Age == "100+", 95, as.numeric(Age)), Pop = as.numeric(Pop)) %>%
  group_by(COUNTRY, CNTRY, ISO3, Age) %>%
  dplyr::summarise(Pop = sum(Pop)) %>%
  ungroup()

# Joining the UN pop data together. Population #'s are reported in thousands so we convert them.
dat_UN_2022 <- rbind(dat_UN0004_2022, dat_UN05plus_2022) %>%
  mutate(Pop = Pop * 1000,
         COUNTRY = if_else(COUNTRY == "Czechia", "Czech Republic", paste(COUNTRY)))

a_2022 <- left_join(lancetdata, dat_UN_2022) %>%
  dplyr::select(Age, Pop, CNTRY, MID, LOW, HIGH) %>%
  filter(!is.na(MID))
a2_2022 <- left_join(a_2022, gbd2_2019) %>%
  mutate(width = ifelse(Age == 0, 1, # setting the width of the interval
                        ifelse(Age == 1, 4,5))) 

ccbaselinedeaths_2022 <- a2_2022 %>%
  # Calculating the low, mid, and high expected number of CC related mortality.
  # This is done by multiplying the population in the HMD (Pop) by the underlying
  # mortality rate from the GBD for the COD C2.9 (Heat Exposure). This calculation is Pop*lowerval for
  # the lower value in the GBD.
  mutate(ccdeaths_low = Pop*lowerval, #
         ccdeaths_mid = Pop*meanval,
         ccdeaths_high = Pop*upperval) %>%
  group_by(CNTRY) %>%
  # The cc_deaths_x variables contain the expected number of deaths using baseline exposure.
  # We then proportionally rescale these values to equal the anticipated mortality in the future.
  dplyr::mutate(ccdeaths_low2 = ccdeaths_low/sum(ccdeaths_low)*LOW,
                ccdeaths_mid2 = (ccdeaths_mid/sum(ccdeaths_mid))*MID,
                ccdeaths_high2 = ccdeaths_high/sum(ccdeaths_high)*HIGH,
                Age = if_else(Age >= 85, 85, Age)) %>%
  group_by(CNTRY, Age) %>%
  dplyr::summarise(ccdeaths_low2 = sum(ccdeaths_low2),
                   ccdeaths_mid2 = sum(ccdeaths_mid2),
                   ccdeaths_high2 =sum(ccdeaths_high2),
                   Pop = sum(Pop)
  ) %>%
  mutate(mxcc_low = ccdeaths_low2/ Pop,
         mxcc_mid = ccdeaths_mid2/ Pop,
         mxcc_high =ccdeaths_high2/ Pop) %>%
  ungroup() %>%
  left_join(., dat_UNlifetables2022) %>%
  dplyr::mutate(width = ifelse(Age == 0, 1, # setting the width of the interval
                               ifelse(Age == 1, 4,5)),
                # With re-scaled values, we recalculated mx values with the anticipated # of CC deaths.
                mx_low = (mx_base + mxcc_low),
                mx_mid = (mx_base + mxcc_mid),
                mx_high = (mx_base + mxcc_high),
                # Calculating a probability of dying for all analyses (base, low, mid, high).
                qx_base = ifelse(Age == 85, 1, (mx_base*width)/(1+((width-ax)*mx_base))),
                qx_low = ifelse(Age == 85, 1, (mx_low*width)/(1+((width-ax)*mx_low))),
                qx_mid = ifelse(Age == 85, 1, (mx_mid*width)/(1+((width-ax)*mx_mid))),
                qx_high = ifelse(Age == 85, 1, (mx_high*width)/(1+((width-ax)*mx_high)))) %>%
  dplyr::group_by(CNTRY) %>%
  mutate(lx_base = ifelse(is.na(lag(cumprod(1-qx_base),1)*100000), 100000, lag(cumprod(1-qx_base),1)*100000),
         dx_base = qx_base*lx_base,
         Lx_base = ifelse(Age == 85, (lx_base/mx_base), ax * lx_base + (width-ax) * lead(lx_base,1)),
         Tx_base = rev(cumsum(rev(Lx_base))),
         ex_base = Tx_base/lx_base,
         lx_low = ifelse(is.na(lag(cumprod(1-qx_low),1)*100000), 100000, lag(cumprod(1-qx_low),1)*100000),
         dx_low = qx_low*lx_low,
         Lx_low = ifelse(Age == 85, (lx_low/mx_low), ax * lx_low + (width-ax) * lead(lx_low,1)),
         Tx_low = rev(cumsum(rev(Lx_low))),
         ex_low = Tx_low/lx_low,
         lx_mid = ifelse(is.na(lag(cumprod(1-qx_mid),1)*100000), 100000, lag(cumprod(1-qx_mid),1)*100000),
         dx_mid = qx_mid*lx_mid,
         Lx_mid = ifelse(Age == 85, (lx_mid/mx_mid), ax * lx_mid + (width-ax) * lead(lx_mid,1)),
         Tx_mid = rev(cumsum(rev(Lx_mid))),
         ex_mid = Tx_mid/lx_mid,
         lx_high = ifelse(is.na(lag(cumprod(1-qx_high),1)*100000), 100000, lag(cumprod(1-qx_high),1)*100000),
         dx_high = qx_high*lx_high,
         Lx_high = ifelse(Age == 85, (lx_high/mx_high), ax * lx_high + (width-ax) * lead(lx_high,1)),
         Tx_high = rev(cumsum(rev(Lx_high))),
         ex_high = Tx_high/lx_high,
         DIF_LOW = ex_low - ex_base,
         DIF_MID = ex_mid - ex_base,
         DIF_HIGH = ex_high - ex_base)

lt_lancet_2022 <- ccbaselinedeaths_2022 %>%
  filter(Age == 0) %>%
  mutate(Base_e0 = ex_base,
         LOW_e0 = ex_low,
         MID_e0 = ex_mid,
         HIGH_e0 = ex_high) %>%
  dplyr::select(CNTRY, Base_e0, LOW_e0, MID_e0, HIGH_e0, DIF_LOW, DIF_MID, DIF_HIGH) %>%
  ungroup() %>%
  arrange(DIF_MID) %>%
  mutate(RANK = row_number(),
         dif_mid = round(DIF_MID, 2)) %>%
  left_join(., countrycodes) %>%
  mutate(COUNTRY = case_when(
    CNTRY == "FRATNP" ~ "France",
    CNTRY == "DEUTNP" ~ "Germany",
    CNTRY == "GBR" ~ "Great Britain",
    TRUE ~ as.character(COUNTRY)),
    ISO3 = case_when(
      CNTRY == "FRATNP" ~ "FRA",
      CNTRY == "DEUTNP" ~ "DEU",
      CNTRY == "GBR_NP" ~ "GBR",
      TRUE ~ as.character(ISO3)),
    Test = ISO3)