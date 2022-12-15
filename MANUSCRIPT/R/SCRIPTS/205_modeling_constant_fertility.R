
dat_UNlifetables2022_cf <- read.xlsx("./MANUSCRIPT/R/DATA-RAW/WPP2022_MORT_F07_1_ABRIDGED_LIFE_TABLE_BOTH_SEXES.xlsx", 
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

dat_UNlifetables2022_cf[is.na(dat_UNlifetables2022_cf)] <- 1

# The UN pop data is in 5-year intervals, but has a separate file that contains 0-1 and 1-4 age groups.
dat_UN0004_2022_cf <- read.xlsx("./MANUSCRIPT/R/DATA-RAW/WPP2022_POP_F03_1_POPULATION_SELECT_AGE_GROUPS_BOTH_SEXES.xlsx",
                             sheet = 5,
                             startRow = 17) %>%
  filter(Year == "2080") %>%
  rename(Country.code=`Location.code`) %>% 
  left_join(., countrycodes) %>%
  dplyr::select(`0` = `0-1`,
                `1` = `0-4`,
                COUNTRY, CNTRY, ISO3) %>%
  gather(Age, Pop, `0`:`1`) %>%
  mutate(Age = as.numeric(Age), Pop = as.numeric(Pop))

dat_UN05plus_2022_cf <- read.xlsx("./MANUSCRIPT/R/DATA-RAW/WPP2022_POP_F02_1_POPULATION_5-YEAR_AGE_GROUPS_BOTH_SEXES.xlsx",
                               sheet = 5,
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
dat_UN_2022_cf <- rbind(dat_UN0004_2022_cf, dat_UN05plus_2022_cf) %>%
  mutate(Pop = Pop * 1000,
         COUNTRY = if_else(COUNTRY == "Czechia", "Czech Republic", paste(COUNTRY)))

dat_gbd <- read_csv("./MANUSCRIPT/R/DATA-PROCESSED/gbd_data.csv") %>%
  mutate(ISO3 = if_else(is.na(ISO3), "CZE", ISO3))

a_cf <- left_join(lancetdata, dat_UN_2022_cf) %>%
  dplyr::select(Age, Pop, CNTRY, MID, LOW, HIGH) %>%
  filter(!is.na(MID))
a2_cf <- left_join(a_cf, dat_gbd) %>%
  mutate(width = ifelse(Age == 0, 1, # setting the width of the interval
                        ifelse(Age == 1, 4,5))) 

ccbaselinedeaths_cf <- a2_cf %>%
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

lt_lancet_2022_cf <- ccbaselinedeaths_cf %>%
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