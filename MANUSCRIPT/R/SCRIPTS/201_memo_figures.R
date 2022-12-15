require(tidyverse)

topfew <- lt_lancet[1:8,]
sorted_countries <- c("ESP", "ITA", "PRT", "FRA", "SVN", "HRV", "LUX", "CHE")

le_UNlifetables <- read.xlsx(
  "./MANUSCRIPT/R/DATA-RAW/WPP2017_MORT_F17_1_ABRIDGED_LIFE_TABLE_BOTH_SEXES.xlsx",
  sheet = 3,
  startRow = 17
) %>%
  filter(Period == "2080-2085") %>%
  dplyr::select(Period, Age = `Age.(x)`,
                ex = `Expectation.of.life.e(x)`,
                `Country.code`) %>%
  left_join(., countrycodes) %>%
  mutate(Age = as.numeric(Age),
         ex = as.numeric(ex)) %>%
  drop_na(.) %>%
  filter(Age == 0)
le_data <- left_join(topfew, le_UNlifetables)
df1 <- data.frame(country = le_data[, "CNTRY"],
             life_expectancy = le_data[, "MID_e0"],
             group = 'le')
colnames(df1) <- c("country", "le", "group")
df2 <- data.frame(country = le_data["CNTRY"],
             life_expectancy = le_data[, "DIF_MID"],
             group = "diff")
colnames(df2) <- c("country", "le", "group")
df <- rbind(df2, df1)
df$le <- abs(df$le)
df$country <- factor(df$country, levels = sorted_countries)
ggplot(df, aes(x = country, y = le, fill = group)) + geom_bar(stat = "identity")  + coord_flip() + labs(title = "Life expectancy reductions from climate change", x = "Country", y = "Life Expectancy") + theme_light() + theme(legend.title = element_blank(), legend.position = "bottom") + scale_fill_discrete(labels = c("Reduction due to climate change", "Life Expectancy"))

scatterdata <- left_join(topfew, lancetdata)
scatterdata$CNTRY <- factor(scatterdata$CNTRY, levels = sorted_countries)

ggplot(scatterdata) + 
  geom_point(aes(x = CNTRY, y = MID, size = MID), col = 'red') + 
  coord_flip() + theme_light() +
  scale_size_area() + labs(y = "Climate change related fatalities per year", x="Country", title="Yearly fatalities related to climate change by the mid 2080s") + guides(size=guide_legend(title="Number of fatalities"))  + theme(legend.position = "bottom")




