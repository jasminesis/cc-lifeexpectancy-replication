###------Figure GBD Death Distribution-----
## @knitr figure-GBD-death-distribution

GBD_data <- read.csv("data/GBD_data.csv") %>%
  #mutate(perdist = log(val)) %>%
  group_by(countrycode, Age) %>%
  summarise(meanval = mean(val),
            upperval = mean(upper),
            lowerval = mean(lower)) %>%
  group_by(countrycode) %>%
  mutate(countmeanval = sum(meanval),
         countupperval = sum(upperval),
         countlowerval = sum(lowerval),
         perdist = (meanval / countmeanval)) %>%
  rename(CNTRY = countrycode)

ggplot(data=GBD_data, aes(x=Age, y=perdist)) +
  geom_line(aes(color=CNTRY), alpha=1) +
  geom_smooth(span=0.3, se=F, col="black", lwd =2) +
  theme_bw() +
  theme(legend.position = c(0.8,0.25),
        legend.title=element_blank(),
        legend.text=element_text(size=4),
        legend.key.size = unit(1,"line")) +
  guides(col = guide_legend(ncol = 4)) +
  scale_y_log10()+
  labs(y="log(Mortality Percentage Distribution)",
       #title = "Mortality distribution of Heat-related mortality in 28 European countries",
       caption = "Source: Global Burden of Disease")
