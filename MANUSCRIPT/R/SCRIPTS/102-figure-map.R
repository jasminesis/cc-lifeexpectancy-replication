###------Map -----
## @knitr figure-map

load(file= "./MANUSCRIPT/R/DATA-RAW/Europe.rda")

# Europe2 <- append_data(Europe, lt_lancet, key.shp = "iso_a3", key.data = "ISO3", ignore.na = TRUE)

Europe2 <- sp::merge(Europe, lt_lancet, by.x = "iso_a3" , by.y = "ISO3")
# bbeur <- bb(Europe2,  xlim=c(.42, .64), ylim=c(.7, 0.93), relative = TRUE)

tm_shape(Europe2) +
  tm_fill("DIF_MID", title = expression(Delta* e[0 ~italic(MID)]), palette = "YlOrBr", style = "jenks") +
  tm_borders(alpha = 0.5) +  
  tm_text("Test" , size="AREA", root=5, legend.size.show= F) +
  tm_legend(legend.frame=T) +
  NULL