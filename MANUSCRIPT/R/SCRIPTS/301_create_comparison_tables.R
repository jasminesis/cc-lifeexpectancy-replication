lt_comp <- lt_lancet_2022 %>% dplyr::select(COUNTRY, DIF_LOW, DIF_MID, DIF_HIGH)
lt_2017 <- lt_lancet %>% dplyr::select(DIF_LOW, DIF_MID, DIF_HIGH)

lt_combined <-
  data.frame(
    lt_comp[, 1:2], lt_2017[, 1], 
    lt_comp[, 3], lt_2017[, 2], 
    lt_comp[, 4], lt_2017[, 3])
colnames(lt_combined) <- c("Country", "Updated data", "Original", "Updated data", "Original", "Updated data", "Original")
lt_combined[1:10, ]

table_lt_combined <- kbl(lt_combined[1:10, ]) %>% 
  add_header_above(c(" "=1,"LOW Scenario" = 2, "MID Scenario"=2, "HIGH Scenario" = 2)) %>% 
  kable_styling(bootstrap_options = "striped", full_width=F)  %>% 
  kable_paper() 
table_lt_combined
table_lt_combined %>% 
  save_kable(file='../updated_vs_original.png')
