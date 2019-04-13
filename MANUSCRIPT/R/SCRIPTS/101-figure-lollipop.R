###------Figure Dots-----
## @knitr figure-lollipop

ggplot(lt_lancet, aes(x=RANK, y=DIF_MID, ymin = DIF_LOW, ymax = DIF_HIGH, label=dif_mid)) +
  geom_linerange(stat="identity") +
  geom_point(stat="identity", fill="black", size=8) +
  geom_hline(yintercept=0) +
  geom_text(color="white", size=2) +
  #scale_x_discrete(limits = rev(levels(the_factor)))
  scale_x_reverse(breaks = lt_lancet$RANK, labels=lt_lancet$COUNTRY) +
  coord_flip() +
  theme_bw() +
  #ylim(-2, 2) +
  theme(panel.grid.minor = element_blank()) +
  
  labs(y=expression(Delta* e[0]),
       x= "Country")