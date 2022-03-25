library(tidyverse)
library(here)
library(colorspace)

usPW <- read.csv(here("projects/EJ/Article/data/national/US_Pairwise_Wilcox.csv"))




# Create Wilcox Plots
usPW <- usPW%>%
  filter(Y_Var =="Releases_km" | Y_Var == "Tanks_km")%>%
  filter(X_Var == "N_P_MINORPCT_BIN" | X_Var == "N_P_LWINCPCT_BIN")

# Reorder the plots
order <- usPW%>%
  mutate(Order = ifelse(group == "Minority vs. Releases",2,
                        ifelse(group == "Minority vs. Tanks",1,
                               ifelse(group == "Low Income vs. Releases",4,3))))
colnames(order)[11] <- 'Effect'
p <- ggplot(order)+
  geom_tile(aes(x = group1, y = group2, fill = Effect))+
  geom_text(aes(x = group1, y = group2, label = p.adj.signif),color = "black", size = 4)+
  scale_fill_continuous_divergingx(palette = "RdYlGn",rev = TRUE, mid = 0.1, l3 = 10, p3 = 2, p4 = .1)+
  labs(title = "Pairwise Wilcox Effect Size Between Bin Pairs",
       y = "Percentile Bin", x = "Percentile Bin")+
  facet_wrap(~fct_reorder(group,Order), ncol = 2)+
  theme(strip.text.x = element_text(size=10),
        strip.background = element_rect(colour="black", fill="#bdbdbd"),
        legend.position="bottom",
        axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))+
  guides(fill= guide_colorbar(barwidth=15))

p
ggsave(filename = here("projects/EJ/Article/figures/national/US_Pairwise_Wilcox_Tanks_Releases.png"), plot = p, width = 7, height = 5, units = "in",dpi = 600)
