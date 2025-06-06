library(tidyverse)
library(ggsci)

setwd("~/Library/CloudStorage/OneDrive-IndianaUniversity/DormanyMutualism/coexistenceExperiment")

count_exh  <- read.table("count_exhaustion.csv", sep = ",", dec = ".", header = T)
design_exh <- read.table("design_exhaustion.csv", sep = ",", dec = ".", header = T)


# plot theme
mytheme<- theme_bw()+
  theme(axis.text = element_text(size=14),
        axis.title = element_text(size=14,face="bold"),
        legend.text = element_text(size=12),
        legend.background = element_blank(),
        legend.title = element_text(size=12,face="bold"),
        plot.title = element_text(size=14, face="bold", hjust=0.5),
        strip.text = element_text(size=12, face="bold"),
        plot.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

# clean data 
count_cleaned <- count_exh %>%
  mutate(dilutionFactor = (10^dilution)*1000/volume_ul) %>%
  rowwise() %>% 
  mutate(Lys = mean(c(as.numeric(lys_r1), as.numeric(lys_r2), as.numeric(lys_r3), as.numeric(lys_r4)), na.rm =T)) %>%
  mutate(His = mean(c(as.numeric(his_r1), as.numeric(his_r2), as.numeric(his_r3), as.numeric(his_r4)), na.rm =T)) %>%
  mutate(across(c(Lys, His), ~ replace(., is.nan(.), 0))) %>%
  select(transfer,dilutionFactor, well, Lys, His) %>% 
  pivot_longer(cols = Lys:His, names_to = 'strain', values_to = "count") %>%
  filter(strain > 1.5) %>%
  mutate(count_mL = count*dilutionFactor) %>%
  group_by(transfer, well, strain) %>% 
  summarise(cfu_mL = mean(count_mL, na.rm = T)) %>% 
  left_join(design_exh, by = "well") %>% 
  filter(culture == "coculture")

# plot data 
ggplot(count_cleaned, aes(x = transfer, y = log10(cfu_mL), color = strain))+
  geom_point()+
  geom_smooth(alpha = 0.1)+
  facet_wrap(~combination)+
  theme_bw()+
  mytheme+
  scale_color_npg()
