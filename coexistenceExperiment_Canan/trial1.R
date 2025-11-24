library(tidyverse)
library(ggsci)

setwd("~/Library/CloudStorage/OneDrive-IndianaUniversity/DormanyMutualism/coexistenceExperiment")

count  <- read.table("count_data.csv", sep = ",", dec = ".", header = T)
design <- read.table("design.csv", sep = ",", dec = ".", header = T)

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
count_cleaned <- count %>%
  mutate(dilutionFactor = dilution*1000/volume_ul) %>%
  rowwise() %>% 
  mutate(Lys = mean(c(as.numeric(lys_r1), as.numeric(lys_r2), as.numeric(lys_r3)), na.rm =T)) %>%
  mutate(His = mean(c(as.numeric(his_r1), as.numeric(his_r2), as.numeric(his_r3)), na.rm =T)) %>%
  mutate(across(c(Lys, His), ~ replace(., is.nan(.), 0))) %>%
  select(transfer,dilutionFactor, well, Lys, His) %>% 
  pivot_longer(cols = Lys:His, names_to = 'strain', values_to = "count") %>%
  filter(strain > 1.5) %>%
  mutate(count_mL = count*dilutionFactor) %>%
  group_by(transfer, well, strain) %>% 
  summarise(cfu_mL = mean(count_mL, na.rm = T)) %>% 
  left_join(design, by = "well") %>% 
  filter(culture == "coculture")

# plot data 
ggplot(count_cleaned, aes(x = transfer, y = log10(cfu_mL), color = strain))+
  geom_point()+
  geom_smooth(alpha = 0.1)+
  facet_wrap(~combination)+
  theme_bw()+
  mytheme+
  scale_color_npg()


#1/CV
CV <- count_cleaned %>% 
  group_by(combination,strain) %>%
  summarise(mean = mean(cfu_mL, na.rm = T), sd = sd(cfu_mL, na.rm = T), 
            InvCV = 1/(sd/mean))

# plot data 
ggplot(CV, aes(y = combination, x = InvCV))+
  geom_point()+
  mytheme+
  facet_grid(~strain)+
  scale_color_npg()
