library(tidyverse)
library(ggsci)

setwd("~/Library/CloudStorage/OneDrive-IndianaUniversity/DormanyMutualism/coexistenceExperiment")

count_exh  <- read.table("count_exhaustion.csv", sep = ",", dec = ".", header = T)
design_exh <- read.table("design_exhaustion.csv", sep = ",", dec = ".", header = T)


# plot theme
mytheme <- theme_bw()+
  #theme(legend.position = 'none')+
  theme(axis.ticks.length = unit(-.22, "cm"))+
  theme(legend.text = element_text(size=12))+
  theme(axis.text = element_text(size = 14), axis.title.y = element_text(size = 14), axis.title.x = element_text(size = 14))+
  #theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  theme(strip.text.x = element_text(size = 14))

# clean data 
count_cleaned <- count_exh %>%
  mutate(dilutionFactor = (10^dilution)*1000/volume_ul) %>%
  rowwise() %>% 
  mutate(Lys = mean(c(as.numeric(lys_r1), as.numeric(lys_r2), as.numeric(lys_r3), as.numeric(lys_r4)), na.rm =T)) %>%
  mutate(His = mean(c(as.numeric(his_r1), as.numeric(his_r2), as.numeric(his_r3), as.numeric(his_r4)), na.rm =T)) %>%
  mutate(across(c(Lys, His), ~ replace(., is.nan(.), NA))) %>%
  select(transfer,dilutionFactor, well, Lys, His) %>% 
  pivot_longer(cols = Lys:His, names_to = 'strain', values_to = "count") %>%
  mutate(count_mL = count*dilutionFactor) %>%
  group_by(transfer, well, strain) %>% 
  summarise(cfu_mL = mean(count_mL, na.rm = T)) %>% 
  left_join(design_exh, by = "well") %>% 
  filter(culture == "coculture") %>% 
  filter(combination == "Lys50His50" | combination == "Lys50HisSpoIIE50") 
  

# plot data 
ggplot(count_cleaned, aes(x = transfer, y = log10(cfu_mL), color = strain))+
  geom_point()+
  geom_smooth(alpha = 0.1)+
  theme_bw()+
  mytheme+
  scale_color_npg()

sem <- function(x) sd(x)/sqrt(length(x))
#1/CV
CV <- count_cleaned %>% 
 group_by(well, combination,strain) %>%
  summarise(mean = mean(cfu_mL, na.rm = T), sd = sd(cfu_mL, na.rm = T), 
            InvCV = 1/(sd/mean)) %>%
  group_by(combination, strain) %>%
  summarise(sem = sem(InvCV), meanCV = mean(InvCV))
 
CV$combination <- factor(CV$combination, levels = c("Lys50HisSpoIIE50", "Lys50His50"))
# plot data 
ggplot(CV, aes(x = combination, y = meanCV, color = strain, group = strain))+
  geom_errorbar(aes(ymin = meanCV - sem, ymax = meanCV + sem), width = 0.2, 
                position = position_dodge(width = 0.9))+
  geom_point(position = position_dodge(width = 0.9), size = 4, shape = 21)+
  geom_line(position = position_dodge(width = 0.9))+
  theme+
  scale_color_npg()+
  scale_x_discrete(labels = c("partner defficient", "partner sporulates"))+
  theme(legend.title = element_blank(), legend.position = c(.1, .8))+
  theme(axis.title.x=element_blank())+
  ylab("stability(1/CV)")

