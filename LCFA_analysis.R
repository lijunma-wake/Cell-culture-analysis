library(readr)
library(tidyverse)

#Analyze OA
OA_reads1 <- read.csv("OA_final_analysis.csv")
OA_reads <- OA_reads1 %>%transform(OA=factor(OA, levels= c("no", "low", "high"))) %>% 
  unite(GT_OA, GT, OA, sep = "_", remove = FALSE) %>% unite(OA_Dox, OA, LCFA, Dox, sep = "_", remove = FALSE)



#Relevel
OA_reads_re <- transform(OA_reads, OA_Dox = factor(OA_Dox, levels = c("no_OA_Dox(-)", "no_OA_Dox(+)",
                                                                   "low_OA_Dox(-)", "low_OA_Dox(+)",
                                                                   "high_OA_Dox(-)", "high_OA_Dox(+)")))

p <- ggplot(data=OA_reads_re, aes(OA_Dox, Adj_survival, color=OA))+
  geom_boxplot(width=0.8,position = "dodge", alpha=0.01)+
  geom_jitter()+ylim(0.6,1.1)+
  labs(title = "Relative cell viability of HEK293 cells without and with Dox induction",
       fill= "OA",
       x="HEK293 cells of different APOL1 genotypes treated with no, low, and high concentrations of OA",
       y="Relative Cell Viability")+
  facet_wrap(~ GT)


ggsave(plot = p, width = 14, height = 6, dpi = 600, filename = "HEK_res_Dox.pdf")

######################################################################

#Analyze PA

PA_reads1 <- read.csv("PA_final_analysis.csv")
PA_reads <- PA_reads1 %>%transform(PA=factor(PA, levels= c("no", "low", "high"))) %>% 
  unite(GT_PA, GT, PA, sep = "_", remove = FALSE) %>% unite(PA_Dox, PA, LCFA, Dox, sep = "_", remove = FALSE)



#Relevel
PA_reads_re <- transform(PA_reads, PA_Dox = factor(PA_Dox, levels = c("no_PA_Dox(-)", "no_PA_Dox(+)",
                                                                      "low_PA_Dox(-)", "low_PA_Dox(+)",
                                                                      "high_PA_Dox(-)", "high_PA_Dox(+)")))

q <- ggplot(data=PA_reads_re, aes(PA_Dox, Adj_survival, color=PA))+
  geom_boxplot(width=0.8,position = "dodge", alpha=0.01)+
  geom_jitter()+ylim(0.6,1.1)+
  labs(title = "Relative cell viability of HEK293 Tet-on APOL1 cells without and with Dox induction by Palmitate treatment",
       fill= "PA",
       x="HEK293 cells of different APOL1 genotypes treated with no, low, and high concentrations of PA",
       y="Relative Cell Viability")+
  facet_wrap(~ GT)

q


ggsave(plot = q, width = 14, height = 6, dpi = 600, filename = "HEK_res_PA.pdf")
