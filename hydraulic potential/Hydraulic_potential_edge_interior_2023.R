

### SET DIRECTORY
getwd()
path = "D:/Yale/Potencial Hidrico"

### IMPORT LIBRARIES
install.packages('ggridges')
install.packages("lubridate")
library(lubridate)
library(tidyr)
library(ggplot2)
library(cowplot)
library(tidyverse)
library(readxl)
library(plyr)
library(dplyr)
library(ggridges)

### IMPORT DATA
hyd_pot_23 <- read.csv(file.path(path, "Potencial_Especies_Compartilhadas.csv"))

hyd_pot_23_2 <- hyd_pot_23 %>%
  mutate(dateR = dmy(as.character(Data)))

hyd_pot_test <- read.csv(file.path(path, "Potencial_Borda_Interior_Original_Compartilhadas.csv"))


hyd_pot_T <- hyd_pot_test %>%
  group_by(Especie_Nome_Comum, Placa) %>%
  spread(periodo, meanlwp_manha) %>%
  spread(periodo.1, meanlwp_tarde) %>%
  mutate(delta = tarde - Manha,
         delta_perc = delta/Manha) %>%
  mutate(dateR = dmy(as.character(Data)))
ungroup() 

#hyd_pot_T %>%
#  count(Especie_Nome_Comum) %>%
#  ggplot(aes(x = reorder(Especie_Nome_Comum, n),
#             y = n)) +
#  geom_col() + 
#  theme_minimal()+
#  coord_flip()

hyd_pot_T %>%
  filter(!is.na(delta_perc)) %>%
  ggplot(aes(x = reorder(Especie_Nome_Comum, delta_perc),
             fill = Especie_Nome_Comum,
             y = delta_perc)) +
  geom_col(position = "dodge") +
  #scale_color_continuous(values)
  theme_minimal()+
  coord_flip()

hyd_pot_T %>%
  filter(!is.na(Manha)) %>%
  ggplot(aes(x = reorder(Especie_Nome_Comum, Manha),
             fill = Manha,
             y = Manha)) +
  geom_col(position = "dodge") +
  #scale_color_continuous(values)
  theme_minimal()+
  coord_flip()+
  labs(y="Hydraulic Potential in the Morning (MPa)",
       x="Species")

hyd_pot_T %>%
  filter(!is.na(tarde)) %>%
  ggplot(aes(x = reorder(Especie_Nome_Comum, tarde),
             fill = tarde,
             y = tarde)) +
  geom_col(position = "dodge") +
  #scale_color_continuous(values)
  theme_minimal()+
  coord_flip()+
  labs(y="Hydraulic Potential in the Afternoon (MPa)",
       x="Species")

hyd_pot_T %>%
  ggplot(aes(x = reorder(Especie_Nome_Comum, delta),
             fill = delta,
             y = delta)) +
  geom_col(position = "dodge") +
  #scale_color_continuous(values)
  theme_minimal()+
  coord_flip()+
  labs(y="Difference in Hydraulic Potential (MPa)",
       x="Species")

hyd_pot_T %>%
  gather(var2, values2, Manha:tarde) %>%
  ggplot(aes(x = reorder(Especie_Nome_Comum, values2),
             fill = var2,
             y = values2)) +
  geom_col(position = "dodge") +
  #scale_color_continuous(values)
  theme_minimal()+
  coord_flip()+
  labs(y="Mean Hydraulic Potential (MPa)",
       x="Species")

hyd_pot_T %>%
  #filter(!is.na(delta_perc)) %>%
  ggplot(aes(x = Manha,
             color = Especie_Nome_Comum,
             y = tarde)) +
  geom_point() +#scale_color_continuous(values)
  geom_smooth(method = "lm", se = F) +
  geom_abline(intercept = 0, slope = 1, color = "red", linetype = 2) +
  theme_minimal()+
  ylim(0, 3) +
  xlim(0, 3) +
  coord_flip()

hyd_pot_T %>%
  select(Placa:tarde) %>%
  gather(var, value, Manha:tarde) %>%
  mutate(varN = as.numeric(as.factor(var))) %>%
  ggplot(aes(x = varN, 
             y = value, 
             color = as.factor(Placa))) +
  geom_point() + 
  geom_line() +
  theme_minimal() +
  facet_wrap(~Especie_Nome_Comum)

ggplot(hyd_pot_T, aes(x=Data, y=Manha, group=Especie_Nome_Comum, color=Especie_Nome_Comum))+
  geom_line(lwd=0.8)+
  theme_classic()


ggplot(hyd_pot_23_2, aes(x = dateR, y = mean_pot, group=Especie_Nome_Comum, color=Especie_Nome_Comum)) +
  geom_line(lwd=0.8) +
  facet_grid(fct_inorder(Periodo)~Local)+
  theme_minimal()+
  labs(y="Mean Hydraulic Potential (MPa)",
       x="Date")+
  theme(
    axis.title.x = element_text(size=8, color='black', face='bold',
                                margin = margin(t=30,r=0,b=0,l=0)),
    axis.title.y = element_text(size=8, color='black', face='bold',
                                margin = margin(t=0,r=30,b=0,l=0)),
    axis.text = element_text(size=8, color='black')
  )

hyd_pot_23_2 %>%
  ggplot(aes(x = dateR, y = mean_pot, group=Periodo, color=Periodo)) +
  stat_summary(fun.data = "mean_cl_boot", alpha = .5,
               linewidth = 2, size = 3, geom = "ribbon") +
  facet_grid(~Local)+
  theme_minimal()+
  labs(y="Mean Hydraulic Potential (MPa)",
       x="Date")


hyd_pot_23_2 %>%
  ggplot(aes(x = dateR, y = mean_pot, group=Local, color=Local)) +
  stat_summary(fun.data = "mean_cl_boot", alpha = .3,
               linewidth = 2, size = 3, geom = "ribbon") +
  facet_grid(~Periodo)+
  theme_minimal()+
  labs(y="Mean Hydraulic Potential (MPa)",
       x="Date")


