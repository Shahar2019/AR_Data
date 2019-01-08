library(tidyverse)
library(vegan)
library(readr)
library(lubridate)
library(gtable)
setwd("~/phd committee/")

dir()
ar_data <- read.csv("~/phd committee/AR_data_fish_101218.csv")
tp_lenght <- read.csv("~/phd committee/tp_lenght.csv")
species_familys <- read.csv("~/phd committee/species_familys.csv")
fish_m_l_tp <- read.csv("~/phd committee/fish_m_l_tp.csv")

ar_data <- merge(ar_data, species_familys, by="species")


ar_data <- ar_data %>% 
  mutate(size = as.numeric(size),
         reef = tolower(reef),
         season = tolower(season),
         biomass = abundance * a * as.numeric(size)^b,
         #biomass = abundance, #* as.numeric(size),
         log_biomass = log(biomass),
         season = factor(season, levels = c("winter","spring","summer","fall")),
         date = as.Date(date,format = "%d/%m/%Y"),
         Distribution = factor(Distribution,levels = c("Indo-Pacific","Eastern Atlantic - Mediterranean")))




species<-unique(ar_data$species)

ar_data %>% 
  group_by(Distribution) %>% 
  summarize(total_abundance = sum(abundance))

ar_data %>% 
  group_by(Distribution) %>% 
  summarize(total_species = length(unique(species)))


############################################
########  temp vs log_abundance / reef #####
############################################

ar_data %>%  
 ggplot()+
  aes(x=temp ,y=log(abundance),group = Distribution,color = Distribution)+
  theme(axis.text=element_text(size=12,face="bold"),
        axis.title=element_text(size=14,face="bold"),
        legend.title = element_text(size =10,face = "bold"),
        legend.text = element_text(size =10,face = "bold"),
        legend.position = "bottom")+
  geom_smooth()+
labs(subtitle=paste("(n =", length(unique(ar_data$date)), ")"),
x=("Temperture [C]"), 
y=("Log(Abundance)"))+
facet_grid(.~ reef)


############################################
########  date vs log_abundance / reef #####
############################################

ar_data %>% 
 ggplot()+
  aes(x=date,y=log(abundance), group = Distribution, color = Distribution)+
  theme(axis.text.x=element_text(size=12,face="bold",angle=90),
        axis.title=element_text(size=14,face="bold"),
        legend.title = element_text(size =10,face = "bold"),
        legend.text = element_text(size =10,face = "bold"),
        legend.position = "bottom")+
  geom_smooth()+
  labs(subtitle=paste("(n =", length(unique(ar_data$date)), ")"),
       x=("Date"), 
       y=("Log_Abundance"))+
  facet_grid(. ~ reef)
#facet_wrap( ~ reef, ncol=5)


############################################
########  temp vs log_abundance  - All #####
############################################

temp<-ar_data %>% 
  ggplot()+
  aes(x=temp,y=log(abundance), group = Distribution, color = Distribution)+
  #aes(x=date,y=log(abundance), group = Distribution,color = Distribution)+
  theme(axis.text=element_text(size=12,face="bold"),
        axis.title=element_text(size=14,face="bold"),
        legend.title = element_text(size =10,face = "bold"),
        legend.text = element_text(size =10,face = "bold"),
        legend.position = "bottom")+
  geom_smooth()+
  labs(subtitle=paste("(n =", length(unique(ar_data$date)), ")"),
       x=("Temperture [C]"), 
       y=("Log_Abundance"))
+facet_grid(. ~ reef)


############################################
########  date vs log_abundance  - All #####
############################################

ar_data %>% 
  ggplot()+
  aes(x=date,y=log(abundance), group = Distribution,color = Distribution)+
  theme(axis.text=element_text(size=12,face="bold"),
        axis.title=element_text(size=14,face="bold"),
        legend.title = element_text(size =10,face = "bold"),
        legend.text = element_text(size =10,face = "bold"),
        legend.position = "bottom")+
  geom_smooth()+
  geom_line(aes(x=(ar_data$date), y=ar_data$temp/18), 
                        label=ar_data$temp, color="black",size=1)+
  geom_smooth()+
  scale_y_continuous(sec.axis = sec_axis(~.*18,name = "Temperture [C]"))+
                                        
    labs(subtitle=paste("(n =", length(unique(ar_data$date)), ")"),
       x=("Date"))



grid.arrange(temp, date, nrow = 1)


?grid.arrange
#facet_grid(. ~ reef)



temp
date

###############################################
ar_data %>% 
  ggplot()+
  aes(x=date,y=log(abundance), group = Distribution, color = Distribution)+
  theme(axis.text.x=element_text(size=12,face="bold",angle=90),
        axis.title=element_text(size=14,face="bold"),
        legend.title = element_text(size =10,face = "bold"),
        legend.text = element_text(size =10,face = "bold"),
        legend.position = "bottom")+
  geom_smooth()+
  labs(subtitle=paste("(n =", length(unique(ar_data$date)), ")"),
       x=("Date"), 
       y=("Log_Abundance"))+
  #facet_grid(. ~ species)
facet_wrap( ~ reef_fidelity)

#######percent_of_species by year  #############
red_sea_species<- data.frame(year = c(1985,1995,2005,2018), percent_of_species = c(16.7,18.9,22.6,43.4))
red_sea_species%>%
  ggplot()+
  aes(x=year,y=percent_of_species)+
        geom_point()
                             
                                                 
grid.arrange(
  temp,
  date,
  nrow = 1,
  bottom = textGrob(
    "this footnote is right-justified",
    gp = gpar(fontface = 3, fontsize = 9),
    hjust = 1,
    x = 1
  )
)                                                 

gtable