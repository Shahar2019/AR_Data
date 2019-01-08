
red_sea_species<- data.frame(year = c(1975,1985,1995,2005,2018), 
                             percent_of_species = c(7.4,16.7,18.9,22.6,43.4),
                             species = c(54, 36, 37, 30, 53),
                             study = c("Diamant 1975", "Spanier 1985", "Spanier 1995", "Edelist 2005", "Malamud 2018"))

red_sea_species%>%
ggplot()+
aes(x=year,y=percent_of_species)+
  geom_point(color="blue")+
  theme(axis.text=element_text(size=14,face="bold"),
        axis.title=element_text(size=14,face="bold"))+
geom_smooth(method='lm',size=1.5)+
  geom_text(aes(x=year, y=species+2), label=red_sea_species$study, color="red", size=4, angle=0 )+
      scale_y_continuous(sec.axis = sec_axis(~.*1.2,name = "% Of Indo-Pacific Origin"))+
  geom_point(aes(x=year, y=species), size=3, color="black")+
  labs(x=("Year"), 
       y=("# Species"))


ar_data %>% 
  ggplot()+
  geom_smooth(aes(x=date, y=temp, color="red", size=2))+
theme(axis.text=element_text(size=14,face="bold"),
      axis.title=element_text(size=14,face="bold"))+
labs(y=("Temperture [C]"))

     