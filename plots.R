library(tidyverse)
library(vegan)
library(readr)
library(lubridate)

setwd("~/phd committee/")

dir()
ar_data <- read.csv("~/phd committee/AR_data_fish_101218.csv")
tp_lenght <- read.csv("~/phd committee/tp_lenght.csv")
species_familys <- read.csv("~/phd committee/species_familys.csv")
fish_m_l_tp <- read.csv("~/phd committee/fish_m_l_tp.csv")

ar_data <- merge(ar_data, species_familys, by="species")


species<-unique(ar_data$species)

ar_data %>% 
  group_by(Distribution) %>% 
  summarize(total_abundance = sum(abundance))

ar_data %>% 
  group_by(Distribution) %>% 
  summarize(total_species = length(unique(species)))



##############################################
ar_data %>%  
  ggplot()+
  aes(x=date,y=temp)+
  geom_point()+
geom_smooth(size=1.5)
######################################
########  TP_by_M by length  ########
#####################################
fish_m_l_tp %>% 
  ggplot()+
  aes(x=Length,y=TP_by_M, group = Origin,color = Origin)+
  theme(axis.text=element_text(size=10,face="bold"),
        axis.title=element_text(size=10,face="bold"),
        
        plot.title=element_text(size=14,face="italic"),
        
        legend.title = element_text(size =10,face = "bold"),
        legend.text = element_text(size =10,face = "bold"))+
geom_point()+
geom_smooth(method='lm',size=1.5)+
labs(title="Fistularia commersonii", 
     subtitle="(n=7)",
     x=("Length [mm]"), 
     y=("Trophic level"))+
scale_colour_manual(values = c("#00BFC4", "#F8766D"))
#############################################################################


########  TP_by_M by Weight ########
fish_m_l_tp %>% 
  ggplot()+
  aes(x=Weight,y=TP_by_M, group = Origin,color = Origin)+
  theme(axis.text=element_text(size=12,face="bold"),
        axis.title=element_text(size=14,face="bold"))+
  geom_point()+
geom_smooth(method='lm', size=1.5)+
labs(subtitle=paste("(n =7"),
     x=("Weight"), 
     y=("Trophic level"))+
  scale_colour_manual(values = c("#00BFC4", "#F8766D"))

########    temp vs log_biomass  #####

ar_data %>% 
  ggplot()+
  aes(x=temp,y=log(abundance),group = Distribution,color = Distribution)+
  theme(axis.text=element_text(size=12,face="bold"),
        axis.title=element_text(size=14,face="bold"))+
  geom_smooth(size=1.5)+
facet_grid(. ~ reef)


########################################

ar_data %>% 
  ggplot()+
  aes(x=temp,y=log(abundance),group = Distribution,color = Distribution)+
  theme(axis.text=element_text(size=12,face="bold"),
        axis.title=element_text(size=14,face="bold"))+
  geom_smooth(size=1.5)+
  facet_grid(. ~ reef_fidelity)

##########################################

ar_data %>% 
  ggplot()+
  aes(x=temp,y=abundance,group = Distribution,color = Distribution)+
  theme(axis.text=element_text(size=12,face="bold"),
        axis.title=element_text(size=14,face="bold"))+
  geom_smooth(size=1.5)+
facet_grid(. ~ reef_fidelity)
########################################
########  date vs log_abundance #####
########################################
ar_data %>% 
  ggplot()+
  aes(x=date,y=log(abundance),group = Distribution,color = Distribution)+
  theme(axis.text=element_text(size=12,face="bold"),
       axis.title=element_text(size=14,face="bold"),
       legend.title = element_text(size =10,face = "bold"),
       legend.text = element_text(size =10,face = "bold"))+
  geom_smooth(size=1.5) +
# ###geom_line(temp_line, aes(x=temp,y=date))+
labs(subtitle=paste("(n =", length(unique(ar_data$date)), ")"),
     x=("Date"), 
     y=("Log(Abundance)"))

########################################
########  temp vs log_abundance #####
########################################


  
  ar_data %>%  
ggplot()+
  aes(x=temp ,y=log(abundance),group = Distribution,color = Distribution)+
  theme(axis.text=element_text(size=12,face="bold"),
        axis.title=element_text(size=14,face="bold"),
        legend.title = element_text(size =10,face = "bold"),
        legend.text = element_text(size =10,face = "bold"))+
  geom_smooth(size=1.5)+
  
  labs(subtitle=paste("(n =", sum(ar_data$abundance), ")"),
       x=("Temperture [C]"), 
       y=("Log(Abundance)"))
  


  
 temp_line<- ar_data %>%  
  ggplot()+
aes(x=date,y=temp)+
  #geom_point()+
  geom_smooth(size=1, color="red")
#facet_grid(. ~ abundance)

date.time<-data.frame(ar_data$date, ar_data$temp)
str(date.time)
head(date.time)


date.time %>%  
  ggplot()+
  aes(x=date,y=temp)
#geom_point()+
geom_smooth(size=3)


########################################
########################################
########################################







#########  temp vs abundance  ###############
  

ar_data %>%  
ggplot()+
  aes(x=temp,y=abundance,group = Distribution,color = Distribution)+
  theme(axis.text=element_text(size=12,face="bold"),
        axis.title=element_text(size=14,face="bold"),
        legend.title = element_text(size =10,face = "bold"),
        legend.text = element_text(size =10,face = "bold"))+
  geom_smooth(size=1.5)+
  labs(subtitle=paste("(n =", sum(ar_data$abundance), ")"),
       x=("Temperture [C]"), 
       y=("Abundance"))
  



#####################################


#########  temp vs abundance  ###############


ar_data %>%  
  ggplot()+
  
  theme(axis.text=element_text(size=12,face="bold"),
        axis.title=element_text(size=14,face="bold"),
        legend.title = element_text(size =10,face = "bold"),
        legend.text = element_text(size =10,face = "bold"))+
  geom_smooth(size=1.5)+
  labs(subtitle=paste("(n =", sum(ar_data$abundance), ")"),
       x=("Temperture [C]"), 
       y=("Abundance"))




#tp_lenght %>% arrange(species)

#######  all tp_lenght by lenght #######

species_list <- tp_lenght$species #c("Mycteroperca rubra" )
tp_lenght %>% 
  filter(species %in% species_list) %>%
  ggplot()+
  aes(x=length ,y=TP,group = species,color = species)+
  geom_point()+
  geom_smooth(method='lm')

#########  Epinephelus marginatus by lenght ##########

tp_lenght %>% 
  filter(species=="Epinephelus marginatus") %>%
  ggplot()+
  aes(x=length,y=TP,group = species,color = species)+
  #geom_point(method='lm')+
  geom_smooth(method='lm')

###########  Siganus rivulatus by length ###########

tp_lenght %>% 
  filter(species=="Siganus rivulatus") %>%
  ggplot()+
  aes(x=length,y=TP,group = species,color = species)+
  #geom_point+
  geom_smooth(method='lm')

########## Siganus's by lenght ##########

species_list <-unique(tp_lenght$species)
#species_list
tp_lenght %>% 
  filter(species %in% species_list[c(5, 6)]) %>%
  ggplot()+
  aes(x=length ,y=TP,group = species,color = species)+
  #geom_point()+
  geom_smooth(method='lm')
#######################################################################
###########  Mycteroperca rubra + Epinephelus marginatus by depth #####
#######################################################################
species_list


Mycteroperca<-tp_lenght$species %in% species_list[1]
sum(Mycteroperca)
marginatus<-tp_lenght$species %in% species_list[4]
sum(marginatus)


species_list <-unique(tp_lenght$species)

tp_lenght %>% 
  filter(species %in% species_list[c(1, 4)]) %>% 
  ggplot()+
  aes(x=depth ,y=TP,group = species,color = species)+
   geom_smooth(method='lm', size=1.5)+
theme(axis.text=element_text(size=10,face="bold"),
      axis.title=element_text(size=10,face="bold"),
      plot.title=element_text(size=14,face="italic"),
      legend.title = element_text(size =10,face = "bold"),
      legend.text = element_text(size =10,face = "bold"))+
  labs(subtitle=paste("(Epinephelus marginatus n =",sum(marginatus),"/",
                      "Mycteroperca rubra n =",sum(Mycteroperca),")"),
       x=("Depth [M]"), 
       y=("Trophic level"), 
       caption="Data Source : Stefan Martinez" )+
facet_grid(. ~ species)+
theme(strip.text.x = element_text(size=10, face="bold.italic"),
      legend.position="none")
      
########################################################################################
########################################################################################




################ species by Family ###############

################ species by season ###############

################ Family by season ################

################ Distribution by season ################





##################################################



#ar_data <- merge(ar_data, species_familys, by="species")
#str(ar_data)
#names(ar_data)[10]<-"reef_name" 
#ab_data_med <- read.csv("~/phd committee/Length_weight_Maturity.csv", stringsAsFactors=FALSE)#load data

#ab_data_med <- ab_data_med[!is.na(ab_data_med$a),]
#ab_data_med <- ab_data_med[2:5]

###############################################
# # ar_data <- as.data.frame(read.csv("AR_data 02-12-18.csv"))#load data
#ar_data$temp[ar_data$date=="2017-11-06"] <- 23
#ar_data$temp[ar_data$date=="2018-04-10"] <- 20.5
#ar_data$temp[ar_data$date=="2018-05-03"] <- 22.5
#ar_data <- ar_data[,1:17]#subset to columns
#head(ar_data)
################################################

#ar_data$species <- as.character(ar_data$species)
#ar_data$species[substr(ar_data$species,1,8) =="Serranus" & substr(ar_data$species,1,10) !="Serranus s" ] <-"Serranus cabrilla"
#ar_data$species[ar_data$species =="sparus aurata"] <-"Sparus aurata"
#ar_data$fish[ar_data$species == "Sepia officinalis"] <- 0
#ar_data$species[substr(ar_data$species,1,7) =="Scomber"] <- "Scomberomorus commerson"
#ar_data$species[ar_data$species =="Epinephelus maginatus"] <- "Epinephelus marginatus"
#ar_data$species[substr(ar_data$species,13,23) =="marginatus"] <- "Epinephelus marginatus"
#ar_data$species[ar_data$species =="Sargocentron rubrum"] <- "Sargocentron rubrum"
#ar_data$species[ar_data$species =="Pempheris vanicolensis"] <- "Pempheris vanicolensis"
#ar_data$species[ar_data$species =="Parablennius zvonimiri "] <- "Parablennius zvonimiri"  
#ar_data$species[ar_data$species =="Parablennius incognitus "] <- "Parablennius incognitus"
#ar_data$species[ar_data$species =="Trachrus sp."] <- "Trachurus sp."

#unique(ab_data_med$species)

#ar_data <- ar_data[ar_data$fish==1,]#fish Only 
#str(ar_data)
#############
#unique(ar_data$species[!(ar_data$species %in% species.reef_fidelity$species)])  # Check Overlap
#unique(ar_data$species[!(ar_data$species %in% ab_data_med$species)])  # Check Overlap 
#ar_data <- merge(ar_data,ab_data_med,by="species")
#ar_data <- merge(ar_data,species.reef_fidelity,by="species")

#########################################################################################################

ar_data <- ar_data %>% 
  filter(fish == 1,species!="",Distribution!="") 
ar_data$Distribution <- factor(ar_data$Distribution) #drop the "" level of distribution

ar_data <- ar_data %>% 
  mutate(size = as.numeric(size),
         reef = tolower(reef),
         season = tolower(season),
         #biomass = abundance * a * as.numeric(size)^b,
         biomass = abundance, #* as.numeric(size),
         log_biomass = log(biomass),
         season = factor(season, levels = c("winter","spring","summer","fall")),
         date = as.Date(date,format = "%d/%m/%Y"),
         Distribution = factor(Distribution,levels = c("Indo-Pacific","Eastern Atlantic - Mediterranean")))

#temp_data <- ar_data %>% 
 # select(date,temp) %>%
  #group_by(date) %>% 
  #summarize(temp = mean(temp,na.rm=T)) 

#ar_data <- ar_data %>% 
# left_join(temp_data,by = "date") %>% 
#rename(old_temp = temp.x,
#      temp = temp.y)

ar_data <- ar_data %>%  #some lines from this date is not from True, but it's the only reef in the water
  filter(!(date == "2018-03-18" & "reef_name" != "True ")) 

ggplot(ar_data)+
  aes(x=season,y=log_biomass,fill = Distribution)+
  geom_boxplot()

anova(lm(log_biomass ~ season * Distribution, data = ar_data)) #two way anova



ar_data %>%  
  
group_by(reef,date,temp,Distribution) %>% 
#group_by(X,date,temp,Distribution) %>% 
    summarize("number of individuals" = sum(abundance)) %>%
  print(n=Inf)
  ggplot()+
  aes(x=date,y=log(`number of individuals`),color = Distribution)+
  geom_point()+
  geom_line(aes(x=date,y=temp))+ #need to put temp over entire x axis, irrelevent of reef
  geom_smooth()+
  #?????????????
    #facet_wrap(~X)

ar_data %>% 
  group_by(reef,date) %>% 
  summarize(count = n()) %>% 
  select(-count) %>%
  group_by(reef) %>% 
  summarize(count = n())

ar_data %>% 
  filter(reef=="AR - 3",season=="spring")


ar_data %>% 
  ggplot()+
  aes(x=date,y=log_biomass,group = Distribution,color = Distribution)+
  geom_point()+
  geom_smooth()


ar_data %>% 
  ggplot()+
  aes(x=temp,y=log_biomass,group = Distribution,color = Distribution)+
  geom_smooth()


ar_mat <- ar_data %>% 
  filter(species !="") %>% 
  group_by(date,X,species,season) %>% 
  summarize(abundance = sum(abundance)) %>% 
  spread(species,abundance,fill = 0) %>% 
  ungroup()

ar_sp_mat <- ar_mat %>% 
  select(-c(1:3))

min(rowSums(ar_sp_mat))

#raremax = 
#rarefaction_data <- ar_sp_mat[which(rowSums(ar_sp_mat) > raremax),]
#rarefaction_data1 <- ar_mat[which(rowSums(ar_sp_mat) > raremax),]

#rare_index_raremax <- rarefy(x=rarefaction_data,sample=raremax)

#rarfied_richness <- data.frame(season = rarefaction_data1$season,
#                               richness = rare_index_raremax)

#ggplot(rarfied_richness)+
#  aes(x=season,y=richness,fill = season)+
#  stat_summary(geom = "bar",fun.data = mean_se)+
#  stat_summary(geom = "errorbar", fun.data = mean_se, position = "dodge",width = 0.3)

renyi_profile=renyi(ar_sp_mat,  scales = c(0, 0.25, 0.5, 1, 2, 4, 8, 16, 32, 64, Inf), hill = T)
renyi_df=as.tibble(renyi_profile) #create a data frame from the function output

#Now let's look what we got:
renyi_df#the rows are the sites and the column are the diffrent Hill numbers

#next line is for writing the data to an Excel sheet. look at the ?write.csv help
write.csv(renyi_df,"renyi.csv")

renyi_df_transposed <- as.data.frame(t(renyi_df))
renyi_df_transposed$q <- as.numeric(rownames(renyi_df_transposed))
renyi_df_long <- gather(renyi_df_transposed,site,value,1:(ncol(renyi_df_transposed)-1))
#this is an advanced function which turns a wide data into a long one
renyi_df_long$season <- rep(ar_mat$season,each = 11)
#we add variable "location" to this data


ggplot(renyi_df_long)+
  aes(x=q,y=value,group = site,color=season)+
  geom_line()+
  scale_x_continuous(limits = c(0,3))

season_mat <- ar_mat %>% 
  filter(season!="winter") %>% 
  group_by(season) %>% 
  summarize_at(.vars = colnames(.[4:ncol(.)]),.funs = sum)


season_reef_mat <- ar_mat %>% 
  group_by(season,X) %>% 
  summarize_at(.vars = colnames(.[4:ncol(.)]),.funs = sum)


season_sp_mat <- season_mat %>% select(-1) %>% as.data.frame()
rownames(season_sp_mat) <- season_mat$season

renyi_profile_season<-renyi(season_sp_mat,scales = c(0, 0.25, 0.5, 1, 2, 4, 8, 16, 32, 64, Inf), hill = T)
renyi_data<-data.frame(t(renyi_profile_season))
rownames(renyi_data)=NULL
renyi_data$q<-c(0, 0.25, 0.5, 1, 2, 4, 8, 16, 32, 64, Inf)
q<-renyi_data$q

plot(q,renyi_data$winter,type = 'l',col = "black",xlim =c(0,3),ylim = c(0,60),ylab = "Diversity")  # I limited the x-axis because I wanted to focus on the low q numbers
plot(q,renyi_data$spring,type = 'l',col="blue",xlim =c(0,3),ylim = c(0,60),ylab = "Diversity")
lines(q,renyi_data$summer,col="red")
lines(q,renyi_data$fall,col="green")
legend("topright",legend = rownames(season_sp_mat),lty=c(1,1),col =c("blue","red","green"))

ar_data %>% 
  group_by(species) %>% 
  summarize(total = sum(abundance)) %>% 
  arrange(desc(total)) %>% 
  print(n=Inf)

total_abundance <- sum(ar_data$abundance)
total_abundance
total_species <- nlevels(unique(ar_data$species))         
total_species           

species_list <- c("Boops boops","Pomadasys stridens","Pagellus acarne",
 
                                   "Plotosus lineatus","Parupeneus forskalii")


ar_data %>% 
  filter(species %in% species_list,date > "2018-04-01") %>% 
  group_by(date,species) %>% 
  summarize(total_abundance =sum(abundance)) %>%
  ggplot()+
  aes(x=date,y=log(total_abundance),group=species,color=species)+
   geom_point()+
   geom_line()





##################################
# 1 # Date vs. Total Richness 
##################################


mat.sea.date <- function(ar_data,dstr = "Indo-Pacific"){
  
  
  ar_sea <- ar_data %>% 
    filter(species !="", Distribution == dstr) %>% 
    group_by(date,species) %>% 
    summarize(abundance = sum(abundance)) %>% 
    spread(species,abundance,fill = 0) %>% 
    ungroup()
  
  
  ar_sea <- as.data.frame(ar_sea)
  rownames(ar_sea)<- ar_sea$date
  ar_sea <- ar_sea[2:length(ar_sea)]
  ar_sea[ar_sea > 0] <- 1
  
  species_cumm <-specaccum(ar_sea, "collector")
  rich <- species_cumm$richness
  sea.date <- rownames(ar_sea)
  sea.total.richness <- data.frame(date = rownames(ar_sea),
                                   total.richness = species_cumm$richness)
  sea.total.richness$distribution <- dstr
  rownames(sea.total.richness)<-c()
  
  return(sea.total.richness)
}

red.rich.date <- mat.sea.date(ar_data,dstr = "Indo-Pacific")

med.rich.date <- mat.sea.date(ar_data,dstr = "Eastern Atlantic - Mediterranean")


species.total.richness <- rbind(red.rich.date,med.rich.date)
species.total.richness$date <- as.Date(species.total.richness$date)
#species.total.richness$date <- as.character(species.total.richness$date)
species.total.richness %>%
  ggplot()+
  aes(x = date, y = total.richness, group = distribution, color = distribution)+
  geom_point() +
  geom_line()+
  scale_colour_manual(values = c("#00BFC4", "#F8766D"))


  


########################################################################################################




#############################   Temperature vs. Total Richness  ###################################
mat.sea.temp <- function(ar_data,dstr = "Indo-Pacific"){
  
  
  ar_sea <- ar_data %>% 
    filter(species !="", Distribution == dstr) %>% 
    group_by(temp,species) %>% 
    summarize(abundance = sum(abundance)) %>% 
    spread(species,abundance,fill = 0) %>% 
    ungroup()
  
  
  ar_sea <- as.data.frame(ar_sea)
  rownames(ar_sea)<- ar_sea$temp
  ar_sea <- ar_sea[2:length(ar_sea)]
  ar_sea[ar_sea > 0] <- 1
  
  species_cumm <-specaccum(ar_sea, "collector")
  rich <- species_cumm$richness
  sea.temp <- rownames(ar_sea)
  sea.total.richness <- data.frame(temp = rownames(ar_sea),
                                   total.richness = species_cumm$richness)
  sea.total.richness$distribution <- dstr
  rownames(sea.total.richness)<-c()
  
  return(sea.total.richness)
}

red.rich.temp <- mat.sea.temp(ar_data,dstr = "Indo-Pacific")

med.rich.temp <- mat.sea.temp(ar_data,dstr = "Eastern Atlantic - Mediterranean")


species.total.richness.temp <- rbind(red.rich.temp,med.rich.temp)
#species.total.richness$temp <- as.Date(species.total.richness$date)
#species.total.richness$date <- as.character(species.total.richness$date)
species.total.richness.temp %>%
  ggplot()+
  aes(x = temp, y = total.richness, group = distribution, color = distribution)+
  theme(axis.text=element_text(size=12,face="bold"),
        axis.title=element_text(size=14,face="bold"))+
  geom_point() +
  geom_line(size=1)+
  scale_colour_manual(values = c("#00BFC4", "#F8766D"))


#############################
#############################   Temperature vs. Total Richness  ###################################
mat.sea.temp <- function(ar_data,dstr = "Indo-Pacific"){
  
  
  ar_sea <- ar_data %>% 
    filter(species !="", Distribution == dstr, date > "2018-04-01" ) %>% 
    group_by(temp,species) %>% 
    summarize(abundance = sum(abundance)) %>% 
    spread(species,abundance,fill = 0) %>% 
    ungroup()
  
  
  ar_sea <- as.data.frame(ar_sea)
  rownames(ar_sea)<- ar_sea$temp
  ar_sea <- ar_sea[2:length(ar_sea)]
  ar_sea[ar_sea > 0] <- 1
  
  species_cumm <-specaccum(ar_sea, "collector")
  rich <- species_cumm$richness
  sea.temp <- rownames(ar_sea)
  sea.total.richness <- data.frame(temp = rownames(ar_sea),
                                   total.richness = species_cumm$richness)
  sea.total.richness$distribution <- dstr
  rownames(sea.total.richness)<-c()
  
  return(sea.total.richness)
}

red.rich.temp <- mat.sea.temp(ar_data,dstr = "Indo-Pacific")

med.rich.temp <- mat.sea.temp(ar_data,dstr = "Eastern Atlantic - Mediterranean")


species.total.richness.temp <- rbind(red.rich.temp,med.rich.temp)
#species.total.richness$temp <- as.Date(species.total.richness$date)
#species.total.richness$date <- as.character(species.total.richness$date)
species.total.richness.temp %>%
  ggplot()+
  aes(x = temp, y = total.richness, group = distribution, color = distribution)+
  geom_point() +
  geom_line()+
  scale_colour_manual(values = c("#00BFC4", "#F8766D"))


########################################################################################################

######      species_list size_max by date      ########

ar_data %>% 
  filter(species %in% species_list,date > "2018-04-01") %>% 
  group_by(date,species) %>% 
  summarize(size.max =max(size)) %>%
  ggplot()+
  aes(x=date,y=size.max,group=species,color=species)+
  geom_point()+
  geom_line()



######      species_list size_max by temp      ########

ar_data %>% 
  filter(species %in% species_list, date > "2018-04-01") %>% 
  group_by(temp,species) %>% 
  summarize(size.max =max(size)) %>%
  ggplot()+
  aes(x=temp,y=size.max,group=species,color=species)+
  geom_point()+
  geom_line()


############## 
species_list

fish_species_list <-unique(ar_data$species)
fish_species_list

ar_data %>% 
group_by(species)%>%
summarize(total = sum(abundance)) %>%
arrange(desc(total)) %>%
print

######### species summarize ########
ar_data %>% 
  group_by(species) %>% 
  summarize(total = sum(abundance)) %>% 
  arrange(desc(total)) %>% 
  print(n=Inf)
##################################

reef_Resident<- ar_data %>% 
  group_by(species, reef_fidelity[1]) %>% 
  summarize(total = sum(abundance))%>%
  arrange(desc(total))  
  #print(n=Inf)
  
  reef_Resident
  

############  
  

mulidea_list <- c("Upeneus moluccensis", "Parupeneus forskalii", "Upeneus pori") 
mulidea_list


ar_data %>% 
  filter(species %in% mulidea_list, date > "2018-04-01") %>% 
  group_by(date,species) %>% 
  summarize(abundance=log(sum(abundance))) %>%
  ggplot()+
  aes(x=date,y=abundance,group=species,color=species)+
  geom_point()+
  geom_line()
########################################################
######################  Occasional  ###################
colnames(ar_data)

ar_data %>%
  filter(reef_fidelity=="Occasional") %>%
  group_by(species) %>%
summarize(abundance = log(sum(abundance))) %>%
  ggplot()+
  aes(x = species, y = abundance, group = species, color = species)+
  geom_point()
  #geom_line()

######################  Resident  ###################
  
ar_data %>%
  filter(reef_fidelity =="Resident ") %>%
  group_by(species) %>%
  summarize(abundance = log(sum(abundance))) %>%
  ggplot()+
  aes(x = species, y = abundance, group = species, color = species)+
  geom_point()+
  geom_line()

#########  Transient  ######################
ar_data %>%
  filter(reef_fidelity=="Transient ") %>%
  group_by(species) %>%
  summarize(abundance = log(sum(abundance))) %>%
  ggplot()+
  aes(x = species, y = abundance, group = species, color = species)+
  geom_point()+
  geom_line()
##############################################
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


ar_data %>% 
  ggplot()+
  aes(x=date,y=log_biomass,group = Distribution,color = Distribution)+
  geom_point()+
  geom_smooth()

########    temp vs log_biomass  #####
ar_data %>% 
  ggplot()+
  aes(x=temp,y=log_biomass,group = Distribution,color = Distribution)+
  theme(axis.text=element_text(size=12,face="bold"),
        axis.title=element_text(size=14,face="bold"),
        legend.title = element_text(size =10,face = "bold"),
        legend.text = element_text(size =10,face = "bold"))+
  geom_smooth(size=1.5)+
  labs(subtitle=paste("(n =", sum(ar_data$abundance), ")"),
       x=("Temperture [C]"), 
       y=("Log_biomass"))
########  date vs abundance   #####
ar_data %>% 
  ggplot()+
  aes(x=date,y=log_biomass, group =reef_fidelity, color = reef_fidelity)+
theme(axis.text=element_text(size=12,face="bold"),
      axis.title=element_text(size=14,face="bold"),
      legend.title = element_text(size =10,face = "bold"),
      legend.text = element_text(size =10,face = "bold"))+
  geom_smooth(size=1.5)+
  labs(subtitle=paste("(n =", sum(ar_data$abundance), ")"),
       x=("Date"), 
       y=("Log_biomass"))+
########################

#ar_data %>% 
  #ggplot()+
  aes(x=temp,y=log(abundance), group =Distribution, color = Distribution)+
  theme(axis.text=element_text(size=12,face="bold"),
        axis.title=element_text(size=14,face="bold"),
        legend.title = element_text(size =10,face = "bold"),
        legend.text = element_text(size =10,face = "bold"))+
  geom_smooth(size=1.5)+
  labs(subtitle=paste("(n =", sum(ar_data$abundance), ")"),
       x=("Temperture [C]"), 
       y=("Log_Abundance"))+
facet_grid(. ~reef )+
facet_wrap(log(abundance))
#
#
#
#
#


##################
ar_data %>% 
  ggplot()+
  aes(x=date,y=log(abundance), group =Distribution, color = Distribution)+
  theme(axis.text=element_text(size=12,face="bold"),
        axis.title=element_text(size=14,face="bold"),
        legend.title = element_text(size =10,face = "bold"),
        legend.text = element_text(size =10,face = "bold"))+
  geom_smooth(size=1.5)+
  labs(subtitle=paste("(n =", sum(ar_data$abundance), ")"),
       x=("Date"), 
       y=("Log_Abundance"))+
  facet_grid(. ~ reef
            )

ar_data %>% 
 ggplot()+
  aes(x=date,y=log(abundance), group = Distribution, color = Distribution)+
  #aes(x=date,y=log(abundance), group = Distribution,color = Distribution)+
  theme(axis.text=element_text(size=12,face="bold"),
        axis.title=element_text(size=14,face="bold"),
        legend.title = element_text(size =10,face = "bold"),
        legend.text = element_text(size =10,face = "bold"))+
  geom_smooth()+
  labs(subtitle=paste("(n =", sum(ar_data$abundance), ")"),
       x=("Date"), 
       y=("Log_Abundance"))+
  facet_grid(. ~ reef)

