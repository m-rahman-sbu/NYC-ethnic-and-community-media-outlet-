install.packages("tidyverse")
install.packages("dplyr")
install.packages("corrplot")
install.packages("ggdendro")
install.packages("SciViews")
install.packages("klaR")
install.packages("stringr")
install.packages("tidyr")
install.packages("RColorBrewer")
install.packages("grDevices")


library(ggplot2)
library(tidyverse)
library(ggdendro)
library(cluster)
library(corrplot)
library(SciViews)
library(MASS)
library(klaR)
library(stringr)
library(tidyr)
library(RColorBrewer)
library(grDevices)

ecm<- read.csv("C:/Users/moharahman/Downloads/Ethnic_and_Community_Media_Print__Digital__and_Broadcast_Outlet_Directory.csv")
View(ecm)
ecm <- ecm[2:7]

head(ecm)
str(ecm)
summary(ecm)
attach(ecm)

ecm$Outlet.Format <- as.factor(ecm$Outlet.Format)
ecm$Type.of.Media <-  as.factor(ecm$Type.of.Media)
ecm$Languages.Served <- as.factor(ecm$Languages.Served)
ecm$Geographic.Focus <- as.factor(ecm$Geographic.Focus)
ecm$Communities.Served <- as.factor(ecm$Communities.Served)


ecm_1 <- mutate(ecm, Type.of.Media = recode(.x=Type.of.Media, "Both"="Print and Digital", ))
ecm_2 <- mutate(ecm_1, Communities.Served = recode(.x=Communities.Served, "-"=""))
ecm_3 <- mutate(ecm_2, Communities.Served = recode(.x=Communities.Served, "South Asia (various)"="South Asia"))
ecm_4 <- mutate(ecm_3, Languages.Served = recode(.x=Languages.Served, "Thai"="Bengali"))
ecm_n <- mutate(ecm_4, Communities.Served = recode(.x=Communities.Served, " South Asia (various)"="South Asia"))

ecm_n[ecm_n==""] <- NA

view(unique(ecm_n$Outlet.Format)) #viewing the unique outlet format 
view(unique(ecm_n$Type.of.Media))
view(na.omit(unique(ecm_n$Languages.Served)))
view(na.omit(unique(ecm_n$Geographic.Focus)))
view(na.omit(unique(ecm_n$Languages.Served)))
view(na.omit(unique(ecm_n$Communities.Served)))

#above ecm_n dataframe is now have no missing but NA values. 

Language.Served <- str_split_fixed(ecm_n$Languages.Served, ",", 16)
ecm_n$Language.Served <- Language.Served

Geographic.Focus <- str_split_fixed(ecm_n$Geographic.Focus, ",", 2)
ecm_n$Geographic.Focus <- Geographic.Focus

Community.Served <- str_split_fixed(ecm_n$Communities.Served, ",", 2)
ecm_n$Community.Served <- Community.Served

ecm_plus <- subset(ecm_n, select = -c(Languages.Served, Communities.Served))

summary(ecm_plus$Outlet.Format) #number of different outlet format
summary(ecm_plus$Type.of.Media) #number of different type of media 

names(ecm_plus) [4] <- "Geo.Served"
names(ecm_plus) [5] <- "Lan.Served"
names(ecm_plus) [6] <- "Com.Served"

length(unlist(ecm_plus[4])) # total number of cells of geography served 
length(unlist(ecm_plus[5])) # total number of cells of languages served
length(unlist(ecm_plus[6])) # total number of cells of communities served 

#now, let's create a new dataframe with all the geography, languages, and communities served 

Geo.Served <- trimws(as.factor(ecm_plus$Geo.Served)) #creating the variable for new dataset
Lan.Served <- trimws(as.factor(ecm_plus$Lan.Served)) # ''
Com.Served <- trimws(as.factor(ecm_plus$Com.Served)) # ''

max_length <- max(c(length(Geo.Served),length(Lan.Served), length(Com.Served)))
service <- data.frame(Geo.Served =c(Geo.Served, rep("", max_length - length(Geo.Served))), 
                      Lan.Served =c(Lan.Served, rep("", max_length - length(Lan.Served))),
                      Com.Served =c(Com.Served, rep("", max_length - length(Com.Served))))

service[service==""] <- NA

view(na.omit(unique(service$Geo.Served))) #checking the unique geography served


view(na.omit(unique(service$Lan.Served))) #checking the unique language served

service_1 <- mutate(service, Lan.Served = recode(.x=Lan.Served, "Bangla"="Bengali"))
service_2 <- mutate(service_1, Lan.Served = recode(.x=Lan.Served, "Bangladeshi"="Bengali"))
view(na.omit(unique(service_2$Lan.Served)))


view(na.omit(unique(service_2$Com.Served))) #checking the unique communities served

service_new <- mutate(service_2, Com.Served = recode(.x=Com.Served, "African-American"="African American"))
view(na.omit(unique(service_new$Com.Served)))


str(service_new)
attach(service_new)

service_new$Geo.Served <- as.factor(service_new$Geo.Served)
service_new$Lan.Served <- as.factor(service_new$Lan.Served)
service_new$Com.Served <- as.factor(service_new$Com.Served)

view(service_new)

#Let's create datasets for each variables where we don't remove the entire row just because of one NA values in one of the variables. 
#Therefore, I will be using filter() function to keep all the complete values in cells of a target variables 

#df with all the available values in Geo.Served variable 
service_new_geo <- filter(service_new, Geo.Served!='NA')

#df with all the available values in Lan.Served variable
service_new_lan <- filter(service_new, Lan.Served!='NA')

#df with all the available values in Com.Served variable
service_new_com <- filter(service_new, Com.Served!='NA')

 
## Viz using the original dataset

#1. ECM Outlet format & Type of Media
ggplot(data = ecm_plus, aes(x=fct_rev(fct_infreq(Outlet.Format)), fill=Type.of.Media))+
  geom_bar()+
  labs(x= "Outlet Format",
       y= "Number of Outlets",
       title = "ECM Outlet format & Type of Media (2022)",
       caption = "By Mohammad Rahman - linkedin.com/in/mohammadarahman/")+
  geom_text(aes(label = ..count..), 
            stat = "count", vjust = .1, size = 3, 
            position = position_stack(.5), colour = "black")+
  guides(fill=guide_legend(title="Type of Media"))


#2. ECM Type of Media 
ggplot(data = ecm_plus, aes(x=fct_rev(fct_infreq(Type.of.Media)), fill=Type.of.Media))+
  geom_bar()+
  labs(x= "Type of Media",
       y= "Number of Outlets",
       title = "ECM Type of Media (2022)",
       caption = "By Mohammad Rahman - linkedin.com/in/mohammadarahman/")+
  guides(fill=guide_legend(title="Outlet Format"))+
  geom_text(aes(label = ..count..), stat = "count", vjust = 1, size = 4, colour = "black")+
  theme_light()+
  theme(legend.position="none")
  

## Viz using the newly created dataframe - service

#3. Geography Served by Ethnic and Community Media Outlet
ggplot(data = service_new_geo, aes(x= fct_rev(fct_infreq(Geo.Served)), fill= Geo.Served))+
  geom_bar()+
  labs(x= "Geography Served (by the Media Outlets)",
       y= "Number of Outlets",
       title = "Geography Served by Ethnic and Community Media Outlet of NYC (2022)",
       caption = "By Mohammad Rahman - linkedin.com/in/mohammadarahman/")+
  scale_fill_discrete(name = "Geography")+
  geom_text(aes(label = ..count..), 
            stat = "count", vjust = 1, size = 4, colour = "black")+
  theme_bw()+
  theme(legend.position="none")


#4. Languages Served by Ethnic and Community Media Outlet
ggplot(data = service_new_lan, aes(x=fct_rev(fct_infreq(Lan.Served)), fill= Lan.Served))+
  geom_bar()+
  labs(x= "Languages Served (by the Media Outlets)",
       y= "Number of Outlets",
       title = "Languages Served by Ethnic and Community Media Outlet of NYC (2022)",
       caption = "By Mohammad Rahman - linkedin.com/in/mohammadarahman/")+
  scale_fill_discrete(name = "Languages")+
  theme(axis.text.x = element_text(angle = 40))+
  geom_text(aes(label = ..count..), stat = "count", vjust = 1, size = 3, colour = "black")+
  theme(legend.position="none")

#5. Communities Served by Ethnic and Community Media Outlet
ggplot(data = service_new_com, aes(x=fct_rev(fct_infreq(Com.Served)), fill= Com.Served))+
  geom_bar()+
  labs(x= "Communities Served (by the Media Outlets)",
       y= "Number of Outlets",
       title = "Communities Served by Ethnic and Community Media Outlet of NYC (2022)",
       caption = "By Mohammad Rahman - linkedin.com/in/mohammadarahman/")+
  scale_fill_discrete(name = "Communities")+
  theme(axis.text.x = element_text(angle = 90))+
  geom_text(aes(label = ..count..), stat = "count", vjust = 1, size = 3, colour = "black")+
  theme(legend.position="none")





