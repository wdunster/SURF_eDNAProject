# clear work space
rm(list=ls()) 


#returns working directory 
getwd() 


#sets working directory to SURF RScripts folder
setwd("C:/Users/willo/OneDrive/Documents/R/SURF RScripts")


#loads libraries needed 
library(readxl) 
library("writexl")
library(dplyr)
library(tidyverse)
library("vegan")
library(ggplot2)
library(plyr)
library(extrafont)
font_import()
loadfonts(device="win") 


#Stores/reads xl file as exceldata, makes it a data frame and prints it
exceldata = read_excel ("Copy1_FishSumByTax.xlsx")
dfdata = data.frame(exceldata)
dfdata


#Removes strings that contain FA or SP 
dfdata_noF <- dfdata %>% select(-contains("FA"))
dfdata_noFS <- dfdata_noF %>% select(-contains("SP"))
write.table(dfdata_noFS, file = "SURF_FishData_Anacapa.txt", sep = "")
write_xlsx(dfdata_noFS, "C:/Users/willo/OneDrive/Documents/R/SURF RScripts//SURF_FishData_xl.xlsx")


#Makes new data frame of just taxonomy 
taxonomy = data.frame(dfdata_noFS$sum.taxonomy)


#If > 0 then change to 1 -- this changes characters too 
PA_SW_data = data.frame(ifelse(dfdata_noFS > 0, 1, 0))
PA_SW_data <- subset(PA_SW_data, select=-sum.taxonomy)


#Makes a new data frame by combining PA_SW_data and taxonomy
PA_data <- data.frame(cbind(taxonomy, PA_SW_data))


#renames first column 
PA_data<- rename(PA_data, sum.taxonomy = dfdata_noFS.sum.taxonomy)


#saves final_data as a .txt file so it can be imported to anacapa
write.table(PA_data,"SURF_FinalFish_data.txt",append = FALSE, sep = "\t", dec = ".", row.names = TRUE, col.names = TRUE)


#saves final_data as an excel file
write_xlsx(PA_data, "C:\\Users\\willo\\OneDrive\\Documents\\R\\CleanedFishData.xlsx")


#preps dataset for vegan functions
dfdata_zeroDist <- dfdata_noFS
dfdata_zeroDist[is.na(dfdata_zeroDist)] <- 0
dfdata_zeroDist <- t(dfdata_zeroDist[,2:length(dfdata_zeroDist)])


#computes shannon index and stores as a dataframe 
shannon <- diversity(dfdata_zeroDist, index = "shannon", MARGIN = 1, base = exp(1))

Diversity = data.frame(shannon)
Diversity$simpson <- diversity(dfdata_zeroDist, "simpson", MARGIN = 1)
Diversity$invsimpson <- diversity(dfdata_zeroDist, "inv", MARGIN = 1)



#rarefys the data (so all out of same number)
r_2 <- rarefy(dfdata_zeroDist, 2)


#storing metadata 
getwd()
metadata = read_excel ("SURF_CleanedMetaData_forR.xlsx")


#binds meta data and diversity data 
newdata <- data.frame(cbind(metadata, Diversity))
PA_data_forGraphs = subset(newdata, select = -c(sum.taxonomy))

write.csv(Diversity, "C:\\Users\\willo\\OneDrive\\Documents\\R\\ShannonDataForTaylor.csv")


###GRAPHS NOT USING###
#shannon vs location 
ggplot(PA_data_forGraphs, aes(x=reorder(location, shannon, na.rm = TRUE), y=shannon, fill=bay_placement)) + geom_boxplot() + theme_classic()
ANOVA_ShannonvsLocation = aov(shannon~location, data=PA_data_forGraphs)
summary(ANOVA_ShannonvsLocation)
TukeyHSD(ANOVA_ShannonvsLocation)

#simpson vs location 
ggplot(PA_data_forGraphs, aes(x=reorder(location,simpson, na.rm = TRUE), y=simpson, fill=bay_placement)) + geom_boxplot() + theme_classic()
ANOVA_SimpsonvsLocation = aov(simpson~ location, data=PA_data_forGraphs)
summary(ANOVA_SimpsonvsLocation)
TukeyHSD(ANOVA_ShannonvsLocation)

#simpson vs salinity
boxplot(simpson~salinity,data=PA_data_forGraphs)
ANOVA_SalinityvsSimpson = aov(simpson~salinity, data=PA_data_forGraphs)
summary(ANOVA_SalinityvsSimpson)


#simpson vs season 
ggplot(PA_data_forGraphs, aes(x=season, y=simpson, fill=season)) + geom_boxplot() + theme_classic()
ANOVA_SimpsonvsSeason = aov(simpson~season, data=PA_data_forGraphs)
summary(ANOVA_SimpsonvsSeason)

#salinity vs bay placement
ggplot(PA_data_forGraphs, aes(x=bay_placement, y=salinity, fill=bay_placement)) + geom_boxplot() + theme_classic()
ANOVA_SalinityvsBay = aov(salinity~bay_placement, data=PA_data_forGraphs)
summary(ANOVA_SalinityvsBay)

ggplot(PA_data_forGraphs, aes(x=compass, y=salinity, fill=compass)) + geom_boxplot() + theme_classic()
ANOVA_SalinityvsComp = aov(salinity~compass, data=PA_data_forGraphs)
summary(ANOVA_SalinityvsComp)

#salinity vs season
ggplot(PA_data_forGraphs, aes(x=season, y=salinity, fill=season)) + geom_violin() + theme_classic()

#salinity vs bay placement
ggplot(PA_data_forGraphs, aes(x=bay_placement, y=salinity, fill=bay_placement)) +
  geom_boxplot(color="blue", fill="darkgreen", alpha =0.2) + 
  labs(x = "Bay PLacement", y = "Salinity") + 
  theme_classic()
ANOVA_SalinityvsBay = aov(salinity~bay_placement, data=PA_data_forGraphs)
summary(ANOVA_SalinityvsBay)
TukeyHSD(ANOVA_SalinityvsBay)

#simpson vs bay placement
ggplot(PA_data_forGraphs, aes(x=bay_placement, y=simpson, fill=bay_placement)) + geom_boxplot() + theme_classic()
ANOVA_SimpsonvsBay = aov(simpson~bay_placement, data=PA_data_forGraphs)
summary(ANOVA_SimpsonvsBay)

ggplot(PA_data_forGraphs, aes(x=compass, y=simpson, fill=compass)) + geom_boxplot() + theme_classic()
ANOVA_SimpsonvsComp = aov(simpson~compass, data=PA_data_forGraphs)
summary(ANOVA_SimpsonvsComp)

#shannon vs bay placement
ggplot(PA_data_forGraphs, aes(x=bay_placement, y=shannon)) + 
  geom_boxplot(color="blue", fill="dark green", alpha=0.2) + theme_classic() + 
  labs(y = "Shannon", x = "Bay Placement")
ANOVA_ShannonvsBay = aov(shannon~bay_placement, data=PA_data_forGraphs)
summary(ANOVA_ShannonvsBay)
TukeyHSD(ANOVA_ShannonvsBay)


###GRAPHS ARE USING###

#shannon vs salinity 
boxplot(shannon~salinity,data=PA_data_forGraphs)
ANOVA_SalinityvsShannon = aov(shannon~salinity, data=PA_data_forGraphs)
summary(ANOVA_SalinityvsShannon)

#Function to take summary stats for shannon index grouped by salinity 
sum_stats <- function(data) {
  data <- data %>%filter(!is.na(shannon))
  df_shannon <- ddply(data, "salinity", summarise, 
                      N = length(shannon), 
                      mean = mean(shannon),
                      sd = sd(shannon), 
                      se = sd/sqrt(N))
  return(df_shannon)}

#Uses function to produce data frame
shannon_stats <- sum_stats(PA_data_forGraphs)


#Plots mean shannon vs salinity
ggplot(shannon_stats, aes(salinity, mean)) + 
  geom_point() +
  theme_classic() +
  geom_smooth(method = "lm", se = FALSE, color="dark green") +
  labs(y="Average Shannon", x="Salinity") +
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=0.2, position=position_dodge(0.05)) +
  theme(text=element_text(family="Times New Roman", face="bold", size=12))

teal = "#0BB9BD"


#shannon vs bay placement
ggplot(PA_data_forGraphs, aes(x=North.South, y=shannon)) + 
  geom_boxplot(color= teal, fill = teal, alpha = 0.2) + theme_classic() + 
  labs(y = "Shannon") +
  theme(axis.title.x = element_blank()) +
  theme(text=element_text(family="Times New Roman", face="bold", size=12)) 
ANOVA_ShannonvsComp = aov(shannon~North.South, data=PA_data_forGraphs)
summary(ANOVA_ShannonvsComp)
TukeyHSD(ANOVA_ShannonvsComp)


#salinity vs bay placement
ggplot(PA_data_forGraphs, aes(x=North.South, y=salinity, fill=compass)) + 
  geom_boxplot(color = teal, fill = teal, alpha = 0.2) + 
  labs(y = "Salinity") + 
  theme_classic() +
  theme(axis.title.x = element_blank()) +
  theme(text=element_text(family="Times New Roman", face="bold", size=12))
ANOVA_SalinityvsComp = aov(salinity~North.South, data=PA_data_forGraphs)
summary(ANOVA_SalinityvsComp)


#shannon vs season
ggplot(PA_data_forGraphs, aes(x=season, y=shannon)) + 
  geom_boxplot(color = teal, fill = teal, alpha = 0.2) + 
  labs(y = "Shannon") + 
  theme_classic() +
  theme(axis.title.x = element_blank()) +
  theme(text=element_text(family="Times New Roman", face="bold", size=12)) 


ANOVA_ShannonvsSeason = aov(shannon~season, data=PA_data_forGraphs)
summary(ANOVA_ShannonvsSeason)

#Top 10 sps
spsData = data.frame(read_excel ("SURF_Top10sps.xlsx"))

RStudio.Version()




