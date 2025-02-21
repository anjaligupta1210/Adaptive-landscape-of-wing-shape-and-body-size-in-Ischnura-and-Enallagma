#Opportunity for sexual selection calculation
#Code - Anjali Gupta

#Set working directory
setwd("YOURWORKINGDIRECTORY")

#import datasets

library(readxl)
dat_2018 <- read_excel("CommunitySampling_2018.xlsx")
dat_2019 <- read_excel("CommunitySampling_2019.xlsx")
dat_2020 <- read_excel("CommunitySampling_2020.xlsx")
dat_2021 <- read_excel("CommunitySampling_2021.xlsx")

#Calculation for 2018

Data_2018 <- dat_2018[, c(3,4,6,8)]
m_2018 <- Data_2018[Data_2018$Sex == 1, ]
ECm_2018 <- m_2018[m_2018$Species == "Enallagma_cyathigerum", ]
IEm_2018 <- m_2018[m_2018$Species == "Ischnura_elegans", ]

library(dplyr)

EC_2018 <- ECm_2018 %>%
    group_by(Locale, Copulation.status) %>%
    summarise(n = n()) %>%
    mutate(Freq = n/sum(n))

IE_2018 <- IEm_2018 %>%
    group_by(Locale, Copulation.status) %>%
    summarise(	n = n()) %>%
    mutate(Freq = n/sum(n))

##below is for Masahito to check

Data_2018 <- dat_2018[, c(3,4,6,8)] %>% mutate(year = "2018")
Data_2019 <- dat_2019[, c(3,4,6,8)] %>% mutate(year = "2019")
Data_2020 <- dat_2020[, c(3,4,6,8)] %>% mutate(year = "2020")
Data_2021 <- dat_2021[, c(3,4,6,8)] %>% mutate(year = "2021")

Data_all_male <- subset(bind_rows(Data_2018, Data_2019, Data_2020, Data_2021), Sex == 1)

test <- subset(Data_all_male, Species == "Enallagma_cyathigerum"&year=="2018"&Locale=="Gunnesbo")

Is_all <- subset(Data_all_male, Species == "Enallagma_cyathigerum"|Species == "Ischnura_elegans") %>%
    group_by(Species, year, Locale) %>%
    summarise(	n = n(),
    			Var_Mating = var(as.integer(Copulation.status)),
    			Mean_Mating = mean(as.integer(Copulation.status))) %>%
    mutate(Is = Var_Mating/Mean_Mating^2)

##

EC_2018_1 <- EC_2018[EC_2018$Copulation.status == "1", ]
IE_2018_1 <- IE_2018[IE_2018$Copulation.status == "1", ]

EC_2018_0 <- EC_2018[EC_2018$Copulation.status == "0", ]
IE_2018_0 <- IE_2018[IE_2018$Copulation.status == "0", ]

colnames(EC_2018_1)[3] <- "n1"
colnames(EC_2018_0)[3] <- "n0"
colnames(IE_2018_1)[3] <- "n1"
colnames(IE_2018_0)[3] <- "n0"

EC_2018_all <- merge(EC_2018_1, EC_2018_0, by = "Locale", all = TRUE)
EC_2018_all[is.na(EC_2018_all)] <- "0"
EC_2018_all$Copulation.status.x <- "1"

IE_2018_all <- merge(IE_2018_1, IE_2018_0, by = "Locale", all = TRUE)
IE_2018_all[is.na(IE_2018_all)] <- "0"
IE_2018_all$Copulation.status.x <- "1"

EC_2018_all$Species <- "Enallagma_cyathigerum"
IE_2018_all$Species <- "Ischnura_elegans"

Is_2018 <- rbind(EC_2018_all, IE_2018_all) 

Is_2018$Year <- "2018"


#Repeating the same thing for other years

#2019

Data_2019 <- dat_2019[, c(3,4,6,8)]
m_2019 <- Data_2019[Data_2019$Sex == 1, ]
ECm_2019 <- m_2019[m_2019$Species == "Enallagma_cyathigerum", ]
IEm_2019 <- m_2019[m_2019$Species == "Ischnura_elegans", ]

EC_2019 <- ECm_2019 %>%
  group_by(Locale, Copulation.status) %>%
  summarise(n = n()) %>%
  mutate(Freq = n/sum(n))

IE_2019 <- IEm_2019 %>%
  group_by(Locale, Copulation.status) %>%
  summarise(n = n()) %>%
  mutate(Freq = n/sum(n))

EC_2019_1 <- EC_2019[EC_2019$Copulation.status == "1", ]
IE_2019_1 <- IE_2019[IE_2019$Copulation.status == "1", ]

EC_2019_0 <- EC_2019[EC_2019$Copulation.status == "0", ]
IE_2019_0 <- IE_2019[IE_2019$Copulation.status == "0", ]

colnames(EC_2019_1)[3] <- "n1"
colnames(EC_2019_0)[3] <- "n0"
colnames(IE_2019_1)[3] <- "n1"
colnames(IE_2019_0)[3] <- "n0"

EC_2019_all <- merge(EC_2019_1, EC_2019_0, by = "Locale", all = TRUE)
EC_2019_all[is.na(EC_2019_all)] <- "0"
EC_2019_all$Copulation.status.x <- "1"

IE_2019_all <- merge(IE_2019_1, IE_2019_0, by = "Locale", all = TRUE)
IE_2019_all[is.na(IE_2019_all)] <- "0"
IE_2019_all$Copulation.status.x <- "1"

EC_2019_all$Species <- "Enallagma_cyathigerum"
IE_2019_all$Species <- "Ischnura_elegans"

Is_2019 <- rbind(EC_2019_all, IE_2019_all) 

Is_2019$Year <- "2019"

#2020

Data_2020 <- dat_2020[, c(3,4,6,8)]
m_2020 <- Data_2020[Data_2020$Sex == 1, ]
ECm_2020 <- m_2020[m_2020$Species == "Enallagma_cyathigerum", ]
IEm_2020 <- m_2020[m_2020$Species == "Ischnura_elegans", ]

EC_2020 <- ECm_2020 %>%
  group_by(Locale, Copulation.status) %>%
  summarise(n = n()) %>%
  mutate(Freq = n/sum(n))

IE_2020 <- IEm_2020 %>%
  group_by(Locale, Copulation.status) %>%
  summarise(n = n()) %>%
  mutate(Freq = n/sum(n))

EC_2020_1 <- EC_2020[EC_2020$Copulation.status == "1", ]
IE_2020_1 <- IE_2020[IE_2020$Copulation.status == "1", ]

EC_2020_0 <- EC_2020[EC_2020$Copulation.status == "0", ]
IE_2020_0 <- IE_2020[IE_2020$Copulation.status == "0", ]

colnames(EC_2020_1)[3] <- "n1"
colnames(EC_2020_0)[3] <- "n0"
colnames(IE_2020_1)[3] <- "n1"
colnames(IE_2020_0)[3] <- "n0"

EC_2020_all <- merge(EC_2020_1, EC_2020_0, by = "Locale", all = TRUE)
EC_2020_all[is.na(EC_2020_all)] <- "0"
EC_2020_all$Copulation.status.x <- "1"

IE_2020_all <- merge(IE_2020_1, IE_2020_0, by = "Locale", all = TRUE)
IE_2020_all[is.na(IE_2020_all)] <- "0"
IE_2020_all$Copulation.status.x <- "1"

EC_2020_all$Species <- "Enallagma_cyathigerum"
IE_2020_all$Species <- "Ischnura_elegans"

Is_2020 <- rbind(EC_2020_all, IE_2020_all) 

Is_2020$Year <- "2020"

#2021

Data_2021 <- dat_2021[, c(3,4,6,8)]
m_2021 <- Data_2021[Data_2021$Sex == 1, ]
ECm_2021 <- m_2021[m_2021$Species == "Enallagma_cyathigerum", ]
IEm_2021 <- m_2021[m_2021$Species == "Ischnura_elegans", ]

EC_2021 <- ECm_2021 %>%
  group_by(Locale, Copulation.status) %>%
  summarise(n = n()) %>%
  mutate(Freq = n/sum(n))

IE_2021 <- IEm_2021 %>%
  group_by(Locale, Copulation.status) %>%
  summarise(n = n()) %>%
  mutate(Freq = n/sum(n))

EC_2021_1 <- EC_2021[EC_2021$Copulation.status == "1", ]
IE_2021_1 <- IE_2021[IE_2021$Copulation.status == "1", ]

EC_2021_0 <- EC_2021[EC_2021$Copulation.status == "0", ]
IE_2021_0 <- IE_2021[IE_2021$Copulation.status == "0", ]

colnames(EC_2021_1)[3] <- "n1"
colnames(EC_2021_0)[3] <- "n0"
colnames(IE_2021_1)[3] <- "n1"
colnames(IE_2021_0)[3] <- "n0"

EC_2021_all <- merge(EC_2021_1, EC_2021_0, by = "Locale", all = TRUE)
EC_2021_all[is.na(EC_2021_all)] <- "0"
EC_2021_all$Copulation.status.x <- "1"

IE_2021_all <- merge(IE_2021_1, IE_2021_0, by = "Locale", all = TRUE)
IE_2021_all[is.na(IE_2021_all)] <- "0"
IE_2021_all$Copulation.status.x <- "1"

EC_2021_all$Species <- "Enallagma_cyathigerum"
IE_2021_all$Species <- "Ischnura_elegans"

Is_2021 <- rbind(EC_2021_all, IE_2021_all) 

Is_2021$Year <- "2021"

#Now, we combine the data files for all the four years

Is_all <- rbind(Is_2018,
                Is_2019,
                Is_2020,
                Is_2021)
Is_all <- Is_all[!(Is_all$Locale=="0"),]

Is_all$n0 <- as.numeric(Is_all$n0)
Is_all$n1 <- as.numeric(Is_all$n1)

Is_all$n <- Is_all$n0 + Is_all$n1

Is_all$Freq.x <- as.numeric(Is_all$Freq.x)
Is_all$Freq.y <- as.numeric(Is_all$Freq.y)

Is <- Is_all %>%
  summarise(Species = Species,
            Locale = Locale,
            MaleMatingSuccess = Freq.x,
            Is = Freq.x * Freq.y,
            Year = Year,
            n = n)

#write.csv(Is, "Is_updated.csv")
