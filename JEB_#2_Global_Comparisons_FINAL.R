#Checked on 2025.02.21 for JEB resubmission

# Set working directory:
setwd("YOURWORKINGDIRECTORY")

library(tidyr)
library(dplyr)
library(geomorph)
library(MASS)
library(lme4)
library(plotly)
library(lmerTest)
library(mgcv)
library(GGally)
library(ggfortify)
library(MuMIn)

#read in data prepared in "JEB_#1_Data_Preparation_FINAL"

#Here we calculate fitness per population/year, and relative each individual's fitness relative to these values

DataPC <- read.csv("DataPC_sorted_locale_fixed.csv") %>% group_by(Season, Genus_, Locale) %>%
				mutate(		male_mean_fitness = mean(Copula),
									MatingSuccess_rel_pop = Copula/male_mean_fitness)

Male_ave <- subset(DataPC, Sex=="Male") %>% group_by(Ind_ID) %>%
				summarise(	Ind_ID = unique(Ind_ID),
							Season = unique(Season),
							Locale = unique(Locale),
							Sex = unique(Sex),
							Genus = unique(Genus_),
							Copula = mean(Copula),
							MatingSuccess = mean(MatingSuccess_rel_pop),
							Eggs = mean(Eggs),
							LD1 = mean(LD1),
							LD12 = mean(LD12),
							PC1_size = mean(PC1_size),
							PC1_size2 = mean(PC1_size2),
							PC1_shape = mean(PC1_shape),
							PC1_shape2 = mean(PC1_shape2),
							PC2_shape = mean(PC2_shape),
							PC2_shape2 = mean(PC2_shape2),
							PC3_shape = mean(PC3_shape),
							PC3_shape2 = mean(PC3_shape2),
							PC4_shape = mean(PC4_shape),
							PC4_shape2 = mean(PC4_shape2),
							PC5_shape = mean(PC5_shape),
							PC5_shape2 = mean(PC5_shape2))

Female_ave <- subset(DataPC, Sex=="Female") %>% group_by(Ind_ID) %>%
				summarise(	Ind_ID = unique(Ind_ID),
							Season = unique(Season),
							Locale = unique(Locale),
							Sex = unique(Sex),
							Genus = unique(Genus_),
							Copula = mean(Copula),
							MatingSuccess = mean(MatingSuccess_rel_pop),
							Eggs = mean(Eggs),
							LD1 = mean(LD1),
							LD12 = mean(LD12),
							PC1_size = mean(PC1_size),
							PC1_size2 = mean(PC1_size2),
							PC1_shape = mean(PC1_shape),
							PC1_shape2 = mean(PC1_shape2),
							PC2_shape = mean(PC2_shape),
							PC2_shape2 = mean(PC2_shape2),
							PC3_shape = mean(PC3_shape),
							PC3_shape2 = mean(PC3_shape2),
							PC4_shape = mean(PC4_shape),
							PC4_shape2 = mean(PC4_shape2),
							PC5_shape = mean(PC5_shape),
							PC5_shape2 = mean(PC5_shape2))

all_ave_Fore <- subset(DataPC, WingID=="Forewing") %>% group_by(Ind_ID) %>%
				summarise(	Ind_ID = unique(Ind_ID),
							Season = unique(Season),
							Locale = unique(Locale),
							Sex = unique(Sex),
							Genus = unique(Genus_),
							Copula = mean(Copula),
							MatingSuccess = mean(MatingSuccess_rel_pop),
							Eggs = mean(Eggs),
							LD1 = mean(LD1),
							LD12 = mean(LD12),
							PC1_size = mean(PC1_size),
							PC1_size2 = mean(PC1_size2),
							PC1_shape = mean(PC1_shape),
							PC1_shape2 = mean(PC1_shape2),
							PC2_shape = mean(PC2_shape),
							PC2_shape2 = mean(PC2_shape2),
							PC3_shape = mean(PC3_shape),
							PC3_shape2 = mean(PC3_shape2),
							PC4_shape = mean(PC4_shape),
							PC4_shape2 = mean(PC4_shape2),
							PC5_shape = mean(PC5_shape),
							PC5_shape2 = mean(PC5_shape2))

all_ave_Hind <- subset(DataPC, WingID=="Hindwing")  %>% group_by(Ind_ID) %>%
				summarise(	Ind_ID = unique(Ind_ID),
							Season = unique(Season),
							Locale = unique(Locale),
							Sex = unique(Sex),
							Genus = unique(Genus_),
							Copula = mean(Copula),
							MatingSuccess = mean(MatingSuccess_rel_pop),
							Eggs = mean(Eggs),
							LD1 = mean(LD1),
							LD12 = mean(LD12),
							PC1_size = mean(PC1_size),
							PC1_size2 = mean(PC1_size2),
							PC1_shape = mean(PC1_shape),
							PC1_shape2 = mean(PC1_shape2),
							PC2_shape = mean(PC2_shape),
							PC2_shape2 = mean(PC2_shape2),
							PC3_shape = mean(PC3_shape),
							PC3_shape2 = mean(PC3_shape2),
							PC4_shape = mean(PC4_shape),
							PC4_shape2 = mean(PC4_shape2),
							PC5_shape = mean(PC5_shape),
							PC5_shape2 = mean(PC5_shape2))

ForeMale_ave <- subset(all_ave_Fore, Sex =="Male") %>% group_by(Season, Locale, Genus) %>% mutate (n_beta = n())
HindMale_ave <- subset(all_ave_Hind, Sex =="Male") %>% group_by(Season, Locale, Genus) %>% mutate (n_beta = n())

Male_all <- left_join(ForeMale_ave, HindMale_ave[,c(1,9)], by = "Ind_ID")

# Female data

Female_ave_fecundity <- subset(Female_ave, Sex =="Female"&Copula=="1") %>% group_by(Season, Genus, Locale) %>% 
					mutate (	n_beta = n(),
									female_mean_fitness = mean(Eggs),
									Fecundity = Eggs/female_mean_fitness)

ForeFemale_ave <- subset(all_ave_Fore, Sex =="Female"&Copula=="1") %>% group_by(Season, Genus, Locale) %>% 
					mutate (	n_beta = n(),
									female_mean_fitness = mean(Eggs),
									Fecundity = Eggs/female_mean_fitness)
HindFemale_ave <- subset(all_ave_Hind, Sex =="Female"&Copula=="1") %>% group_by(Season, Genus, Locale) %>% 
					mutate (	n_beta = n(),
									female_mean_fitness = mean(Eggs),
									Fecundity = Eggs/female_mean_fitness)

Female_all <- left_join(ForeFemale_ave, HindFemale_ave[,c(1,9)], by = "Ind_ID")

#############################
#############################
#############################

## Table 1

#Mean standardized selection coefficients were calculated using the approach below. Quadratic coefficients were multiplied by 2 before inputting into Table1

#MLI - male_Linear coefficient_Ischnura
#MIQ - male_Ischnura_Quadratic coeff
#MLE - male_Linear coefficient_Enallagma & so on...

#PC1_size - Males

MLI <- lmer(MatingSuccess ~ PC1_size + (1 | Locale) , data = subset(Male_ave, Genus=="Ischnura_elegans"))
summary(MLI)

MIQ <- lmer(MatingSuccess ~ PC1_size + PC1_size2 + (1 | Locale) , data = subset(Male_ave, Genus=="Ischnura_elegans"))
summary(MIQ)
summary(MIQ)$coefficients[3,]*2

MLE <- lmer(MatingSuccess ~ PC1_size + (1 | Locale), data = subset(Male_ave, Genus=="Enallagma_cyathigerum"))
summary(MLE)

MEQ <- lmer(MatingSuccess ~ PC1_size + PC1_size2+ (1 | Locale), data = subset(Male_ave, Genus=="Enallagma_cyathigerum"))
summary(MEQ)
summary(MEQ)$coefficients[3,]*2

#PC1_size - Females

FLI <- lmer(Fecundity ~ PC1_size+ (1 | Locale), data = subset(Female_ave_fecundity, Genus=="Ischnura_elegans"))
summary(FLI)

FLQ <- lmer(Fecundity ~ PC1_size + PC1_size2+ (1 | Locale), data = subset(Female_ave_fecundity, Genus=="Ischnura_elegans"))
summary(FLQ)
summary(FLQ)$coefficients[3,]*2

FLE <- lmer(Fecundity ~ PC1_size+ (1 | Locale), data = subset(Female_ave_fecundity, Genus=="Enallagma_cyathigerum"))
summary(FLE)

FLQ <- lmer(Fecundity ~ PC1_size + PC1_size2+ (1 | Locale), data = subset(Female_ave_fecundity, Genus=="Enallagma_cyathigerum"))
summary(FLQ)
summary(FLQ)$coefficients[3,]*2

#Remember Quadratic estimate as well as SE are multiplied by 2 before inputting into the table

#LD1 - Male Forewing

LI <- lmer(MatingSuccess ~ LD1+ (1 | Locale) , data = subset(ForeMale_ave, Genus=="Ischnura_elegans"))
summary(LI)

LIQ <- lmer(MatingSuccess ~ LD1 + LD12+ (1 | Locale), data = subset(ForeMale_ave, Genus=="Ischnura_elegans"))
summary(LIQ)
summary(LIQ)$coefficients[3,]*2

LE <- lmer(MatingSuccess ~ LD1+ (1 | Locale), data = subset(ForeMale_ave, Genus=="Enallagma_cyathigerum"))
summary(LE)

LEQ <- lmer(MatingSuccess ~ LD1 + LD12+ (1 | Locale), data = subset(ForeMale_ave, Genus=="Enallagma_cyathigerum"))
summary(LEQ)
summary(LEQ)$coefficients[3,]*2

#LD1 - Male Hindwing

LI <- lmer(MatingSuccess ~ LD1+ (1 | Locale), data = subset(HindMale_ave, Genus=="Ischnura_elegans"))
summary(LI)

LIQ <- lmer(MatingSuccess ~ LD1 + LD12+ (1 | Locale), data = subset(HindMale_ave, Genus=="Ischnura_elegans"))
summary(LIQ)
summary(LIQ)$coefficients[3,]*2

LE <- lmer(MatingSuccess ~ LD1+ (1 | Locale), data = subset(HindMale_ave, Genus=="Enallagma_cyathigerum"))
summary(LE)

LEQ <- lmer(MatingSuccess ~ LD1 + LD12+ (1 | Locale), data = subset(HindMale_ave, Genus=="Enallagma_cyathigerum"))
summary(LEQ)
summary(LEQ)$coefficients[3,]*2

#LD1 - Female Forewing

LI <- lmer(Fecundity ~ LD1+ (1 | Locale), data = subset(ForeFemale_ave, Genus=="Ischnura_elegans"))
summary(LI)

LIQ <- lmer(Fecundity ~ LD1 + LD12+ (1 | Locale), data = subset(ForeFemale_ave, Genus=="Ischnura_elegans"))
summary(LIQ)
summary(LIQ)$coefficients[3,]*2

LE <- lmer(Fecundity ~ LD1+ (1 | Locale), data = subset(ForeFemale_ave, Genus=="Enallagma_cyathigerum"))
summary(LE)

LEQ <- lmer(Fecundity ~ LD1 + LD12+ (1 | Locale), data = subset(ForeFemale_ave, Genus=="Enallagma_cyathigerum"))
summary(LEQ)
summary(LEQ)$coefficients[3,]*2

#LD1 - Female Hindwing

LI <- lmer(Fecundity ~ LD1+ (1 | Locale), data = subset(HindFemale_ave, Genus=="Ischnura_elegans"))
summary(LI)

LIQ <- lmer(Fecundity ~ LD1 + LD12+ (1 | Locale), data = subset(HindFemale_ave, Genus=="Ischnura_elegans"))
summary(LIQ)
summary(LIQ)$coefficients[3,]*2

LE <- lmer(Fecundity ~ LD1+ (1 | Locale), data = subset(HindFemale_ave, Genus=="Enallagma_cyathigerum"))
summary(LE)

LEQ <- lmer(Fecundity ~ LD1 + LD12+ (1 | Locale), data = subset(HindFemale_ave, Genus=="Enallagma_cyathigerum"))
summary(LEQ)
summary(LEQ)$coefficients[3,]*2

#############################
#############################
#############################

#Table S1

#Forewing

F_Isch <- lmer(Fecundity ~ LD1 + LD12 + PC1_size2 + PC1_size + LD1:PC1_size + (1|Locale), data = subset(ForeFemale_ave, Genus=="Ischnura_elegans"))
F_Ena <- lmer(Fecundity ~ LD1 + LD12 + PC1_size2 + PC1_size + LD1:PC1_size+ (1|Locale), data = subset(ForeFemale_ave, Genus=="Enallagma_cyathigerum"))

M_Isch <- lmer(MatingSuccess ~ LD1 + LD12 + PC1_size2 + PC1_size + LD1:PC1_size+ (1|Locale), data = subset(ForeMale_ave, Genus=="Ischnura_elegans"))
M_Ena <- lmer(MatingSuccess ~ LD1 + LD12 + PC1_size2 + PC1_size + LD1:PC1_size+ (1|Locale), data = subset(ForeMale_ave, Genus=="Enallagma_cyathigerum"))

summary(F_Isch)
summary(F_Isch)$coefficients[c(3,4),]*2
summary(F_Ena)
summary(F_Ena)$coefficients[c(3,4),]*2
summary(M_Isch)
summary(M_Isch)$coefficients[c(3,4),]*2
summary(M_Ena)
summary(M_Ena)$coefficients[c(3,4),]*2

#Hindwing

F_Isch <- lmer(Fecundity ~ LD1 + LD12 + PC1_size2 + PC1_size + LD1:PC1_size + (1|Locale), data = subset(HindFemale_ave, Genus=="Ischnura_elegans"))
F_Ena <- lmer(Fecundity ~ LD1 + LD12 + PC1_size2 + PC1_size + LD1:PC1_size+ (1|Locale), data = subset(HindFemale_ave, Genus=="Enallagma_cyathigerum"))

M_Isch <- lmer(MatingSuccess ~ LD1 + LD12 + PC1_size2 + PC1_size + LD1:PC1_size+ (1|Locale), data = subset(HindMale_ave, Genus=="Ischnura_elegans"))
M_Ena <- lmer(MatingSuccess ~ LD1 + LD12 + PC1_size2 + PC1_size + LD1:PC1_size+ (1|Locale), data = subset(HindMale_ave, Genus=="Enallagma_cyathigerum"))

summary(F_Isch)
summary(F_Isch)$coefficients[c(3,4),]*2
summary(F_Ena)
summary(F_Ena)$coefficients[c(3,4),]*2
summary(M_Isch)
summary(M_Isch)$coefficients[c(3,4),]*2
summary(M_Ena)
summary(M_Ena)$coefficients[c(3,4),]*2

#############################
#############################
#############################

#Obtaining variance standardized selection estimates as reported in Table 1.

#Male Body Size

LI <- lmer(MatingSuccess ~ normPC1_size + (1 | Locale), 
				data = subset(Male_ave, Genus=="Ischnura_elegans") %>% 
				mutate(	normPC1_size=PC1_size/sd(PC1_size))) 
summary(LI)

LIQ <- lmer(MatingSuccess ~ normPC1_size + normPC1_size2+ (1 | Locale),
			data = subset(Male_ave, Genus=="Ischnura_elegans")%>%
			mutate(	normPC1_size=PC1_size/sd(PC1_size),
							normPC1_size2 = normPC1_size^2))
summary(LIQ)
summary(LIQ)$coefficients[3,]*2

LE <- lmer(MatingSuccess ~ normPC1_size+ (1 | Locale), 
			data = subset(Male_ave, Genus=="Enallagma_cyathigerum")%>%
			mutate(normPC1_size=PC1_size/sd(PC1_size)))
summary(LE)

LEQ <- lmer(MatingSuccess ~ normPC1_size + normPC1_size2+ (1 | Locale), 
			data = subset(Male_ave, Genus=="Enallagma_cyathigerum")%>%
			mutate(	normPC1_size=PC1_size/sd(PC1_size),
							normPC1_size2 = normPC1_size^2))
summary(LEQ)
summary(LEQ)$coefficients[3,]*2

#normPC1_size: Female Body Size

LI <- lmer(Fecundity ~ normPC1_size+ (1 | Locale), 
			data = subset(Female_ave_fecundity, Genus=="Ischnura_elegans")%>%
			mutate(normPC1_size=PC1_size/sd(PC1_size)))
summary(LI)

LIQ <- lmer(Fecundity ~ normPC1_size + normPC1_size2+ (1 | Locale), 
			data = subset(Female_ave_fecundity, Genus=="Ischnura_elegans")%>%
			mutate(	normPC1_size=PC1_size/sd(PC1_size),
							normPC1_size2 = normPC1_size^2))
summary(LIQ)
summary(LIQ)$coefficients[3,]*2

LE <- lmer(Fecundity ~ normPC1_size+ (1 | Locale), 
			data = subset(Female_ave_fecundity, Genus=="Enallagma_cyathigerum")%>%
			mutate(normPC1_size=PC1_size/sd(PC1_size)))
summary(LE)

LEQ <- lmer(Fecundity ~ normPC1_size + normPC1_size2+ (1 | Locale), 
			data = subset(Female_ave_fecundity, Genus=="Enallagma_cyathigerum")%>%
			mutate(	normPC1_size=PC1_size/sd(PC1_size),
					normPC1_size2 = normPC1_size^2))
summary(LEQ)
summary(LEQ)$coefficients[3,]*2

#normLD1: Male Forewing Shape

LI <- lmer(MatingSuccess ~ normLD1+ (1 | Locale), 
			data = subset(ForeMale_ave, Genus=="Ischnura_elegans")%>%
			mutate(normLD1 = LD1/sd(LD1)))
summary(LI)

LIQ <- lmer(MatingSuccess ~ normLD1 + normLD12+ (1 | Locale), 
			data = subset(ForeMale_ave, Genus=="Ischnura_elegans")%>%
			mutate(	normLD1 = LD1/sd(LD1),
							normLD12 = normLD1^2))
summary(LIQ)
summary(LIQ)$coefficients[3,]*2

LE <- lmer(MatingSuccess ~ normLD1+ (1 | Locale), 
			data = subset(ForeMale_ave, Genus=="Enallagma_cyathigerum")%>%
			mutate(normLD1 = LD1/sd(LD1)))
summary(LE)

LEQ <- lmer(MatingSuccess ~ normLD1 + normLD12+ (1 | Locale), 
			data = subset(ForeMale_ave, Genus=="Enallagma_cyathigerum")%>%
			mutate(	normLD1 = LD1/sd(LD1),
							normLD12 = normLD1^2))
summary(LEQ)
summary(LEQ)$coefficients[3,]*2

#normLD1: Male Hindwing Shape

LI <- lmer(MatingSuccess ~ normLD1+ (1 | Locale), 
			data = subset(HindMale_ave, Genus=="Ischnura_elegans")%>%
			mutate(normLD1 = LD1/sd(LD1)))
summary(LI)

LIQ <- lmer(MatingSuccess ~ normLD1 + normLD12+ (1 | Locale),
			data = subset(HindMale_ave, Genus=="Ischnura_elegans")%>%
			mutate(	normLD1 = LD1/sd(LD1),
							normLD12 = normLD1^2))
summary(LIQ)
summary(LIQ)$coefficients[3,]*2

LE <- lmer(MatingSuccess ~ normLD1+ (1 | Locale), 
			data = subset(HindMale_ave, Genus=="Enallagma_cyathigerum")%>%
			mutate(normLD1 = LD1/sd(LD1)))
summary(LE)

LEQ <- lmer(MatingSuccess ~ normLD1 + normLD12+ (1 | Locale), 
			data = subset(HindMale_ave, Genus=="Enallagma_cyathigerum")%>%
			mutate(	normLD1 = LD1/sd(LD1),
							normLD12 = normLD1^2))
summary(LEQ)
summary(LEQ)$coefficients[3,]*2

#normLD1: Female Forewing Shape

LI <- lmer(Fecundity ~ normLD1+ (1 | Locale), 
			data = subset(ForeFemale_ave, Genus=="Ischnura_elegans")%>%
			mutate(normLD1 = LD1/sd(LD1)))
summary(LI)

LIQ <- lmer(Fecundity ~ normLD1 + normLD12+ (1 | Locale), 
			data = subset(ForeFemale_ave, Genus=="Ischnura_elegans")%>%
			mutate(	normLD1 = LD1/sd(LD1),
							normLD12 = normLD1^2))
summary(LIQ)
summary(LIQ)$coefficients[3,]*2

LE <- lmer(Fecundity ~ normLD1+ (1 | Locale), 
			data = subset(ForeFemale_ave, Genus=="Enallagma_cyathigerum")%>%
			mutate(normLD1 = LD1/sd(LD1)))
summary(LE)

LEQ <- lmer(Fecundity ~ normLD1 + normLD12+ (1 | Locale), 
			data = subset(ForeFemale_ave, Genus=="Enallagma_cyathigerum")%>%
			mutate(	normLD1 = LD1/sd(LD1),
							normLD12 = normLD1^2))
summary(LEQ)
summary(LEQ)$coefficients[3,]*2

#normLD1: Female Hindwing Shape

LI <- lmer(Fecundity ~ normLD1+ (1 | Locale), 
			data = subset(HindFemale_ave, Genus=="Ischnura_elegans")%>%
			mutate(normLD1 = LD1/sd(LD1)))
summary(LI)

LIQ <- lmer(Fecundity ~ normLD1 + normLD12+ (1 | Locale), 
			data = subset(HindFemale_ave, Genus=="Ischnura_elegans")%>%
			mutate(	normLD1 = LD1/sd(LD1),
							normLD12 = normLD1^2))
summary(LIQ)
summary(LIQ)$coefficients[3,]*2

LE <- lmer(Fecundity ~ normLD1+ (1 | Locale), 
			data = subset(HindFemale_ave, Genus=="Enallagma_cyathigerum")%>%
			mutate(normLD1 = LD1/sd(LD1)))
summary(LE)

LEQ <- lmer(Fecundity ~ normLD1 + normLD12+ (1 | Locale), 
			data = subset(HindFemale_ave, Genus=="Enallagma_cyathigerum")%>%
			mutate(	normLD1 = LD1/sd(LD1),
							normLD12 = normLD1^2))
summary(LEQ)
summary(LEQ)$coefficients[3,]*2

#############################
#############################
#############################

#Comparison of adaptive landscape between species
#Table S2

#Male forewing

MF_Model1 <- lmer(MatingSuccess ~ Genus + LD1 + LD12 + PC1_size + PC1_size2 + Genus:LD1 + Genus:LD12 + Genus:PC1_size + Genus:PC1_size2 + (1 | Locale), data = ForeMale_ave)
summary(MF_Model1)

MF_Model2 <- lmer(MatingSuccess ~ Genus + LD1 + LD12 + PC1_size + PC1_size2 + Genus:LD1 + Genus:LD12 + Genus:PC1_size2 + (1 | Locale), data = ForeMale_ave)
summary(MF_Model2)

MF_Model3 <- lmer(MatingSuccess ~ Genus + LD1 + LD12 + PC1_size + PC1_size2 + Genus:LD12 + Genus:PC1_size2 + (1 | Locale), data = ForeMale_ave)
summary(MF_Model3)

MF_Model4 <- lmer(MatingSuccess ~ Genus + LD1 + LD12 + PC1_size + PC1_size2 + Genus:PC1_size2 + (1 | Locale), data = ForeMale_ave)
summary(MF_Model4)

MF_Model5 <- lmer(MatingSuccess ~ Genus + LD1 + LD12 + PC1_size + PC1_size2 + (1 | Locale), data = ForeMale_ave)
summary(MF_Model5)

AICc(MF_Model1, MF_Model2, MF_Model3, MF_Model4, MF_Model5)

#Male hindwing

MH_Model1 <- lmer(MatingSuccess ~ Genus + LD1 + LD12 + PC1_size + PC1_size2 + Genus:LD1 + Genus:LD12 + Genus:PC1_size + Genus:PC1_size2 + (1 | Locale), data = HindMale_ave)
summary(MH_Model1)

MH_Model2 <- lmer(MatingSuccess ~ Genus + LD1 + LD12 + PC1_size + PC1_size2 + Genus:LD1 + Genus:LD12 + Genus:PC1_size2 + (1 | Locale), data = HindMale_ave)
summary(MH_Model2)

MH_Model3 <- lmer(MatingSuccess ~ Genus + LD1 + LD12 + PC1_size + PC1_size2 + Genus:LD1 + Genus:PC1_size2 + (1 | Locale), data = HindMale_ave)
summary(MH_Model3)

MH_Model4 <- lmer(MatingSuccess ~ Genus + LD1 + LD12 + PC1_size + PC1_size2 + Genus:PC1_size2 + (1 | Locale), data = HindMale_ave)
summary(MH_Model4)

MH_Model5 <- lmer(MatingSuccess ~ Genus + LD1 + LD12 + PC1_size + PC1_size2 + (1 | Locale), data = HindMale_ave)
summary(MH_Model5)

AICc(MH_Model1, MH_Model2, MH_Model3, MH_Model4, MH_Model5)

#Female forewing

MF_Model1 <- lmer(Fecundity ~ Genus + LD1 + LD12 + PC1_size + PC1_size2 + Genus:LD1 + Genus:LD12 + Genus:PC1_size + Genus:PC1_size2 + (1 | Locale), data = ForeFemale_ave)
summary(MF_Model1)

MF_Model2 <-  lmer(Fecundity ~ Genus + LD1 + LD12 + PC1_size + PC1_size2 + Genus:LD12 + Genus:PC1_size + Genus:PC1_size2  + (1 | Locale), data = ForeFemale_ave)
summary(MF_Model2)

MF_Model3 <- lmer(Fecundity ~ Genus + LD1 + LD12 + PC1_size + PC1_size2 + Genus:PC1_size + Genus:PC1_size2  +  (1 | Locale), data = ForeFemale_ave)
summary(MF_Model3)

MF_Model4 <- lmer(Fecundity ~ Genus + LD1 + LD12 + PC1_size + PC1_size2 + Genus:PC1_size  + (1 | Locale), data = ForeFemale_ave)
summary(MF_Model4)

MF_Model5 <- lmer(Fecundity ~ Genus + LD1 + LD12 + PC1_size + PC1_size2 + (1 | Locale), data = ForeFemale_ave)
summary(MF_Model5)

AICc(MF_Model1, MF_Model2, MF_Model3, MF_Model4, MF_Model5)

#Female hindwing

MF_Model1 <- lmer(Fecundity ~ Genus + LD1 + LD12 + PC1_size + PC1_size2 + Genus:LD1 + Genus:LD12 + Genus:PC1_size + Genus:PC1_size2 + (1 | Locale), data = HindFemale_ave)
summary(MF_Model1)

MF_Model2 <-  lmer(Fecundity ~ Genus + LD1 + LD12 + PC1_size + PC1_size2 + Genus:LD12 + Genus:PC1_size + Genus:PC1_size2  + (1 | Locale), data = HindFemale_ave)
summary(MF_Model2)

MF_Model3 <- lmer(Fecundity ~ Genus + LD1 + LD12 + PC1_size + PC1_size2 + Genus:PC1_size + Genus:PC1_size2  + (1 | Locale), data = HindFemale_ave)
summary(MF_Model3)

MF_Model4 <- lmer(Fecundity ~ Genus + LD1 + LD12 + PC1_size + PC1_size2 + Genus:PC1_size  + (1 | Locale), data = HindFemale_ave)
summary(MF_Model4)

MF_Model5 <- lmer(Fecundity ~ Genus + LD1 + LD12 + PC1_size + PC1_size2 + (1 | Locale), data = HindFemale_ave)
summary(MF_Model5)

AICc(MF_Model1, MF_Model2, MF_Model3, MF_Model4, MF_Model5)

#############################
#############################
#############################

#Figure S9
## Below we plot selection on PCs to raise confidence that the pattern we report is not limtied to LD1

ForeMale_ave <- ForeMale_ave %>% mutate(community_ID = paste(Genus, Locale, Season, sep = "_")) 
ForeFemale_ave <- ForeFemale_ave %>% mutate(community_ID = paste(Genus, Locale, Season, sep = "_")) 

MPC1 <- ggplot(subset(ForeMale_ave, n_beta > 20), aes(PC1_shape, MatingSuccess, colour = Genus)) + 
			geom_point(alpha=0.1) + 
			geom_smooth(method = "lm", formula = y ~ splines::bs(x, 3), size = 0.5, se = FALSE, lty = 2) + 
			labs(y = "Relative fitness (male)", x = "Forewing shape (PC1)") + 
			theme_classic() + 
			theme(text = element_text(size = 11)) + 
			guides(colour=guide_legend("Species")) + 
			scale_colour_discrete("Species", labels = c(expression(italic("Enallagma cyathigerum")),expression(italic("Ischnura elegans")))) + 
			theme(	aspect.ratio = 1,
						strip.background = element_blank(),
						legend.position="none")+
			geom_line(stat = "smooth", method = "lm",data = subset(ForeMale_ave, n_beta > 20),  lwd = .75, lty = 1, alpha = .5, se = FALSE, aes(group = community_ID))

FPC1 <- ggplot(subset(ForeFemale_ave, n_beta > 20), aes(PC1_shape, Fecundity, colour = Genus)) + 
			geom_point(alpha=0.1) + 
			geom_smooth(method = "lm", formula = y ~ splines::bs(x, 3), size = 0.5, se = FALSE, lty = 2) + 
			labs(y = "Relative fitness (female)", x = "Forewing shape (PC1)") + 
			theme_classic() + 
			theme(text = element_text(size = 11)) + 
			guides(colour=guide_legend("Species")) + 
			scale_colour_discrete("Species", labels = c(expression(italic("Enallagma cyathigerum")),expression(italic("Ischnura elegans")))) +
			theme(	aspect.ratio = 1,
						strip.background = element_blank(),
						legend.position="none")+
			geom_line(stat = "smooth", method = "lm",data = subset(ForeFemale_ave, n_beta > 20),  lwd = .75, lty = 1, alpha = .5, se = FALSE, aes(group = community_ID))

MPC2 <- ggplot(subset(ForeMale_ave, n_beta > 20), aes(PC2_shape, MatingSuccess, colour = Genus)) + 
			geom_point(alpha=0.1) + 
			geom_smooth(method = "lm", formula = y ~ splines::bs(x, 3), size = 0.5, se = FALSE, lty = 2) + 
			labs(y = "Relative fitness (male)", x = "Forewing shape (PC2)") + 
			theme_classic() + 
			theme(text = element_text(size = 11)) + 
			guides(colour=guide_legend("Species")) + 
			scale_colour_discrete("Species", labels = c(expression(italic("Enallagma cyathigerum")),expression(italic("Ischnura elegans")))) + 
			theme(	aspect.ratio = 1,
						strip.background = element_blank(),
						legend.position="none")+
			geom_line(stat = "smooth", method = "lm",data = subset(ForeMale_ave, n_beta > 20),  lwd = .75, lty = 1, alpha = .5, se = FALSE, aes(group = community_ID))

FPC2 <- ggplot(subset(ForeFemale_ave, n_beta > 20), aes(PC2_shape, Fecundity, colour = Genus)) + 
			geom_point(alpha=0.1) + 
			geom_smooth(method = "lm", formula = y ~ splines::bs(x, 3), size = 0.5, se = FALSE, lty = 2) + 
			labs(y = "Relative fitness (female)", x = "Forewing shape (PC2)") + 
			theme_classic() + 
			theme(text = element_text(size = 11)) + 
			guides(colour=guide_legend("Species")) + 
			scale_colour_discrete("Species", labels = c(expression(italic("Enallagma cyathigerum")),expression(italic("Ischnura elegans")))) +
			theme(	aspect.ratio = 1,
						strip.background = element_blank(),
						legend.position="none")+
			geom_line(stat = "smooth", method = "lm",data = subset(ForeFemale_ave, n_beta > 20),  lwd = .75, lty = 1, alpha = .5, se = FALSE, aes(group = community_ID))

MPC3 <- ggplot(subset(ForeMale_ave, n_beta > 20), aes(PC3_shape, MatingSuccess, colour = Genus)) + 
			geom_point(alpha=0.1) + 
			geom_smooth(method = "lm", formula = y ~ splines::bs(x, 3), size = 0.5, se = FALSE, lty = 2) + 
			labs(y = "Relative fitness (male)", x = "Forewing shape (PC3)") + 
			theme_classic() + 
			theme(text = element_text(size = 11)) + 
			guides(colour=guide_legend("Species")) + 
			scale_colour_discrete("Species", labels = c(expression(italic("Enallagma cyathigerum")),expression(italic("Ischnura elegans")))) + 
			theme(	aspect.ratio = 1,
						strip.background = element_blank(),
						legend.position="none")+
			geom_line(stat = "smooth", method = "lm",data = subset(ForeMale_ave, n_beta > 20),  lwd = .75, lty = 1, alpha = .5, se = FALSE, aes(group = community_ID))

FPC3 <- ggplot(subset(ForeFemale_ave, n_beta > 20), aes(PC3_shape, Fecundity, colour = Genus)) + 
			geom_point(alpha=0.1) + 
			geom_smooth(method = "lm", formula = y ~ splines::bs(x, 3), size = 0.5, se = FALSE, lty = 2) + 
			labs(y = "Relative fitness (female)", x = "Forewing shape (PC3)") + 
			theme_classic() + 
			theme(text = element_text(size = 11)) + 
			guides(colour=guide_legend("Species")) + 
			scale_colour_discrete("Species", labels = c(expression(italic("Enallagma cyathigerum")),expression(italic("Ischnura elegans")))) +
			theme(	aspect.ratio = 1,
						strip.background = element_blank(),
						legend.position="none")+
			geom_line(stat = "smooth", method = "lm",data = subset(ForeFemale_ave, n_beta > 20),  lwd = .75, lty = 1, alpha = .5, se = FALSE, aes(group = community_ID))

MPC4 <- ggplot(subset(ForeMale_ave, n_beta > 20), aes(PC4_shape, MatingSuccess, colour = Genus)) + 
			geom_point(alpha=0.1) + 
			geom_smooth(method = "lm", formula = y ~ splines::bs(x, 3), size = 0.5, se = FALSE, lty = 2) + 
			labs(y = "Relative fitness (male)", x = "Forewing shape (PC4)") + 
			theme_classic() + 
			theme(text = element_text(size = 11)) + 
			guides(colour=guide_legend("Species")) + 
			scale_colour_discrete("Species", labels = c(expression(italic("Enallagma cyathigerum")),expression(italic("Ischnura elegans")))) + 
			theme(	aspect.ratio = 1,
						strip.background = element_blank(),
						legend.position="none")+
			geom_line(stat = "smooth", method = "lm",data = subset(ForeMale_ave, n_beta > 20),  lwd = .75, lty = 1, alpha = .5, se = FALSE, aes(group = community_ID))

FPC4 <- ggplot(subset(ForeFemale_ave, n_beta > 20), aes(PC4_shape, Fecundity, colour = Genus)) + 
			geom_point(alpha=0.1) + 
			geom_smooth(method = "lm", formula = y ~ splines::bs(x, 3), size = 0.5, se = FALSE, lty = 2) + 
			labs(y = "Relative fitness (female)", x = "Forewing shape (PC4)") + 
			theme_classic() + 
			theme(text = element_text(size = 11)) + 
			guides(colour=guide_legend("Species")) + 
			scale_colour_discrete("Species", labels = c(expression(italic("Enallagma cyathigerum")),expression(italic("Ischnura elegans")))) +
			theme(	aspect.ratio = 1,
						strip.background = element_blank(),
						legend.position="none")+
			geom_line(stat = "smooth", method = "lm",data = subset(ForeFemale_ave, n_beta > 20),  lwd = .75, lty = 1, alpha = .5, se = FALSE, aes(group = community_ID))

MPC5 <- ggplot(subset(ForeMale_ave, n_beta > 20), aes(PC5_shape, MatingSuccess, colour = Genus)) + 
			geom_point(alpha=0.1) + 
			geom_smooth(method = "lm", formula = y ~ splines::bs(x, 3), size = 0.5, se = FALSE, lty = 2) + 
			labs(y = "Relative fitness (male)", x = "Forewing shape (PC5)") + 
			theme_classic() + 
			theme(text = element_text(size = 11)) + 
			guides(colour=guide_legend("Species")) + 
			scale_colour_discrete("Species", labels = c(expression(italic("Enallagma cyathigerum")),expression(italic("Ischnura elegans")))) + 
			theme(	aspect.ratio = 1,
						strip.background = element_blank(),
						legend.position="none")+
			geom_line(stat = "smooth", method = "lm",data = subset(ForeMale_ave, n_beta > 20),  lwd = .75, lty = 1, alpha = .5, se = FALSE, aes(group = community_ID))

FPC5 <- ggplot(subset(ForeFemale_ave, n_beta > 20), aes(PC5_shape, Fecundity, colour = Genus)) + 
			geom_point(alpha=0.1) + 
			geom_smooth(method = "lm", formula = y ~ splines::bs(x, 3), size = 0.5, se = FALSE, lty = 2) + 
			labs(y = "Relative fitness (female)", x = "Forewing shape (PC5)") + 
			theme_classic() + 
			theme(text = element_text(size = 11)) + 
			guides(colour=guide_legend("Species")) + 
			scale_colour_discrete("Species", labels = c(expression(italic("Enallagma cyathigerum")),expression(italic("Ischnura elegans")))) +
			theme(	aspect.ratio = 1,
						strip.background = element_blank())+
			geom_line(stat = "smooth", method = "lm",data = subset(ForeFemale_ave, n_beta > 20),  lwd = .75, lty = 1, alpha = .5, se = FALSE, aes(group = community_ID))

male = cowplot::plot_grid(MPC1, MPC2, MPC3,MPC4,MPC5, nrow = 1,
				label_size = 8, label_fontface = 'bold', label_fontfamily = 'Helvetica', hjust = 0)

female = cowplot::plot_grid(FPC1, FPC2, FPC3,FPC4,FPC5, nrow = 1,
				label_size = 8, label_fontface = 'bold', label_fontfamily = 'Helvetica', hjust = 0)

cowplot::plot_grid(male, female, nrow = 2,
				label_size = 8, label_fontface = 'bold', label_fontfamily = 'Helvetica', hjust = 0)

library(ggpubr)

ggarrange(MPC1, MPC2, MPC3,MPC4,MPC5, FPC1, FPC2, FPC3,FPC4,FPC5, ncol = 5, nrow = 2, common.legend = TRUE, legend="bottom", labels = "AUTO", hjust = 0)