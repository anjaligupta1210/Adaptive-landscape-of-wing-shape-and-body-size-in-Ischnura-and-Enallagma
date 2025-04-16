#Checked on 2025.02.21 for JEB resubmission

setwd("YOURWORKINGDIRECTORY")

library(tidyr)
library(dplyr)
library(geomorph)
library(MASS)
library(lme4)
library(plotly)
library(lmerTest)
library(MuMIn)
library(mgcv)
library(GGally)
library(ggfortify)
library(cowplot)
library(readxl)
library(ggplot2)
library(ggh4x)
library(ggpubr)
library(broom)

#############################
#############################
#############################

#In the following chunk, we will summarize mating system parameters per population per year per species.

CommunitySampling_2018 <- read_excel("CommunitySampling_2018.xlsx")
CommunitySampling_2019 <- read_excel("CommunitySampling_2019.xlsx")
CommunitySampling_2020 <- read_excel("CommunitySampling_2020.xlsx")
CommunitySampling_2021 <- read_excel("CommunitySampling_2021.xlsx")

IE <- rbind(subset(CommunitySampling_2018, Species=="Ischnura_elegans"),subset(CommunitySampling_2019, Species=="Ischnura_elegans"),subset(CommunitySampling_2020, Species=="Ischnura_elegans"),subset(CommunitySampling_2021, Species=="Ischnura_elegans"))

EC <- rbind(subset(CommunitySampling_2018, Species=="Enallagma_cyathigerum"),subset(CommunitySampling_2019, Species=="Enallagma_cyathigerum"),subset(CommunitySampling_2020, Species=="Enallagma_cyathigerum"),subset(CommunitySampling_2021, Species=="Enallagma_cyathigerum"))

Data <- rbind(IE, EC)

Data <- Data %>% drop_na(Sex)

#FOR MALE AND FEMALE DENSITY PER MINUTE

(DataIE <- subset(Data, Species=="Ischnura_elegans") %>%
    group_by(Species, Locale, Date, Sex, Year, Catching.time.minutes.) %>%
    summarise(n = n()))

(DataEC <- subset(Data, Species=="Enallagma_cyathigerum") %>%
    group_by(Species, Locale, Date, Sex, Year, Catching.time.minutes.) %>%
    summarise(n = n()))

Data1 <- rbind(DataIE, DataEC)

Data1$Density <- Data1$n/Data1$Catching.time.minutes.

#FOR ANDROCHROME DENSITY

Fem <- subset(Data, Sex=="0")

Fem <- Fem %>% drop_na(Morph)

(FemIE <- subset(Fem, Species=="Ischnura_elegans") %>%
    group_by(Species, Locale, Year, Morph) %>%
    summarise(n = n()))

(FemEC <- subset(Fem, Species=="Enallagma_cyathigerum") %>%
    group_by(Species, Locale, Year, Morph) %>%
    summarise(n = n()))

Data2 <- rbind(FemIE, FemEC)

Andro <- subset(Data2, Morph=="Androchrome")
Gyno <- subset(Data2, Morph=="Gynochrome")

colnames(Andro)[5] <- "nA"
colnames(Gyno)[5] <- "nG"

Data3 <- merge(Andro, Gyno, by= c("Species","Locale","Year"), all = TRUE)

Data3 <- Data3[, c(1:3,5,7)] 

Data3[is.na(Data3)] <- 0

Data3$AndrochromeFrequency <- Data3$nA / (Data3$nG + Data3$nA)
	
#Proportion of Copulating Males

Data4 <- subset(Data, Sex=="1")

IE1 <- subset(Data4, Species=="Ischnura_elegans") %>%
    group_by(Species, Locale, Copulation.status, Year, Date, Catching.time.minutes.) %>%
    summarise(n = n())

EC1 <- subset(Data4, Species=="Enallagma_cyathigerum") %>%
    group_by(Species, Locale, Copulation.status, Year, Date, Catching.time.minutes.) %>%
    summarise(n = n())

Data5 <- rbind(IE1, EC1)

Single <- subset(Data5, Copulation.status=="0")
Mated <- subset(Data5, Copulation.status=="1")

colnames(Single)[7] <- "n0" 
colnames(Mated)[7] <- "n1"

Data6 <- merge(Single, Mated, by=c("Species", "Locale", "Year", "Date", "Catching.time.minutes."), all = TRUE)

Data6 <- Data6[, c(1:5,7,9)]


Data6[is.na(Data6)] <- 0

Data6$ProportionCopula <- Data6$n1 / (Data6$n0 + Data6$n1)


#OPERATIONAL SEX RATIO

DataM <- subset(Data, Age == "Mature")

(DataM.IE <- subset(DataM, Species=="Ischnura_elegans") %>%
    group_by(Species, Locale, Year, Sex) %>%
    summarise(n = n()) %>%
    mutate(Freq = n/sum(n)))

(DataM.EC <- subset(DataM, Species=="Enallagma_cyathigerum") %>%
    group_by(Species, Locale, Year, Sex) %>%
    summarise(n = n()) %>%
    mutate(Freq = n/sum(n)))

DataM1 <- rbind(DataM.EC, DataM.IE)

IEM0 <- subset(DataM.IE, Sex=="0")
IEM1 <- subset(DataM.IE, Sex=="1")

colnames(IEM0)[5] <- "n0"
colnames(IEM0)[6] <- "Freq0"

colnames(IEM1)[5] <- "n1"
colnames(IEM1)[6] <- "Freq1"

ECM0 <- subset(DataM.EC, Sex=="0")
ECM1 <- subset(DataM.EC, Sex=="1")

colnames(ECM0)[5] <- "n0"
colnames(ECM0)[6] <- "Freq0"

colnames(ECM1)[5] <- "n1"
colnames(ECM1)[6] <- "Freq1"

FileIE <- merge(IEM0, IEM1, by=c("Species","Locale", "Year"), all.x = TRUE, all.y = TRUE)
FileEC <- merge(ECM0, ECM1, by=c("Species","Locale", "Year"), all.x = TRUE, all.y = TRUE)

File <- rbind(FileIE, FileEC)

File <- File[,c(1:3,5,8)]

File[is.na(File)] <- 0

File$OSR <- File$n1/File$n0

File <- File[File$n0 != 0, ]

#File is the data file containing information of Operational Sex Ratio (OSR)

#Opportunity for sexual selection (see another chunk to see how we obtain this)

Is <- read.csv("Is_updated.csv")

EC <- Is[Is$Species == "Enallagma_cyathigerum", ]
IE <- Is[Is$Species == "Ischnura_elegans", ]

# Compile a data on all ecology/social system per year/location/species

Male_density <- subset(Data1, Sex=="1") %>% ungroup() %>%
		mutate(community_ID = paste(Species, Locale, Year, sep = "_")) %>%
		dplyr::select(community_ID, Species, Locale, n, Density) %>% group_by(Species, Locale,community_ID) %>%
		summarize(	MaleDensity_mean = mean(Density),
							MaleDensity_var = var(Density),
							Density_n = n())

Female_density <- subset(Data1, Sex=="0") %>% ungroup() %>%
		mutate(		community_ID = paste(Species, Locale, Year, sep = "_")) %>%
		dplyr::select(			community_ID, Density) %>% group_by(community_ID) %>%
		summarize(	FemaleDensity_mean = mean(Density),
							FemaleDensity_var = var(Density))

OSR_summary <- File %>%
		mutate(	community_ID = paste(Species, Locale, Year, sep = "_")) %>%
		dplyr::select(		community_ID , Year , n0, n1, OSR)

AndFreq <- Data3 %>%
		mutate(community_ID = paste(Species, Locale, Year, sep = "_"))%>%
		dplyr::select(		community_ID , nA, nG, AndrochromeFrequency)

Prop_cop <- Data6 %>% ungroup() %>%
		mutate(community_ID = paste(Species, Locale, Year, sep = "_")) %>%  group_by(community_ID) %>%
		summarize(	ProportionCopula_mean = mean(ProportionCopula),
							ProportionCopula_var = var(ProportionCopula),
							ProportionCopula_n = n())%>%
		dplyr::select(		community_ID , ProportionCopula_mean, ProportionCopula_var, ProportionCopula_n)

Is_summary <- Is %>%
		mutate(	OppSel= Is/MaleMatingSuccess^2,
						community_ID = paste(Species, Locale, Year, sep = "_")) %>%
		dplyr::select(		community_ID , MaleMatingSuccess, Is, OppSel)

full_ecology_data <- 	left_join(Male_density, Female_density, by = "community_ID") %>%
									left_join(., OSR_summary, by = "community_ID")%>%
									left_join(., AndFreq, by = "community_ID")%>%
									left_join(., Prop_cop, by = "community_ID")%>%
									left_join(., Is_summary, by = "community_ID")

#############################
#############################
#############################

# Evaluate selection gradients per location (this is the same as the code provided in "JEB_#3_Population_Comparisons_FINAL")

DataPC <- read.csv("DataPC_sorted_locale_fixed.csv") %>% group_by(Season, Genus_, Locale) %>%
				mutate(		male_mean_fitness = mean(Copula),
									MatingSuccess_rel_pop = Copula/male_mean_fitness)

all_ave_Fore <- subset(DataPC, WingID=="Forewing") %>% group_by(Ind_ID) %>%
				summarise(	Ind_ID = unique(Ind_ID),
							Season = unique(Season),
							Locale = unique(Locale),
							Sex = unique(Sex),
							Genus = unique(Genus_),
							Copula = mean(Copula),
							MatingSuccess = mean(MatingSuccess_rel_pop),
							Eggs = mean(Eggs),
							#Fecundity = mean(Fecundity_rel_pop),
							LD1 = mean(LD1),
							LD12 = mean(LD12),
							PC1_size = mean(PC1_size),
							PC1_size2 = mean(PC1_size2))

all_ave_Hind <- subset(DataPC, WingID=="Hindwing") %>% group_by(Ind_ID) %>%
				summarise(	Ind_ID = unique(Ind_ID),
							Season = unique(Season),
							Locale = unique(Locale),
							Sex = unique(Sex),
							Genus = unique(Genus_),
							Copula = mean(Copula),
							MatingSuccess = mean(MatingSuccess_rel_pop),
							Eggs = mean(Eggs),
							#Fecundity = mean(Fecundity_rel_pop),
							LD1 = mean(LD1),
							LD12 = mean(LD12),
							PC1_size = mean(PC1_size),
							PC1_size2 = mean(PC1_size2))

ForeMale <- subset(all_ave_Fore, Sex =="Male") %>% group_by(Season, Locale, Genus) %>% mutate (n_beta = n())
HindMale <- subset(all_ave_Hind, Sex =="Male") %>% group_by(Season, Locale, Genus) %>% mutate (n_beta = n())
ForeFemale <- subset(all_ave_Fore, Sex =="Female"&Copula=="1") %>% group_by(Season, Genus, Locale) %>% 
					mutate (	n_beta = n(),
									female_mean_fitness = mean(Eggs),
									Fecundity = Eggs/female_mean_fitness)
HindFemale <- subset(all_ave_Hind, Sex =="Female"&Copula=="1") %>% group_by(Season, Genus, Locale) %>% 
					mutate (	n_beta = n(),
									female_mean_fitness = mean(Eggs),
									Fecundity = Eggs/female_mean_fitness)

#Beta of body size

Male_directional_PC1_size_run <- subset(ForeMale, n_beta > 20) %>% group_by(Season, Genus, Locale) %>% 
  do(tidy(lm(MatingSuccess ~ PC1_size, data = .), conf.int = TRUE))

Male_directional_PC1_size <- subset(Male_directional_PC1_size_run, term=="PC1_size")%>%
					mutate(	mbeta = estimate,
									mbeta_se = std.error,
									mbeta_p = p.value,
									community_ID = paste(Genus, Locale, Season, sep = "_")) %>% 
					dplyr::select(community_ID,	mbeta, mbeta_se, mbeta_p)

Female_directional_PC1_size_run <- subset(ForeFemale, n_beta > 20) %>% group_by(Season, Genus, Locale) %>% 
  do(tidy(lm(Fecundity ~ PC1_size, data = .), conf.int = TRUE))

Female_directional_PC1_size <- subset(Female_directional_PC1_size_run, term=="PC1_size")%>%
					mutate(	fbeta = estimate,
									fbeta_se = std.error,
									fbeta_p = p.value,
									community_ID = paste(Genus, Locale, Season, sep = "_")) %>% ungroup() %>%
					dplyr::select(community_ID,	fbeta, fbeta_se, fbeta_p)

beta_ecology <- 	left_join(full_ecology_data, Male_directional_PC1_size, by = "community_ID") %>%
							left_join(., Female_directional_PC1_size,  by = "community_ID")

mbeta_mod1 <- lm(mbeta~ MaleDensity_mean+Species, weights = 1/mbeta_se, data = subset(beta_ecology))
mbeta_mod2 <- lm(mbeta~ FemaleDensity_mean+Species, weights = 1/mbeta_se, data = subset(beta_ecology))
mbeta_mod3 <- lm(mbeta~ OSR+Species, weights = 1/mbeta_se, data = subset(beta_ecology))
mbeta_mod4 <- lm(mbeta~ AndrochromeFrequency+Species, weights = 1/mbeta_se, data = subset(beta_ecology))
mbeta_mod5 <- lm(mbeta~ ProportionCopula_mean+Species, weights = 1/mbeta_se, data = subset(beta_ecology))
mbeta_mod6 <- lm(mbeta~ OppSel+Species, weights = 1/mbeta_se, data = subset(beta_ecology))

#Table S8

summary(mbeta_mod1)
summary(mbeta_mod2)
summary(mbeta_mod3)
summary(mbeta_mod4)
summary(mbeta_mod5)
summary(mbeta_mod6)

#############################
#############################
#############################

# Forewing LD1

Fore_Male_directional_LD1_run <- subset(ForeMale, n_beta > 20) %>% group_by(Season, Genus, Locale) %>% 
  do(tidy(lm(MatingSuccess ~ LD1, data = .), conf.int = TRUE))

Fore_Male_directional_LD1 <- subset(Fore_Male_directional_LD1_run, term=="LD1")%>%
					mutate(	mbeta = estimate,
									mbeta_se = std.error,
									mbeta_p = p.value,
									community_ID = paste(Genus, Locale, Season, sep = "_")) %>% 
					dplyr::select(community_ID,	mbeta, mbeta_se, mbeta_p)

Fore_Female_directional_LD1_run <- subset(ForeFemale, n_beta > 20) %>% group_by(Season, Genus, Locale) %>% 
  do(tidy(lm(Fecundity ~ LD1, data = .), conf.int = TRUE))

Fore_Female_directional_LD1 <- subset(Fore_Female_directional_LD1_run, term=="LD1")%>%
					mutate(	fbeta = estimate,
									fbeta_se = std.error,
									fbeta_p = p.value,
									community_ID = paste(Genus, Locale, Season, sep = "_")) %>% ungroup() %>%
					dplyr::select(community_ID,	fbeta, fbeta_se, fbeta_p)

fore_beta_ecology <- 	left_join(full_ecology_data, Fore_Male_directional_LD1, by = "community_ID") %>%
							left_join(., Fore_Female_directional_LD1,  by = "community_ID")

fore_mbeta_mod1 <- lm(mbeta ~ MaleDensity_mean + Species, weights = 1/mbeta_se, data = subset(fore_beta_ecology))
fore_mbeta_mod2 <- lm(mbeta ~ FemaleDensity_mean+ Species, weights = 1/mbeta_se, data = subset(fore_beta_ecology))
fore_mbeta_mod3 <- lm(mbeta ~ OSR+ Species, weights = 1/mbeta_se, data = subset(fore_beta_ecology))
fore_mbeta_mod4 <- lm(mbeta ~ AndrochromeFrequency+ Species, weights = 1/mbeta_se, data = subset(fore_beta_ecology))
fore_mbeta_mod5 <- lm(mbeta ~ ProportionCopula_mean+ Species, weights = 1/mbeta_se, data = subset(fore_beta_ecology))
fore_mbeta_mod6 <- lm(mbeta ~ OppSel+ Species, weights = 1/mbeta_se, data = subset(fore_beta_ecology))

#Table S9, top

summary(fore_mbeta_mod1)
summary(fore_mbeta_mod2)
summary(fore_mbeta_mod3)
summary(fore_mbeta_mod4)
summary(fore_mbeta_mod5)
summary(fore_mbeta_mod6)

#For this plot, I want to integrate mean shape data.

Male_LD1 <- subset(ForeMale, n_beta > 20) %>% group_by(Season, Genus, Locale) %>% 
					dplyr::select(n_beta, LD1) %>%
					summarise(	male_mean_LD1 = mean(LD1)) %>%
					mutate(community_ID = paste(Genus, Locale, Season, sep = "_")) %>% ungroup() %>%
					dplyr::select(community_ID, male_mean_LD1 )

Female_LD1 <- subset(ForeFemale, n_beta > 20) %>% group_by(Season, Genus, Locale) %>% 
					dplyr::select(n_beta, LD1) %>%
					summarise(	female_mean_LD1 = mean(LD1)) %>%
					mutate(community_ID = paste(Genus, Locale, Season, sep = "_")) %>% ungroup() %>%
					dplyr::select(community_ID, female_mean_LD1 )

fore_beta_summary <- 	left_join(fore_beta_ecology, Male_LD1, by = "community_ID") %>%
							left_join(., Female_LD1,  by = "community_ID")

fore_mbeta_mod7 <- lm(mbeta ~ male_mean_LD1 + Species, weights = 1/mbeta_se, data = subset(fore_beta_summary))
summary(fore_mbeta_mod7)

#############################
#############################
#############################

# Hindwing LD1

Hind_Male_directional_LD1_run <- subset(HindMale, n_beta > 20) %>% group_by(Season, Genus, Locale) %>% 
  do(tidy(lm(MatingSuccess ~ LD1, data = .), conf.int = TRUE))

Hind_Male_directional_LD1 <- subset(Hind_Male_directional_LD1_run, term=="LD1")%>%
					mutate(	mbeta = estimate,
									mbeta_se = std.error,
									mbeta_p = p.value,
									community_ID = paste(Genus, Locale, Season, sep = "_")) %>% 
					dplyr::select(community_ID,	mbeta, mbeta_se, mbeta_p)

Hind_Female_directional_LD1_run <- subset(HindFemale, n_beta > 20) %>% group_by(Season, Genus, Locale) %>% 
  do(tidy(lm(Fecundity ~ LD1, data = .), conf.int = TRUE))

Hind_Female_directional_LD1 <- subset(Hind_Female_directional_LD1_run, term=="LD1")%>%
					mutate(	fbeta = estimate,
									fbeta_se = std.error,
									fbeta_p = p.value,
									community_ID = paste(Genus, Locale, Season, sep = "_")) %>% ungroup() %>%
					dplyr::select(community_ID,	fbeta, fbeta_se, fbeta_p)

Hind_beta_ecology <- 	left_join(full_ecology_data, Hind_Male_directional_LD1, by = "community_ID") %>%
							left_join(., Hind_Female_directional_LD1,  by = "community_ID")

Hind_mbeta_mod1 <- lm(mbeta ~ MaleDensity_mean + Species, weights = 1/mbeta_se, data = subset(Hind_beta_ecology))
Hind_mbeta_mod2 <- lm(mbeta ~ FemaleDensity_mean+ Species, weights = 1/mbeta_se, data = subset(Hind_beta_ecology))
Hind_mbeta_mod3 <- lm(mbeta ~ OSR+ Species, weights = 1/mbeta_se, data = subset(Hind_beta_ecology))
Hind_mbeta_mod4 <- lm(mbeta ~ AndrochromeFrequency+ Species, weights = 1/mbeta_se, data = subset(Hind_beta_ecology))
Hind_mbeta_mod5 <- lm(mbeta ~ ProportionCopula_mean+ Species, weights = 1/mbeta_se, data = subset(Hind_beta_ecology))
Hind_mbeta_mod6 <- lm(mbeta ~ OppSel+ Species, weights = 1/mbeta_se, data = subset(Hind_beta_ecology))


#Table S9, bottom

summary(Hind_mbeta_mod1)
summary(Hind_mbeta_mod2)
summary(Hind_mbeta_mod3)
summary(Hind_mbeta_mod4)
summary(Hind_mbeta_mod5)
summary(Hind_mbeta_mod6)

hind_Male_LD1 <- subset(HindMale, n_beta > 20) %>% group_by(Season, Genus, Locale) %>% 
					dplyr::select(n_beta, LD1) %>%
					summarise(	male_mean_LD1 = mean(LD1)) %>%
					mutate(community_ID = paste(Genus, Locale, Season, sep = "_")) %>% ungroup() %>%
					dplyr::select(community_ID, male_mean_LD1 )

hind_Female_LD1 <- subset(HindFemale, n_beta > 20) %>% group_by(Season, Genus, Locale) %>% 
					dplyr::select(n_beta, LD1) %>%
					summarise(	female_mean_LD1 = mean(LD1)) %>%
					mutate(community_ID = paste(Genus, Locale, Season, sep = "_")) %>% ungroup() %>%
					dplyr::select(community_ID, female_mean_LD1 )

hind_beta_summary <- 	left_join(Hind_beta_ecology, hind_Male_LD1, by = "community_ID") %>%
							left_join(., hind_Female_LD1,  by = "community_ID")

hind_mbeta_mod7 <- lm(mbeta ~ male_mean_LD1 + Species, weights = 1/mbeta_se, data = subset(hind_beta_summary))
summary(hind_mbeta_mod7)

#############################
#############################
#############################

#Plot

#Figure 5

OSR_size = ggplot(data = subset(beta_ecology, ! mbeta == "NA"&!community_ID == "Ischnura_elegans_Hoje_A_7_2021"), aes(x = OSR, y= mbeta, color = Species)) +
	geom_point(alpha = .5, stroke = .5) + 
	geom_segment(data = . %>% filter (Species == "Ischnura_elegans"), aes(x = min(OSR), xend = max(OSR), y = min(OSR)* as.vector(mbeta_mod3$ coefficients[2]) + as.vector(mbeta_mod3$ coefficients[3] + mbeta_mod3$ coefficients[1]), yend =  max(OSR)* as.vector(mbeta_mod3$ coefficients[2]) + as.vector(mbeta_mod3$ coefficients[3] + mbeta_mod3$ coefficients[1])), lty = 1, alpha = .5)+
	geom_segment(data = . %>% filter (Species == "Enallagma_cyathigerum"), aes(x = min(OSR), xend = max(OSR), y = min(OSR)* as.vector(mbeta_mod3$ coefficients[2]) + as.vector(mbeta_mod3$ coefficients[1]), yend =  max(OSR)* as.vector(mbeta_mod3$ coefficients[2]) + as.vector(mbeta_mod3$ coefficients[1])), lty = 2, alpha = .5)+
	scale_colour_discrete("Species", labels = c(expression(italic("Enallagma cyathigerum")),expression(italic("Ischnura elegans"))))+
	xlab("OSR") + ylab(expression(beta["male"]*" (Size PC1)")) + 
	theme_classic()+
	annotate("text", x = 9, y = -2, label = expression(r^2*" = 47.1%"))+
	geom_errorbar(aes(ymin = mbeta - mbeta_se, ymax = mbeta + mbeta_se), color = "black", lty = 1, alpha = 0.2, width = 0)+
	theme(	legend.position = "none",
			plot.margin = margin(5,2.5,2.5,2.5, "mm"),
			legend.title = element_blank())

Andro_size = ggplot(data = subset(beta_ecology, ! mbeta == "NA"&!community_ID == "Ischnura_elegans_Hoje_A_7_2021"), aes(x = AndrochromeFrequency, y= mbeta, color = Species)) +
	geom_point(alpha = .5, stroke = .5) + 
	geom_segment(data = . %>% filter (Species == "Ischnura_elegans"), aes(x = min(AndrochromeFrequency), xend = max(AndrochromeFrequency), y = min(AndrochromeFrequency)* as.vector(mbeta_mod4$ coefficients[2]) + as.vector(mbeta_mod4$ coefficients[3] + mbeta_mod4$ coefficients[1]), yend =  max(AndrochromeFrequency)* as.vector(mbeta_mod4$ coefficients[2]) + as.vector(mbeta_mod4$ coefficients[3] + mbeta_mod4$ coefficients[1])), lty = 1, alpha = .5)+
	geom_segment(data = . %>% filter (Species == "Enallagma_cyathigerum"), aes(x = min(AndrochromeFrequency), xend = max(AndrochromeFrequency), y = min(AndrochromeFrequency)* as.vector(mbeta_mod4$ coefficients[2]) + as.vector(mbeta_mod4$ coefficients[1]), yend =  max(AndrochromeFrequency)* as.vector(mbeta_mod4$ coefficients[2]) + as.vector(mbeta_mod4$ coefficients[1])), lty = 2, alpha = .5)+
	scale_colour_discrete("Species", labels = c(expression(italic("Enallagma cyathigerum")),expression(italic("Ischnura elegans"))))+
	xlab("Androchrome Frequency") + ylab(expression(beta["male"]*" (Size PC1)")) + 
	annotate("text", x = 0.15, y = -2, label = expression(r^2*" = 6.0%"))+
	theme_classic()+
	geom_errorbar(aes(ymin = mbeta - mbeta_se, ymax = mbeta + mbeta_se), color = "black", lty = 1, alpha = 0.2, width = 0)+
	theme(	legend.position = "none",
			axis.title.y = element_blank(),
			plot.margin = margin(5,2.5,2.5,2.5, "mm"),
			legend.title = element_blank())

OppSel_size = ggplot(data = subset(beta_ecology, ! mbeta == "NA"&!community_ID == "Ischnura_elegans_Hoje_A_7_2021"), aes(x = OppSel, y= mbeta, color = Species)) +
	geom_point(alpha = .5, stroke = .5) + 
	geom_segment(data = . %>% filter (Species == "Ischnura_elegans"), aes(x = min(OppSel), xend = max(OppSel), y = min(OppSel)* as.vector(mbeta_mod6$ coefficients[2]) + as.vector(mbeta_mod6$ coefficients[3] + mbeta_mod6$ coefficients[1]), yend =  max(OppSel)* as.vector(mbeta_mod6$ coefficients[2]) + as.vector(mbeta_mod6$ coefficients[3] + mbeta_mod6$ coefficients[1])), lty = 1, alpha = .5)+
	geom_segment(data = . %>% filter (Species == "Enallagma_cyathigerum"), aes(x = min(OppSel), xend = max(OppSel), y = min(OppSel)* as.vector(mbeta_mod6$ coefficients[2]) + as.vector(mbeta_mod6$ coefficients[1]), yend =  max(OppSel)* as.vector(mbeta_mod6$ coefficients[2]) + as.vector(mbeta_mod6$ coefficients[1])), lty = 2, alpha = .5)+
	scale_colour_discrete("Species", labels = c(expression(italic("Enallagma cyathigerum")),expression(italic("Ischnura elegans"))))+
	xlab("Opportunity for Selection") + ylab(expression(beta["male"]*" (Size PC1)")) + 
	theme_classic()+
	annotate("text", x = 13, y = -2, label = expression(r^2*" = 21.2%"))+
	geom_errorbar(aes(ymin = mbeta - mbeta_se, ymax = mbeta + mbeta_se), color = "black", lty = 1, alpha = 0.2, width = 0)+
	theme(	axis.title.y = element_blank(),
			plot.margin = margin(5,2.5,2.5,2.5, "mm"),
			legend.title = element_blank())

ggarrange(OSR_size, Andro_size, OppSel_size, ncol = 3, nrow = 1, labels = "AUTO", font.label = list(size = 12), common.legend = TRUE, legend = "bottom")

#############################
#############################
#############################

#Figure S8

OSR_fore = ggplot(data = subset(fore_beta_ecology, ! mbeta == "NA"&!community_ID == "Ischnura_elegans_Hoje_A_7_2021"), aes(x = OSR, y= mbeta, color = Species)) +
	geom_point(alpha = .5, stroke = .5) + 
	geom_segment(data = . %>% filter (Species == "Ischnura_elegans"), aes(x = min(OSR), xend = max(OSR), y = min(OSR)* as.vector(fore_mbeta_mod3$ coefficients[2]) + as.vector(fore_mbeta_mod3$ coefficients[3] + fore_mbeta_mod3$ coefficients[1]), yend =  max(OSR)* as.vector(fore_mbeta_mod3$ coefficients[2]) + as.vector(fore_mbeta_mod3$ coefficients[3] + fore_mbeta_mod3$ coefficients[1])), lty = 1, alpha = .5)+
	geom_segment(data = . %>% filter (Species == "Enallagma_cyathigerum"), aes(x = min(OSR), xend = max(OSR), y = min(OSR)* as.vector(fore_mbeta_mod3$ coefficients[2]) + as.vector(fore_mbeta_mod3$ coefficients[1]), yend =  max(OSR)* as.vector(fore_mbeta_mod3$ coefficients[2]) + as.vector(fore_mbeta_mod3$ coefficients[1])), lty = 2, alpha = .5)+
	scale_colour_discrete("Species", labels = c(expression(italic("Enallagma cyathigerum")),expression(italic("Ischnura elegans"))))+
	xlab("OSR") + ylab(expression(beta*" (Forewing LD1)")) + 
	theme_classic()+
	annotate("text", x = 9, y = -0.5, label = expression(r^2*" = 34.3%"))+
	geom_errorbar(aes(ymin = mbeta - mbeta_se, ymax = mbeta + mbeta_se), color = "black", lty = 1, alpha = 0.2, width = 0)+
	theme(	legend.position = "none",
			plot.margin = margin(5,2.5,2.5,2.5, "mm"),
			legend.title = element_blank())

Andro_fore = ggplot(data = subset(fore_beta_ecology, ! mbeta == "NA"&!community_ID == "Ischnura_elegans_Hoje_A_7_2021"), aes(x = AndrochromeFrequency, y= mbeta, color = Species)) +
	geom_point(alpha = .5, stroke = .5) + 
	geom_segment(data = . %>% filter (Species == "Ischnura_elegans"), aes(x = min(AndrochromeFrequency), xend = max(AndrochromeFrequency), y = min(AndrochromeFrequency)* as.vector(fore_mbeta_mod4$ coefficients[2]) + as.vector(fore_mbeta_mod4$ coefficients[3] + fore_mbeta_mod4$ coefficients[1]), yend =  max(AndrochromeFrequency)* as.vector(fore_mbeta_mod4$ coefficients[2]) + as.vector(fore_mbeta_mod4$ coefficients[3] + fore_mbeta_mod4$ coefficients[1])), lty = 1, alpha = .5)+
	geom_segment(data = . %>% filter (Species == "Enallagma_cyathigerum"), aes(x = min(AndrochromeFrequency), xend = max(AndrochromeFrequency), y = min(AndrochromeFrequency)* as.vector(fore_mbeta_mod4$ coefficients[2]) + as.vector(fore_mbeta_mod4$ coefficients[1]), yend =  max(AndrochromeFrequency)* as.vector(fore_mbeta_mod4$ coefficients[2]) + as.vector(fore_mbeta_mod4$ coefficients[1])), lty = 2, alpha = .5)+
	scale_colour_discrete("Species", labels = c(expression(italic("Enallagma cyathigerum")),expression(italic("Ischnura elegans"))))+
	xlab("Androchrome Frequency") + ylab(expression(beta*" (Forewing LD1)")) + 
	annotate("text", x = 0.15, y = -0.5, label = expression(r^2*" = 13.2%"))+
	theme_classic()+
	geom_errorbar(aes(ymin = mbeta - mbeta_se, ymax = mbeta + mbeta_se), color = "black", lty = 1, alpha = 0.2, width = 0)+
	theme(	legend.position = "none",
			axis.title.y = element_blank(),
			plot.margin = margin(5,2.5,2.5,2.5, "mm"),
			legend.title = element_blank())

OppSel_fore = ggplot(data = subset(fore_beta_ecology, ! mbeta == "NA"&!community_ID == "Ischnura_elegans_Hoje_A_7_2021"), aes(x = OppSel, y= mbeta, color = Species)) +
	geom_point(alpha = .5, stroke = .5) + 
	geom_segment(data = . %>% filter (Species == "Ischnura_elegans"), aes(x = min(OppSel), xend = max(OppSel), y = min(OppSel)* as.vector(fore_mbeta_mod6$ coefficients[2]) + as.vector(fore_mbeta_mod6 $ coefficients[3] + fore_mbeta_mod6 $ coefficients[1]), yend =  max(OppSel)* as.vector(fore_mbeta_mod6 $ coefficients[2]) + as.vector(fore_mbeta_mod6 $ coefficients[3] + fore_mbeta_mod6 $ coefficients[1])), lty = 1, alpha = .5)+
		geom_segment(data = . %>% filter (Species == "Enallagma_cyathigerum"), aes(x = min(OppSel), xend = max(OppSel), y = min(OppSel)* as.vector(fore_mbeta_mod6$ coefficients[2]) + as.vector(fore_mbeta_mod6 $ coefficients[1] + fore_mbeta_mod6 $ coefficients[1]), yend =  max(OppSel)* as.vector(fore_mbeta_mod6 $ coefficients[2]) + as.vector(fore_mbeta_mod6 $ coefficients[1] + fore_mbeta_mod6 $ coefficients[1])), lty = 1, alpha = .5)+
	scale_colour_discrete("Species", labels = c(expression(italic("Enallagma cyathigerum")),expression(italic("Ischnura elegans"))))+
	xlab("Opportunity for Selection") + ylab(expression(beta*" (Forewing LD1)")) + 
	annotate("text", x = 5, y = -0.5, label = expression(r^2*" = 19.9%"))+
	theme_classic()+
	geom_errorbar(aes(ymin = mbeta - mbeta_se, ymax = mbeta + mbeta_se), color = "black", lty = 1, alpha = 0.2, width = 0)+
	theme(	legend.position = "none",
			axis.title.y = element_blank(),
			plot.margin = margin(5,2.5,2.5,2.5, "mm"),
			legend.title = element_blank())

size_fore = ggplot(data = subset(fore_beta_summary, ! mbeta == "NA"&!community_ID == "Ischnura_elegans_Hoje_A_7_2021"), aes(x = male_mean_LD1, y= mbeta, color = Species)) +
	geom_point(alpha = .5, stroke = .5) + 
	geom_segment(data = . %>% filter (Species == "Ischnura_elegans"), aes(x = min(male_mean_LD1), xend = max(male_mean_LD1), y = min(male_mean_LD1)* as.vector(fore_mbeta_mod7$ coefficients[2]) + as.vector(fore_mbeta_mod7 $ coefficients[3] + fore_mbeta_mod7 $ coefficients[1]), yend =  max(male_mean_LD1)* as.vector(fore_mbeta_mod7 $ coefficients[2]) + as.vector(fore_mbeta_mod7 $ coefficients[3] + fore_mbeta_mod7 $ coefficients[1])), lty = 1, alpha = .5)+
	geom_segment(data = . %>% filter (Species == "Enallagma_cyathigerum"), aes(x = min(male_mean_LD1), xend = max(male_mean_LD1), y = min(male_mean_LD1)* as.vector(fore_mbeta_mod7$ coefficients[2]) + as.vector(fore_mbeta_mod7$ coefficients[1]), yend =  max(male_mean_LD1)* as.vector(fore_mbeta_mod7$ coefficients[2]) + as.vector(fore_mbeta_mod7$ coefficients[1])), lty = 2, alpha = .5)+
	scale_colour_discrete("Species", labels = c(expression(italic("Enallagma cyathigerum")),expression(italic("Ischnura elegans"))))+
	xlab("Forewing LD1") + ylab(expression(beta*" (Forewing LD1)")) + 
	theme_classic()+
	annotate("text", x = 0.183, y = -0.5, label = expression(r^2*" = 2.6%"))+
	geom_errorbar(aes(ymin = mbeta - mbeta_se, ymax = mbeta + mbeta_se), color = "black", lty = 1, alpha = 0.2, width = 0)+
	theme(	axis.title.y = element_blank(),
			plot.margin = margin(5,2.5,2.5,2.5, "mm"),
			legend.title = element_blank())

OSR_Hind = ggplot(data = subset(Hind_beta_ecology, ! mbeta == "NA"&!community_ID == "Ischnura_elegans_Hoje_A_7_2021"), aes(x = OSR, y= mbeta, color = Species)) +
	geom_point(alpha = .5, stroke = .5) + 
	geom_segment(data = . %>% filter (Species == "Ischnura_elegans"), aes(x = min(OSR), xend = max(OSR), y = min(OSR)* as.vector(Hind_mbeta_mod3$ coefficients[2]) + as.vector(Hind_mbeta_mod3$ coefficients[3] + Hind_mbeta_mod3$ coefficients[1]), yend =  max(OSR)* as.vector(Hind_mbeta_mod3$ coefficients[2]) + as.vector(Hind_mbeta_mod3$ coefficients[3] + Hind_mbeta_mod3$ coefficients[1])), lty = 1, alpha = .5)+
	geom_segment(data = . %>% filter (Species == "Enallagma_cyathigerum"), aes(x = min(OSR), xend = max(OSR), y = min(OSR)* as.vector(Hind_mbeta_mod3$ coefficients[2]) + as.vector(Hind_mbeta_mod3$ coefficients[1]), yend =  max(OSR)* as.vector(Hind_mbeta_mod3$ coefficients[2]) + as.vector(Hind_mbeta_mod3$ coefficients[1])), lty = 2, alpha = .5)+
	scale_colour_discrete("Species", labels = c(expression(italic("Enallagma cyathigerum")),expression(italic("Ischnura elegans"))))+
	xlab("OSR") + ylab(expression(beta*" (Hindwing LD1)")) + 
	theme_classic()+
	annotate("text", x = 9, y = -0.3, label = expression(r^2*" = 18.9%"))+
	geom_errorbar(aes(ymin = mbeta - mbeta_se, ymax = mbeta + mbeta_se), color = "black", lty = 1, alpha = 0.2, width = 0)+
	theme(	legend.position = "none",
			plot.margin = margin(5,2.5,2.5,2.5, "mm"),
			legend.title = element_blank())

Andro_Hind = ggplot(data = subset(Hind_beta_ecology, ! mbeta == "NA"&!community_ID == "Ischnura_elegans_Hoje_A_7_2021"), aes(x = AndrochromeFrequency, y= mbeta, color = Species)) +
	geom_point(alpha = .5, stroke = .5) + 
	geom_segment(data = . %>% filter (Species == "Ischnura_elegans"), aes(x = min(AndrochromeFrequency), xend = max(AndrochromeFrequency), y = min(AndrochromeFrequency)* as.vector(Hind_mbeta_mod4$ coefficients[2]) + as.vector(Hind_mbeta_mod4$ coefficients[3] + Hind_mbeta_mod4$ coefficients[1]), yend =  max(AndrochromeFrequency)* as.vector(Hind_mbeta_mod4$ coefficients[2]) + as.vector(Hind_mbeta_mod4$ coefficients[3] + Hind_mbeta_mod4$ coefficients[1])), lty = 1, alpha = .5)+
	geom_segment(data = . %>% filter (Species == "Enallagma_cyathigerum"), aes(x = min(AndrochromeFrequency), xend = max(AndrochromeFrequency), y = min(AndrochromeFrequency)* as.vector(Hind_mbeta_mod4$ coefficients[2]) + as.vector(Hind_mbeta_mod4$ coefficients[1]), yend =  max(AndrochromeFrequency)* as.vector(Hind_mbeta_mod4$ coefficients[2]) + as.vector(Hind_mbeta_mod4$ coefficients[1])), lty = 2, alpha = .5)+
	scale_colour_discrete("Species", labels = c(expression(italic("Enallagma cyathigerum")),expression(italic("Ischnura elegans"))))+
	xlab("Androchrome Frequency") + ylab(expression(beta*" (Hindwing LD1)")) + 
	annotate("text", x = 0.15, y = -0.3, label = expression(r^2*" = 2.1%"))+
	theme_classic()+
	geom_errorbar(aes(ymin = mbeta - mbeta_se, ymax = mbeta + mbeta_se), color = "black", lty = 1, alpha = 0.2, width = 0)+
	theme(	legend.position = "none",
			axis.title.y = element_blank(),
			plot.margin = margin(5,2.5,2.5,2.5, "mm"),
			legend.title = element_blank())

OppSel_hind = ggplot(data = subset(hind_beta_summary, ! mbeta == "NA"&!community_ID == "Ischnura_elegans_Hoje_A_7_2021"), aes(x = OppSel, y= mbeta, color = Species)) +
	geom_point(alpha = .5, stroke = .5) + 
	geom_segment(data = . %>% filter (Species == "Ischnura_elegans"), aes(x = min(OppSel), xend = max(OppSel), y = min(OppSel)* as.vector(Hind_mbeta_mod6 $ coefficients[2]) + as.vector(Hind_mbeta_mod6 $ coefficients[3] + Hind_mbeta_mod6 $ coefficients[1]), yend =  max(OppSel)* as.vector(Hind_mbeta_mod6 $ coefficients[2]) + as.vector(Hind_mbeta_mod6 $ coefficients[3] + Hind_mbeta_mod6 $ coefficients[1])), lty = 1, alpha = .5)+
		geom_segment(data = . %>% filter (Species == "Enallagma_cyathigerum"), aes(x = min(OppSel), xend = max(OppSel), y = min(OppSel)* as.vector(Hind_mbeta_mod6 $ coefficients[2]) + as.vector(Hind_mbeta_mod6 $ coefficients[1] + Hind_mbeta_mod6 $ coefficients[1]), yend =  max(OppSel)* as.vector(Hind_mbeta_mod6 $ coefficients[2]) + as.vector(Hind_mbeta_mod6 $ coefficients[1] + Hind_mbeta_mod6 $ coefficients[1])), lty = 1, alpha = .5)+
	scale_colour_discrete("Species", labels = c(expression(italic("Enallagma cyathigerum")),expression(italic("Ischnura elegans"))))+
	xlab("Opportunity for Selection") + ylab(expression(beta*" (Hindwing LD1)")) + 
	annotate("text", x = 5, y = -0.3, label = expression(r^2*" = 21.7%"))+
	theme_classic()+
	geom_errorbar(aes(ymin = mbeta - mbeta_se, ymax = mbeta + mbeta_se), color = "black", lty = 1, alpha = 0.2, width = 0)+
	theme(	legend.position = "none",
			axis.title.y = element_blank(),
			plot.margin = margin(5,2.5,2.5,2.5, "mm"),
			legend.title = element_blank())

size_hind_Isch = ggplot(data = subset(hind_beta_summary, ! mbeta == "NA"&!community_ID == "Ischnura_elegans_Hoje_A_7_2021"), aes(x = male_mean_LD1, y= mbeta, color = Species)) +
	geom_point(alpha = .5, stroke = .5) + 
	geom_segment(data = . %>% filter (Species == "Ischnura_elegans"), aes(x = min(male_mean_LD1), xend = max(male_mean_LD1), y = min(male_mean_LD1)* as.vector(hind_mbeta_mod7$ coefficients[2]) + as.vector(hind_mbeta_mod7 $ coefficients[3] + hind_mbeta_mod7 $ coefficients[1]), yend =  max(male_mean_LD1)* as.vector(hind_mbeta_mod7 $ coefficients[2]) + as.vector(hind_mbeta_mod7 $ coefficients[3] + hind_mbeta_mod7 $ coefficients[1])), lty = 1, alpha = .5)+
	geom_segment(data = . %>% filter (Species == "Enallagma_cyathigerum"), aes(x = min(male_mean_LD1), xend = max(male_mean_LD1), y = min(male_mean_LD1)* as.vector(hind_mbeta_mod7$ coefficients[2]) + as.vector(hind_mbeta_mod7$ coefficients[1]), yend =  max(male_mean_LD1)* as.vector(hind_mbeta_mod7$ coefficients[2]) + as.vector(hind_mbeta_mod7$ coefficients[1])), lty = 2, alpha = .5)+
	scale_colour_discrete("Species", labels = c(expression(italic("Enallagma cyathigerum")),expression(italic("Ischnura elegans"))))+
	xlab("Hindwing LD1") + ylab(expression(beta*" (Hindwing LD1)")) + 
	theme_classic()+
	geom_errorbar(aes(ymin = mbeta - mbeta_se, ymax = mbeta + mbeta_se), color = "black", lty = 1, alpha = 0.2, width = 0)+
	theme(	axis.title.y = element_blank(),
			plot.margin = margin(5,2.5,2.5,2.5, "mm"),
			legend.title = element_blank())

ggarrange(OSR_fore ,Andro_fore , OppSel_fore , OSR_Hind, Andro_Hind , OppSel_hind, ncol = 3, nrow = 2, labels = "AUTO", common.legend = TRUE, legend = "bottom")