#Checked on 2025.02.21 for JEB resubmission

setwd("~/Dropbox/Enallagma_Anjali_2021/ManuscriptWingShape_Data_Rcode")

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
library(cowplot)
library(readxl)
library(ggplot2)
library(ggh4x)
library(ggpubr)

#############################
#############################
#############################

#Figure S10

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

#Opportunity for sexual selection

Is <- read.csv("Is_updated.csv")

EC <- Is[Is$Species == "Enallagma_cyathigerum", ]
IE <- Is[Is$Species == "Ischnura_elegans", ]


#Multipanel plot

Is1 <- ggplot(Is, aes(x=Species, y=Is/MaleMatingSuccess^2, fill=Species)) + 
			geom_boxplot(outlier.shape = NA, size = .1) + 
			geom_jitter(size=1, alpha=0.05, width = .1) + 
			labs(y = "Opportunity for sexual selection", x = "") + 
			#coord_cartesian(ylim = c(0, 2750)) + 
			theme_classic() + 
			theme(text = element_text(size = 8)) + 
			theme(axis.text.x = element_blank()) + 
			theme(aspect.ratio = 1) +
  			annotate('text', x = 1.4, y = 25, size = 4, label = '* *', col = 'black') +
  			geom_segment(aes(x = 1.1, y = 25, xend = 1.8, yend = 25), col = 'black', lwd = 0.2) +
  			geom_segment(aes(x = 1.4, y = 25, xend = 1.4, yend = 27), col = 'black', lwd = 0.2) +
  			geom_segment(aes(x = 1.1, y = 25, xend = 1.1, yend = 23), col = 'black', lwd = 0.2) +
  			geom_segment(aes(x = 1.8, y = 25, xend = 1.8, yend = 23), col = 'black', lwd = 0.2)

OSR1 <- ggplot(File, aes(x=Species, y = OSR, fill=Species)) + 
			geom_boxplot(outlier.shape = NA, size = .1) + 
			geom_jitter(size=1, alpha=0.05, width = .1) + 
			labs(y = "Operational Sex Ratio", x = "") + 
			coord_cartesian(ylim = c(0,22)) + 
			theme_classic() + 
			theme(text = element_text(size = 8)) + 
			theme(axis.text.x = element_blank()) + 
			theme(aspect.ratio = 1) + 
			guides(colour=guide_legend("Species")) + 
			scale_colour_discrete("Species", labels = c(expression(italic("Enallagma cyathigerum")),expression(italic("Ischnura elegans"))))  +
  			annotate('text', x = 1.45, y = 16.8, size = 4, label = '* * *', col = 'black') +
  			geom_segment(aes(x = 1.1, y = 15.45, xend = 1.8, yend = 15.45), col = 'black', lwd = 0.2) +
  			geom_segment(aes(x = 1.45, y = 15.45, xend = 1.45, yend = 16.475), col = 'black', lwd = 0.2) +
  			geom_segment(aes(x = 1.1, y = 15.45, xend = 1.1, yend = 14.475), col = 'black', lwd = 0.2) +
  			geom_segment(aes(x = 1.8, y = 15.45, xend = 1.8, yend = 14.475), col = 'black', lwd = 0.2)

PCM1 <- ggplot(Data6, aes(x=Species, y = ProportionCopula, fill=Species)) + 
			geom_boxplot(outlier.shape = NA, size = .1) + 
			geom_jitter(size=1, alpha=0.05, width = .1) + 
			labs(y = "Proportion Copulating Males", x = "") + 
			coord_cartesian(ylim = c(0, 0.7)) + 
			theme_classic() + 
			theme(text = element_text(size = 8)) + 
			theme(axis.text.x = element_blank()) + 
			theme(aspect.ratio = 1) + 
			guides(colour=guide_legend("Species")) + 
			scale_colour_discrete("Species", labels = c(expression(italic("Enallagma cyathigerum")),expression(italic("Ischnura elegans"))))  +
  			annotate('text', x = 1.45, y = 0.49, size = 4, label = '* * *', col = 'black') +
  			geom_segment(aes(x = 1.1, y = 0.45, xend = 1.8, yend = 0.45), col = 'black', lwd = 0.2) +
  			geom_segment(aes(x = 1.45, y = 0.45, xend = 1.45, yend = 0.475), col = 'black', lwd = 0.2) +
  			geom_segment(aes(x = 1.1, y = 0.45, xend = 1.1, yend = 0.4), col = 'black', lwd = 0.2) +
  			geom_segment(aes(x = 1.8, y = 0.45, xend = 1.8, yend = 0.4), col = 'black', lwd = 0.2)

AF1 <- ggplot(Data3, aes(x=Species, y = AndrochromeFrequency, fill=Species)) + 
			geom_boxplot(outlier.shape = NA, size = .1) + 
			geom_jitter(size=1, alpha=0.05, width = .1) + 
			labs(y = "Androchrome Frequency", x = "") + 
			theme_classic() + 
			theme(text = element_text(size = 8)) + 
			theme(axis.text.x = element_blank()) + 
			theme(aspect.ratio = 1) + 
			guides(colour=guide_legend("Species")) + 
			scale_colour_discrete("Species", labels = c(expression(italic("Enallagma cyathigerum")),expression(italic("Ischnura elegans"))))  +
  			annotate('text', x = 1.4, y = 1.8, size = 4, label = '* * *', col = 'black') +
  			geom_segment(aes(x = 1, y = 1.5, xend = 1.8, yend = 1.5), col = 'black', lwd = 0.2) +
  			geom_segment(aes(x = 1.4, y = 1.5, xend = 1.4, yend = 1.6), col = 'black', lwd = 0.2) +
 			geom_segment(aes(x = 1, y = 1.5, xend = 1, yend = 1.4), col = 'black', lwd = 0.2) +
  			geom_segment(aes(x = 1.8, y = 1.5, xend = 1.8, yend = 1.4), col = 'black', lwd = 0.2)

MLD1 <- ggplot(subset(Data1, Sex=="1"), aes(x=Species, y = Density, fill=Species)) + 
			geom_boxplot(outlier.shape = NA, size = .1) + 
			geom_jitter(size=1, alpha=0.05, width = .1) + 
			labs(y = "Male density", x = "") + 
			coord_cartesian(ylim = c(0,2.2)) + 
			theme_classic() + 
			theme(text = element_text(size = 8)) + 
			theme(axis.text.x = element_blank()) + 
			theme(aspect.ratio = 1) + 
			guides(colour=guide_legend("Species")) + 
			scale_colour_discrete("Species", labels = c(expression(italic("Enallagma cyathigerum")),expression(italic("Ischnura elegans"))))  +
  			annotate('text', x = 1.45, y = 1.1, size = 4, label = 'n.s.', col = 'black') +
  			geom_segment(aes(x = 1.1, y = 0.9, xend = 1.8, yend = 0.9), col = 'black', lwd = 0.2) +
  			geom_segment(aes(x = 1.45, y = 0.9, xend = 1.45, yend = 0.95), col = 'black', lwd = 0.2) +
  			geom_segment(aes(x = 1.1, y = 0.9, xend = 1.1, yend = 0.8), col = 'black', lwd = 0.2) +
  			geom_segment(aes(x = 1.8, y = 0.9, xend = 1.8, yend = 0.8), col = 'black', lwd = 0.2)

FLD1 <- ggplot(subset(Data1, Sex=="0"), aes(x=Species, y = Density, fill=Species)) + 
			geom_boxplot(outlier.shape = NA, size = .1) + 
			geom_jitter(size=1, alpha=0.05, width = .1) + 
			labs(y = "Female density", x = "") + 
			#coord_cartesian(ylim = c(0, 0.7)) + 
			theme_classic() + 
			theme(text = element_text(size = 8)) + 
			theme(axis.text.x = element_blank()) + 
			theme(aspect.ratio = 1) + 
			guides(colour=guide_legend("Species")) + 
			scale_colour_discrete("Species", labels = c(expression(italic("Enallagma cyathigerum")),expression(italic("Ischnura elegans"))))  +
  			annotate('text', x = 1.45, y = 0.49, size = 4, label = '* * *', col = 'black') +
  			geom_segment(aes(x = 1.1, y = 0.45, xend = 1.8, yend = 0.45), col = 'black', lwd = 0.2) +
  			geom_segment(aes(x = 1.45, y = 0.45, xend = 1.45, yend = 0.475), col = 'black', lwd = 0.2) +
  			geom_segment(aes(x = 1.1, y = 0.45, xend = 1.1, yend = 0.4), col = 'black', lwd = 0.2) +
  			geom_segment(aes(x = 1.8, y = 0.45, xend = 1.8, yend = 0.4), col = 'black', lwd = 0.2)

ggarrange(MLD1, FLD1, OSR1, AF1, PCM1, Is1, ncol = 3, nrow = 2, labels = "AUTO", common.legend = TRUE, legend = "bottom")

#############################
#############################
#############################

#Figure 3

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

MLD <- ggplot(ForeMale_ave, aes(LD1, MatingSuccess, colour = Genus)) + 
			geom_point(alpha=0.2) + 
			geom_smooth(method = "lm", formula = y ~ splines::bs(x, 3), size = 1) + 
			labs(y = "Relative fitness (male)", x = "Forewing shape (LD1)") + 
			scale_y_continuous(limits = c(0, 2.1))+
			theme_classic() + 
			theme(text = element_text(size = 11)) + 
			guides(colour=guide_legend("Species")) + 
			scale_colour_discrete("Species", labels = c(expression(italic("Enallagma cyathigerum")),expression(italic("Ischnura elegans")))) + 
			theme(	aspect.ratio = 1,
						strip.background = element_blank(),
						legend.position="none")

FLD <- ggplot(ForeFemale_ave, aes(LD1, Fecundity, colour = Genus)) + 
			geom_point(alpha=0.2) + 
			geom_smooth(method = "lm", formula = y ~ splines::bs(x, 3), size = 1) + 
			labs(y = "Relative fitness (female)", x = "Forewing shape (LD1)") + 
			ylim(c(0, 5.1)) + 
			theme_classic() + 
			theme(text = element_text(size = 11)) + 
			guides(colour=guide_legend("Species")) + 
			scale_colour_discrete("Species", labels = c(expression(italic("Enallagma cyathigerum")),expression(italic("Ischnura elegans")))) +
			theme(	aspect.ratio = 1,
						strip.background = element_blank(),
						legend.position="none")


MLDH <- ggplot(HindMale_ave, aes(LD1, MatingSuccess, colour = Genus)) + 
			geom_point(alpha=0.2) + 
			geom_smooth(method = "lm", formula = y ~ splines::bs(x, 3), size = 1) + 
			labs(y = "Relative fitness (male)", x = "Hindwing shape (LD1)") + 
			scale_y_continuous(limits = c(0, 2.1))+
			theme_classic() + 
			theme(text = element_text(size = 11)) + 
			guides(colour=guide_legend("Species")) + 
			scale_colour_discrete("Species", labels = c(expression(italic("Enallagma cyathigerum")),expression(italic("Ischnura elegans")))) + 
			theme(	aspect.ratio = 1,
						strip.background = element_blank(),
						legend.position="none")
			

FLDH <- ggplot(HindFemale_ave, aes(LD1, Fecundity, colour = Genus)) + 
			geom_point(alpha=0.2) + 
			geom_smooth(method = "lm", formula = y ~ splines::bs(x, 3), size = 1) + 
			labs(y = "Relative fitness (female)", x = "Hindwing shape (LD1)") + 
			ylim(c(0, 5.1)) + 
			theme_classic() + 
			theme(text = element_text(size = 11)) + 
			guides(colour=guide_legend("Species")) + 
			scale_colour_discrete("Species", labels = c(expression(italic("Enallagma cyathigerum")),expression(italic("Ischnura elegans")))) + 
			theme(	aspect.ratio = 1,
						strip.background = element_blank(),
						legend.position="none")

sizepc1m <- ggplot(Male_ave, aes(PC1_size, MatingSuccess, colour = Genus)) + 
			geom_point(alpha=0.1) + 
			geom_smooth(method = "lm", formula = y ~ splines::bs(x, 3), size = 1) + 
			labs(y = "Relative fitness (male)", x = "Body size (PC1)") + 
			scale_y_continuous(limits = c(0, 2.1))+
			theme_classic() + 
			theme(text = element_text(size = 11)) + 
			guides(colour=guide_legend("Species")) + 
			scale_colour_discrete("Species", labels = c(expression(italic("Enallagma cyathigerum")),expression(italic("Ischnura elegans")))) +
			theme(	aspect.ratio = 1,
						strip.background = element_blank(),
						legend.position="none")

sizepc1f <- ggplot(Female_ave_fecundity, aes(PC1_size, Fecundity, colour = Genus)) + 
			geom_point(alpha=0.1) + 
			geom_smooth(method = "lm", formula = y ~ splines::bs(x, 3), size = 1) + 
			labs(y = "Relative fitness (female)", x = "Body size (PC1)") + 
			ylim(c(0, 5.1)) + 
			theme_classic() + 
			theme(text = element_text(size = 11)) + 
			guides(colour=guide_legend("Species")) + 
			scale_colour_discrete("Species", labels = c(expression(italic("Enallagma cyathigerum")),expression(italic("Ischnura elegans")))) + 
			theme(	aspect.ratio = 1,
						strip.background = element_blank(),
						legend.position="none")

library(ggpubr)

ggarrange(sizepc1m, MLD, MLDH, sizepc1f, FLD, FLDH, nrow=2, ncol=3, common.legend = TRUE, legend="bottom", labels = "AUTO")

#############################
#############################
#############################

#Fig. 7

ForeMale <- ForeMale_ave %>% mutate(community_ID = paste(Genus, Locale, Season, sep = "_")) 
					
Fore_Male = ggplot(ForeMale, aes(LD1, MatingSuccess, colour = Genus)) + 
			#geom_point(alpha=0.1, size = 0.5) + 
			geom_smooth(method = "lm", formula = y ~ splines::bs(x, 3), size = 0.5, se = FALSE, lty = 2) + 
			labs(y = "Relative fitness (male)", x = "Forewing shape (LD1)") + 
			#scale_y_continuous(breaks = c(0,1), limits = c(0, 2.2))+
			theme_classic() + 
			theme(text = element_text(size = 11)) + 
			guides(colour=guide_legend("Species")) + 
			scale_colour_discrete("Species", labels = c(expression(italic("Enallagma cyathigerum")),expression(italic("Ischnura elegans")))) + 
			theme(	aspect.ratio = 1,
						strip.background = element_blank(),
						legend.position="none") +
			geom_line(stat = "smooth", method = "lm",data = subset(ForeMale, n_beta > 20),  lwd = .75, lty = 1, alpha = .5, se = FALSE, aes(group = community_ID))

HindMale <- HindMale_ave %>% mutate(community_ID = paste(Genus, Locale, Season, sep = "_")) 
					
Hind_Male = ggplot(HindMale, aes(LD1, MatingSuccess, colour = Genus)) + 
			#geom_point(alpha=0.1, size = 0.5) + 
			geom_smooth(method = "lm", formula = y ~ splines::bs(x, 3), size = 0.5, se = FALSE, lty = 2) + 
			labs(y = "Relative fitness (male)", x = "Hindwing shape (LD1)") + 
			#scale_y_continuous(breaks = c(0,1), limits = c(0, 2.2))+
			theme_classic() + 
			theme(text = element_text(size = 11)) + 
			guides(colour=guide_legend("Species")) + 
			scale_colour_discrete("Species", labels = c(expression(italic("Enallagma cyathigerum")),expression(italic("Ischnura elegans")))) + 
			theme(	aspect.ratio = 1,
						strip.background = element_blank(),
						legend.position="none") +
			geom_line(stat = "smooth", method = "lm",data = subset(HindMale, n_beta > 20),  lwd = .75, lty = 1, alpha = .5, se = FALSE, aes(group = community_ID))

ForeFemale <- ForeFemale_ave %>% mutate(community_ID = paste(Genus, Locale, Season, sep = "_")) 
					
Fore_Female = ggplot(ForeFemale, aes(LD1, Fecundity, colour = Genus)) + 
			#geom_point(alpha=0.1, size = 0.5) + 
			geom_smooth(method = "lm", formula = y ~ splines::bs(x, 3), size = 0.5, se = FALSE, lty = 2) + 
			labs(y = "Relative fitness (female)", x = "Forewing shape (LD1)") + 
			#scale_y_continuous(limits = c(0, 1.2))+
			theme_classic() + 
			theme(text = element_text(size = 11)) + 
			guides(colour=guide_legend("Species")) + 
			scale_colour_discrete("Species", labels = c(expression(italic("Enallagma cyathigerum")),expression(italic("Ischnura elegans")))) + 
			theme(	aspect.ratio = 1,
						strip.background = element_blank(),
						legend.position="none") +
			geom_line(stat = "smooth", method = "lm",data = subset(ForeFemale, n_beta > 20),  lwd = .75, lty = 1, alpha = .5, se = FALSE, aes(group = community_ID))

HindFemale <- HindFemale_ave %>% mutate(community_ID = paste(Genus, Locale, Season, sep = "_")) 
					
Hind_Female = ggplot(HindFemale, aes(LD1, Fecundity, colour = Genus)) + 
			#geom_point(alpha=0.1, size = 0.5) + 
			geom_smooth(method = "lm", formula = y ~ splines::bs(x, 3), size = 0.5, se = FALSE, lty = 2) + 
			labs(y = "Relative fitness (female)", x = "Hindwing shape (LD1)") + 
			#scale_y_continuous(limits = c(0, 1.2))+
			theme_classic() + 
			theme(text = element_text(size = 11)) + 
			guides(colour=guide_legend("Species")) + 
			scale_colour_discrete("Species", labels = c(expression(italic("Enallagma cyathigerum")),expression(italic("Ischnura elegans")))) + 
			theme(	aspect.ratio = 1,
						strip.background = element_blank(),
						legend.position="none") +
			geom_line(stat = "smooth", method = "lm",data = subset(HindFemale, n_beta > 20),  lwd = .75, lty = 1, alpha = .5, se = FALSE, aes(group = community_ID))


ggarrange(Fore_Male, Hind_Male, Fore_Female, Hind_Female, nrow=2, ncol=2, common.legend = TRUE, legend="bottom", labels = "AUTO")

#############################
#############################
#############################

#Figure S6

ggplot(data = ForeMale_ave, aes(x = Genus, y = PC1_size)) +
	geom_boxplot()+
	geom_jitter()+
	theme_classic()

LD.M1 <- gam(MatingSuccess ~ LD1 + PC1_size + LD1*PC1_size, data = subset(ForeMale_ave, Genus=="Ischnura_elegans"))
LD.M2 <- gam(MatingSuccess ~ LD1 + PC1_size + LD1*PC1_size, data = subset(ForeMale_ave, Genus=="Enallagma_cyathigerum"))
LD.F1 <- gam(Fecundity ~ LD1 + PC1_size + LD1*PC1_size, data = subset(ForeFemale_ave, Genus=="Ischnura_elegans"))
LD.F2 <- gam(Fecundity ~ LD1 + PC1_size + LD1*PC1_size, data = subset(ForeFemale_ave, Genus=="Enallagma_cyathigerum"))

pdf(file = "~/Dropbox/Research/Odonata_wing/Paper #1/JEB/figures/gam_forewing_JEB.pdf",   
    width = 5, # The width of the plot in inches
    height = 5)

par(mfrow=c(2,2), mar = c(2.2,2.2,2.2,2.2))

vis.gam(LD.M1, view = c('LD1', 'PC1_size'),  n.grid = 100, color = "cm", ylab = "", xlab = "", main = " ", lwd  = 1, plot.type = "contour", xaxt="n", yaxt="n")
title("A", adj = 0, line = 1)
title(xlab = "Forewing shape (LD1)", line = 0.5)
title(ylab = "Body size (PC1)", line = 0.5)
axis(side = 3, cex.axis = 0.6, lwd = .5, tck = -.02, mgp = c(0.2,0.2,0.2), line = 0)
axis(side = 4, cex.axis = 0.6, lwd = .5, tck = -.02, mgp = c(0.2,0.2,0.2), line = 0)

vis.gam(LD.M2, view = c('LD1', 'PC1_size'),  n.grid = 100, color = "cm", ylab = "", xlab = "", main = " ", cex.lab = .5, cex.axis = .5, lwd  = 1, plot.type = "contour", xaxt="n", yaxt="n")
title("B",  adj = 0, line = 1)
title(xlab = "Forewing shape (LD1)", line = 0.5)
title(ylab = "Body size (PC1)", line = 0.5)
axis(side = 3, cex.axis = 0.6, lwd = .5, tck = -.02, mgp = c(0.2,0.2,0.2), line = 0)
axis(side = 4, cex.axis = 0.6, lwd = .5, tck = -.02, mgp = c(0.2,0.2,0.2), line = 0)

vis.gam(LD.F1, view = c('LD1', 'PC1_size'),  n.grid = 100, color = "cm", ylab = "", xlab = "", main = " ", cex.lab = .5, cex.axis = .5, lwd  = 1, plot.type = "contour", xaxt="n", yaxt="n")
title("C",  adj = 0, line = 1)
title(xlab = "Forewing shape (LD1)", line = 0.5)
title(ylab = "Body size (PC1)", line = 0.5)
axis(side = 3, cex.axis = 0.6, lwd = .5, tck = -.02, mgp = c(0.2,0.2,0.2), line = 0)
axis(side = 4, cex.axis = 0.6, lwd = .5, tck = -.02, mgp = c(0.2,0.2,0.2), line = 0)

vis.gam(LD.F2, view = c('LD1', 'PC1_size'),  n.grid = 100, color = "cm", ylab = "", xlab = "", main = " ", cex.lab = .5, cex.axis = .5, lwd  = 1, plot.type = "contour", xaxt="n", yaxt="n")
title("D",  adj = 0, line = 1)
title(xlab = "Forewing shape (LD1)", line = 0.5)
title(ylab = "Body size (PC1)", line = 0.5)
axis(side = 3, cex.axis = 0.6, lwd = .5, tck = -.02, mgp = c(0.2,0.2,0.2), line = 0)
axis(side = 4, cex.axis = 0.6, lwd = .5, tck = -.02, mgp = c(0.2,0.2,0.2), line = 0)

dev.off()

#############################
#############################
#############################

#Figure S7

LD.M1 <- gam(MatingSuccess ~ LD1 + PC1_size + LD1*PC1_size, data = subset(HindMale_ave, Genus=="Ischnura_elegans"))
LD.M2 <- gam(MatingSuccess ~ LD1 + PC1_size + LD1*PC1_size, data = subset(HindMale_ave, Genus=="Enallagma_cyathigerum"))
LD.F1 <- gam(Fecundity ~ LD1 + PC1_size + LD1*PC1_size, data = subset(HindFemale_ave, Genus=="Ischnura_elegans"))
LD.F2 <- gam(Fecundity ~ LD1 + PC1_size + LD1*PC1_size, data = subset(HindFemale_ave, Genus=="Enallagma_cyathigerum"))

pdf(file = "~/Dropbox/Research/Odonata_wing/Paper #1/JEB/figures/gam_hindwing_JEB.pdf",     
    width = 5, # The width of the plot in inches
    height = 5)

par(mfrow=c(2,2), mar = c(2.2,2.2,2.2,2.2))

vis.gam(LD.M1, view = c('LD1', 'PC1_size'),  n.grid = 100, color = "cm", ylab = "", xlab = "", main = " ", lwd  = 1, plot.type = "contour", xaxt="n", yaxt="n")
title("A", adj = 0, line = 1)
title(xlab = "Hindwing shape (LD1)", line = 0.5)
title(ylab = "Body size (PC1)", line = 0.5)
axis(side = 3, cex.axis = 0.6, lwd = .5, tck = -.02, mgp = c(0.2,0.2,0.2), line = 0)
axis(side = 4, cex.axis = 0.6, lwd = .5, tck = -.02, mgp = c(0.2,0.2,0.2), line = 0)

vis.gam(LD.M2, view = c('LD1', 'PC1_size'),  n.grid = 100, color = "cm", ylab = "", xlab = "", main = " ", cex.lab = .5, cex.axis = .5, lwd  = 1, plot.type = "contour", xaxt="n", yaxt="n")
title("B",  adj = 0, line = 1)
title(xlab = "Hindwing shape (LD1)", line = 0.5)
title(ylab = "Body size (PC1)", line = 0.5)
axis(side = 3, cex.axis = 0.6, lwd = .5, tck = -.02, mgp = c(0.2,0.2,0.2), line = 0)
axis(side = 4, cex.axis = 0.6, lwd = .5, tck = -.02, mgp = c(0.2,0.2,0.2), line = 0)

vis.gam(LD.F1, view = c('LD1', 'PC1_size'),  n.grid = 100, color = "cm", ylab = "", xlab = "", main = " ", cex.lab = .5, cex.axis = .5, lwd  = 1, plot.type = "contour", xaxt="n", yaxt="n")
title("C",  adj = 0, line = 1)
title(xlab = "Hindwing shape (LD1)", line = 0.5)
title(ylab = "Body size (PC1)", line = 0.5)
axis(side = 3, cex.axis = 0.6, lwd = .5, tck = -.02, mgp = c(0.2,0.2,0.2), line = 0)
axis(side = 4, cex.axis = 0.6, lwd = .5, tck = -.02, mgp = c(0.2,0.2,0.2), line = 0)

vis.gam(LD.F2, view = c('LD1', 'PC1_size'),  n.grid = 100, color = "cm", ylab = "", xlab = "", main = " ", cex.lab = .5, cex.axis = .5, lwd  = 1, plot.type = "contour", xaxt="n", yaxt="n")
title("D",  adj = 0, line = 1)
title(xlab = "Hindwing shape (LD1)", line = 0.5)
title(ylab = "Body size (PC1)", line = 0.5)
axis(side = 3, cex.axis = 0.6, lwd = .5, tck = -.02, mgp = c(0.2,0.2,0.2), line = 0)
axis(side = 4, cex.axis = 0.6, lwd = .5, tck = -.02, mgp = c(0.2,0.2,0.2), line = 0)

dev.off()