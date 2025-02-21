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

# The purpose of this chunk of code is to evaluate selection gradients per location, rather than the species as a whole. 

# First, arrange data in ways that we want to analyze (almost the same as the pervious code but we prefer this so that this chunk is not dependent of the former)

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

#############################
#############################
#############################

# Below we estimate beta per location/year. Note that we subset data where we have more than 20 samples to estiamte beta

#PC1 size (Table S3)

Male_directional_PC1_size_run <- subset(ForeMale, n_beta > 20) %>% group_by(Season, Genus, Locale) %>% 
  do(tidy(lm(MatingSuccess ~ PC1_size, data = .), conf.int = TRUE))

Male_directional_PC1_size <- subset(Male_directional_PC1_size_run, term=="PC1_size")%>%
					mutate(	mbeta = estimate,
									mbeta_se = std.error,
									mbeta_p = p.value,
									community_ID = paste(Genus, Locale, Season, sep = "_")) %>% 
					dplyr::select(community_ID,	mbeta, mbeta_se, mbeta_p)

Male_quadratic_PC1_size_run <- subset(ForeMale, n_beta > 20) %>% group_by(Season, Genus, Locale) %>% 
  do(tidy(lm(MatingSuccess ~ PC1_size + PC1_size2, data = .), conf.int = TRUE))

Male_quadratic_PC1_size <- subset(Male_quadratic_PC1_size_run, term=="PC1_size2")%>%
					mutate(	mgamma = estimate*2,
									mgamma_se = std.error*2,
									mgamma_p = p.value,
									community_ID = paste(Genus, Locale, Season, sep = "_")) %>% ungroup() %>%
					dplyr::select(community_ID,	mgamma, mgamma_se, mgamma_p)


Male_PC1_size <- subset(ForeMale, n_beta > 20) %>% group_by(Season, Genus, Locale) %>% 
					dplyr::select(n_beta, PC1_size) %>%
					summarise(	n = mean(n_beta),
										mean_PC1_size = mean(PC1_size),
										sd = sd(PC1_size)) %>%
					mutate(community_ID = paste(Genus, Locale, Season, sep = "_")) %>% ungroup() %>%
					dplyr::select(community_ID, n, mean_PC1_size, sd )

#Below gives the male part of Table S3

male_summary <- 	left_join(Male_directional_PC1_size, Male_quadratic_PC1_size, by = "community_ID") %>%
							left_join(., Male_PC1_size,  by = "community_ID")

#Female

Female_directional_PC1_size_run <- subset(ForeFemale, n_beta > 20) %>% group_by(Season, Genus, Locale) %>% 
  do(tidy(lm(Fecundity ~ PC1_size, data = .), conf.int = TRUE))

Female_directional_PC1_size <- subset(Female_directional_PC1_size_run, term=="PC1_size")%>%
					mutate(	fbeta = estimate,
									fbeta_se = std.error,
									fbeta_p = p.value,
									community_ID = paste(Genus, Locale, Season, sep = "_")) %>% 
					dplyr::select(community_ID,	fbeta, fbeta_se, fbeta_p)

Female_quadratic_PC1_size_run <- subset(ForeFemale, n_beta > 20) %>% group_by(Season, Genus, Locale) %>% 
  do(tidy(lm(Fecundity ~ PC1_size + PC1_size2, data = .), conf.int = TRUE))

Female_quadratic_PC1_size <- subset(Female_quadratic_PC1_size_run, term=="PC1_size2")%>%
					mutate(	fgamma = estimate*2,
									fgamma_se = std.error*2,
									fgamma_p = p.value,
									community_ID = paste(Genus, Locale, Season, sep = "_")) %>% ungroup() %>%
					dplyr::select(community_ID,	fgamma, fgamma_se, fgamma_p)


Female_PC1_size <- subset(ForeFemale, n_beta > 20) %>% group_by(Season, Genus, Locale) %>% 
					dplyr::select(n_beta, PC1_size) %>%
					summarise(	n = mean(n_beta),
										mean_PC1_size = mean(PC1_size),
										sd = sd(PC1_size)) %>%
					mutate(community_ID = paste(Genus, Locale, Season, sep = "_")) %>% ungroup() %>%
					dplyr::select(community_ID, n, mean_PC1_size, sd )

#Below gives the female part of Table S3

Female_summary <- 	left_join(Female_directional_PC1_size, Female_quadratic_PC1_size, by = "community_ID") %>%
							left_join(., Female_PC1_size,  by = "community_ID")


#############################
#############################
#############################

#Fore wing LD1  (Table S4)

Male_directional_LD1_run <- subset(ForeMale, n_beta > 20) %>% group_by(Season, Genus, Locale) %>% 
  do(tidy(lm(MatingSuccess ~ LD1, data = .), conf.int = TRUE))

Male_directional_LD1 <- subset(Male_directional_LD1_run, term=="LD1")%>%
					mutate(	mbeta = estimate,
									mbeta_se = std.error,
									mbeta_p = p.value,
									community_ID = paste(Genus, Locale, Season, sep = "_")) %>% 
					dplyr::select(community_ID,	mbeta, mbeta_se, mbeta_p)

Male_quadratic_LD1_run <- subset(ForeMale, n_beta > 20) %>% group_by(Season, Genus, Locale) %>% 
  do(tidy(lm(MatingSuccess ~ LD1 + LD12, data = .), conf.int = TRUE))

Male_quadratic_LD1 <- subset(Male_quadratic_LD1_run, term=="LD12")%>%
					mutate(	mgamma = estimate*2,
									mgamma_se = std.error*2,
									mgamma_p = p.value,
									community_ID = paste(Genus, Locale, Season, sep = "_")) %>% ungroup() %>%
					dplyr::select(community_ID,	mgamma, mgamma_se, mgamma_p)

Male_LD1 <- subset(ForeMale, n_beta > 20) %>% group_by(Season, Genus, Locale) %>% 
					dplyr::select(n_beta, LD1) %>%
					summarise(	n = mean(n_beta),
										mean_LD1 = mean(LD1),
										sd = sd(LD1)) %>%
					mutate(community_ID = paste(Genus, Locale, Season, sep = "_")) %>% ungroup() %>%
					dplyr::select(community_ID, n, mean_LD1, sd )

fore_male_summary <- 	left_join(Male_directional_LD1, Male_quadratic_LD1, by = "community_ID") %>%
							left_join(., Male_LD1,  by = "community_ID")

#Female

Female_directional_LD1_run <- subset(ForeFemale, n_beta > 20) %>% group_by(Season, Genus, Locale) %>% 
  do(tidy(lm(Fecundity ~ LD1, data = .), conf.int = TRUE))

Female_directional_LD1 <- subset(Female_directional_LD1_run, term=="LD1")%>%
					mutate(	fbeta = estimate,
									fbeta_se = std.error,
									fbeta_p = p.value,
									community_ID = paste(Genus, Locale, Season, sep = "_")) %>% 
					dplyr::select(community_ID,	fbeta, fbeta_se, fbeta_p)

Female_quadratic_LD1_run <- subset(ForeFemale, n_beta > 20) %>% group_by(Season, Genus, Locale) %>% 
  do(tidy(lm(Fecundity ~ LD1 + LD12, data = .), conf.int = TRUE))

Female_quadratic_LD1 <- subset(Female_quadratic_LD1_run, term=="LD12")%>%
					mutate(	fgamma = estimate*2,
									fgamma_se = std.error*2,
									fgamma_p = p.value,
									community_ID = paste(Genus, Locale, Season, sep = "_")) %>% ungroup() %>%
					dplyr::select(community_ID,	fgamma, fgamma_se, fgamma_p)


Female_LD1 <- subset(ForeFemale, n_beta > 20) %>% group_by(Season, Genus, Locale) %>% 
					dplyr::select(n_beta, LD1) %>%
					summarise(	n = mean(n_beta),
										mean_LD1 = mean(LD1),
										sd = sd(LD1)) %>%
					mutate(community_ID = paste(Genus, Locale, Season, sep = "_")) %>% ungroup() %>%
					dplyr::select(community_ID, n, mean_LD1, sd )

fore_Female_summary <- 	left_join(Female_directional_LD1, Female_quadratic_LD1, by = "community_ID") %>%
							left_join(., Female_LD1,  by = "community_ID")

#############################
#############################
#############################

#Hind wing (Table S5)

Male_directional_LD1_run <- subset(HindMale, n_beta > 20) %>% group_by(Season, Genus, Locale) %>% 
  do(tidy(lm(MatingSuccess ~ LD1, data = .), conf.int = TRUE))

Male_directional_LD1 <- subset(Male_directional_LD1_run, term=="LD1")%>%
					mutate(	mbeta = estimate,
									mbeta_se = std.error,
									mbeta_p = p.value,
									community_ID = paste(Genus, Locale, Season, sep = "_")) %>% 
					dplyr::select(community_ID,	mbeta, mbeta_se, mbeta_p)

Male_quadratic_LD1_run <- subset(HindMale, n_beta > 20) %>% group_by(Season, Genus, Locale) %>% 
  do(tidy(lm(MatingSuccess ~ LD1 + LD12, data = .), conf.int = TRUE))

Male_quadratic_LD1 <- subset(Male_quadratic_LD1_run, term=="LD12")%>%
					mutate(	mgamma = estimate*2,
									mgamma_se = std.error*2,
									mgamma_p = p.value,
									community_ID = paste(Genus, Locale, Season, sep = "_")) %>% ungroup() %>%
					dplyr::select(community_ID,	mgamma, mgamma_se, mgamma_p)

Male_LD1 <- subset(HindMale, n_beta > 20) %>% group_by(Season, Genus, Locale) %>% 
					dplyr::select(n_beta, LD1) %>%
					summarise(	n = mean(n_beta),
										mean_LD1 = mean(LD1),
										sd = sd(LD1)) %>%
					mutate(community_ID = paste(Genus, Locale, Season, sep = "_")) %>% ungroup() %>%
					dplyr::select(community_ID, n, mean_LD1, sd )

Hind_male_summary <- 	left_join(Male_directional_LD1, Male_quadratic_LD1, by = "community_ID") %>%
							left_join(., Male_LD1,  by = "community_ID")



Female_directional_LD1_run <- subset(HindFemale, n_beta > 20) %>% group_by(Season, Genus, Locale) %>% 
  do(tidy(lm(Fecundity ~ LD1, data = .), conf.int = TRUE))

Female_directional_LD1 <- subset(Female_directional_LD1_run, term=="LD1")%>%
					mutate(	fbeta = estimate,
									fbeta_se = std.error,
									fbeta_p = p.value,
									community_ID = paste(Genus, Locale, Season, sep = "_")) %>% 
					dplyr::select(community_ID,	fbeta, fbeta_se, fbeta_p)

Female_quadratic_LD1_run <- subset(HindFemale, n_beta > 20) %>% group_by(Season, Genus, Locale) %>% 
  do(tidy(lm(Fecundity ~ LD1 + LD12, data = .), conf.int = TRUE))

Female_quadratic_LD1 <- subset(Female_quadratic_LD1_run, term=="LD12")%>%
					mutate(	fgamma = estimate*2,
									fgamma_se = std.error*2,
									fgamma_p = p.value,
									community_ID = paste(Genus, Locale, Season, sep = "_")) %>% ungroup() %>%
					dplyr::select(community_ID,	fgamma, fgamma_se, fgamma_p)


Female_LD1 <- subset(HindFemale, n_beta > 20) %>% group_by(Season, Genus, Locale) %>% 
					dplyr::select(n_beta, LD1) %>%
					summarise(	n = mean(n_beta),
										mean_LD1 = mean(LD1),
										sd = sd(LD1)) %>%
					mutate(community_ID = paste(Genus, Locale, Season, sep = "_")) %>% ungroup() %>%
					dplyr::select(community_ID, n, mean_LD1, sd )

Hind_Female_summary <- 	left_join(Female_directional_LD1, Female_quadratic_LD1, by = "community_ID") %>%
							left_join(., Female_LD1,  by = "community_ID")

#############################
#############################
#############################

#Show all summaries

#Size

subset(male_summary , Genus == "Ischnura_elegans")
subset(male_summary , Genus == "Enallagma_cyathigerum")
subset(Female_summary , Genus == "Ischnura_elegans")
subset(Female_summary , Genus == "Enallagma_cyathigerum")

var(male_summary$mbeta)
var(Female_summary$fbeta)


#Forewing

subset(fore_male_summary , Genus == "Ischnura_elegans")
subset(fore_male_summary , Genus == "Enallagma_cyathigerum")
subset(fore_Female_summary , Genus == "Ischnura_elegans")
subset(fore_Female_summary , Genus == "Enallagma_cyathigerum")

var(fore_male_summary $mbeta)
var(fore_Female_summary $fbeta)

#Hindwing

subset(Hind_male_summary , Genus == "Ischnura_elegans")
subset(Hind_male_summary , Genus == "Enallagma_cyathigerum")
subset(Hind_Female_summary , Genus == "Ischnura_elegans")
subset(Hind_Female_summary , Genus == "Enallagma_cyathigerum")

var(Hind_male_summary $mbeta)
var(Hind_Female_summary $fbeta)

#Phew!!

#Figure 4

mSize = ggplot(data = male_summary, aes(x = Genus, y = mbeta, color = Genus)) + 
	geom_boxplot(alpha = 0.5, width = .5) +
	geom_jitter(width = .1, shape = 21, alpha = .5, stroke = 1) +
	geom_hline(yintercept = 0, lty = 2) +
	theme_classic()+
	scale_colour_discrete("Species", labels = c(expression(italic("Enallagma cyathigerum")),expression(italic("Ischnura elegans"))))+
	xlab("species") + ylab(expression(beta["male"]*" (size PC1)")) + 
	theme(	axis.text.x = element_blank(),
				axis.title.x = element_blank(),
				legend.title = element_blank())

fSize = ggplot(data = Female_summary, aes(x = Genus, y = fbeta, color = Genus)) + 
	geom_boxplot(alpha = 0.5, width = .5) +
	geom_jitter(width = .1, shape = 21, alpha = .5, stroke = 1) +
	geom_hline(yintercept = 0, lty = 2) +
	theme_classic()+
	xlab("species") + ylab(expression(beta["female"]*" (size PC1)")) + 
	theme(	axis.text.x = element_blank(),
				axis.title.x = element_blank(),
				legend.title = element_blank())

mFore = ggplot(data = fore_male_summary, aes(x = Genus, y = mbeta, color = Genus)) + 
	geom_boxplot(alpha = 0.5, width = .5, outlier.alpha = 0) +
	geom_jitter(width = .1, shape = 21, alpha = .5, stroke = 1) +
	geom_hline(yintercept = 0, lty = 2) +
	theme_classic()+
	xlab("species") + ylab(expression(beta["male"]*" (forewing LD1)")) + 
	theme(	axis.text.x = element_blank(),
				axis.title.x = element_blank(),
				legend.title = element_blank())

fFore = ggplot(data = fore_Female_summary, aes(x = Genus, y = fbeta, color = Genus)) + 
	geom_boxplot(alpha = 0.5, width = .5, outlier.alpha = 0) +
	geom_jitter(width = .1, shape = 21, alpha = .5, stroke = 1) +
	geom_hline(yintercept = 0, lty = 2) +
	theme_classic()+
	xlab("species") + ylab(expression(beta["female"]*" (forewing LD1)")) + 
	theme(	axis.text.x = element_blank(),
				axis.title.x = element_blank(),
				legend.title = element_blank())

mHind = ggplot(data = Hind_male_summary, aes(x = Genus, y = mbeta, color = Genus)) + 
	geom_boxplot(alpha = 0.5, width = .5, outlier.alpha = 0) +
	geom_jitter(width = .1, shape = 21, alpha = .5, stroke = 1) +
	geom_hline(yintercept = 0, lty = 2) +
	theme_classic()+
	xlab("species") + ylab(expression(beta["male"]*" (hindwing LD1)")) + 
	theme(	axis.text.x = element_blank(),
				axis.title.x = element_blank(),
				legend.title = element_blank())

fHind = ggplot(data = Hind_Female_summary, aes(x = Genus, y = fbeta, color = Genus)) + 
	geom_boxplot(alpha = 0.5, width = .5, outlier.alpha = 0) +
	geom_jitter(width = .1, shape = 21, alpha = .5, stroke = 1) +
	geom_hline(yintercept = 0, lty = 2) +
	theme_classic()+
	xlab("species") + ylab(expression(beta["female"]*" (hindwing LD1)")) + 
	theme(	axis.text.x = element_blank(),
				axis.title.x = element_blank(),
				legend.title = element_blank())

ggarrange(mSize, mFore, mHind, fSize, fFore, fHind, ncol = 3, nrow = 2, labels = "AUTO", common.legend = TRUE, legend = "bottom")

#gamma (we do not show this any more but it's complementary to the paper)

mSize = ggplot(data = male_summary, aes(x = Genus, y = mgamma, color = Genus)) + 
	geom_boxplot(alpha = 0.5, width = .5) +
	geom_jitter(width = .1, shape = 21, alpha = .5, stroke = 1) +
	geom_hline(yintercept = 0, lty = 2) +
	theme_classic()+
	scale_colour_discrete("Species", labels = c(expression(italic("Enallagma cyathigerum")),expression(italic("Ischnura elegans"))))+
	xlab("species") + ylab(expression(gamma["male"]*" (size PC1)")) + 
	theme(	axis.text.x = element_blank(),
				axis.title.x = element_blank(),
				legend.title = element_blank())

fSize = ggplot(data = Female_summary, aes(x = Genus, y = fgamma, color = Genus)) + 
	geom_boxplot(alpha = 0.5, width = .5) +
	geom_jitter(width = .1, shape = 21, alpha = .5, stroke = 1) +
	geom_hline(yintercept = 0, lty = 2) +
	theme_classic()+
	xlab("species") + ylab(expression(gamma["female"]*" (size PC1)")) + 
	theme(	axis.text.x = element_blank(),
				axis.title.x = element_blank(),
				legend.title = element_blank())

mFore = ggplot(data = fore_male_summary, aes(x = Genus, y = mgamma, color = Genus)) + 
	geom_boxplot(alpha = 0.5, width = .5, outlier.alpha = 0) +
	geom_jitter(width = .1, shape = 21, alpha = .5, stroke = 1) +
	geom_hline(yintercept = 0, lty = 2) +
	theme_classic()+
	xlab("species") + ylab(expression(gamma["male"]*" (forewing LD1)")) + 
	theme(	axis.text.x = element_blank(),
				axis.title.x = element_blank(),
				legend.title = element_blank())

fFore = ggplot(data = fore_Female_summary, aes(x = Genus, y = fgamma, color = Genus)) + 
	geom_boxplot(alpha = 0.5, width = .5, outlier.alpha = 0) +
	geom_jitter(width = .1, shape = 21, alpha = .5, stroke = 1) +
	geom_hline(yintercept = 0, lty = 2) +
	theme_classic()+
	xlab("species") + ylab(expression(gamma["female"]*" (forewing LD1)")) + 
	theme(	axis.text.x = element_blank(),
				axis.title.x = element_blank(),
				legend.title = element_blank())

mHind = ggplot(data = Hind_male_summary, aes(x = Genus, y = mgamma, color = Genus)) + 
	geom_boxplot(alpha = 0.5, width = .5, outlier.alpha = 0) +
	geom_jitter(width = .1, shape = 21, alpha = .5, stroke = 1) +
	geom_hline(yintercept = 0, lty = 2) +
	theme_classic()+
	xlab("species") + ylab(expression(gamma["male"]*" (hindwing LD1)")) + 
	theme(	axis.text.x = element_blank(),
				axis.title.x = element_blank(),
				legend.title = element_blank())

fHind = ggplot(data = Hind_Female_summary, aes(x = Genus, y = fgamma, color = Genus)) + 
	geom_boxplot(alpha = 0.5, width = .5, outlier.alpha = 0) +
	geom_jitter(width = .1, shape = 21, alpha = .5, stroke = 1) +
	geom_hline(yintercept = 0, lty = 2) +
	theme_classic()+
	xlab("species") + ylab(expression(gamma["female"]*" (hindwing LD1)")) + 
	theme(	axis.text.x = element_blank(),
				axis.title.x = element_blank(),
				legend.title = element_blank())

ggarrange(mSize, mFore, mHind, fSize, fFore, fHind, ncol = 3, nrow = 2, labels = "AUTO", common.legend = TRUE, legend = "bottom")

#############################
#############################
#############################

#Statistical analyses for the differences in selection between species (Table S7)

#PC1 size

mbeta_PC1_size_mod <- lmer(mbeta~Genus+(1|Season), weights = 1/mbeta_se, data = male_summary)
mgamma_PC1_size_mod <- lmer(mgamma~Genus+(1|Season), weights = 1/mgamma_se, data = male_summary)
summary(mbeta_PC1_size_mod)
summary(mgamma_PC1_size_mod)

mbeta_PC1_size_mod <- lm(mbeta~Genus, weights = 1/mbeta_se, data = male_summary)
mgamma_PC1_size_mod <- lm(mgamma~Genus, weights = 1/mgamma_se, data = male_summary)
summary(mbeta_PC1_size_mod)
summary(mgamma_PC1_size_mod)

fbeta_PC1_size_mod <- lmer(fbeta~Genus+(1|Season), weights = 1/fbeta_se, data = Female_summary)
fgamma_PC1_size_mod <- lmer(fgamma~Genus+(1|Season), weights = 1/fgamma_se, data = Female_summary)
summary(fbeta_PC1_size_mod)
summary(fgamma_PC1_size_mod)

fbeta_PC1_size_mod <- lm(fbeta~Genus, weights = 1/fbeta_se, data = Female_summary)
fgamma_PC1_size_mod <- lm(fgamma~Genus, weights = 1/fgamma_se, data = Female_summary)
summary(fbeta_PC1_size_mod)
summary(fgamma_PC1_size_mod)

#LD1 forewing

mbeta_LD1_fore_mod <- lmer(mbeta~Genus+(1|Season), weights = 1/mbeta_se, data = fore_male_summary)
mgamma_LD1_fore_mod <- lmer(mgamma~Genus+(1|Season), weights = 1/mgamma_se, data = fore_male_summary)
summary(mbeta_LD1_fore_mod)
summary(mgamma_LD1_fore_mod)

mbeta_LD1_fore_mod <- lm(mbeta~Genus, weights = 1/mbeta_se, data = fore_male_summary)
mgamma_LD1_fore_mod <- lm(mgamma~Genus, weights = 1/mgamma_se, data = fore_male_summary)
summary(mbeta_LD1_fore_mod)
summary(mgamma_LD1_fore_mod)

fbeta_LD1_fore_mod <- lmer(fbeta~Genus+(1|Season), weights = 1/fbeta_se, data = fore_Female_summary)
fgamma_LD1_fore_mod <- lmer(fgamma~Genus+(1|Season), weights = 1/fgamma_se, data = fore_Female_summary)
summary(fbeta_LD1_fore_mod)
summary(fgamma_LD1_fore_mod)

fbeta_LD1_fore_mod <- lm(fbeta~Genus, weights = 1/fbeta_se, data = fore_Female_summary)
fgamma_LD1_fore_mod <- lm(fgamma~Genus, weights = 1/fgamma_se, data = fore_Female_summary)
summary(fbeta_LD1_fore_mod)
summary(fgamma_LD1_fore_mod)

#LD1 hindwing

mbeta_LD1_hind_mod <- lmer(mbeta~Genus+(1|Season), weights = 1/mbeta_se, data = Hind_male_summary)
mgamma_LD1_hind_mod <- lmer(mgamma~Genus+(1|Season), weights = 1/mgamma_se, data = Hind_male_summary)
summary(mbeta_LD1_hind_mod)
summary(mgamma_LD1_hind_mod)

mbeta_LD1_hind_mod <- lm(mbeta~Genus, weights = 1/mbeta_se, data = Hind_male_summary)
mgamma_LD1_hind_mod <- lm(mgamma~Genus, weights = 1/mgamma_se, data = Hind_male_summary)
summary(mbeta_LD1_hind_mod)
summary(mgamma_LD1_hind_mod)

fbeta_LD1_hind_mod <- lmer(fbeta~Genus+(1|Season), weights = 1/fbeta_se, data = Hind_Female_summary)
fgamma_LD1_hind_mod <- lmer(fgamma~Genus+(1|Season), weights = 1/fgamma_se, data = Hind_Female_summary)
summary(fbeta_LD1_hind_mod)
summary(fgamma_LD1_hind_mod)

fbeta_LD1_hind_mod <- lm(fbeta~Genus, weights = 1/fbeta_se, data = Hind_Female_summary)
fgamma_LD1_hind_mod <- lm(fgamma~Genus, weights = 1/fgamma_se, data = Hind_Female_summary)
summary(fbeta_LD1_hind_mod)
summary(fgamma_LD1_hind_mod)
