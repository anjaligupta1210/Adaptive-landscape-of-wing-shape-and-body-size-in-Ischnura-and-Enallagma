#Checked on 2025.02.21 for JEB resubmission
#The code here works fine and produces figures as it should be. 

#Code for Gupta, Svensson, Frietsch and Tsuboi

# Set working directory:
setwd("YOURWORKINGDIRECTORY")
setwd("~/Dropbox/Enallagma_Anjali_2021/ManuscriptWingShape_Data_Rcode")


##GPA AND ANALYZING WING SHAPE

#LOAD DATA FILE

library(readxl)
Data <- read_excel("Data.xlsx")

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

Data <- Data %>% drop_na(Copula)

Male <- subset(Data, Sex=="Male")
Female <- subset(Data, Sex=="Female")

IMale <- subset(Male, Genus=="Ischnura elegans")
EMale <- subset(Male, Genus=="Enallagma cyathigerum")

IFemale <- subset(Female, Genus=="Ischnura elegans")
EFemale <- subset(Female, Genus=="Enallagma cyathigerum")


#Standardising fitness traits at the species level. Note that this estimate is not used any longer but here just for the sake of documenting what has been done. These lines will add some new columns to the data, without which some downstream codes will not work. I know it does not make a lot of sense but I hope readers will sympathsize us that this happen when a manuscript developes over the course of multiple years going through several revisions.

#First, calculate species, sex, and copulation-status specific means (and sample sizes)

(IMaleData <- IMale %>%
    group_by(Sex, Copula) %>%
    summarise(n = n()) %>%
    mutate(Freq = n/sum(n)))
(EMaleData <- EMale %>%
    group_by(Sex, Copula) %>%
    summarise(n = n()) %>%
    mutate(Freq = n/sum(n)))

(IFemaleData <- IFemale %>%
    group_by(Sex, Copula) %>%
    summarise(n = n()) %>%
    mutate(Freq = n/sum(n)))
(EFemaleData <- EFemale %>%
    group_by(Sex, Copula) %>%
    summarise(n = n()) %>%
    mutate(Freq = n/sum(n)))

#calculate fitness relative to the species average. 

IMale$MatingSuccess <- IMale$Copula/0.5010768 # IMaleData $Freq[2]
EMale$MatingSuccess <- EMale$Copula/0.4897579 # EMaleData $Freq[2]

IFemale$MatingSuccess <- IFemale$Copula/0.693816 # IFemaleData $Freq[2]
EFemale$MatingSuccess <- EFemale$Copula/0.6994489 # EFemaleData$Freq[2]

IFemale$Eggs[is.na(IFemale$Eggs)] = 0
EFemale$Eggs[is.na(EFemale$Eggs)] = 0

IFemale$Fecundity <- IFemale$Eggs/146.6 # as.numeric(summary(IFemale$Eggs)[4])
EFemale$Fecundity <- EFemale$Eggs/215 # as.numeric(summary(EFemale $Eggs)[4])

IMale$Fecundity <- NA
EMale$Fecundity <- NA

Data1 <- rbind(IMale, EMale, IFemale, EFemale)

############

#From here onwards, the actual codes ARE used for the published analyses and results

# Geometric morphometrics and dimensional reduction

#Generalized Procrustes Analysis (GPA) of landmarks Data

#Step 1 - take out coordinates

Data_coords = Data1[, c(24:57)]

#Step 2 - make an array of coordinates - specify the no. of landmarks and dimensions in the arrayspecs function

Data_array = arrayspecs(Data_coords, 17, 2)

#Step 3 - Run GPA

Data_GPA = gpagen(Data_array) 

#Now, all coordinates have been standardized in terms of centroid size, rotation, and the place of centroid, all observations have centroid size of 1.

#Step 4 - plot GPA Data to see the spread of data before and after GPA

plotAllSpecimens(Data_array, mean = TRUE, links = NULL, label = FALSE, plot.param = list(pt.cex = .1, mean.bg = "yellow", mean.cex = 1))
plot(Data_GPA, mean = TRUE, links = NULL, label = FALSE, plot.param = list(pt.cex = .1, mean.bg = "yellow", mean.cex = 1))

#LDA OF SHAPE TRAITS

Shape.lda <- lda(Data1[, 59:92], grouping = Data1$Genus)
Shape.lda.scores <- predict(Shape.lda)$x
Shape_LD <- as.data.frame(Shape.lda.scores)

Data1 <- cbind(Data1, Shape_LD)

Data1$Genus <- as.factor(Data1$Genus)
Data1$Locale <- as.factor(Data1$Locale)

Male <- subset(Data1, Sex=="Male")
Female <- subset(Data1, Sex=="Female")

GPA_coords <- arrayspecs(Data1[,c(59:92)], 17, 2)

##shape.predictor geomorph

preds_LD <- shape.predictor(GPA_coords, x = predict(Shape.lda)$x, pred1 = max(Shape.lda.scores)*2, pred2 = min(Shape.lda.scores)*2)

#this will show you a shape corresponds to a specimen with maximum value*2 of LD1
plotRefToTarget(mshape(GPA_coords), preds_LD$pred1)

#this will show you a shape corresponds to a specimen with minimum value*2 of LD1
plotRefToTarget(mshape(GPA_coords), preds_LD$pred2)

#Fig2 B) LD1 prediction figures are obtained from preds_LD: pred1 and pred2

## The following is used for Figure S4 and S9

#PC SHAPE MODELS AND GRAPHS

Shape_PCA = prcomp(Data1[, c(59:92)], scale. = T) 
summary(Shape_PCA)

Shape_PCs = Shape_PCA$x

Data1_full = cbind(Data1, Shape_PCs)

#This will give you FigS4

ggplot(data = Data1_full, aes(x = PC1, y = PC2, colour = Genus, shape = WingID, linetype = WingID)) + 
		scale_shape_manual(values=c(0,16)) + 
		scale_linetype_manual(values=c(3,5)) + 
		geom_point(alpha = .1, size = .5) +
		theme_classic() + 
		stat_ellipse(size = 1, alpha = .75) +
		labs(x = "PC1 (41.9%)", y = "PC2 (19.2%)")+ 
		theme(	axis.text.x = element_text(size = 10), 
				legend.text = element_text(size = 10), 
				legend.title = element_blank(),
				axis.text.y = element_text(size = 10), 
				axis.title = element_text(size = 10))+ 
		guides(colour=guide_legend("Species")) + 
		scale_colour_discrete("Species", labels = c(expression(italic("Enallagma cyathigerum")),expression(italic("Ischnura elegans"))))

#Adding Shape PCscores to the data

ShapePC <- cbind(Data1,Shape_PCA$x[,1:5])

#Change the column headers from PC1,PC2,PC3,PC4,PC5 to PC1_shape,PC2_shape,PC3_shape,PC4_shape,PC5_shape.

colnames(ShapePC)[96] <- "PC1_shape"
colnames(ShapePC)[97] <- "PC2_shape"
colnames(ShapePC)[98] <- "PC3_shape"
colnames(ShapePC)[99] <- "PC4_shape"
colnames(ShapePC)[100] <- "PC5_shape"

#The following lines will produce shape predictions as part of Figure S4

##shape.predictor geomorph

preds_PC1 <- shape.predictor(GPA_coords, x = Shape_PCA$x[,1], pred1 = max(Shape_PCA$x[,1])*2, pred2 = min(Shape_PCA$x[,1])*2)

plotRefToTarget(mshape(GPA_coords), preds_PC1$pred1)
plotRefToTarget(mshape(GPA_coords), preds_PC1$pred2)


preds_PC2 <- shape.predictor(GPA_coords, x = Shape_PCA$x[,2], pred1 = max(Shape_PCA$x[,2])*2, pred2 = min(Shape_PCA$x[,2])*1.2)

plotRefToTarget(mshape(GPA_coords), preds_PC2$pred1)
plotRefToTarget(mshape(GPA_coords), preds_PC2$pred2)

#PCA of Size

ShapePC <- ShapePC %>% drop_na(Wing) %>% drop_na(Thorax) %>% drop_na(Length) %>% drop_na(Abdomen) %>% drop_na(S_4)

Size_PCA = prcomp((log(ShapePC[, c(15:19)])), scale = FALSE) #PCA run on covariance matrix, scale=False, natural logged the size trait in order to make the unit of size and the unit of shape to be comparable (i.e. both are proportional to its own size). This also makes all the downstream analyses on the "mean-standardized" scale. 
summary(Size_PCA)

#Figure S3

autoplot(Size_PCA, data = ShapePC, colour = 'Genus', shape = 'Sex') + scale_shape_manual(values=c(0,16)) + theme_classic() + theme(axis.text.x = element_text(size = 14), legend.text = element_text(size = 14), axis.text.y = element_text(size = 14), axis.title = element_text(size = 20))+ guides(colour=guide_legend("Species")) + scale_colour_discrete("Species", labels = c(expression(italic("Enallagma cyathigerum")),expression(italic("Ischnura elegans"))))

#Adding Size PCscores to the data

DataPC <- cbind(ShapePC,Size_PCA$x[,1:2])

#Change the column headers from PC1,PC2,PC3,PC4 to PC1_size,PC2_size,PC3_size,PC4_size.

colnames(DataPC)[101] <- "PC1_size"
colnames(DataPC)[102] <- "PC2_size"

DataPC$PC1_size <- -1*DataPC$PC1_size

#creating squared PC columns

DataPC$PC1_shape2 <- DataPC$PC1_shape^2 
DataPC$PC2_shape2 <- DataPC$PC2_shape^2
DataPC$PC3_shape2 <- DataPC$PC3_shape^2
DataPC$PC4_shape2 <- DataPC$PC4_shape^2
DataPC$PC5_shape2 <- DataPC$PC5_shape^2
DataPC$PC1_size2 <- DataPC$PC1_size^2
DataPC$PC2_size2 <- DataPC$PC2_size^2
DataPC$LD12 <- DataPC$LD1^2

#write.csv(DataPC,"DataPC.csv")

#Now, the date is created at your WD. We then made two manual changes to this data
#replace the first column (which is just number from 1 to x) by a column titled "Ind_ID" that concatenate "Season_ID" and "Genus". It was done retrpspectively like this as some replicates of ID from different years were noticed at some later part of the analyses
#similarly, a typo in a few labels of the column "locale" was identified through revisions. Here, some locale was written with Swedish vowels with umlauts and circles in some cases but not all others. We thus manually changed these names to use English alphabets. An example is "H<f6>je <c5> 14" -> "Hoje_A_14". At the same time of doing this, we also made some abbreviation more explicit. "Gerp" -> "Genarp" to make downstream analyses more intuitive
#We then save the file as "DataPC_sorted_locale_fixed.csv". This is the data used for all analyses

#Repeatability with respect to images

Data_rep <- read_excel("Data_LMRenamed.xlsx")

repeated_data_prep = Data_rep %>% group_by(Genus, Season_ID) %>%
		mutate(wing_n = n())

repeated_data =  subset(repeated_data_prep, wing_n > 4)

repeatability = repeated_data %>% group_by(Genus, Season_ID, WingSide, WingID) %>% 
	mutate(mean_X.1 = mean(X.1),
				mean_Y.1 = mean(Y.1),
				mean_X.2 = mean(X.2),
				mean_Y.2 = mean(Y.2),
				mean_X.3 = mean(X.3),
				mean_Y.3 = mean(Y.3),
				mean_X.4 = mean(X.4),
				mean_Y.4 = mean(Y.4),
				mean_X.5 = mean(X.5),
				mean_Y.5 = mean(Y.5),
				mean_X.6 = mean(X.6),
				mean_Y.6 = mean(Y.6),
				mean_X.7 = mean(X.7),
				mean_Y.7 = mean(Y.7),
				mean_X.8 = mean(X.8),
				mean_Y.8 = mean(Y.8),
				mean_X.9 = mean(X.9),
				mean_Y.9 = mean(Y.9),
				mean_X.10 = mean(X.10),
				mean_Y.10 = mean(Y.10),
				mean_X.11 = mean(X.11),
				mean_Y.11 = mean(Y.11),
				mean_X.12 = mean(X.12),
				mean_Y.12 = mean(Y.12),
				mean_X.13 = mean(X.13),
				mean_Y.13 = mean(Y.13),
				mean_X.14 = mean(X.14),
				mean_Y.14 = mean(Y.14),
				mean_X.15 = mean(X.15),
				mean_Y.15 = mean(Y.15),
				mean_X.16 = mean(X.16),
				mean_Y.16 = mean(Y.16),
				mean_X.17 = mean(X.17),
				mean_Y.17 = mean(Y.17),
				mean_dif_X.1 = abs(mean_X.1 - X.1),
				mean_dif_Y.1 = abs(mean_Y.1 - Y.1),
				mean_dif_X.2 = abs(mean_X.2 - X.2),
				mean_dif_Y.2 = abs(mean_Y.2 - Y.2),
				mean_dif_X.3 = abs(mean_X.3 - X.3),
				mean_dif_Y.3 = abs(mean_Y.3 - Y.3),
				mean_dif_X.4 = abs(mean_X.4 - X.4),
				mean_dif_Y.4 = abs(mean_Y.4 - Y.4),
				mean_dif_X.5 = abs(mean_X.5 - X.5),
				mean_dif_Y.5 = abs(mean_Y.5 - Y.5),
				mean_dif_X.6 = abs(mean_X.6 - X.6),
				mean_dif_Y.6 = abs(mean_Y.6 - Y.6),
				mean_dif_X.7 = abs(mean_X.7 - X.7),
				mean_dif_Y.7 = abs(mean_Y.7 - Y.7),
				mean_dif_X.8 = abs(mean_X.8 - X.8),
				mean_dif_Y.8 = abs(mean_Y.8 - Y.8),
				mean_dif_X.9 = abs(mean_X.9 - X.9),
				mean_dif_Y.9 = abs(mean_Y.9 - Y.9),
				mean_dif_X.10 = abs(mean_X.10 - X.10),
				mean_dif_Y.10 = abs(mean_Y.10 - Y.10),
				mean_dif_X.11 = abs(mean_X.11 - X.11),
				mean_dif_Y.11 = abs(mean_Y.11 - Y.11),
				mean_dif_X.12 = abs(mean_X.12 - X.12),
				mean_dif_Y.12 = abs(mean_Y.12 - Y.12),
				mean_dif_X.13 = abs(mean_X.13 - X.13),
				mean_dif_Y.13 = abs(mean_Y.13 - Y.13),
				mean_dif_X.14 = abs(mean_X.14 - X.14),
				mean_dif_Y.14 = abs(mean_Y.14 - Y.14),
				mean_dif_X.15 = abs(mean_X.15 - X.15),
				mean_dif_Y.15 = abs(mean_Y.15 - Y.15),
				mean_dif_X.16 = abs(mean_X.16 - X.16),
				mean_dif_Y.16 = abs(mean_Y.16 - Y.16),
				mean_dif_X.17 = abs(mean_X.17 - X.17),
				mean_dif_Y.17 = abs(mean_Y.17 - Y.17),
				repeatability_X.1 = mean_dif_X.1/ abs(mean_X.1),
				repeatability_Y.1 = mean_dif_Y.1/ abs(mean_Y.1),
				repeatability_X.2 = mean_dif_X.2/ abs(mean_X.2),
				repeatability_Y.2 = mean_dif_Y.2/ abs(mean_Y.2),
				repeatability_X.3 = mean_dif_X.3/ abs(mean_X.3),
				repeatability_Y.3 = mean_dif_Y.3/ abs(mean_Y.3),
				repeatability_X.4 = mean_dif_X.4/ abs(mean_X.4),
				repeatability_Y.4 = mean_dif_Y.4/ abs(mean_Y.4),
				repeatability_X.5 = mean_dif_X.5/ abs(mean_X.5),
				repeatability_Y.5 = mean_dif_Y.5/ abs(mean_Y.5),
				repeatability_X.6 = mean_dif_X.6/ abs(mean_X.6),
				repeatability_Y.6 = mean_dif_Y.6/ abs(mean_Y.6),
				repeatability_X.7 = mean_dif_X.7/ abs(mean_X.7),
				repeatability_Y.7 = mean_dif_Y.7/ abs(mean_Y.7),
				repeatability_X.8 = mean_dif_X.8/ abs(mean_X.8),
				repeatability_Y.8 = mean_dif_Y.8/ abs(mean_Y.8),
				repeatability_X.9 = mean_dif_X.9/ abs(mean_X.9),
				repeatability_Y.9 = mean_dif_Y.9/ abs(mean_Y.9),
				repeatability_X.10 = mean_dif_X.10/ abs(mean_X.10),
				repeatability_Y.10 = mean_dif_Y.10/ abs(mean_Y.10),
				repeatability_X.11 = mean_dif_X.11/ abs(mean_X.11),
				repeatability_Y.11 = mean_dif_Y.11/ abs(mean_Y.11),
				repeatability_X.12 = mean_dif_X.12/ abs(mean_X.12),
				repeatability_Y.12 = mean_dif_Y.12/ abs(mean_Y.12),
				repeatability_X.13 = mean_dif_X.13/ abs(mean_X.13),
				repeatability_Y.13 = mean_dif_Y.13/ abs(mean_Y.13),
				repeatability_X.14 = mean_dif_X.14/ abs(mean_X.14),
				repeatability_Y.14 = mean_dif_Y.14/ abs(mean_Y.14),
				repeatability_X.15 = mean_dif_X.15/ abs(mean_X.15),
				repeatability_Y.15 = mean_dif_Y.15/ abs(mean_Y.15),
				repeatability_X.16 = mean_dif_X.16/ abs(mean_X.16),
				repeatability_Y.16 = mean_dif_Y.16/ abs(mean_Y.16),
				repeatability_X.17 = mean_dif_X.17/ abs(mean_X.17),
				repeatability_Y.17 = mean_dif_Y.17/ abs(mean_Y.17))

repeatability_summary = subset(repeatability [,c(1, 128:161)])

names(repeatability_summary) <- c(	"Season_ID", 
															"repeatability_X1", 
															"repeatability_Y1", 
															"repeatability_X2", 
															"repeatability_Y2", 
															"repeatability_X3", 
															"repeatability_Y3", 
															"repeatability_X4", 
															"repeatability_Y4", 
															"repeatability_X5", 
															"repeatability_Y5", 
															"repeatability_X6", 
															"repeatability_Y6", 
															"repeatability_X7", 
															"repeatability_Y7", 
															"repeatability_X8", 
															"repeatability_Y8", 
															"repeatability_X9", 
															"repeatability_Y9", 
															"repeatability_X10", 
															"repeatability_Y10", 
															"repeatability_X11", 
															"repeatability_Y11", 
															"repeatability_X12", 
															"repeatability_Y12", 
															"repeatability_X13", 
															"repeatability_Y13", 
															"repeatability_X14", 
															"repeatability_Y14", 
															"repeatability_X15", 
															"repeatability_Y15", 
															"repeatability_X16", 
															"repeatability_Y16", 
															"repeatability_X17", 
															"repeatability_Y17")

repeatability_summary_gathered = gather(repeatability_summary, key = "Season_ID", value = "repeatability")%>%
	mutate(Season_ID = factor(Season_ID, levels = c(
															"repeatability_X1", 
															"repeatability_Y1", 
															"repeatability_X2", 
															"repeatability_Y2", 
															"repeatability_X3", 
															"repeatability_Y3", 
															"repeatability_X4", 
															"repeatability_Y4", 
															"repeatability_X5", 
															"repeatability_Y5", 
															"repeatability_X6", 
															"repeatability_Y6", 
															"repeatability_X7", 
															"repeatability_Y7", 
															"repeatability_X8", 
															"repeatability_Y8", 
															"repeatability_X9", 
															"repeatability_Y9", 
															"repeatability_X10", 
															"repeatability_Y10", 
															"repeatability_X11", 
															"repeatability_Y11", 
															"repeatability_X12", 
															"repeatability_Y12", 
															"repeatability_X13", 
															"repeatability_Y13", 
															"repeatability_X14", 
															"repeatability_Y14", 
															"repeatability_X15", 
															"repeatability_Y15", 
															"repeatability_X16", 
															"repeatability_Y16", 
															"repeatability_X17", 
															"repeatability_Y17")))

ggplot(data = repeatability_summary_gathered, aes( x = Season_ID, y = repeatability*100 ))+
	geom_boxplot(size = .2, outlier.size = .1, outlier.shape = NA) +
	geom_jitter(size = .1, alpha = .1, width = .1)+
	theme_classic()+
	#ylim(0, 2) +
	xlab("") + ylab("measurement error (% of centroid size)")+
	theme(	axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1), 
			legend.position = "none")

range(repeatability_summary_gathered$repeatability*100)
mean(repeatability_summary_gathered$repeatability*100)
sqrt(var(repeatability_summary_gathered$repeatability*100)/length(repeatability_summary_gathered$repeatability))

ggsave('~/Dropbox/Supervisions/Anjali/Manuscript/Manuscript_Draft1/1st round comment/updated by Masahito/Fig. repeatability.pdf',h = 100, w = 140, units = 'mm', scale = 1)