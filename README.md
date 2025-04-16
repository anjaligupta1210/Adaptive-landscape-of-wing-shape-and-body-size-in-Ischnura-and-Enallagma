# Persistence of the ecological niche in pond damselflies underlies a stable adaptive zone despite varying selection

**Authors**

Anjali Gupta, Erik I. Svensson, Henrik Frietsch and Masahito Tsuboi

**Contact**

[masa.tsuboi@gmail.com](mailto:masa.tsuboi@gmail.com)

### Description

This data package contains morphological and ecological data of two pond damselfly species, *Ischnura elegans* and *Enallagma cyathigerum*, from southern Sweden. Data were obtained by the authors. Code was written by Anjali Gupta and Masahito Tsuboi.

**Data Files**

**Data is available in**

1. **Is_updated.csv**: Data file for opportunity of sexual selection and male mating success - derived from community sampling from 2018-2021. Units are specified in the main text. A description to each column in the data can be found below -

   Species: *Ischnura elegans* / *Enallagma cyathigerum,*

   Locale: Location site of specimen collected,

   MaleMatingSuccess: mean proportion of male (belonging to one species) mating at a particular locale in one field season

   Is: opportunity for sexual selection ( variance in male mating success / ((average male mating success)^2) )

   Year: Year/Field season of specimen collection

   n: frequency
2. **DataPC_sorted_locale_fixed.csv**: Data file for wing shape and body size PC components with intuitive locale names to evaluate selection gradients. Units are specified in the main text. A description to each column in the data can be found below -

   Ind_ID: Unique ID for each specimen,

   Season_ID: ID for each specimen for a particular field season,

   Genus, Genus_, Species: Genus and species name for the specimen,

   Date: Date of specimen collection,

   Locale: Location site of specimen collected,

   Cop_ID: ID for individuals caught in a couple (the male and female caught in a couple get the same ID), NA for individuals that are caught not mating,

   Sex: Male/Female,

   Age: Mature/Immature based on age at the time of specimen collection,

   Copula_, Copula: Single (0)/ Couple (1) - based on whether the individual caught non-mating / mating,

   Thor_col: Thorax color, NA if not scored,

   Col_phase: NA for this dataset,

   Parasite: Number of mites on the body of the specimen when captured,

   Morph: Female color morph type, NA for males,

   Wing: Wing length of the specimen,

   Thorax: Thorax length of the specimen,

   Length: Total body length of the specimen,

   Abdomen: Abdomen length of the specimen,

   S_4: S4 segment / abdomenal width of the specimen,

   Eggs: No of eggs laid by a female caught in a couple (NA for males and single-caught females)

   Season: Year of field season

   WingSide: Right/left wing specimen

   WingID: Forewing/ Hindwing

   X0,Y0 - X16,Y16: X and Y coordinates of the 17 landmarks on wing

   GPA COORDS NEXT: NA for this data

   X.1,Y.1 - X.17,Y.17:  Generalized Procrustes analysis transformed X and Y coordinates of the 17 landmarks on wing 

   MatingSuccess: mean proportion of male (belonging to one species) mating at a particular locale in one field season

   Fecundity: mean-standardized eggs laid by a female, NA for males and single-caught females

   LD1: Component values of major axis - LD1 from linear discriminant function analysis on wing shape

   PC1_shape - PC5_shape:  Component values of PC1-PC5 axes from principal component analysis on wing shape

   PC1size - PC2_size: Component values of PC1-PC2 axes from principal component analysis on body size

   PC1_shape2 - PC5_shape2: Squared values of PC1_shape - PC5_shape

   PC1size2 - PC2_size2: Squared values of PC1size - PC2_size

   LD12: Squared values of LD1
3. **Data_LMRenamed.csv**: Data file for repeated measurements of same specimens to evaluate repeatability. Units are pixels. A description to each column in the data can be found below -

   Season_ID: ID for each specimen for a particular field season,

   Genus, Species: Genus and species name for the specimen,

   Date: Date of specimen collection,

   Locale: Location site of specimen collected,

   Cop_ID: ID for individuals caught in a couple (the male and female caught in a couple get the same ID), NA for individuals that are caught not mating,

   Sex: Male/Female,

   Age: Mature/Immature based on age at the time of specimen collection,

   Copula_, Copula: Single (0)/ Couple (1) - based on whether the individual caught non-mating / mating,

   Thor_col: Thorax color, NA if not scored,

   Col_phase: NA for this dataset,

   Parasite: Number of mites on the body of the specimen when captured,

   Morph: Female color morph type, NA for males,

   Wing: Wing length of the specimen,

   Thorax: Thorax length of the specimen,

   Length: Total body length of the specimen,

   Abdomen: Abdomen length of the specimen,

   S_4: S4 segment / abdomenal width of the specimen,

   Eggs: No of eggs laid by a female caught in a couple (NA for males and single-caught females)

   Season: Year of field season

   WingSide: Right/left wing specimen

   WingID: Forewing/ Hindwing

   X0,Y0 - X16,Y16: X and Y coordinates of the 17 landmarks on wing

   GPA COORDS NEXT: NA for this data

   X.1,Y.1 - X.17,Y.17:  Generalized Procrustes analysis transformed X and Y coordinates of the 17 landmarks on wing 
4. **Data.csv**: Raw data for all specimens of Ischnura and Enallagma focused sampling. All data used for the presented analyses are in data file #2. This is for archiving purpose only. A description to each column in the data can be found below -

   Season_ID: ID for each specimen for a particular field season,

   Genus, Species: Genus and species name for the specimen,

   Date: Date of specimen collection,

   Locale: Location site of specimen collected,

   Cop_ID: ID for individuals caught in a couple (the male and female caught in a couple get the same ID), NA for individuals that are caught not mating,

   Sex: Male/Female,

   Age: Mature/Immature based on age at the time of specimen collection,

   Copula_, Copula: Single (0)/ Couple (1) - based on whether the individual caught non-mating / mating,

   Thor_col: Thorax color, NA if not scored,

   Col_phase: NA for this dataset,

   Parasite: Number of mites on the body of the specimen when captured,

   Morph: Female color morph type, NA for males,

   Wing: Wing length of the specimen,

   Thorax: Thorax length of the specimen,

   Length: Total body length of the specimen,

   Abdomen: Abdomen length of the specimen,

   S_4: S4 segment / abdomenal width of the specimen,

   Eggs: No of eggs laid by a female caught in a couple (NA for males and single-caught females)

   Season: Year of field season

   WingSide: Right/left wing specimen

   WingID: Forewing/ Hindwing

   X0,Y0 - X16,Y16: X and Y coordinates of the 17 landmarks on wing

   GPA COORDS NEXT: NA for this data

   X.1,Y.1 - X.17,Y.17:  Generalized Procrustes analysis transformed X and Y coordinates of the 17 landmarks on wing
5. **CommunitySampling_2018.csv**: Raw data for Odonata community sampling for 2018. Each entry represents a observation. A description to each column in the data can be found below -

   Date: Date of specimen collection,

   Time: Time of specimen collection,

   Locale: Location site of specimen collected,

   Species: Genus and species name for the specimen,

   Age: Mature/Immature based on age at the time of specimen collection,

   Morph: Female color morph type, NA for males,

   Copulation.status: Single (0)/ Couple (1) - based on whether the individual caught non-mating / mating,

   Parasites: Number of mites on the body of the specimen when captured,

   Catching.time.minutes.: Time (in minutes) spent during specimen collection during one field visit,

   Year: Year of field season,

   Ischunra.elegans.morph: *I. elegans* female color morph type, NA for males and other species,

   Ischunra.elegans.phase: *I. elegans* female color phase type based on thorax color, NA for males and other species
6. **CommunitySampling_2019.csv**: Raw data for Odonata community sampling for 2019. Each entry represents a observation. A description to each column in the data can be found below -

   Date: Date of specimen collection,

   Time: Time of specimen collection,

   Locale: Location site of specimen collected,

   Species: Genus and species name for the specimen,

   Age: Mature/Immature based on age at the time of specimen collection,

   Morph: Female color morph type, NA for males,

   Copulation.status: Single (0)/ Couple (1) - based on whether the individual caught non-mating / mating,

   Parasites: Number of mites on the body of the specimen when captured,

   Catching.time.minutes.: Time (in minutes) spent during specimen collection during one field visit,

   Year: Year of field season,

   Ischunra.elegans.morph: *I. elegans* female color morph type, NA for males and other species,

   Ischunra.elegans.phase: *I. elegans* female color phase type based on thorax color, NA for males and other species
7. **CommunitySampling_2020.csv**: Raw data for Odonata community sampling for 2020. Each entry represents a observation. A description to each column in the data can be found below -

   Date: Date of specimen collection,

   Time: Time of specimen collection,

   Locale: Location site of specimen collected,

   Species: Genus and species name for the specimen,

   Age: Mature/Immature based on age at the time of specimen collection,

   Morph: Female color morph type, NA for males,

   Copulation.status: Single (0)/ Couple (1) - based on whether the individual caught non-mating / mating,

   Parasites: Number of mites on the body of the specimen when captured,

   Catching.time.minutes.: Time (in minutes) spent during specimen collection during one field visit,

   Year: Year of field season,

   Ischunra.elegans.morph: *I. elegans* female color morph type, NA for males and other species,

   Ischunra.elegans.phase: *I. elegans* female color phase type based on thorax color, NA for males and other species
8. **CommunitySampling_2021.csv**: Raw data for Odonata community sampling for 2021. Each entry represents a observation. A description to each column in the data can be found below -

   Date: Date of specimen collection,

   Time: Time of specimen collection,

   Locale: Location site of specimen collected,

   Species: Genus and species name for the specimen,

   Age: Mature/Immature based on age at the time of specimen collection,

   Morph: Female color morph type, NA for males,

   Copulation.status: Single (0)/ Couple (1) - based on whether the individual caught non-mating / mating,

   Parasites: Number of mites on the body of the specimen when captured,

   Catching.time.minutes.: Time (in minutes) spent during specimen collection during one field visit,

   Year: Year of field season,

   Ischunra.elegans.morph: *I. elegans* female color morph type, NA for males and other species,

   Ischunra.elegans.phase: *I. elegans* female color phase type based on thorax color, NA for males and other species

**Code/Software**

**R Code is available in**

1. **JEB_#1_Data_Preparation_FINAL.R**: Code for Generalized Procustes Analysis, Principal Component Analysis, and landmarking repeatability. This code reorganizes **Data.csv** to produce **DataPC_sorted_locale_fixed.csv**
2. **JEB_#2_Global_Comparisons_FINAL.R**: Code to evaluate selection per species and choose best models.
3. **JEB_#3_Population_Comparisons_FINAL.R**: Code to evaluate selection per population and choose best models.
4. **JEB_#4_prereq_OppSel_FINAL.R**: Code to calculate opportunity for sexual selection and male mating success estimates. This code reorganizes **CommunitySampling_2018.csv**, **CommunitySampling_2019.csv**, **CommunitySampling_2020.csv**, and **CommunitySampling_2021.csv** to produce **Is_updated.csv**
5. **JEB_#4_Selection_and_ecology_FINAL.R**: Code to summarize mating system parameters per population per year per species, Table S8, Table S9, Figure 5 and Figure S8.
6. **JEB_#5_OtherFigures_FINAL.R**: Code for Figure 3, Figure S6, Figure S7, Figure S10
7. **Figure S2.R**: Code for Figure S2.

