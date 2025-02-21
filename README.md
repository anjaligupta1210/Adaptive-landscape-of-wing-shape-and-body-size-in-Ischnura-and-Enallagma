# Data and code for [Persistence of the ecological niche in pond damselflies underlies a stable adaptive zone despite varying selection](https://www.biorxiv.org/content/10.1101/2022.10.19.512907v3)


## Data Files

### Data is available in 

1. [Is_updated.csv](https://github.com/anjaligupta1210/Adaptive-landscape-of-wing-shape-and-body-size-in-Ischnura-and-Enallagma/blob/main/Is_updated.csv): Data file for opportunity of sexual selection and male mating success - derived from community sampling from 2018-2021
2. [DataPC_sorted_locale_fixed.csv](https://github.com/anjaligupta1210/Adaptive-landscape-of-wing-shape-and-body-size-in-Ischnura-and-Enallagma/blob/main/DataPC_sorted_locale_fixed.csv): Data file for wing shape and body size PC components with intuitive locale names to evaluate selection gradients
3. [Data_LMRenamed.xlsx](https://github.com/anjaligupta1210/Adaptive-landscape-of-wing-shape-and-body-size-in-Ischnura-and-Enallagma/blob/main/Data_LMRenamed.xlsx): Data file for repeated measurements of same specimens to evaluate repeatability
4. [Data.xlsx](https://github.com/anjaligupta1210/Adaptive-landscape-of-wing-shape-and-body-size-in-Ischnura-and-Enallagma/blob/main/Data.xlsx): Raw data for all specimens of Ischnura and Enallagma focused sampling
5. [CommunitySampling_2018.xlsx](https://github.com/anjaligupta1210/Adaptive-landscape-of-wing-shape-and-body-size-in-Ischnura-and-Enallagma/blob/main/CommunitySampling_2018.xlsx): Raw data for Odonata community sampling for 2018
6. [CommunitySampling_2019.xlsx](https://github.com/anjaligupta1210/Adaptive-landscape-of-wing-shape-and-body-size-in-Ischnura-and-Enallagma/blob/main/CommunitySampling_2019.xlsx): Raw data for Odonata community sampling for 2019
7. [CommunitySampling_2020.xlsx](https://github.com/anjaligupta1210/Adaptive-landscape-of-wing-shape-and-body-size-in-Ischnura-and-Enallagma/blob/main/CommunitySampling_2020.xlsx): Raw data for Odonata community sampling for 2020
8. [CommunitySampling_2021.xlsx](https://github.com/anjaligupta1210/Adaptive-landscape-of-wing-shape-and-body-size-in-Ischnura-and-Enallagma/blob/main/CommunitySampling_2021.xlsx): Raw data for Odonata community sampling for 2021


## R Code 

### R code is available in 

1. [JEB_#1_Data_Preparation_FINAL.R](https://github.com/anjaligupta1210/Adaptive-landscape-of-wing-shape-and-body-size-in-Ischnura-and-Enallagma/blob/main/JEB_%231_Data_Preparation_FINAL.R): Code for Generalized Procustes Analysis, Principal Component Analysis, and landmarking repeatability. This code reorganizes [Data.xlsx](https://github.com/anjaligupta1210/Adaptive-landscape-of-wing-shape-and-body-size-in-Ischnura-and-Enallagma/blob/main/Data.xlsx) to produce [DataPC_sorted_locale_fixed.csv](https://github.com/anjaligupta1210/Adaptive-landscape-of-wing-shape-and-body-size-in-Ischnura-and-Enallagma/blob/main/DataPC_sorted_locale_fixed.csv)
2. [JEB_#2_Global_Comparisons_FINAL.R](https://github.com/anjaligupta1210/Adaptive-landscape-of-wing-shape-and-body-size-in-Ischnura-and-Enallagma/blob/main/JEB_%232_Global_Comparisons_FINAL.R): Code to evaluate selection per species and choose best models.
3. [JEB_#3_Population_Comparisons_FINAL.R](https://github.com/anjaligupta1210/Adaptive-landscape-of-wing-shape-and-body-size-in-Ischnura-and-Enallagma/blob/main/JEB_%233_Population_Comparisons_FINAL.R): Code to evaluate selection per population and choose best models.
4. [JEB_#4_prereq_OppSel_FINAL.R](https://github.com/anjaligupta1210/Adaptive-landscape-of-wing-shape-and-body-size-in-Ischnura-and-Enallagma/blob/main/JEB_%234_Selection_and_ecology_FINAL.R): Code to calculate opportunity for sexual selection and male mating success estimates. This code reorganizes [CommunitySampling_2018.xlsx](https://github.com/anjaligupta1210/Adaptive-landscape-of-wing-shape-and-body-size-in-Ischnura-and-Enallagma/blob/main/CommunitySampling_2018.xlsx), [CommunitySampling_2019.xlsx](https://github.com/anjaligupta1210/Adaptive-landscape-of-wing-shape-and-body-size-in-Ischnura-and-Enallagma/blob/main/CommunitySampling_2019.xlsx), [CommunitySampling_2020.xlsx](https://github.com/anjaligupta1210/Adaptive-landscape-of-wing-shape-and-body-size-in-Ischnura-and-Enallagma/blob/main/CommunitySampling_2020.xlsx), and [CommunitySampling_2021.xlsx](https://github.com/anjaligupta1210/Adaptive-landscape-of-wing-shape-and-body-size-in-Ischnura-and-Enallagma/blob/main/CommunitySampling_2021.xlsx) to produce [Is_updated.csv](https://github.com/anjaligupta1210/Adaptive-landscape-of-wing-shape-and-body-size-in-Ischnura-and-Enallagma/blob/main/Is_updated.csv)
5. [JEB_#4_Selection_and_ecology_FINAL.R](https://github.com/anjaligupta1210/Adaptive-landscape-of-wing-shape-and-body-size-in-Ischnura-and-Enallagma/blob/main/JEB_%234_prereq_OppSel_FINAL.R): Code to summarize mating system parameters per population per year per species, Table S8, Table S9, Figure 5 and Figure S8.
6. [JEB_#5_OtherFigures_FINAL.R](https://github.com/anjaligupta1210/Adaptive-landscape-of-wing-shape-and-body-size-in-Ischnura-and-Enallagma/blob/main/JEB_%235_OtherFigures_FINAL.R): Code for Figure 3, Figure S6, Figure S7, Figure S10
7. [Figure S2.R](https://github.com/anjaligupta1210/Adaptive-landscape-of-wing-shape-and-body-size-in-Ischnura-and-Enallagma/blob/main/Figure%20S2.R): Code for Figure S2.


