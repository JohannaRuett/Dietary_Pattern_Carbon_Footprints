#### Masterthesis Johanna Rütt ####
#### Script for modelling and analyzing carbon footprints of western European dietary
#### patterns performing probabilistic simulation ####
#### Includes vizualization of results ####
#### Mai 2021

#Abbreviations for dietary pattern carbon footprints:
#O_high = omnivorous diet (7 times meat per week)
#O_medium = omnivorous diet (3-4 times meat per week)
#O_low = omnivorous diet (1-2 times meat per week)
#VT = vegetarian diet
#VN = vegan diet


#set working directory
setwd("/Users/johanna/Documents/MA/02_Calc")

#### Call packages ####
library(decisionSupport)
library(dplyr) 
library(extrafont) 
library(magrittr)
library(ggplot2) 
library(ggpubr)
library(ggstance)
library(tidyverse) 
library(moments)
library(reshape2)
library(overlapping)
library(cowplot)
library(svglite)

# Define sheets and folders
input_table <- ("co2e_dist.csv")

legend_file <- ("co2e_label.csv")
results_folder <- "results"
figures_folder <- "figures"

# Estimate variables out of upper and lower bounds
make_variables <- function(est,n=1)
{ x <- random(rho=est, n=n, method = "calculate")
for(i in colnames(x)) assign(i, as.numeric(x[1,i]),envir = .GlobalEnv)}
make_variables(estimate_read_csv(input_table))
dir.create(figures_folder)
dir.create(results_folder)
##

# Build carbon footprint simulation as a template to simulate carbon footprints for 
# different dietary patterns
CF_Function <- function(oils, beans,
                        nuts,
                        rice,
                        cereals,
                        dairy,
                        eggs,
                        fish_seaf,
                        fruits,
                        pork_orm,
                        poultry,
                        other,
                        potatoes,
                        vegetables,
                        cheese,
                        beef,
                        bwater,
                        juice,
                        soda,
                        coffee,
                        tea,
                        milk,
                        milkaltern,
                        wine,
                        beer,
                        otheralc,
                        oils_co2e,
                        beans_co2e,
                        nuts_co2e,
                        rice_co2e,
                        cereals_co2e,
                        dairy_co2e,
                        eggs_co2e,
                        fish_seaf_co2e,
                        fruits_co2e,
                        pork_orm_co2e,
                        poultry_co2e,
                        other_co2e,
                        potatoes_co2e,
                        vegetables_co2e,
                        cheese_co2e,
                        beef_co2e,
                        bwater_co2e,
                        juice_co2e,
                        soda_co2e,
                        coffee_co2e,
                        tea_co2e,
                        milk_co2e,
                        milkaltern_co2e,
                        wine_co2e,
                        beer_co2e,
                        otheralc_co2e){
  
  # Functions included
  CF <- oils *  oils_co2e + beans * beans_co2e + nuts * nuts_co2e +
    rice * rice_co2e + cereals * cereals_co2e + dairy * dairy_co2e + eggs * eggs_co2e +
    fish_seaf * fish_seaf_co2e + fruits * fruits_co2e + pork_orm * pork_orm_co2e + 
    poultry * poultry_co2e + other * other_co2e + potatoes * potatoes_co2e +
    vegetables * vegetables_co2e + cheese * cheese_co2e + beef * beef_co2e +
    bwater * bwater_co2e + juice * juice_co2e + soda *
    soda_co2e + coffee * coffee_co2e + tea * tea_co2e +
    milk * milk_co2e + milkaltern * milkaltern_co2e + wine * wine_co2e +
    beer * beer_co2e + otheralc * otheralc_co2e
  
  return(list(CF = CF))}

Simulation <- function(){
  
  #O_high simulation 
  Simulation_O_high <- CF_Function(oils = O_high_oils, beans = O_high_beans,
                                   nuts = O_high_nuts,
                                   rice = O_high_rice,
                                   cereals = O_high_cereals,
                                   dairy = O_high_dairy,
                                   eggs = O_high_eggs,
                                   fish_seaf = O_high_fish_seaf,
                                   fruits = O_high_fruits,
                                   pork_orm = O_high_pork_orm,
                                   poultry = O_high_poultry,
                                   other = O_high_other,
                                   potatoes = O_high_potatoes,
                                   vegetables = O_high_vegetables,
                                   cheese = O_high_cheese,
                                   beef = O_high_beef,
                                   bwater = O_high_bwater,
                                   juice = O_high_juice,
                                   soda = O_high_soda,
                                   coffee = O_high_coffee,
                                   tea = O_high_tea,
                                   milk = O_high_milk,
                                   milkaltern = O_high_milkaltern,
                                   wine = O_high_wine,
                                   beer = O_high_beer,
                                   otheralc = O_high_otheralc,
                                   oils_co2e = oils_co2e,
                                   beans_co2e = beans_co2e,
                                   nuts_co2e = nuts_co2e,
                                   rice_co2e = rice_co2e,
                                   cereals_co2e = cereals_co2e,
                                   dairy_co2e = dairy_co2e,
                                   eggs_co2e = eggs_co2e,
                                   fish_seaf_co2e = fish_seaf_co2e,
                                   fruits_co2e = fruits_co2e,
                                   pork_orm_co2e = pork_orm_co2e,
                                   poultry_co2e = poultry_co2e,
                                   other_co2e = other_co2e,
                                   potatoes_co2e = potatoes_co2e,
                                   vegetables_co2e = vegetables_co2e,
                                   cheese_co2e = cheese_co2e,
                                   beef_co2e = beef_co2e,
                                   bwater_co2e = bwater_co2e,
                                   juice_co2e = juice_co2e,
                                   soda_co2e = soda_co2e,
                                   coffee_co2e = coffee_co2e,
                                   tea_co2e = tea_co2e,
                                   milk_co2e = milk_co2e,
                                   milkaltern_co2e = milkaltern_co2e,
                                   wine_co2e = wine_co2e,
                                   beer_co2e = beer_co2e,
                                   otheralc_co2e = otheralc_co2e)
  
  CF_O_high<- Simulation_O_high$CF
  
  #O_medium simulation 
  Simulation_O_medium <- CF_Function(oils = O_medium_oils, beans = O_medium_beans,
                                     nuts = O_medium_nuts,
                                     rice = O_medium_rice,
                                     cereals = O_medium_cereals,
                                     dairy = O_medium_dairy,
                                     eggs = O_medium_eggs,
                                     fish_seaf = O_medium_fish_seaf,
                                     fruits = O_medium_fruits,
                                     pork_orm = O_medium_pork_orm,
                                     poultry = O_medium_poultry,
                                     other = O_medium_other,
                                     potatoes = O_medium_potatoes,
                                     vegetables = O_medium_vegetables,
                                     cheese = O_medium_cheese,
                                     beef = O_medium_beef,
                                     bwater = O_medium_bwater,
                                     juice = O_medium_juice,
                                     soda = O_medium_soda,
                                     coffee = O_medium_coffee,
                                     tea = O_medium_tea,
                                     milk = O_medium_milk,
                                     milkaltern = O_medium_milkaltern,
                                     wine = O_medium_wine,
                                     beer = O_medium_beer,
                                     otheralc = O_medium_otheralc,
                                     oils_co2e = oils_co2e,
                                     beans_co2e = beans_co2e,
                                     nuts_co2e = nuts_co2e,
                                     rice_co2e = rice_co2e,
                                     cereals_co2e = cereals_co2e,
                                     dairy_co2e = dairy_co2e,
                                     eggs_co2e = eggs_co2e,
                                     fish_seaf_co2e = fish_seaf_co2e,
                                     fruits_co2e = fruits_co2e,
                                     pork_orm_co2e = pork_orm_co2e,
                                     poultry_co2e = poultry_co2e,
                                     other_co2e = other_co2e,
                                     potatoes_co2e = potatoes_co2e,
                                     vegetables_co2e = vegetables_co2e,
                                     cheese_co2e = cheese_co2e,
                                     beef_co2e = beef_co2e,
                                     bwater_co2e = bwater_co2e,
                                     juice_co2e = juice_co2e,
                                     soda_co2e = soda_co2e,
                                     coffee_co2e = coffee_co2e,
                                     tea_co2e = tea_co2e,
                                     milk_co2e = milk_co2e,
                                     milkaltern_co2e = milkaltern_co2e,
                                     wine_co2e = wine_co2e,
                                     beer_co2e = beer_co2e,
                                     otheralc_co2e = otheralc_co2e)
  
  CF_O_medium<- Simulation_O_medium$CF
  
  #O_low simulation 
  Simulation_O_low <- CF_Function(oils = O_low_oils, beans = O_low_beans,
                                  nuts = O_low_nuts,
                                  rice = O_low_rice,
                                  cereals = O_low_cereals,
                                  dairy = O_low_dairy,
                                  eggs = O_low_eggs,
                                  fish_seaf = O_low_fish_seaf,
                                  fruits = O_low_fruits,
                                  pork_orm = O_low_pork_orm,
                                  poultry = O_low_poultry,
                                  other = O_low_other,
                                  potatoes = O_low_potatoes,
                                  vegetables = O_low_vegetables,
                                  cheese = O_low_cheese,
                                  beef = O_low_beef,
                                  bwater = O_low_bwater,
                                  juice = O_low_juice,
                                  soda = O_low_soda,
                                  coffee = O_low_coffee,
                                  tea = O_low_tea,
                                  milk = O_low_milk,
                                  milkaltern = O_low_milkaltern,
                                  wine = O_low_wine,
                                  beer = O_low_beer,
                                  otheralc = O_low_otheralc,
                                  oils_co2e = oils_co2e,
                                  beans_co2e = beans_co2e,
                                  nuts_co2e = nuts_co2e,
                                  rice_co2e = rice_co2e,
                                  cereals_co2e = cereals_co2e,
                                  dairy_co2e = dairy_co2e,
                                  eggs_co2e = eggs_co2e,
                                  fish_seaf_co2e = fish_seaf_co2e,
                                  fruits_co2e = fruits_co2e,
                                  pork_orm_co2e = pork_orm_co2e,
                                  poultry_co2e = poultry_co2e,
                                  other_co2e = other_co2e,
                                  potatoes_co2e = potatoes_co2e,
                                  vegetables_co2e = vegetables_co2e,
                                  cheese_co2e = cheese_co2e,
                                  beef_co2e = beef_co2e,
                                  bwater_co2e = bwater_co2e,
                                  juice_co2e = juice_co2e,
                                  soda_co2e = soda_co2e,
                                  coffee_co2e = coffee_co2e,
                                  tea_co2e = tea_co2e,
                                  milk_co2e = milk_co2e,
                                  milkaltern_co2e = milkaltern_co2e,
                                  wine_co2e = wine_co2e,
                                  beer_co2e = beer_co2e,
                                  otheralc_co2e = otheralc_co2e)
  
  CF_O_low<- Simulation_O_low$CF
  
  #VT simulation 
  Simulation_VT <- CF_Function(oils = VT_oils, beans = VT_beans,
                               nuts = VT_nuts,
                               rice = VT_rice,
                               cereals = VT_cereals,
                               dairy = VT_dairy,
                               eggs = VT_eggs,
                               fish_seaf = VT_fish_seaf,
                               fruits = VT_fruits,
                               pork_orm = VT_pork_orm,
                               poultry = VT_poultry,
                               other = VT_other,
                               potatoes = VT_potatoes,
                               vegetables = VT_vegetables,
                               cheese = VT_cheese,
                               beef = VT_beef,
                               bwater = VT_bwater,
                               juice = VT_juice,
                               soda = VT_soda,
                               coffee = VT_coffee,
                               tea = VT_tea,
                               milk = VT_milk,
                               milkaltern = VT_milkaltern,
                               wine = VT_wine,
                               beer = VT_beer,
                               otheralc = VT_otheralc,
                               oils_co2e = oils_co2e,
                               beans_co2e = beans_co2e,
                               nuts_co2e = nuts_co2e,
                               rice_co2e = rice_co2e,
                               cereals_co2e = cereals_co2e,
                               dairy_co2e = dairy_co2e,
                               eggs_co2e = eggs_co2e,
                               fish_seaf_co2e = fish_seaf_co2e,
                               fruits_co2e = fruits_co2e,
                               pork_orm_co2e = pork_orm_co2e,
                               poultry_co2e = poultry_co2e,
                               other_co2e = other_co2e,
                               potatoes_co2e = potatoes_co2e,
                               vegetables_co2e = vegetables_co2e,
                               cheese_co2e = cheese_co2e,
                               beef_co2e = beef_co2e,
                               bwater_co2e = bwater_co2e,
                               juice_co2e = juice_co2e,
                               soda_co2e = soda_co2e,
                               coffee_co2e = coffee_co2e,
                               tea_co2e = tea_co2e,
                               milk_co2e = milk_co2e,
                               milkaltern_co2e = milkaltern_co2e,
                               wine_co2e = wine_co2e,
                               beer_co2e = beer_co2e,
                               otheralc_co2e = otheralc_co2e)
  
  CF_VT<- Simulation_VT$CF
  
  #VN simulation 
  Simulation_VN <- CF_Function(oils = VN_oils, beans = VN_beans,
                               nuts = VN_nuts,
                               rice = VN_rice,
                               cereals = VN_cereals,
                               dairy = VN_dairy,
                               eggs = VN_eggs, 
                               fish_seaf = VN_fish_seaf,
                               fruits = VN_fruits,
                               pork_orm = VN_pork_orm,
                               poultry = VN_poultry,
                               other = VN_other,
                               potatoes = VN_potatoes,
                               vegetables = VN_vegetables,
                               cheese = VN_cheese,
                               beef = VN_beef,
                               bwater = VN_bwater,
                               juice = VN_juice,
                               soda = VN_soda,
                               coffee = VN_coffee,
                               tea = VN_tea,
                               milk = VN_milk,
                               milkaltern = VN_milkaltern,
                               wine = VN_wine,
                               beer = VN_beer,
                               otheralc = VN_otheralc,
                               oils_co2e = oils_co2e,
                               beans_co2e = beans_co2e,
                               nuts_co2e = nuts_co2e,
                               rice_co2e = rice_co2e,
                               cereals_co2e = cereals_co2e,
                               dairy_co2e = dairy_co2e,
                               eggs_co2e = eggs_co2e,
                               fish_seaf_co2e = fish_seaf_co2e,
                               fruits_co2e = fruits_co2e,
                               pork_orm_co2e = pork_orm_co2e,
                               poultry_co2e = poultry_co2e,
                               other_co2e = other_co2e,
                               potatoes_co2e = potatoes_co2e,
                               vegetables_co2e = vegetables_co2e,
                               cheese_co2e = cheese_co2e,
                               beef_co2e = beef_co2e,
                               bwater_co2e = bwater_co2e,
                               juice_co2e = juice_co2e,
                               soda_co2e = soda_co2e,
                               coffee_co2e = coffee_co2e,
                               tea_co2e = tea_co2e,
                               milk_co2e = milk_co2e,
                               milkaltern_co2e = milkaltern_co2e,
                               wine_co2e = wine_co2e,
                               beer_co2e = beer_co2e,
                               otheralc_co2e = otheralc_co2e)
  
  CF_VN<- Simulation_VN$CF
  
  ##combine O_high, O_medium, O_low, VT and VN scenarios 
  return(list(CF_O_high = CF_O_high,
              CF_O_medium = CF_O_medium,
              CF_O_low = CF_O_low,
              CF_VT = CF_VT,
              CF_VN = CF_VN))}

decisionSupport(input_table,
                results_folder,
                write_table = TRUE,
                Simulation, 10000, 
                functionSyntax = "plainNames")

#Calculate VN carbon footprint (since no simulated values feed into the carbon footprint)
CF_VN <- VN_oils *  oils_co2e + VN_beans * beans_co2e + VN_nuts * nuts_co2e +
  VN_rice * rice_co2e + VN_cereals * cereals_co2e + VN_dairy * dairy_co2e + VN_eggs * eggs_co2e +
  VN_fish_seaf * fish_seaf_co2e + VN_fruits * fruits_co2e + VN_pork_orm * pork_orm_co2e + 
  VN_poultry * poultry_co2e + VN_other * other_co2e + VN_potatoes * potatoes_co2e +
  VN_vegetables * vegetables_co2e + VN_cheese * cheese_co2e + VN_beef * beef_co2e +
  VN_bwater * bwater_co2e + VN_juice * juice_co2e + VN_soda *
  soda_co2e + VN_coffee * coffee_co2e + VN_tea * tea_co2e +
  VN_milk * milk_co2e + VN_milkaltern * milkaltern_co2e + VN_wine * wine_co2e +
  VN_beer * beer_co2e + VN_otheralc * otheralc_co2e

# welfareDecisionSummary
welfare_summary <- read.csv(paste(results_folder,"/welfareDecisionSummary.csv",sep = ""))

#### End of calculations regarding Decision Analysis ####

calc_SUSLA <- read.csv("CALCSUSLA.csv")

### Create table with all footprints ###
data_CFs <- rbind(data_VN, data_VT, data_O_low, data_O_medium, data_O_high)

data_CF_distance <- cbind(data_VN$values, data_VT$values, 
                          data_O_low$values, data_O_medium$values, data_O_high$values)

#### Calculate the share of 2- and 1.5-degree global warming thresholds trespassed ####

threshold2 <- c(sum(data_VN$values<2365)/10000,
                sum(data_VT$values<2365)/10000,
                sum(data_O_low$values<2365)/10000,
                sum(data_O_medium$values<2365)/10000,
                sum(data_O_high$values<2365)/10000)

threshold1.5 <- c(sum(data_VN$values<1337)/10000,
                  sum(data_VT$values<1337)/10000,
                  sum(data_O_low$values<1337)/10000,
                  sum(data_O_medium$values<1337)/10000,
                  sum(data_O_high$values<1337)/10000)

dietary_pattern <- c("VN", "VT", "O_low", "O_medium", "O_high")

threshold <- data.frame(dietary_pattern, threshold2, threshold1.5)

write.csv(threshold, "thresholdRitchie.csv")

#### Calculate the share of food system specific
#### 2- and 1.5-degree global warming thresholds trespassed ####
#Only VT and VN regarded because other dietary patterns already trespass 
#non-food system specific thresholds

threshold2share <- c(sum(data_VN$values<(2365/100*35))/10000,
                     sum(data_VT$values<(2365/100*35))/10000)
dietary_pattern2 <- c("VN", "VT")
thresholdshare <- data.frame(dietary_pattern2, threshold2share)


#####Calculate overlaps of dietary pattern carbon footprints#####

x$ind <- as.numeric(x$ind)
x$ind.1 <- as.numeric(x$ind.1)
x$ind.2 <- as.numeric(x$ind.2)
x$ind.3 <- as.numeric(x$ind.3)

# , X3=x$ind.2, X4=x$ind.3

listoverlap <- list(x$values, x$values.1, x$values.2, x$values.3)

overlapresult <- overlap(listoverlap, plot=TRUE)
overlapresult <- overlap(listoverlap)

#####Calculate differences of dietary pattern carbon footprints######

VNx <- data_VN$values
O_highx <- data_O_high$values
O_mediumx <- data_O_medium$values
O_lowx <- data_O_low$values
VTx <- data_VT$values

#compared to O_high
O_medium <- (O_mediumx - O_highx)
O_low <- (O_lowx - O_highx)
VT <- (VTx - O_highx)
VN <- (VNx - O_highx)

#compared to O_medium
O_high <- (O_highx - O_mediumx)
diff_O_medium1 <- (O_lowx - O_mediumx)
diff_O_mediumVT <- (VTx - O_mediumx)
diff_O_mediumVN <- (VNx - O_mediumx)

#compared to O_low
diff_O_low7 <- (O_highx - O_lowx)
diff_O_low3 <- (O_mediumx - O_lowx)
diff_O_lowVT <- (VTx - O_lowx)
diff_O_lowVN <- (VNx - O_lowx)

#compared to VTtarian
diff_VTO_high <- (O_highx - VTx)
diff_VTO_medium <- (O_mediumx - VTx)
diff_VTO_low <- (O_lowx - VTx)
diff_VTVN <- (VNx - VTx)

#compared to VN
diff_VNO_high <- (O_highx - VNx)
diff_VNO_medium <- (O_mediumx - VNx)
diff_VNO_low <- (O_lowx - VNx)
diff_VNVT <- (VTx - VNx)

####Calculate overlaps of dietary switches####

x$ind <- as.numeric(x$ind)
x$ind.1 <- as.numeric(x$ind.1)
x$ind.2 <- as.numeric(x$ind.2)
x$ind.3 <- as.numeric(x$ind.3)

listoverlap_switch <- list(O_medium,
                           O_low,
                           VT,
                           VN, diff_O_medium1, diff_O_mediumVT,
                           diff_O_mediumVN, diff_O_lowVT, diff_O_lowVN,
                           diff_VTVN)

overlapresult_switch <- overlap(listoverlap_switch, plot=TRUE)
overlapresult <- overlap(listoverlap)

###Create a summary of dietary pattern carbon footprint differences calculated 

stat_diff_O_high3 <- abs(c(quantile(O_medium, 
                                    c(0, 0.05, 0.1, 0.25, 0.5, 0.75, 0.9, 0.95, 1)), mean(O_medium)))
stat_diff_O_high1 <- abs(c(quantile(O_low, 
                                    c(0, 0.05, 0.1, 0.25, 0.5, 0.75, 0.9, 0.95, 1)), mean(O_low)))
stat_diff_O_highVT <- abs(c(quantile(VT, 
                                     c(0, 0.05, 0.1, 0.25, 0.5, 0.75, 0.9, 0.95, 1)), mean(VT)))
stat_diff_O_highVN <- abs(c(quantile(VN, 
                                     c(0, 0.05, 0.1, 0.25, 0.5, 0.75, 0.9, 0.95, 1)), mean(VN)))
stat_diff_O_medium1 <- abs(c(quantile(diff_O_medium1, 
                                      c(0, 0.05, 0.1, 0.25, 0.5, 0.75, 0.9, 0.95, 1)), mean(diff_O_medium1)))
stat_diff_O_mediumVT <- abs(c(quantile(diff_O_mediumVT, 
                                       c(0, 0.05, 0.1, 0.25, 0.5, 0.75, 0.9, 0.95, 1)), mean(diff_O_mediumVT)))
stat_diff_O_mediumVN <- abs(c(quantile(diff_O_mediumVN, 
                                       c(0, 0.05, 0.1, 0.25, 0.5, 0.75, 0.9, 0.95, 1)), mean(diff_O_mediumVN)))
stat_diff_O_lowVT <- abs(c(quantile(diff_O_lowVT, 
                                    c(0, 0.05, 0.1, 0.25, 0.5, 0.75, 0.9, 0.95, 1)), mean(diff_O_lowVT)))
stat_diff_O_lowVN <- abs(c(quantile(diff_O_lowVN, 
                                    c(0, 0.05, 0.1, 0.25, 0.5, 0.75, 0.9, 0.95, 1)), mean(diff_O_lowVN)))
stat_diff_VTVN <- abs(c(quantile(diff_VTVN, 
                                 c(0, 0.05, 0.1, 0.25, 0.5, 0.75, 0.9, 0.95, 1)), mean(diff_VTVN)))

stat_diff <- rbind(stat_diff_O_high3, stat_diff_O_high1, stat_diff_O_highVT, stat_diff_O_highVN,
                   stat_diff_O_medium1, stat_diff_O_mediumVT, stat_diff_O_mediumVN,
                   stat_diff_O_lowVT, stat_diff_O_lowVN,
                   stat_diff_VTVN)

write.csv(stat_diff, "stat_diff.csv")

#### Vizualize modelling results ####



#### End of R-Script####