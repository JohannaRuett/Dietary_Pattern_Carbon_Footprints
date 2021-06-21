#### Masterthesis Johanna Rütt ####
#### Script for modelling and analyzing carbon footprints of western European dietary
#### patterns performing probabilistic simulation ####
#### Includes visualization of results ####
#### June 2021

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

write.csv(threshold, "thresholds_gw.csv")

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

###### Vizualize modelling results ######

#### Dietary Pattern Carbon Footprints - Plotting ####

#O_high
data_O_high <- read.csv("results/mcSimulationResults.csv")
data_O_high <- dplyr::select(data_O_high, starts_with("CF_O_high")) %>%
  stack(drop=FALSE)
data_O_high$values <- as.numeric(data_O_high$values) 
distribution_O_high <- ggplot(data_O_high, aes(x = values, y = ind, fill = ind)) +
  geom_density(aes(y = ..density.., alpha = 0.5)) + #..density for density estimate, ..scaled to max of 1
  scale_fill_manual(labels = ("O_high"), values = ("firebrick4"),guide="legend") +
  geom_vline(aes(xintercept = 0)) +
  geom_vline(aes(xintercept = calc_SUSLA$CF[1]), color="firebrick4", linetype = "dashed") +
  ylim(0, 0.003) +
  xlim(0,8000)+
  annotate(geom = "text",
           label = "SUSLA CF",
           x = calc_SUSLA$CF[1],
           y = 0.0025,
           angle = 270, 
           vjust = 1.5,
           size = 5) +
  theme(axis.title.x=element_text(),
        axis.title.y = element_text(),
        axis.text.x=element_text(),
        axis.ticks.x=element_line(),
        legend.title = element_blank(),
        legend.position = "none",
        legend.text = element_text(size=20),
        axis.text = element_text(size=18),
        axis.title = element_text(size=15,face="bold"),
        axis.line = element_line(colour = "black"),
        panel.background = element_blank()) +
  xlab("Carbon footprint (kgCO2e per capita per year)")+ 
  ylab("Density") 

ggsave(path = "figures", "distribution_O_high.png", device = "png",  width = 40, height = 20, units = "cm")

#O_medium
data_O_medium <- read.csv("results/mcSimulationResults.csv")
data_O_medium <- dplyr::select(data_O_medium, starts_with("CF_O_medium")) %>%
  stack(drop=FALSE)
data_O_medium$values <- as.numeric(data_O_medium$values) 
distribution_O_medium <- ggplot(data_O_medium, aes(x = values, y = ind, fill = ind)) +
  geom_density(aes(y = ..density.., alpha = 0.5)) +
  scale_fill_manual(labels = ("O_medium"), values = ("red"),guide="legend") +
  #  geom_boxploth(aes(x = values, y = 0.03), width = 0.03, fill = "limegreen" ) +
  geom_vline(aes(xintercept = 0)) +
  geom_vline(aes(xintercept = calc_SUSLA$CF[2]), color="red", linetype = "dashed") +
  ylim(0, 0.003) +
  xlim(0,8000)+
  annotate(geom = "text",
           label = "SUSLA CF",
           x = calc_SUSLA$CF[2],
           y = 0.0025,
           angle = 270, 
           vjust = 1.5,
           size = 5) +
  theme(axis.title.x=element_text(),
        axis.title.y = element_text(),
        axis.text.x=element_text(),
        axis.ticks.x=element_line(),
        legend.title = element_blank(),
        legend.position = "none",
        legend.text = element_text(size=20),
        axis.text = element_text(size=18),
        axis.title = element_text(size=15,face="bold"),
        axis.line = element_line(colour = "black"),
        panel.background = element_blank()) +
  xlab("Carbon footprint (kgCO2e per capita per year)")+ 
  ylab("Density") 

ggsave(path = "figures", "distribution_O_medium.png", device = "png",  width = 40, height = 20, units = "cm")

#Olow
data_O_low <- read.csv("results/mcSimulationResults.csv")
data_O_low <- dplyr::select(data_O_low, starts_with("CF_O_low")) %>%
  stack(drop=FALSE)
data_O_low$values <- as.numeric(data_O_low$values) 
distribution_O_low <- ggplot(data_O_low, aes(x = values, y = ind, fill = ind)) +
  geom_density(aes(y = ..density.., alpha = 0.5)) +
  scale_fill_manual(labels = ("O_low"), values = ("salmon"),guide="legend") +
  #geom_boxploth(aes(x = values, y = 0.03), width = 0.03, fill = "limegreen" ) +
  geom_vline(aes(xintercept = 0)) +
  geom_vline(aes(xintercept = calc_SUSLA$CF[3]), color="salmon", linetype = "dashed") +
  ylim(0, 0.003) +
  xlim(0,8000)+
  annotate(geom = "text",
           label = "SUSLA CF",
           x = calc_SUSLA$CF[3],
           y= 0.0025,
           angle = 270, 
           vjust = 1.5,
           size = 5) +
  theme(axis.title.x=element_text(),
        axis.title.y = element_text(),
        axis.text.x=element_text(),
        axis.ticks.x=element_line(),
        legend.title = element_blank(),
        legend.position = "none",
        legend.text = element_text(size=20),
        axis.text = element_text(size=18),
        axis.title = element_text(size=15,face="bold"),
        axis.line = element_line(colour = "black"),
        panel.background = element_blank()) +
  xlab("Carbon footprint (kgCO2e per capita per year)")+ 
  ylab("Density") 

ggsave(path = "figures", "distribution_O_low.png", device = "png",  width = 40, height = 20, units = "cm")

#Vegetarian
data_VT <- read.csv("results/mcSimulationResults.csv")
data_VT <- dplyr::select(data_VT, starts_with("CF_VT")) %>%
  stack(drop=FALSE)
data_VT$values <- as.numeric(data_VT$values) 
distribution_VT <- ggplot(data_VT, aes(x = values, y = ind, fill = ind)) +
  geom_density(aes(y = ..density.., alpha = 0.5)) +
  scale_fill_manual(labels = ("VT"), values = ("limegreen"),guide="legend") +
  #  geom_boxploth(aes(x = values, y = 0.03), width = 0.03, fill = "limegreen" ) +
  geom_vline(aes(xintercept = 0)) +
  geom_vline(aes(xintercept = calc_SUSLA$CF[4]), color="darkseagreen4", linetype = "dashed") +
  ylim(0, 0.003) +
  xlim(0,8000)+
  annotate(geom = "text",
           label = "SUSLA CF",
           x = calc_SUSLA$CF[4],
           y = 0.0025,
           angle = 270, 
           vjust = 1.5,
           size = 5) +
  theme(axis.title.x=element_text(),
        axis.title.y = element_text(),
        axis.text.x=element_text(),
        axis.ticks.x=element_line(),
        legend.title = element_blank(),
        legend.position = "none",
        legend.text = element_text(size=20),
        axis.text = element_text(size=18),
        axis.title = element_text(size=15,face="bold"),
        axis.line = element_line(colour = "black"),
        panel.background = element_blank()) +
  xlab("Carbon footprint (kgCO2e per capita per year)")+ 
  ylab("Density") 

ggsave(path = "figures", "distribution_VT.png", device = "png",  width = 40, height = 20, units = "cm")

#Vegan
data_VN <- read.csv("results/mcSimulationResults.csv")
data_VN <- dplyr::select(data_VN, starts_with("CF_VN")) %>%
  stack(drop=FALSE)
data_VN$values <- as.numeric(data_VN$values) 
distribution_VN <- ggplot(data_VN, aes(x = values, y = ind)) +
  geom_vline(aes(xintercept = values[2]), color = "darkgreen") +
  geom_vline(aes(xintercept = 0)) +
  geom_vline(aes(xintercept = calc_SUSLA$CF[5]), color = "darkgreen", linetype = "dashed") +
  ylim(0, 0.003) +
  xlim(0,8000)+
  annotate(geom = "text",
           label = "SUSLA CF",
           x = calc_SUSLA$CF[5],
           y = 0.0025,
           angle = 270, 
           vjust = 1.5,
           size = 5) +
  theme(axis.title.x=element_text(),
        axis.title.y = element_text(),
        axis.text.x=element_text(),
        axis.ticks.x=element_line(),
        legend.title = element_blank(),
        legend.position = "none",
        legend.text = element_text(size=20),
        axis.text = element_text(size=18),
        axis.title = element_text(size=15,face="bold"),
        axis.line = element_line(colour = "black"),
        panel.background = element_blank()) +
  xlab("Carbon footprint (kgCO2e per capita per year)")+ 
  ylab("Density") 

ggsave(path = "figures", "distribution_VN.png", device = "png",  width = 40, height = 20, units = "cm")

#Combine plots in a grid

plot_grid(distribution_VN,
          distribution_VT,
          distribution_O_low,
          distribution_O_medium,
          distribution_O_high,
          labels = c('Vegan','Vegetarian',
                     "O_low",
                     "O_medium",
                     "O_high"),
          label_size = 18,
          label_x = 0.5,
          label_y = 0.9,
          scale = 0.9,
          nrow = 2)


ggsave(path = "figures", "compare_all_pf.png", device = "png",  
       width = 40, height = 30, units = "cm")

#Combine distributions in a single plot

x <- data.frame(data_O_high, data_O_medium, data_O_low, data_VT)
data <- melt(x)

distribution_all <- ggplot(data, aes(x=value, fill=variable)) + 
  geom_density(alpha = 0.5) +
  geom_vline(aes(xintercept = CF_VN, color = "VN"), size = 0.75) +
  scale_color_manual(values = "darkgreen")+
  scale_fill_manual(values = c("firebrick4", "red", "salmon", "limegreen")) +
  geom_vline(aes(xintercept = 0)) +
  xlim(0, 10000) +
  ylim(0, 0.003) +
  theme(axis.title.x=element_text(),
        axis.title.y = element_text(),
        axis.text.x=element_text(),
        axis.ticks.x=element_line(),
        legend.title = element_blank(),
        legend.position = "none",
        legend.text = element_text(size=20),
        axis.text = element_text(size=20),
        axis.title = element_text(size=18,face="bold"),
        axis.line = element_line(colour = "black"),
        panel.background = element_blank()) +
  xlab("Carbon footprint (kgCO2e per capita per year)")+ 
  ylab("Density") 

ggsave(path = "figures", "distribution_all.png", device = "png",  width = 40, height = 20, units = "cm")

#Include 1.5- and 2-degree global warming greenhouse gas emission thresholds
#in the plots

distribution_all <- ggplot(data, aes(x=value, fill=variable)) + 
  geom_density(alpha = 0.5) +
  geom_vline(aes(xintercept = CF_VN, color = "VN"), size = 0.75) +
  scale_color_manual(values = "darkgreen")+
  scale_fill_manual(values = c("firebrick4", "red", "salmon", "limegreen")) +
  geom_vline(aes(xintercept = 0)) +
  geom_vline(xintercept = 2365, linetype = "dashed", color = "gold3", size = 1) +
  geom_vline(xintercept = 1337, linetype = "dashed", color = "orange3", size = 1) +
  annotate(geom = "text",
           label = c("2-degree threshold", "1.5-degree threshold"),
           color = c("gold3", "orange3"),
           x = c(2345, 1337),
           y = c(0.0025, 0.0025),
           angle = 270, 
           vjust = -1.5,
           size = 5) +
  xlim(0, 10000) +
  ylim(0, 0.003) +
  theme(axis.title.x=element_text(),
        axis.title.y = element_text(),
        axis.text.x=element_text(),
        axis.ticks.x=element_line(),
        legend.title = element_blank(),
        legend.position = "none",
        legend.text = element_text(size=20),
        axis.text = element_text(size=20),
        axis.title = element_text(size=18,face="bold"),
        axis.line = element_line(colour = "black"),
        panel.background = element_blank()) +
  xlab("Carbon footprint (kgCO2e per capita per year)")+ 
  ylab("Density") 

ggsave(path = "figures", "distribution_gwthresholds.png", device = "png",  
       width = 40, height = 20, units = "cm")

#Include food system specific 1.5- and 2-degree global warming greenhouse gas 
#emission thresholds in the plots

distribution_all <- ggplot(data, aes(x=value, fill=variable)) +
  annotate("rect", xmin = 1337/100*9.5, xmax = 1337/100*35, ymin = 0, ymax =Inf,
           fill = "orange3", alpha = 0.2)+
  annotate("rect", xmin = 2365/100*9.5, xmax = 2365/100*35, ymin = 0, ymax =Inf,
           fill = "gold", alpha = 0.2)+
  geom_density(alpha = 0.5) +
  geom_vline(xintercept = CF_VN, color = "darkgreen", size = 0.75) +
  scale_fill_manual(values = c("firebrick4", "red", "salmon", "limegreen")) +
  geom_vline(xintercept = (1337/100*9.5), color = "orange3", linetype = "dashed", size = 0.5) +
  geom_vline(xintercept = (1337/100*35), color = "orange3", linetype = "dashed", size = 0.5) +
  geom_vline(xintercept = (2365/100*9.5), color = "gold3", linetype = "dashed", size = 0.5) +
  geom_vline(xintercept = (2365/100*35), color = "gold3", linetype = "dashed", size = 0.5) +
  annotate(geom = "text",
           label = c("1.5-degree threshold EU food system", "2-degree threshold EU food system"),
           x = c(1337/100*35, 2365/100*35),
           y = c(0.0015, 0.0015),
           color = c("orange3", "gold3"),
           angle = 270, 
           vjust = 2,
           size = 5) +
  geom_vline(aes(xintercept = 0)) +
  xlim(0, 10000) +
  ylim(0, 0.003) +
  theme(axis.title.x=element_text(),
        axis.title.y = element_text(),
        axis.text.x=element_text(),
        axis.ticks.x=element_line(),
        legend.title = element_blank(),
        legend.position = "none",
        legend.text = element_text(size=20),
        axis.text = element_text(size=20),
        axis.title = element_text(size=18,face="bold"),
        axis.line = element_line(colour = "black"),
        panel.background = element_blank()) +
  xlab("Carbon footprint (kgCO2e per capita per year)")+ 
  ylab("Density") 

ggsave(path = "figures", "distribution_gwthresholdsfood.png", device = "png",  
       width = 40, height = 20, units = "cm")

#####Changes of carbon footprint when switching dietary patterns #####

#Switching from O_high 
diff_O_high <- data.frame(O_medium,
                          O_low,
                          VT, 
                          VN)
data_diff_O_high <- melt(diff_O_high)

diff_O_high_pf <- ggplot(data_diff_O_high, aes(x=value, fill=variable)) + 
  geom_density(alpha=0.5) +
  scale_fill_manual(name = "CF change when switching to", values = c("red", "salmon","limegreen", "darkgreen")) +
  geom_vline(aes(xintercept = 0)) +
  geom_vline(aes(xintercept = (-739), color = "O_medium"), linetype = "dashed") +
  geom_vline(aes(xintercept = (-1638), color = "O_low"), linetype = "dashed") +
  geom_vline(aes(xintercept = (-1819), color = "VT"), linetype = "dashed") +
  geom_vline(aes(xintercept = (-2153), color = "VN"), linetype = "dashed") +
  scale_color_manual(name = "SUSLA CF change when switching to", values = c("salmon","red", "darkgreen","darkseagreen4")) +
  xlim(-6000, 6000) +
  coord_cartesian(ylim = c(0, 0.003))+
  #  ylim(0, 0.01) +
  theme(axis.title.x=element_text(),
        axis.title.y = element_text(),
        axis.text.x=element_text(),
        axis.ticks.x=element_line(),
        legend.title = element_text(),
        legend.text = element_text(size=14),
        legend.position = "none",
        axis.text = element_text(size=14),
        axis.title = element_text(size=14,face="bold"),
        axis.line = element_line(colour = "black"),
        panel.background = element_blank()) +
  xlab("Changes in CF (kgCO2e per capita per year)")+ 
  ylab("Density") 

ggsave(path = "figures", "diff_0_high.png", device = "png",  width = 40, height = 20, units = "cm")

#Switching from O_medium

diff_O_medium <- data.frame(O_high,
                            diff_O_medium1,
                            diff_O_mediumVT,
                            diff_O_mediumVN)
data_diff_O_medium <- melt(diff_O_medium)

diff_O_medium_pf <- ggplot(data_diff_O_medium, aes(x=value, fill=variable)) + geom_density(alpha=0.5) +
  geom_vline(aes(xintercept = 0)) +
  geom_vline(aes(xintercept = (739), color = "O_high"), linetype = "dashed") +
  scale_color_manual(name = "SUSLA CF change when switching to", values = c("firebrick4")) +
  geom_vline(xintercept = (-845), color = "salmon", linetype = "dashed") +
  geom_vline(xintercept = (-1026), color = "darkseagreen4", linetype = "dashed") +
  geom_vline(xintercept = (-1360), color = "darkgreen", linetype = "dashed") +
  scale_fill_manual(name = "CF change when switching to", values = c("firebrick4", "salmon", "limegreen", "darkgreen")) +
  xlim(-6000, 6000) +
  # ylim(0, 0.01) +
  coord_cartesian(ylim = c(0, 0.003))+
  theme(axis.title.x=element_text(),
        axis.title.y = element_text(),
        axis.text.x=element_text(),
        axis.ticks.x=element_line(),
        legend.title = element_blank(),
        legend.text = element_text(size=14),
        legend.position = "none",
        axis.text = element_text(size=14),
        axis.title = element_text(size=14,face="bold"),
        axis.line = element_line(colour = "black"),
        panel.background = element_blank()) +
  xlab("Changes in CF (kgCO2e per capita per year)")+ 
  ylab("Density") 

ggsave(path = "figures", "diff_O_medium.png", device = "png",  width = 40, height = 20, units = "cm")

#Switching from O_low

diff_O_low <- data.frame(diff_O_low7,
                         diff_O_low3,
                         diff_O_lowVT,
                         diff_O_lowVN)
data_diff_O_low <- melt(diff_O_low)

diff_O_low_pf <- ggplot(data_diff_O_low, aes(x=value, fill=variable)) + geom_density(alpha=0.5) +
  geom_vline(aes(xintercept = 0)) +
  geom_vline(xintercept = (1638), color = "firebrick4", linetype = "dashed") +
  geom_vline(xintercept = (845), color = "red", linetype = "dashed") +
  geom_vline(xintercept = (-181), color = "darkseagreen4", linetype = "dashed") +
  geom_vline(xintercept = (-515), color = "darkgreen", linetype = "dashed") +
  scale_fill_manual(values = c("firebrick4", "red", "limegreen", "darkgreen")) +
  xlim(-6000, 6000) +
  #  coord_cartesian(ylim = c(0, 0.003))+
  ylim(0, 0.01) +
  theme(axis.title.x=element_text(),
        axis.title.y = element_text(),
        axis.text.x=element_text(),
        axis.ticks.x=element_line(),
        legend.title = element_blank(),
        legend.text = element_text(size=14),
        legend.position = "none",
        axis.text = element_text(size=14),
        axis.title = element_text(size=14,face="bold"),
        axis.line = element_line(colour = "black"),
        panel.background = element_blank()) +
  xlab("Changes in CF (kgCO2e per capita per year)")+ 
  ylab("Density") 

ggsave(path = "figures", "diff_O_low_max.png", device = "png",  width = 40, height = 20, units = "cm")

#Switching from Vegetarian

diff_VT <- data.frame(diff_VTO_high,
                      diff_VTO_medium,
                      diff_VTO_low,
                      diff_VTVN)
data_diff_VT <- melt(diff_VT)

diff_VT_pf <- ggplot(data_diff_VT, aes(x=value, fill=variable)) + geom_density(alpha=0.5) +
  geom_vline(aes(xintercept = 0)) +
  geom_vline(xintercept = (1819), color = "firebrick4", linetype = "dashed") +
  geom_vline(xintercept = (1026), color = "red", linetype = "dashed") +
  geom_vline(xintercept = (181), color = "salmon", linetype = "dashed") +
  geom_vline(xintercept = (-334), color = "darkgreen", linetype = "dashed") +
  scale_fill_manual(values = c("firebrick4", "red", "salmon", "darkgreen")) +
  xlim(-6000, 6000) +
  coord_cartesian(ylim = c(0, 0.003))+
  #  ylim(0, 0.01) +
  theme(axis.title.x=element_text(),
        axis.title.y = element_text(),
        axis.text.x=element_text(),
        axis.ticks.x=element_line(),
        legend.title = element_blank(),
        legend.text = element_text(size=14),
        legend.position = "none",
        axis.text = element_text(size=14),
        axis.title = element_text(size=14,face="bold"),
        axis.line = element_line(colour = "black"),
        panel.background = element_blank()) +
  xlab("Changes in CF (kgCO2e per capita per year)")+ 
  ylab("Density") 

ggsave(path = "figures", "diff_VT.png", device = "png",  width = 40, height = 20, units = "cm")

#Switching from Vegan

diff_VN <- data.frame(diff_VNO_high,
                      diff_VNO_medium,
                      diff_VNO_low,
                      diff_VNVT)
data_diff_VN <- melt(diff_VN)

diff_VN_pf <- ggplot(data_diff_VN, aes(x=value, fill=variable)) + geom_density(alpha=0.5) +
  geom_vline(aes(xintercept = 0)) +
  geom_vline(xintercept = (2153), color = "firebrick4", linetype = "dashed") +
  geom_vline(xintercept = (1360), color = "red", linetype = "dashed") +
  geom_vline(xintercept = (515), color = "salmon", linetype = "dashed") +
  geom_vline(xintercept = (334), color = "darkseagreen4", linetype = "dashed") +
  scale_fill_manual(values = c("firebrick4", "red", "salmon", "limegreen")) +
  #  scale_y_continuous(limits = c(0,0.003)) +
  xlim(-6000, 6000) +
  coord_cartesian(ylim = c(0, 0.003))+
  #  ylim(0, 0.01) +
  theme(axis.title.x=element_text(),
        axis.title.y = element_text(),
        axis.text.x=element_text(),
        axis.ticks.x=element_line(),
        legend.title = element_blank(),
        legend.text = element_text(size=14),
        legend.position = "none",
        axis.text = element_text(size=14),
        axis.title = element_text(size=14,face="bold"),
        axis.line = element_line(colour = "black"),
        panel.background = element_blank()) +
  xlab("Changes in CF (kgCO2e per capita per year)")+ 
  ylab("Density") 

ggsave(path = "figures", "diff_VN.png", device = "png",  width = 40, 
       height = 20, units = "cm")

#Combine plots

plot_grid(diff_VN_pf,
          diff_VT_pf,
          diff_O_low_pf,
          diff_O_medium_pf,
          diff_O_high_pf,
          labels = c('Change from \n Vegan','Change from \n Vegetarian',
                     'Change from \n O_low',"Change from \n O_medium",
                     'Change from \n O_high'),
          label_size = 16,
          label_x = 0.175,
          hjust = -0.06,
          vjust = 2,
          scale = 0.9,
          ncol = 2)

ggsave(path = "figures", file="diff_all.png",
       width = 40, height = 40, units = "cm")

####VIP Plots####
#O_high
VIP_data_Tests <- read.csv("results/CF_O_high_pls_results.csv",
                           header = TRUE) %>%
  data.frame(.) %>%
  .[order(-.$VIP),] %>%
  head(., 8) 

colnames(VIP_data_Tests)[colnames(VIP_data_Tests)=="X"] <- "Variable"

VIP_data_Tests$Variable <- as.character(VIP_data_Tests$Variable) 

VIP_data_Tests <- VIP_data_Tests[order(VIP_data_Tests$VIP), ]  # sort
VIP_data_Tests$Variable <- factor(VIP_data_Tests$Variable, levels = VIP_data_Tests$Variable)
VIP_data_Tests$VIP <- round(VIP_data_Tests$VIP, digits= 2)

VIP_data_Tests$color <- ifelse(VIP_data_Tests$VIP>0.8, "forestgreen", "white")
VIP_data_Tests$color <- as.character(VIP_data_Tests$color)
VIP_barplot_O_high <- ggplot(VIP_data_Tests, aes(Variable, VIP, label = VIP, fill = VIP)) +
  geom_bar(width = 1, stat = "identity", color = "black", fill = VIP_data_Tests$color) +
  geom_text(aes(y=3, label=VIP), size = 4.8, color="black", hjust = 1)+ #or max as VIP
  ggtitle(" ") +
  geom_hline(yintercept = 0.8) +
  theme_bw()+theme(panel.grid=element_blank())+
  coord_flip()+
  ylim(0,4)+
  theme(plot.title = element_text(color = "black", size = 15, face = "bold"),
        axis.title.y = element_blank(),
        legend.title = element_blank(),
        legend.text = element_blank(),
        legend.position = "none",
        axis.title.x=element_text(),
        axis.text = element_text(size=15),
        axis.title = element_text(size=15,face="bold"),
        axis.line = element_line(colour = "black"),
        panel.background = element_blank())
ggsave(path = "figures", "VIP_O_high.png", device = "png",  width = 20, height = 20, units = "cm")

#O_medium
VIP_data_Tests <- read.csv("results/CF_O_medium_pls_results.csv",
                           header = TRUE) %>%
  data.frame(.) %>%
  .[order(-.$VIP),] %>%
  head(., 8) 

colnames(VIP_data_Tests)[colnames(VIP_data_Tests)=="X"] <- "Variable"

VIP_data_Tests$Variable <- as.character(VIP_data_Tests$Variable) 

VIP_data_Tests <- VIP_data_Tests[order(VIP_data_Tests$VIP), ]  # sort
VIP_data_Tests$Variable <- factor(VIP_data_Tests$Variable, levels = VIP_data_Tests$Variable)
VIP_data_Tests$VIP <- round(VIP_data_Tests$VIP, digits= 2)

VIP_data_Tests$color <- ifelse(VIP_data_Tests$VIP>0.8, "forestgreen", "white")
VIP_data_Tests$color <- as.character(VIP_data_Tests$color)
VIP_barplot_O_medium <- ggplot(VIP_data_Tests, aes(Variable, VIP, label = VIP, fill = VIP)) +
  geom_bar(width = 1, stat = "identity", color = "black", fill = VIP_data_Tests$color) +
  geom_text(aes(y=3, label=VIP), size = 4.8, color="black", hjust = 1)+ #or max as VIP
  ggtitle(" ") +
  geom_hline(yintercept = 0.8) +
  theme_bw()+theme(panel.grid=element_blank())+
  coord_flip()+
  ylim(0,4)+
  theme(plot.title = element_text(color = "black", size = 15, face = "bold"),
        axis.title.y = element_blank(),
        legend.title = element_blank(),
        legend.text = element_blank(),
        legend.position = "none",
        axis.title.x=element_text(),
        axis.text = element_text(size=15),
        axis.title = element_text(size=15,face="bold"),
        axis.line = element_line(colour = "black"),
        panel.background = element_blank())
ggsave(path = "figures", "VIP_O_medium.png", device = "png",  width = 20, height = 20, units = "cm")

#O_low
VIP_data_Tests <- read.csv("results/CF_O_low_pls_results.csv",
                           header = TRUE) %>%
  data.frame(.) %>%
  .[order(-.$VIP),] %>%
  head(., 8) 

colnames(VIP_data_Tests)[colnames(VIP_data_Tests)=="X"] <- "Variable"

VIP_data_Tests$Variable <- as.character(VIP_data_Tests$Variable) 

VIP_data_Tests <- VIP_data_Tests[order(VIP_data_Tests$VIP), ]  # sort
VIP_data_Tests$Variable <- factor(VIP_data_Tests$Variable, levels = VIP_data_Tests$Variable)
VIP_data_Tests$VIP <- round(VIP_data_Tests$VIP, digits= 2)

VIP_data_Tests$color <- ifelse(VIP_data_Tests$VIP>0.8, "forestgreen", "white")
VIP_data_Tests$color <- as.character(VIP_data_Tests$color)
VIP_barplot_O_low <- ggplot(VIP_data_Tests, aes(Variable, VIP, label = VIP, fill = VIP)) +
  geom_bar(width = 1, stat = "identity", color = "black", fill = VIP_data_Tests$color) +
  geom_text(aes(y=3, label=VIP), size = 4.8, color="black", hjust = 1)+ #or max as VIP
  ggtitle(" ") +
  geom_hline(yintercept = 0.8) +
  theme_bw()+theme(panel.grid=element_blank())+
  coord_flip()+
  ylim(0,4)+
  theme(plot.title = element_text(color = "black", size = 15, face = "bold"),
        axis.title.y = element_blank(),
        legend.title = element_blank(),
        legend.text = element_blank(),
        legend.position = "none",
        axis.title.x=element_text(),
        axis.text = element_text(size=15),
        axis.title = element_text(size=15,face="bold"),
        axis.line = element_line(colour = "black"),
        panel.background = element_blank())
ggsave(path = "figures", "VIP_O_low.png", device = "png",  width = 20, height = 20, units = "cm")

#Vegetarian
VIP_data_Tests <- read.csv("results/CF_VT_pls_results.csv",
                           header = TRUE) %>%
  data.frame(.) %>%
  .[order(-.$VIP),] %>%
  head(., 8) 

colnames(VIP_data_Tests)[colnames(VIP_data_Tests)=="X"] <- "Variable"

VIP_data_Tests$Variable <- as.character(VIP_data_Tests$Variable) 

VIP_data_Tests <- VIP_data_Tests[order(VIP_data_Tests$VIP), ]  # sort
VIP_data_Tests$Variable <- factor(VIP_data_Tests$Variable, levels = VIP_data_Tests$Variable)
VIP_data_Tests$VIP <- round(VIP_data_Tests$VIP, digits= 2)

VIP_data_Tests$color <- ifelse(VIP_data_Tests$VIP>0.8, "forestgreen", "white")
VIP_data_Tests$color <- as.character(VIP_data_Tests$color)
VIP_barplot_VT <- ggplot(VIP_data_Tests, aes(Variable, VIP, label = VIP, fill = VIP)) +
  geom_bar(width = 1, stat = "identity", color = "black", fill = VIP_data_Tests$color) +
  geom_text(aes(y=3, label=VIP), size = 4.8, color="black", hjust = 1)+ #or max as VIP
  ggtitle(" ") +
  geom_hline(yintercept = 0.8) +
  theme_bw()+theme(panel.grid=element_blank())+
  coord_flip()+
  ylim(0,4)+
  theme(plot.title = element_text(color = "black", size = 15, face = "bold"),
        axis.title.y = element_blank(),
        legend.title = element_blank(),
        legend.text = element_blank(),
        legend.position = "none",
        axis.title.x=element_text(),
        axis.text = element_text(size=15),
        axis.title = element_text(size=15,face="bold"),
        axis.line = element_line(colour = "black"),
        panel.background = element_blank())
ggsave(path = "figures", "VIP_VT.png", device = "png",  width = 20, height = 20, units = "cm")

#Combine VIP plots

plot_grid(VIP_barplot_O_high,
          VIP_barplot_O_medium,
          VIP_barplot_O_low,
          VIP_barplot_VT,
          labels = c('O_high','O_medium',"O_low","Vegetarian"),
          label_x = 0.2,
          ncol = 2)

ggsave(path = "figures", file="VIP_all.png",
       width = 40, height = 30, units = "cm")

####Plot carbon footprints found in literature####

lit <- read.csv("scenarios_lit.csv")

lit_graph_study <- ggplot(lit, aes(y=CF, x=study, color= diet_type)) + 
  geom_point(size=4) +
  geom_hline(yintercept = (1337), color = "orange3", linetype = "dashed") +
  geom_hline(yintercept = (2365), color = "gold3", linetype = "dashed") +
  annotate(geom = "text",
           label = c("2-degree threshold", "1.5-degree threshold"),
           color = c("gold3", "orange3"),
           x = c(8.9,8.9),
           y = c(2425, 1397),
           size = 5) +
  scale_color_manual(values = c("O_medium" = "red",
                                "Vegetarian"="limegreen",
                                "Vegan"="darkgreen",
                                "Pescetarian" = "blue",
                                "O_low" = "salmon",
                                "O_high" = "firebrick4")) +
  theme(axis.title.x=element_text(),
        axis.title.y = element_text(),
        axis.text.x=element_text(angle = 45, hjust=1), #if 90° then add  vjust=0.5
        axis.ticks.x=element_line(),
        legend.title = element_blank(),
        legend.text = element_text(size=14),
        legend.position = "right",
        axis.text = element_text(size=12),
        axis.title = element_text(size=14,face="bold"),
        axis.line = element_line(colour = "black"),
        panel.background = element_blank()) +
  ylab("Carbon footprint (kgCO2e per capita per year)") +
  xlab("Study")

ggsave(path = "figures", "lit_study.png", device = "png",  
       width = 40, height = 20, units = "cm")

lit_graph_order <- ggplot(lit, aes(y=CF, x=(reorder(diet_type, number)), 
                                   color = diet_type)) +
  geom_boxplot(color="black") +
  geom_point(size=4) +
  geom_hline(yintercept = (1337), color = "orange3", linetype = "dashed") +
  geom_hline(yintercept = (2365), color = "gold3", linetype = "dashed") +
  # scale_x_discrete(lit) +
  scale_color_manual(values = c("O_medium" = "red",
                                "Vegetarian"="limegreen",
                                "Vegan"="darkgreen",
                                "PES" = "blue",
                                "O_low" = "salmon",
                                "O_high" = "firebrick4")) +
  annotate(geom = "text",
           label = c("2-degree threshold", "1.5-degree threshold"),
           color = c("gold3", "orange3"),
           x = c(6.1,6.1),
           y = c(2425, 1397),
           size = 5) +
  theme(axis.title.x=element_text(),
        axis.title.y = element_text(),
        axis.text.x=element_text(angle = 45, hjust=1),
        axis.ticks.x=element_line(),
        legend.title = element_blank(),
        legend.text = element_text(size=14),
        legend.position = "none",
        axis.text = element_text(size=12),
        axis.title = element_text(size=14,face="bold"),
        axis.line = element_line(colour = "black"),
        panel.background = element_blank()) +
  ylab("Carbon footprint (kgCO2e per capita per year)") +
  xlab("Dietary pattern")

ggsave(path = "figures", "lit_order.png", device = "png",  
       width = 40, height = 20, units = "cm")

plot_grid(lit_graph_study, lit_graph_order,
          label_x = 0.2,
          nrow = 2,
          scale = 0.9)

ggsave(path = "figures", "lit_studyorder.png", device = "png",  
       width = 40, height = 40, units = "cm")

####Compare results to carbon footprints in literature####

#Vegan carbon footprints
Vegan_lit<- read.csv("VN_lit.csv")
data_boxplotVN <- rbind(data_VN,Vegan_lit)
boxplot_VN <- ggplot(data_boxplotVN, aes(y=ind, x=values, fill = ind,
                                         color= ind )) +
  geom_boxplot(alpha=0.5) +
  scale_fill_manual(values = c("darkgreen", "darkgreen", "darkgreen","darkgreen",
                               "darkgreen", "darkgreen", "darkgreen", "darkgreen")) +
  scale_color_manual(values = c("darkgreen", "darkgreen", "darkgreen","darkgreen",
                                "darkgreen", "darkgreen", "darkgreen", "darkgreen")) +
  scale_y_discrete(limits=rev) +
  stat_summary(fun=mean, colour=c("darkgreen"), geom="point", 
               shape=18, size=3, show.legend=FALSE) + 
  scale_x_continuous(breaks = c(0, 1000, 2000, 3000, 4000, 5000, 6000, 7000, 8000,
                                9000, 10000)) + #changes the ticks displayed
  xlim(0, 10000) +
  theme(axis.title.x=element_text(),
        axis.title.y = element_blank(),
        axis.text.x=element_text(),
        axis.ticks.x=element_line(),
        legend.title = element_blank(),
        legend.position = "none",
        legend.text = element_text(size=20),
        axis.text = element_text(size=14),
        axis.title = element_text(size=10,face="bold"),
        axis.line = element_line(colour = "black"),
        panel.background = element_blank(),
        panel.border = element_rect(colour = "black", fill=NA)) +
  ylab("study")+ 
  xlab("Carbon footprint (kgCO2e per capita per year)") 
ggsave(path = "figures", "boxplot_VN.png", device = "png",  width = 40, 
       height = 8, units = "cm")

#Vegetarian carbon footprints
VT_lit<- read.csv("VT_lit.csv")
data_boxplotVT <- rbind(data_VT,VT_lit)
boxplot_VT <- ggplot(data_boxplotVT, aes(y=ind, x=values, fill = ind,
                                         color= ind )) +
  geom_boxplot(alpha=0.5) +
  scale_fill_manual(values = c("limegreen", "limegreen", "limegreen","limegreen",
                               "limegreen", "limegreen", "limegreen", "limegreen", 
                               "limegreen", "limegreen", "limegreen")) +
  scale_color_manual(values = c("black", "limegreen", "limegreen","limegreen",
                                "limegreen", "limegreen", "limegreen", "limegreen", 
                                "limegreen", "limegreen", "limegreen")) +
  scale_y_discrete(limits=rev) +
  stat_summary(fun=mean, colour=c("black", "limegreen", "limegreen","limegreen",
                                  "limegreen", "limegreen", "limegreen", "limegreen", 
                                  "limegreen", "limegreen", "limegreen"), geom="point", 
               shape=18, size=3, show.legend=FALSE) + 
  scale_x_continuous(breaks = c(0, 1000, 2000, 3000, 4000, 5000, 6000, 7000, 8000,
                                9000, 10000)) + #changes the ticks displayed
  # scale_x_discrete(name = "dietary pattern", limits = c("CF_VN,", 
  xlim(0, 10000) +
  theme(axis.title.x=element_text(),
        axis.title.y = element_blank(),
        axis.text.x=element_text(),
        axis.ticks.x=element_line(),
        legend.title = element_blank(),
        legend.position = "none",
        legend.text = element_text(size=20),
        axis.text = element_text(size=14),
        axis.title = element_text(size=10,face="bold"),
        axis.line = element_line(colour = "black"),
        panel.background = element_blank(),
        panel.border = element_rect(colour = "black", fill=NA)) +
  ylab("study")+ 
  xlab("Carbon footprint (kgCO2e per capita per year)") 
ggsave(path = "figures", "boxplot_VT.png", device = "png",  width = 50, height = 10, units = "cm")

#Omnivorous carbon footprints
O_lit<- read.csv("O_lit.csv")
data_boxplotOMN <- rbind(data_O_low, data_O_medium, data_O_high, O_lit)
boxplot_OMN <- ggplot(data_boxplotOMN, aes(y=ind, x=values, fill = ind,
                                           color= ind )) +
  geom_boxplot(alpha=0.5) +
  scale_fill_manual(values = c("salmon","red","firebrick4","salmon","red","firebrick4",
                               "red","red","red","salmon",
                               "red","salmon","red","red","salmon","red","red",
                               "red","red","red","salmon")) +
  scale_color_manual(values = c("black","black","black","salmon","red","firebrick4",
                                "red","red","red","salmon", 
                                "red","salmon","red","salmon","red","red","red",
                                "red","red","red","salmon")) +
  scale_y_discrete(limits=rev) +
  stat_summary(fun=mean, colour=c("black","black","black","salmon","red","firebrick4",
                                  "red","red","red","salmon", 
                                  "red","salmon","red","salmon","red","red","red",
                                  "red","red","red","salmon"), geom="point", 
               shape=18, size=3, show.legend=FALSE) + 
  scale_x_continuous(breaks = c(0, 1000, 2000, 3000, 4000, 5000, 6000, 7000, 8000,
                                9000, 10000)) + #changes the ticks displayed
  xlim(0, 10000) +
  theme(axis.title.x=element_text(),
        axis.title.y = element_blank(),
        axis.text.x=element_text(),
        axis.ticks.x=element_line(),
        legend.title = element_blank(),
        legend.position = "none",
        legend.text = element_text(size=20),
        axis.text = element_text(size=14),
        axis.title = element_text(size=10,face="bold"),
        axis.line = element_line(colour = "black"),
        panel.background = element_blank(),
        panel.border = element_rect(colour = "black", fill=NA)) +
  ylab("study")+ 
  xlab("Carbon footprint (kgCO2e per capita per year)") 
ggsave(path = "figures", "boxplot_OMN_pf.png", device = "png",  width = 40, height = 20, units = "cm")

#Combine plots
plot_grid(boxplot_VN, boxplot_VT, boxplot_OMN,
          rel_heights = c(5,7,13),
          label_x = 0.2,
          nrow = 3,
          axis="r")


ggsave(path = "figures", "lit_compare.png", device = "png",  
       width = 30, height = 50, units = "cm")

#### End of R-Script####