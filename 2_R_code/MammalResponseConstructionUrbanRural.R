# FM1847 Urban-Rural gradient pre-construction data manuscript

# Objectives for the study ####
## 1. Compare the mammal community composition at sites along an urban-rural gradient
## 2. Assess how road construction changes the mammal community

# Analyses for the study ####
## PERMANOVA (in PRIMER v7)
## SIMPER (in PRIMER v7)
## Correspondence analysis (for visuals)
## Diel activity (for those species with large enough sample sizes)


######################################################################################################################

# Required packages ####
## The following packages are used and are called with ::
  #if(!require(openxlsx)){install.packages("openxlsx"); require(openxlsx)}
  #if(!require(lubridate)){install.packages("lubridate"); require(lubridate)}
  #if(!require(dplyr)){install.packages("dplyr"); require(dplyr)}
  #if(!require(tidyr)){install.packages("tidyr"); require(tidyr)}
  #if(!require(vegan)){install.packages("vegan"); require(vegan)}

## The following packages are used and need to be loaded
require(cameraTrapping)

######################################################################################################################
############################################## Part 1: Loading the Data ##############################################
######################################################################################################################

# Load the raw data ####
## Camera data
AP1_precon <- read.table("20210702_AllPictures_PreCon_w_ghosts.txt")
AP1_earlycon <- read.table("20210702_AllPictures_ConEarly_w_ghosts.txt")

## Some labelling data
species_categories <- openxlsx::read.xlsx("Species_labels.xlsx")

## Site specific data
EnvData <- openxlsx::read.xlsx("EnvData_Camera.xlsx", sheet = 1)
EnvData$Urbanization <- factor(EnvData$Urbanization, levels = c("Urban", "Periurban", "Rural"))

## CT data
CTtable_raw <- openxlsx::read.xlsx("CTtable_FM1847.xlsx", sheet = 3, detectDates = TRUE)
CTtable_WCS <- subset(CTtable_raw, Type == "WCS")


# Prepping and organizing the data ####
## CT Table
CTtable <- cameraTrapping::ctdates_fun(CTtable_WCS)

## Camera data
### Converting raw data table to something useful
AP2_precon <- cameraTrapping::APFun_env(AP1_precon, EnvData, start_date = "2019-07-01", interval = 1801)
AP2_earlycon <- cameraTrapping::APFun_env(AP1_earlycon, EnvData, start_date = "2019-07-01", interval = 1801)

### Combining data tables
AP2_precon$timeperiod <- 1
AP2_earlycon$timeperiod <- 2
AP2_30min <- rbind(AP2_precon, AP2_earlycon)
AP2_30min_WCS <- subset(AP2_30min, Type=="WCS")
rm(AP2_precon, AP2_earlycon)

### Extracting only mammals
AP3_30min <- merge.data.frame(AP2_30min_WCS, species_categories, by.x = "Species", by.y = "Label")
AP3_30min_mammals <- subset(AP3_30min, Class=="Mammalia")
AP3_30min_mammals$yearmonth <- with(AP3_30min_mammals, paste(lubridate::year(Date), formatC(lubridate::month(Date), width = 2, flag = "0"), sep = ""))


# Calculating number of events ####
## Summarizing the data by site and time period 
eventsall <- dplyr::summarise(dplyr::group_by(AP3_30min_mammals, Urbanization, Station, timeperiod, Class, Species_Short), Individuals = dplyr::n())
eventsallspecies <- tidyr::pivot_wider(eventsall, names_from = Species_Short, values_from = Individuals, values_fill = 0)

## Summarizing the data by site, time period, and month 
eventsmonth <- dplyr::summarise(dplyr::group_by(AP3_30min_mammals, Urbanization, Station, timeperiod, yearmonth, Species_Short), Individuals = dplyr::n())
eventsmonthspecies <- tidyr::pivot_wider(eventsmonth, names_from = Species_Short, values_from = Individuals, values_fill = 0)

# Prepping PERMANOVAs ####
#openxlsx::write.xlsx(list(Data = eventsmonthspecies[,5:21], Factors = eventsmonthspecies[,1:4]), file = paste("FM1847_PERMANOVA_", format(Sys.Date(), "%Y%m%d"), ".xlsx", sep = ""))


# Table of number of detections by species and site and construction period ####
eventsspecies1 <- dplyr::summarise(dplyr::group_by(AP3_30min, Urbanization, timeperiod, Class, Species.y), events = dplyr::n())
eventsspecies1$group <- with(eventsspecies1, paste(Urbanization, timeperiod, sep = "_"))
eventsspecies2 <- tidyr::pivot_wider(eventsspecies1[,3:6], names_from = group, values_from = events, values_fill = 0)
eventsspecies3 <- eventsspecies2[order(paste(eventsspecies2$Class, eventsspecies2$Species.y, sep = "_")),]
eventsspecies3

openxlsx::write.xlsx(eventsspecies3, file = paste("Detections_AllSpecies_", format(Sys.Date(), "%Y%m%d"), ".xlsx", sep = ""))


######################################################################################################################
#################################### Part 2: Assessment of Camera Trap Efficiency ####################################
######################################################################################################################

# Trapping Effort ####
trapeffort <- cameraTrapping::trapEffort(CTtable, "Station", sessions = T, sessions = T, sessioncol = "timeperiod")
trapeffort
trapeffort_summary <- dplyr::summarise(dplyr::group_by(trapeffort, timeperiod), a_mean = mean(activenights), a_min = min(activenights), a_max = max(activenights), t_mean = mean(totalnights), t_min = min(totalnights), t_max = max(totalnights))


# Image effort ####
imageeffort <- cameraTrapping::imageEffort(timelapse = list(PreCon = AP1_precon, EarlyCon = AP1_earlycon), type = "dataorganize")
imageeffort


# Effort Combined ####
effort <- data.frame("TotalNights" = c(apply(trapeffort_summary, 1, function(x){paste(x[5], " (", x[6], ", ", x[7], ")", sep = "")}), with(trapeffort, paste(mean(totalnights), " (", min(totalnights), ", ", max(totalnights), ")", sep = ""))), 
           "ActiveNights" = c(apply(trapeffort_summary, 1, function(x){paste(x[2], " (", x[3], ", ", x[4], ")", sep = "")}), with(trapeffort, paste(mean(activenights), " (", min(activenights), ", ", max(activenights), ")", sep = ""))),
           imageeffort[,c(1,2,5)])
colnames(effort) <- c("TotalNights", "ActiveNights", "TotalPics", "AnimalPics", "SuccessRate")
effort
rm(trapeffort_summary)


# Detection rates of individual species ####
## Detections/100 trap nights = detections * activenights/100
det1 <- data.frame(sort = paste(eventsallspecies$Station, eventsallspecies$timeperiod, sep = "_"), eventsallspecies)
trapeffort$sort <- paste(trapeffort$Station, trapeffort$timeperiod, sep = "_")

det2 <- merge.data.frame(det1, trapeffort[3:5], by = "sort", all.x = T)[,c(1:5,23:24,6:22)]
det3 <- data.frame(det2[,1:7], det2[,8:24]*det2[,6]/100)

## Preparing data for plotting
det4 <- aggregate(det3[,6:24], by = list(Urbanization = det3$Urbanization, timeperiod = det3$timeperiod), mean)
det4
det_plot1 <- tidyr::pivot_longer(det4[,-c(17)], cols = c(Armadillo:Rodent,Hog:Skunk), names_to = "Species", values_to = "Detections")
det_plot1$Urbanization <- factor(det_plot1$Urbanization, levels = c("Urban", "Periurban", "Rural"), labels = c("Urban", "Peri-urban", "Rural"))
det_plot1$timeperiod <- factor(det_plot1$timeperiod)
det_plot2 <- merge.data.frame(det_plot1, species_categories, by.x = "Species", by.y = "Species_Short", all.x = T)[,c(2:5,8,6)]
colnames(det_plot2) <- colnames(det_plot1)
det_plot2$Species <- factor(det_plot2$Species, 
                            levels = c("Bobcat", "Coyote", "Domestic cat", "Domestic dog", "Long-tailed weasel", "Northern raccoon", "Striped skunk", "Nine-banded armadillo", "Feral hog", "Javelina", "Nilgai", "White-tailed deer", "Virginia opossum", "Eastern cottontail", "Mexican ground squirrel", "Rodent"), 
                            labels = c("Bobcat", "Coyote", "Domestic cat", "Domestic dog", "Weasel", "Raccoon", "Striped skunk", "Armadillo", "Feral hog", "Javelina", "Nilgai", "Deer", "Opossum", "Cottontail", "Ground squirrel", "Rodent"))
levels(det_plot2$Species)

detPlot.fun <- function(detections, size.text, size.lab){
  #detections <- det_plot2
  #size.text <- 3
  #size.lab <- 20
  
  require(ggplot2)
  
  theme.plot <- theme(text = element_text(family = "serif")) + 
    theme(plot.margin = unit(c(.1,.1,.1,.1), "inch"), 
          #plot.background = element_blank(), 
          plot.title = element_text(hjust = 0.5, size = size.lab*1.25, margin = margin(b = 0.5, unit = "inch"))) +
    theme(axis.ticks = element_line(color = "black", size = 1, linetype = "solid"), 
          axis.line = element_line(color = NA, size = .1, linetype = "solid"), 
          axis.title=element_text(size=size.lab, margin = margin(t = 0.25, unit="inch")),  
          axis.title.x = element_text(vjust = 0), 
          axis.title.y = element_text(angle = 90, vjust = 1.5), 
          axis.text = element_text(size = size.lab*0.75, color = "black"), 
          axis.text.x = element_text(angle = 0, hjust = 0.5), 
          axis.text.y = element_text(angle = 0, hjust = 0)) + 
    theme(panel.border = element_rect(fill = NA, color = "black"), 
          panel.background = element_rect(fill = NA, color = NA), 
          panel.grid.major = element_line(color = "gray80")) + 
    theme(legend.margin=margin(c(0.15,0.15,0.15,0.15), unit = "inch"), 
          legend.background = element_rect(fill = NA, color = NA), 
          legend.text=element_text(size = size.lab*0.75), 
          legend.title=element_text(size=size.lab*0.75), 
          #legend.position = "top", 
          #legend.key = element_rect(color = "black", fill = NA), 
          legend.key.height = unit(0.25,"inch"), 
          legend.key.width = unit(0.25, "inch")) + 
    theme(strip.background = element_rect(fill = "gray85", color = "black"), 
          strip.text = element_text(size = size.lab*0.75), 
          strip.text.x = element_text(margin = margin(t = 0.05, r = 0.05, b = 0.05, l = 0.05, unit = "inch")), 
          strip.text.y = element_text(angle = -90, margin = margin(t = 0.05, r = 0.05, b = 0.05, l = 0.05, unit = "inch")))
  
  plot.out <- ggplot(det_plot2, aes(x = Urbanization, fill = timeperiod)) + 
    geom_bar(mapping = aes(y = Detections), stat = "identity", position = position_dodge2()) + 
    scale_x_discrete("Urbanization Level", labels = c("U", "P", "R")) + 
    scale_y_continuous("Average Detections/100 Trap Nights") + 
    scale_fill_manual("Construction Period", values = c("#9EBCDA", "#8856A7"), labels = c("Pre-con", "Con")) + 
    facet_wrap(vars(Species), nrow = 4, ncol = 4, scales = "free_y") + 
    theme.plot
  
  return(plot.out)
  rm(theme.plot)
  #rm(detections, size.text, size.lab)
}

plot.dets <- detPlot.fun(det_plot2, 3, 12)
plot.dets

ggsave(filename = paste("Detections_", format(Sys.Date(), "%Y%m%d"), ".tif", sep = ""), plot = plot.dets, device = "tiff", width = 6.5, height = 4.0, dpi = 600, compression = "lzw")



######################################################################################################################
########################################## Part 3: Correspondence Analysis ###########################################
######################################################################################################################

# Running the correspondence analysis ####
ca <- vegan::decorana(eventsmonthspecies[,5:21], ira = 1)
ca_plot <- list(sitescores = data.frame(eventsmonthspecies[,1:4],ca$rproj), speciesscores = data.frame(ca$cproj))
ca_plot[[1]]$timeperiod <- as.factor(ca_plot[[1]]$timeperiod)
ca_plot[[1]]$Urbanization <- factor(ca_plot[[1]]$Urbanization, levels = c("Urban", "Peri-urban", "Rural"))
ca_plot[[1]]$Station <- factor(ca_plot[[1]]$Station)

# Creating the plotting function ####
caplot.fun <- function(data, x1, x2, fill, xcoord=c(-2.5,3.5), ycoord=c(-2.5,4.5), size.text,size.lab){
  #data = ca_plot
  #x1 = "RA1"
  #x2 = "RA2"
  #fill = "Station"
  #xcoord=c(-2.5,3.5)
  #ycoord=c(-2.5,4.5)
  #size.text = 9
  #size.lab = 45
  
  require(ggplot2)
  
  theme.plot <- theme(text = element_text(family = "serif")) + 
    theme(plot.margin = unit(c(.1,.1,.1,.1), "inch"), 
          #plot.background = element_blank(), 
          plot.title = element_text(hjust = 0.5, size = size.lab*1.25, margin = margin(b = 0.5, unit = "inch"))) +
    theme(axis.ticks = element_line(color = "black", size = 1, linetype = "solid"), 
          axis.line = element_line(color = NA, size = .1, linetype = "solid"), 
          axis.title=element_text(size=size.lab, margin = margin(t = 0.25, unit="inch")),  
          axis.title.x = element_text(vjust = 0), 
          axis.title.y = element_text(angle = 90, vjust = 1.5), 
          axis.text = element_text(size = size.lab*0.75, color = "black"), 
          axis.text.x = element_text(angle = 0, hjust = 0.5), 
          axis.text.y = element_text(angle = 0, hjust = 0)) + 
    theme(panel.border = element_rect(fill = NA, color = "black"), 
          panel.background = element_rect(fill = NA, color = NA), 
          panel.grid.major = element_line(color = "gray80")) + 
    theme(legend.margin=margin(c(0.15,0.15,0.15,0.15), unit = "inch"), 
          legend.background = element_rect(fill = NA, color = NA), 
          legend.text=element_text(size = size.lab*0.75), 
          legend.title=element_text(size=size.lab*0.75), 
          #legend.position = "top", 
          #legend.key = element_rect(color = "black", fill = NA), 
          legend.key.height = unit(0.25,"inch"), 
          legend.key.width = unit(0.25, "inch")) + 
    theme(strip.background = element_rect(fill = "gray85", color = "black"), 
          strip.text = element_text(size = size.lab*0.75), 
          strip.text.x = element_text(margin = margin(t = 0.05, r = 0.05, b = 0.05, l = 0.05, unit = "inch")), 
          strip.text.y = element_text(angle = -90, margin = margin(t = 0.05, r = 0.05, b = 0.05, l = 0.05, unit = "inch")))
  
  if(length(levels(data[[1]][[fill]]))==3){
    scale <- scale_color_manual("Urbanization", values = c("#F03B20", "#FEB24C", "#FFEDA0"), labels = c("Urban", "Peri-urban", "Rural ")) 
  }else if(length(levels(data[[1]][[fill]]))==5){
    scale <- scale_color_manual("Site", values = c("red", "blue", "purple", "orange", "black"))
  }

  plot.out <- ggplot(mapping = aes_string(x = x1, y = x2)) + 
    geom_blank(data = data[[1]]) + 
    geom_point(data = data[[1]], aes_string(color = fill, shape = "timeperiod"), size = 2.5) + 
    geom_text(data = data[[2]], aes(label = rownames(data[[2]]), family = "serif"), size = size.text) + 
    scale +
    scale_shape_manual("Construction Period", values = c(15,17), labels = c("Pre-con", "Con")) + 
    coord_cartesian(xcoord,ycoord) + 
    theme.plot
  
  return(plot.out)
  rm(theme.plot,plot.out)
  #rm(data,x1,x2,fill,xcoord,ycoord,size.lab,size.text)
}


# Creating the Correspondence Diagram ####
## Creating sites by urbanization level
size.text.ca <- 3
size.lab.ca <- 12

plot.ca_12 <- caplot.fun(ca_plot, "RA1", "RA2", fill = "Urbanization", size.text = size.text.ca, size.lab = size.lab.ca)
plot.ca_13 <- caplot.fun(ca_plot, "RA1", "RA3", fill = "Urbanization", xcoord = c(-3.5,3.5), ycoord = c(-3.5,4.5), size.text = size.text.ca, size.lab = size.lab.ca)
plot.ca_23 <- caplot.fun(ca_plot, "RA2", "RA3", fill = "Urbanization", xcoord = c(-3.5,3.5), ycoord = c(-3.5,3.5), size.text = size.text.ca, size.lab = size.lab.ca)

plot.ca_12
plot.ca_13
plot.ca_23

## Creating plots by site instead of urbanization level
plot.ca.site_12 <- caplot.fun(ca_plot, "RA1", "RA2", fill = "Station", size.text = size.text.ca, size.lab = size.lab.ca)
plot.ca.site_13 <- caplot.fun(ca_plot, "RA1", "RA3", fill = "Station", xcoord = c(-3.5,3.5), ycoord = c(-3.5,4.5), size.text = size.text.ca, size.lab = size.lab.ca)
plot.ca.site_23 <- caplot.fun(ca_plot, "RA2", "RA3", fill = "Station", xcoord = c(-3.5,3.5), ycoord = c(-3.5,3.5), size.text = size.text.ca, size.lab = size.lab.ca)

## Combining plots into a single plot
plot.ca <- ggpubr::ggarrange(plotlist = list(plot.ca_12, plot.ca_13), ncol = 2, legend = "top", common.legend = T)
plot.ca
ggsave(filename = paste("CA_combined_", format(Sys.Date(), "%Y%m%d"), ".tif", sep = ""), plot = plot.ca, device = "tiff", width = 6.5, height = 4, dpi = 600, compression = "lzw")

plot.ca.site <- ggpubr::ggarrange(plotlist = list(plot.ca.site_12, plot.ca.site_13), ncol = 2, legend = "top", common.legend = T)
plot.ca.site
ggsave(filename = paste("CA_combined_site_", format(Sys.Date(), "%Y%m%d"), ".tif", sep = ""), plot = plot.ca.site, device = "tiff", width = 6.5, height = 4, dpi = 600, compression = "lzw")


######################################################################################################################
############################################### Part 4: Diel Activity ################################################
######################################################################################################################

# Prepping the data ####
AP2_precon <- cameraTrapping::APFun_env(AP1_precon, EnvData, start_date = "2019-07-01", interval = 61)
AP2_earlycon <- cameraTrapping::APFun_env(AP1_earlycon, EnvData, start_date = "2019-07-01", interval = 61)
AP2_precon$timeperiod <- 1
AP2_earlycon$timeperiod <- 2
AP2_1min <- rbind(AP2_precon, AP2_earlycon)
AP2_1min_WCS <- subset(AP2_1min, Type=="WCS")
rm(AP2_precon, AP2_earlycon)


# Checking number of events ####
AP2_1min_WCS$urbantime <- with(AP2_1min_WCS, paste(Urbanization, timeperiod, sep = "_"))

## Urbanization alone
with(AP2_1min_WCS, table(Species, Urbanization))
#Species                    Periurban Rural Urban
## Mammals
  #bobcat                        530   135     0   (Maybe)
  #coyote                         89    43     0   (NO)
  #domestic_cat                   94    38   940   (Maybe)
  #domestic_dog                   75    10    16   (NO)
  #eastern_cottontail            510  3839     2   (Maybe)
  #feral_hog                       6     0     0   (NO)
  #javelina                        2     3     0   (NO)
  #long-tailed_weasel              1     2     0   (NO)
  #mexican_ground_squirrel         3     8     0   (NO)
  #nilgai                         42     2     0   (NO)
  #nine-banded_armadillo         263   268   283   (YES)
  #northern_raccoon             1378   104   260   (YES)
  #rodent                       1181  4264   283   (Not a single species)
  #striped_skunk                   0     3     0   (NO)
  #unk_mammal                    108   101    47   (Not a single species)
  #virginia_opossum             3100  1209  2483   (YES)
  #white-tailed_deer             495   145     0   (Maybe)
## Non-mammals
  #american_alligator              1     0     0
  #bird                          399   541  2908
  #coral_snake                     0     2     0
  #human                         397   536   172
  #rio_grande_leopard_frog         3     0     0
  #texas_indigo_snake              8     1     0
  #texas_spiny_lizard              2    68    12
  #texas_tortoise                  0     1     0
  #unk_amphibian                   7     0     0
  #unk_lizard                      1     1     0
  #unk_snake                       3     0     0
  #unk_turtle                      0     1     0
  #unknown                        61    55    30
  #whiptail_lizard                33     8     0

## timeperiod alone
with(AP2_1min_WCS, table(Species, timeperiod))
#Species                     1    2
## Mammals
  #bobcat                   524  141   (YES)
  #coyote                    87   45   (NO)
  #domestic_cat             671  401   (YES)
  #domestic_dog              93    8   (NO)
  #eastern_cottontail      3254 1097   (YES)
  #feral_hog                  6    0   (NO)
  #javelina                   2    3   (NO)
  #long-tailed_weasel         1    2   (NO)
  #mexican_ground_squirrel   10    1   (NO)
  #nilgai                    32   12   (NO)
  #nine-banded_armadillo    449  365   (YES)
  #northern_raccoon        1122  620   (YES)
  #rodent                  4417 1311   (Not a single species)
  #striped_skunk              0    3   (NO)
  #unk_mammal               200   56   (Not a single species)
  #virginia_opossum        4008 2784   (YES)
  #white-tailed_deer        622   18   (Maybe)
## Non-mammals
  #american_alligator         1    0
  #bird                    1702 2146
  #coral_snake                1    1
  #human                    643  462
  #rio_grande_leopard_frog    3    0
  #texas_indigo_snake         8    1
  #texas_spiny_lizard         1   81
  #texas_tortoise             1    0
  #unk_amphibian              5    2
  #unk_lizard                 2    0
  #unk_snake                  3    0
  #unk_turtle                 0    1
  #unknown                  138    8
  #whiptail_lizard            4   37

## Urbanization + timeperiod
with(AP2_1min_WCS, table(Species, urbantime))
#Species                   Periurban_1 Periurban_2 Rural_1 Rural_2 Urban_1 Urban_2
## Mammals
  #bobcat                          417         113     107      28       0       0   (Maybe)
  #coyote                           70          19      17      26       0       0   (NO)
  #domestic_cat                     27          67      26      12     618     322   (Maybe)
  #domestic_dog                     71           4      10       0      12       4   (NO)
  #eastern_cottontail              447          63    2806    1033       1       1   (Maybe)
  #feral_hog                         6           0       0       0       0       0   (NO)
  #javelina                          2           0       0       3       0       0   (NO)
  #long-tailed_weasel                0           1       1       1       0       0   (NO)
  #mexican_ground_squirrel           3           0       7       1       0       0   (NO)
  #nilgai                           30          12       2       0       0       0   (NO)
  #nine-banded_armadillo           164          99     194      74      91     192   (YES)
  #northern_raccoon                976         402      61      43      85     175   (Maybe)
  #rodent                         1105          76    3198    1066     114     169   (Not a single species)
  #striped_skunk                     0           0       0       3       0       0   (NO)
  #unk_mammal                      105           3      50      51      45       2   (Not a single species)
  #virginia_opossum               2143         957     784     425    1081    1402   (YES)
  #white-tailed_deer               481          14     141       4       0       0   (Maybe)
## Non-mammals
  #american_alligator                1           0       0       0       0       0
  #bird                            305          94     319     222    1078    1830
  #coral_snake                       0           0       1       1       0       0
  #human                           294         103     256     280      93      79
  #rio_grande_leopard_frog           3           0       0       0       0       0
  #texas_indigo_snake                8           0       0       1       0       0
  #texas_spiny_lizard                0           2       0      68       1      11
  #texas_tortoise                    0           0       1       0       0       0
  #unk_amphibian                     5           2       0       0       0       0
  #unk_lizard                        1           0       1       0       0       0
  #unk_snake                         3           0       0       0       0       0
  #unk_turtle                        0           0       0       1       0       0
  #unknown                          60           1      49       6      29       1
  #whiptail_lizard                   3          30       1       7       0       0


# Selecting Species ####
## Urbanization only
specs_urban_all <- subset(species_categories, species_categories$Label %in% c("nine-banded_armadillo", "northern_raccoon", "virginia_opossum"))
specs_urban_PR <- subset(species_categories, species_categories$Label %in% c("bobcat", "eastern_cottontail", "white-tailed_deer"))
specs_urban_UP <- subset(species_categories, species_categories$Label %in% c("domestic_cat"))

## Time Period only
specs_time <- subset(species_categories, species_categories$Label %in% c("bobcat", "domestic_cat", "eastern_cottontail", "nine-banded_armadillo", "northern_raccoon", "virginia_op"))

## Urbanization and Time Period
specs_ut_all <- subset(species_categories, species_categories$Label %in% c("nine-banded_armadillo", "northern_raccoon", "virginia_opossum"))
specs_ut_PR <- subset(species_categories, species_categories$Label %in% c("bobcat", "eastern_cottontail"))
specs_ut_UP <- subset(species_categories, species_categories$Label %in% c("domestic_cat"))
specs_ut_P1R1 <- subset(species_categories, species_categories$Label %in% c("white-tailed_deer"))

# Running activity analysis ####
## A function for removing bad activity data
actCheckFun <- function(act, threshold = 70){
  #act <- act_ut_all
  #threshold <- 70
  
  if(length(act) != 2){
    stop("This function depends on an activity dataset created by the actFun function in the cameraTrapping package.")
  }
  if(names(act)[1] != "data"){
    stop("This function depends on an activity dataset created by the actFun function in the cameraTrapping package.")
  }
  if(names(act)[2] != "activity"){
    stop("This function depends on an activity dataset created by the actFun function in the cameraTrapping package.")
  }
  
  d <- act[[1]]
  a <- act[[2]]
  
  test <- lapply(d, function(y){
    which(lapply(y, function(z){nrow(z)}) > threshold)
  })
  
  d1 <- lapply(1:length(d), function(i){d[[i]][test[[i]]]})
  names(d1) <- names(d)
  a1 <- lapply(1:length(a), function(i){a[[i]][test[[i]]]})
  names(a1) <- names(a)
  act2 <- list(data = d1, activity = a1)
  return(act2)
  rm(d, a, test, d1, a1, act2)
  #rm(act, threshold)
}

## Urbanization
### For those species with data in all locations
act_urban_all <- cameraTrapping::actFun(AP2_1min_WCS, split = T, splitcol = "Urbanization", species = specs_urban_all[,1], bw = NULL, rep = 9999)
### For those species with only peri-urban and rural data
AP2_1min_WCS_PR <- subset(AP2_1min_WCS, AP2_1min_WCS$Urbanization != "Urban")
act_urban_PR <- cameraTrapping::actFun(AP2_1min_WCS_PR, split = T, splitcol = "Urbanization", species = specs_urban_PR[,1], bw = NULL, rep = 9999)
### For those species with only peri-urban and urban data
AP2_1min_WCS_UP <- subset(AP2_1min_WCS, AP2_1min_WCS$Urbanization != "Rural")
act_urban_UP <- cameraTrapping::actFun(AP2_1min_WCS_UP, split = T, splitcol = "Urbanization", species = specs_urban_UP[,1], bw = NULL, rep = 9999)

## Time period
act_time <- cameraTrapping::actFun(AP2_1min_WCS, split = T, splitcol = "timeperiod", species = specs_time[,1], bw = NULL, rep = 9999)

## Urbanization and Time Period
### For species with data in all factors
act_ut_all <- cameraTrapping::actFun(AP2_1min_WCS, split = T, splitcol = "urbantime", species = specs_ut_all[,1], bw = NULL, rep = 9999)
act_ut_all <- actCheckFun(act_ut_all)
### For species in peri-urban and rural but all time periods
act_ut_PR <- cameraTrapping::actFun(AP2_1min_WCS_PR, split = T, splitcol = "urbantime", species = specs_ut_PR[,1], bw = NULL, rep = 9999)
act_ut_PR <- actCheckFun(act_ut_PR)
### For species in peri-urban and urban but all time periods
act_ut_UP <- cameraTrapping::actFun(AP2_1min_WCS_UP, split = T, splitcol = "urbantime", species = specs_ut_UP[,1], bw = NULL, rep = 9999)
act_ut_UP <- actCheckFun(act_ut_UP)
### For species in peri-urban and rural but only pre-construction
AP2_1min_WCS_P1R1 <- subset(AP2_1min_WCS, AP2_1min_WCS$urbantime %in% c("Periurban_1", "Rural_1"))
act_ut_P1R1 <- cameraTrapping::actFun(AP2_1min_WCS_P1R1, split = T, splitcol = "urbantime", species = specs_ut_P1R1[,1], bw = NULL, rep = 9999)


# Comparing diel activity with the Wald test ####
## A function for doing this
compAct <- function(act, specs){
  #act <- act_urban_PR
  #specs <- specs_urban_PR
  
  full <- unlist(act$activity)
  x1 <- lapply(1:nrow(specs), function(i){
    x <- full[grep(specs[i,1], names(full))]
    names(x) <- do.call(rbind, strsplit(names(x), "[.]"))[,1]
    return(x)
  })
  names(x1) <- specs[,1]
  
  x2 <- do.call(rbind, lapply(1:length(x1), function(a){
    l <- sort(names(x1[[a]]))
    i <- rep(1:(length(l) - 1), (length(l) - 1):1)
    j <- unlist(sapply(2:length(l), function(i) i:length(l)))
    
    data.frame(species = specs[a,2], comparison = paste(l[i], l[j], sep = "-"), activity::compareAct(x1[[a]]))
  }))
  
  return(x2)
  rm(full, x1, x2)
  #rm(act, specs)
}

## Urbanization
### For those species with data in all locations
act_comp_urban_all <-  compAct(act = act_urban_all, specs = specs_urban_all)
### For those species with only peri-urban and rural data
act_comp_urban_PR <-  compAct(act = act_urban_PR, specs = specs_urban_PR)
### For those species with only peri-urban and urban data
act_comp_urban_UP <-  compAct(act = act_urban_UP, specs = specs_urban_UP)
### Combining the analyses together
act_comp_urban <- rbind(act_comp_urban_all, act_comp_urban_PR, act_comp_urban_UP)
rownames(act_comp_urban) <- NULL
act_comp_urban
rm(act_comp_urban_all, act_comp_urban_PR, act_comp_urban_UP)

## Time Period
act_comp_time <- compAct(act = act_time, specs = specs_time)
rownames(act_comp_time) <- NULL
act_comp_time

## Urbanization and Time Period
### For those species with data in all (or almost all) periods
act_comp_ut_all <- compAct(act = act_ut_all, specs = specs_ut_all)
### For those species with only peri-urban and rural data
act_comp_ut_PR <- compAct(act = act_ut_PR, specs = specs_ut_PR)
### For those species with only peri-urban and urban data
act_comp_ut_UP <- compAct(act = act_ut_UP, specs = specs_ut_UP)
### For those species with only data in the pre-construction period for peri-urban and urban
act_comp_ut_P1R1 <- compAct(act = act_ut_P1R1, specs = specs_ut_P1R1)
### Combining the analyses together
act_comp_ut <- rbind(act_comp_ut_all, act_comp_ut_PR, act_comp_ut_UP, act_comp_ut_P1R1)
rownames(act_comp_ut) <- NULL
act_comp_ut
rm(act_comp_ut_all, act_comp_ut_PR, act_comp_ut_UP, act_comp_ut_P1R1)

# Creating plots of diel activity ####
## A function for plotting activity
actPlot.fun <- function(act, specs, colscheme, prefix, limits = c(0, 0.225), cexnum = 1){
  #act <- act_ut_all
  #specs <- specs_ut_all
  #colscheme <- act_plot_col
  #prefix <- "ut"
  #limits <- c(0, 0.225)
  #cexnum <- 1
  
  require(activity)
  message(paste("Graphs are being created. They will be exported to: \n", getwd(), sep = ""))
  
  full <- unlist(act$activity)
  x1 <- lapply(1:nrow(specs), function(i){
    x <- full[grep(specs[i,1], names(full))]
    names(x) <- do.call(rbind, strsplit(names(x), "[.]"))[,1]
    return(x)
  })
  names(x1) <- specs[,1]
  
  # Graph Creation
  pbapply::pblapply(1:nrow(specs), function(s){
    tiff(filename = paste("Activity_", specs[s,1], "_", prefix, "_", format(Sys.Date(), "%Y%m%d"), ".tiff", sep = ""), units = "in", width = 7, height = 5, res = 600, compression = "lzw")
    par(mfrow = c(1,1), mar = c(4.250,4.250,1.75,0.150), cex.axis = cexnum*1.25, cex.lab = cexnum*1.5, cex.main = cexnum*2, family = "serif")
    sites <- names(x1[[s]])
    colors <- colscheme[colscheme[[1]] %in% sites,]
    rows <- nrow(colors)
    
    lapply(1:rows, function(i){
      if(i==1){
        plot(x1[[s]][[i]], main = specs[s,2], yunit = "density", data = "none", centre = "night", cline = list(col = NA), tline = list(col = colors[i,3], lty = colors[i,4], lwd = 1.5), ylim = limits)
      }else{
        plot(x1[[s]][[i]], yunit = "density", data = "none", centre = "night", cline = list(col = NA), tline = list(col = colors[i,3], lty = colors[i,4], lwd = 1.5), ylim = limits, add = T)
      }
    })
    legend("topleft", colors[1:rows,2], col = colors[1:rows,3], lty = colors[1:rows,4], lwd = 1.5)
    dev.off()
    #rm(sites, colors, rows, s, i)
  })
  
  rm(full, x1)
  #rm(act, specs, colscheme, prefix, limits, cexnum)
}

## Defining color schemes
act_plot_col <- data.frame(Site = c("Urban", "Periurban", "Rural", "1", "2", "Periurban_1", "Periurban_2", "Rural_1", "Rural_2", "Urban_1", "Urban_2"), 
                           Name = c("Urban", "Peri-urban", "Rural", "Pre-con", "Con", "Peri-urban Pre-con", "Peri-urban Con", "Rural Pre-con", "Rural Con", "Urban Pre-con", "Urban Con"), 
                           Color = c("#F03B20", "#FEB24C", "#FFEDA0", "#9EBCDA", "#8856A7", "#FEB24C", "#FEB24C", "#FFEDA0", "#FFEDA0", "#F03B20", "#F03B20"), 
                           linetype = c(1, 1, 1, 1, 1, 1, 2, 1, 2, 1, 2))

## Urbanization
actPlot.fun(act = act_urban_all, specs = specs_urban_all, colscheme = act_plot_col, prefix = "urban", cexnum = 1)
actPlot.fun(act = act_urban_PR, specs = specs_urban_PR, colscheme = act_plot_col, prefix = "urban", cexnum = 1)
actPlot.fun(act = act_urban_UP, specs = specs_urban_UP, colscheme = act_plot_col, prefix = "urban", cexnum = 1)

## Time Period
actPlot.fun(act = act_time, specs = specs_time, colscheme = act_plot_col, prefix = "con", cexnum = 1)

## Urbanization within time periods
actPlot.fun(act = act_ut_all, specs = specs_ut_all, colscheme = act_plot_col, prefix = "ut", limits = c(0, 0.275), cexnum = 1)
actPlot.fun(act = act_ut_PR, specs = specs_ut_PR, colscheme = act_plot_col, prefix = "ut", limits = c(0, 0.275), cexnum = 1)
actPlot.fun(act = act_ut_UP, specs = specs_ut_UP, colscheme = act_plot_col, prefix = "ut", limits = c(0, 0.275), cexnum = 1)
actPlot.fun(act = act_ut_P1R1, specs = specs_ut_P1R1, colscheme = act_plot_col, prefix = "ut", limits = c(0, 0.275), cexnum = 1)


######################################################################################################################
############################################# Part 5: Species Diversity ##############################################
######################################################################################################################

# Calculation of Species Diversity ####
## Diversity function
diverseFun <- function(ds, cols, name){
  #ds <- eventsallspecies
  #cols <- c(5:11,13:21)
  #name <- c("Station", "timeperiod")
  
  diverse <- t(apply(ds[,cols], 1, FUN = function(x){c(S = sum(x != 0), N = sum(x), richness = (sum(x != 0) - 1)/log(sum(x)), evenness = vegan::diversity(x)/log(sum(x != 0)), shannon = vegan::diversity(x), simpson = vegan::diversity(x, index = "simpson"))}))
  if(length(name) == nrow(diverse)){
    rownames(diverse) <- name
  }else if(length(name)==1){
    rownames(diverse) <- ds[[name]]
  }else if(length(name)==2){
    rownames(diverse) <- paste(ds[[name[1]]], ds[[name[2]]], sep = "_")
  }else if(is.null(name)){
    rownames(diverse) <- NULL
  }else{
    warning("Please provide either a vector of names or up to 2 column names to create a name")
    rownames(diverse) <- NULL
  }
  return(diverse)
  rm(diverse)
  #rm(ds, name)
}

## For each site
diverseAll <- diverseFun(eventsallspecies, c(5:11,13:21), c("Station", "timeperiod"))
diverseAll

## By urbanization level
eventsurban <- dplyr::summarise(dplyr::group_by(AP3_30min_mammals, Urbanization, timeperiod, Class, Species_Short), Individuals = dplyr::n())
eventsurbanspecies <- tidyr::pivot_wider(eventsurban, names_from = Species_Short, values_from = Individuals, values_fill = 0)
eventsurbanspecies

diverseUrban <- diverseFun(eventsurbanspecies, c(4:10,12:20), c("Urbanization", "timeperiod"))
diverseUrban


######################################################################################################################
###################################################### Old Data ######################################################
######################################################################################################################

# Creating plots of diel activity ####
lapply(1:nrow(specs_urban_all), function(s){
  cexnum <- 1
  tiff(filename = paste("Activity_", specs_urban_all[s,1], "_", format(Sys.Date(), "%Y%m%d"), ".tiff", sep = ""), units = "in", width = 7, height = 5, res = 900, compression = "lzw")
  par(mfrow = c(1,1), mar = c(4.250,4.250,1.75,0.150), cex.axis = cexnum*1.25, cex.lab = cexnum*1.5, cex.main = cexnum*2, family = "serif")
  plot(act_urban_all$activity[[1]][[s]], main = specs_urban_all[s,2], yunit = "density", data = "none", centre = "night", cline = list(col = NA), tline = list(col = act_plot_col[1,2]), ylim = c(0,0.225))
  plot(act_urban_all$activity[[2]][[s]], yunit = "density", data = "none", centre = "night", cline = list(col = NA), tline = list(col = act_plot_col[2,2]), ylim = c(0,0.225), add = T)
  plot(act_urban_all$activity[[3]][[s]], yunit = "density", data = "none", centre = "night", cline = list(col = NA), tline = list(col = act_plot_col[3,2]), ylim = c(0,0.225), add = T)
  legend("topleft", act_plot_col[1:3,1], col = act_plot_col[1:3,2], lty = 1)
  dev.off()
})

lapply(1:nrow(specs_urban_PR), function(s){
  cexnum <- 1
  tiff(filename = paste("Activity_", specs_urban_PR[s,1], "_", format(Sys.Date(), "%Y%m%d"), ".tiff", sep = ""), units = "in", width = 7, height = 5, res = 900, compression = "lzw")
  par(mfrow = c(1,1), mar = c(4.250,4.250,1.75,0.150), cex.axis = cexnum*1.25, cex.lab = cexnum*1.5, cex.main = cexnum*2, family = "serif")
  plot(act_urban_PR$activity[[1]][[s]], main = specs_urban_PR[s,2], yunit = "density", data = "none", centre = "night", cline = list(col = NA), tline = list(col = act_plot_col[2,2]), ylim = c(0,0.225))
  plot(act_urban_PR$activity[[2]][[s]], yunit = "density", data = "none", centre = "night", cline = list(col = NA), tline = list(col = act_plot_col[3,2]), ylim = c(0,0.225), add = T)
  legend("topleft", act_plot_col[2:3,1], col = act_plot_col[2:3,2], lty = 1)
  dev.off()
})

lapply(1:nrow(specs_urban_UP), function(s){
  cexnum <- 1
  tiff(filename = paste("Activity_", specs_UP[s,1], "_", format(Sys.Date(), "%Y%m%d"), ".tiff", sep = ""), units = "in", width = 7, height = 5, res = 900, compression = "lzw")
  par(mfrow = c(1,1), mar = c(4.250,4.250,1.75,0.150), cex.axis = cexnum*1.25, cex.lab = cexnum*1.5, cex.main = cexnum*2, family = "serif")
  plot(act_urban_UP$activity[[1]][[s]], main = specs_urban_UP[s,2], yunit = "density", data = "none", centre = "night", cline = list(col = NA), tline = list(col = act_plot_col[1,2]), ylim = c(0,0.225))
  plot(act_urban_UP$activity[[2]][[s]], yunit = "density", data = "none", centre = "night", cline = list(col = NA), tline = list(col = act_plot_col[2,2]), ylim = c(0,0.225), add = T)
  legend("topleft", act_plot_col[1:2,1], col = act_plot_col[1:2,2], lty = 1)
  dev.off()
})

## Time period
lapply(1:nrow(specs_time), function(s){
  cexnum <- 1
  tiff(filename = paste("Activity_", specs_time[s,1], "_", format(Sys.Date(), "%Y%m%d"), ".tiff", sep = ""), units = "in", width = 7, height = 5, res = 900, compression = "lzw")
  par(mfrow = c(1,1), mar = c(4.250,4.250,1.75,0.150), cex.axis = cexnum*1.25, cex.lab = cexnum*1.5, cex.main = cexnum*2, family = "serif")
  plot(act_time$activity[[1]][[s]], main = specs_time[s,2], yunit = "density", data = "none", centre = "night", cline = list(col = NA), tline = list(col = act_plot_col[4,2]), ylim = c(0,0.225))
  plot(act_time$activity[[2]][[s]], yunit = "density", data = "none", centre = "night", cline = list(col = NA), tline = list(col = act_plot_col[5,2]), ylim = c(0,0.225), add = T)
  legend("topleft", act_plot_col[4:5,1], col = act_plot_col[4:5,2], lty = 1)
  dev.off()
})
