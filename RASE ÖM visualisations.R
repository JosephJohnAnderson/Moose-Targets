## Load packages
library(ggplot2)
library(ggvis)
library(tidyr)
library(car)
library(patchwork)
library(dplyr)

## Data summary function to find means and various SE data
summarySE <- function(data=NULL, measurevar, groupvars=NULL, na.rm=TRUE,
                      conf.interval=.95, .drop=TRUE) {
  library(plyr)
  
  length2 <- function (x, na.rm=TRUE) {
    if (na.rm) sum(!is.na(x))
    else       length(x)
  }
  
  datac <- ddply(data, groupvars, .drop=.drop,
                 .fun = function(xx, col) {
                   c(N    = length2(xx[[col]], na.rm=na.rm),
                     mean = mean   (xx[[col]], na.rm=na.rm),
                     sd   = sd     (xx[[col]], na.rm=na.rm)
                   )
                 },
                 measurevar
  )
  
  datac <- rename(datac, c("mean" = measurevar))
  
  datac$se <- datac$sd / sqrt(datac$N)
  
  ciMult <- qt(conf.interval/2 + .5, datac$N-1)
  datac$ci <- datac$se * ciMult
  
  return(datac)
}


## import data set
library(readxl)
OM_trees_21_25 <- read_excel("~/PhD/Projects/Öster Malma exclosures/Data/Master/TallestTrees_ÖM_R.xlsx")

## Select important variables
tree_data <- OM_trees_21_25 %>% select(Distrikt, Hagn_id, Behandling, Datum, Trädslag, Medelhöjd)

## Select only "Kontroll" and "Fast" for Hagn_id
tree_data <- tree_data[tree_data$Hagn_id %in% c("Fast", "Kontroll"), ]

# Select only Tall and Gran
Barrtrad <- tree_data %>%
  filter(Trädslag %in% c("Tall", "Gran")) %>%
  group_by(Distrikt, Hagn_id, Behandling, Datum) %>%
  slice_max(Medelhöjd, n = 1) %>%
  mutate(Trädslag = "Barrträd")  # Rename Trädslag to "Barrträd"

# Set colours
colors_all <- c("Barrträd" = "black", "Rönn" = "blue", "Asp" = "blue", "Salix" = "blue", "Ek" = "blue")

## create data subsets for RASE species
Rönn <- tree_data [tree_data$Trädslag == "Rönn", ]
Asp <- tree_data [tree_data$Trädslag == "Asp", ]
Salix <- tree_data [tree_data$Trädslag == "Salix", ]
Ek <- tree_data [tree_data$Trädslag == "Ek", ]

## Combine the two data frames
tree_2 <- rbind(Barrtrad, Rönn)
tree_2$Trädslag <- ifelse(tree_2$Trädslag %in% c("Rönn", "Asp", "Salix", "Ek"), "RASE", "Barrträd")
tree_2$Trädslag <- factor(tree_2$Trädslag, levels = c("Barrträd", "RASE"))
tree_h <- summarySE(tree_2, measurevar="Medelhöjd", groupvars = c("Datum", "Hagn_id", "Trädslag"))

## Set group colors
colors_all <- c("Barrträd" = "black", "RASE" = "blue")

## Plot
barr_rönn <- ggplot(tree_h, aes(x= Datum, y= Medelhöjd, color = Trädslag, linetype = Hagn_id)) +
  geom_point(size= 1.7, position = position_dodge(0.0))+
  geom_line(aes(linetype = Hagn_id), position = position_dodge(0.0))+
  scale_linetype_manual(values = c(1,2,3,4))+
  scale_shape_manual(values=c(19,21))+
  scale_color_manual(values = colors_all)+
  theme_classic()+
  theme(legend.position="bottom")+
  theme(plot.title = element_text(family = "", face = "bold", colour = "black", size = "", hjust = 0.5, vjust = 0))+
  geom_errorbar(aes(ymin= Medelhöjd-se, ymax= Medelhöjd+se, linetype = Hagn_id), width = 0, 
                position = position_dodge(0.0), linetype = 1)+
  #labs(title = "Rönn", colour = "Trädslag", linetype = "Behandling", x = "", y = "Medelhöjd (cm)")+
  labs(title = "Rönn", linetype = "Behandling", x = "", y = "Medelhöjd (cm)")+
  scale_x_continuous(breaks=c(2012,2013,2014,2015,2016,2017,2018,2019,2020,2021,2022,2023,2024,2025))+
  guides(linetype = guide_legend(order = 1),  # Behandling first
         colour = guide_legend(order = 2))  # Trädslag second
print(barr_rönn)

## Combine the two data frames
tree_2 <- rbind(Barrtrad, Asp)
tree_2$Trädslag <- ifelse(tree_2$Trädslag %in% c("Rönn", "Asp", "Salix", "Ek"), "RASE", "Barrträd")
tree_2$Trädslag <- factor(tree_2$Trädslag, levels = c("Barrträd", "RASE"))
tree_h <- summarySE(tree_2, measurevar="Medelhöjd", groupvars = c("Datum", "Hagn_id", "Trädslag"))

## Set group colors
colors_all <- c("Barrträd" = "black", "RASE" = "blue")

## Plot
barr_asp <- ggplot(tree_h, aes(x= Datum, y= Medelhöjd, color = Trädslag, linetype = Hagn_id)) +
  geom_point(size= 1.7, position = position_dodge(0.0))+
  geom_line(aes(linetype = Hagn_id), position = position_dodge(0.0))+
  scale_linetype_manual(values = c(1,2,3,4))+
  scale_shape_manual(values=c(19,21))+
  scale_color_manual(values = colors_all)+
  theme_classic()+
  theme(legend.position="bottom")+
  theme(plot.title = element_text(family = "", face = "bold", colour = "black", size = "", hjust = 0.5, vjust = 0))+
  geom_errorbar(aes(ymin= Medelhöjd-se, ymax= Medelhöjd+se, linetype = Hagn_id), width = 0, 
                position = position_dodge(0.0), linetype = 1)+
  # labs(title = "Asp", colour = "Trädslag", linetype = "Behandling", x = "", y = "Medelhöjd (cm)")+
  labs(title = "Asp", linetype = "Behandling", x = "", y = "Medelhöjd (cm)")+
  scale_x_continuous(breaks=c(2012,2013,2014,2015,2016,2017,2018,2019,2020,2021,2022,2023,2024,2025))+
  guides(linetype = guide_legend(order = 1),  # Behandling first
         colour = guide_legend(order = 2))  # Trädslag second
print(barr_asp)

## Combine the two data frames
tree_2 <- rbind(Barrtrad, Salix)
tree_2$Trädslag <- ifelse(tree_2$Trädslag %in% c("Rönn", "Asp", "Salix", "Ek"), "RASE", "Barrträd")
tree_2$Trädslag <- factor(tree_2$Trädslag, levels = c("Barrträd", "RASE"))
tree_h <- summarySE(tree_2, measurevar="Medelhöjd", groupvars = c("Datum", "Hagn_id", "Trädslag"))

## Set group colors
colors_all <- c("Barrträd" = "black", "RASE" = "blue")

## Plot
barr_salix <- ggplot(tree_h, aes(x= Datum, y= Medelhöjd, color = Trädslag, linetype = Hagn_id)) +
  geom_point(size= 1.7, position = position_dodge(0.0))+
  geom_line(aes(linetype = Hagn_id), position = position_dodge(0.0))+
  scale_linetype_manual(values = c(1,2,3,4))+
  scale_shape_manual(values=c(19,21))+
  scale_color_manual(values = colors_all)+
  theme_classic()+
  theme(legend.position="bottom")+
  theme(plot.title = element_text(family = "", face = "bold", colour = "black", size = "", hjust = 0.5, vjust = 0))+
  geom_errorbar(aes(ymin= Medelhöjd-se, ymax= Medelhöjd+se, linetype = Hagn_id), width = 0, 
                position = position_dodge(0.0), linetype = 1)+
  # labs(title = "Salix", colour = "Trädslag", linetype = "Behandling", x = "", y = "Medelhöjd (cm)")+
  labs(title = "Salix", linetype = "Behandling", x = "", y = "Medelhöjd (cm)")+
  scale_x_continuous(breaks=c(2012,2013,2014,2015,2016,2017,2018,2019,2020,2021,2022,2023,2024,2025))+
  guides(linetype = guide_legend(order = 1),  # Behandling first
         colour = guide_legend(order = 2))  # Trädslag second
print(barr_salix)

## Combine the two data frames
tree_2 <- rbind(Barrtrad, Ek)
tree_2$Trädslag <- ifelse(tree_2$Trädslag %in% c("Rönn", "Asp", "Salix", "Ek"), "RASE", "Barrträd")
tree_2$Trädslag <- factor(tree_2$Trädslag, levels = c("Barrträd", "RASE"))
tree_h <- summarySE(tree_2, measurevar="Medelhöjd", groupvars = c("Datum", "Hagn_id", "Trädslag"))

## Set group colors
colors_all <- c("Barrträd" = "black", "RASE" = "blue")

## Plot
barr_ek <- ggplot(tree_h, aes(x= Datum, y= Medelhöjd, color = Trädslag, linetype = Hagn_id)) +
  geom_point(size= 1.7, position = position_dodge(0.0))+
  geom_line(aes(linetype = Hagn_id), position = position_dodge(0.0))+
  scale_linetype_manual(values = c(1,2,3,4))+
  scale_shape_manual(values=c(19,21))+
  scale_color_manual(values = colors_all)+
  theme_classic()+
  theme(legend.position="bottom")+
  theme(plot.title = element_text(family = "", face = "bold", colour = "black", size = "", hjust = 0.5, vjust = 0))+
  geom_errorbar(aes(ymin= Medelhöjd-se, ymax= Medelhöjd+se, linetype = Hagn_id), width = 0, 
                position = position_dodge(0.0), linetype = 1)+
  # labs(title = "Ek", colour = "Trädslag", linetype = "Behandling", x = "", y = "Medelhöjd (cm)")+
  labs(title = "Ek", linetype = "Behandling", x = "", y = "Medelhöjd (cm)")+
  scale_x_continuous(breaks=c(2012,2013,2014,2015,2016,2017,2018,2019,2020,2021,2022,2023,2024,2025))+
  guides(linetype = guide_legend(order = 1),  # Behandling first
         colour = guide_legend(order = 2))  # Trädslag second
print(barr_ek)

# Combine the four plots into a 2x2 grid
library(patchwork)

Combined_plot <- (barr_rönn + barr_asp + barr_salix + barr_ek) +
  plot_layout(ncol = 2, guides = "collect") &
  theme(legend.position = "bottom")

Combined_plot

## Export with ggsave (change file name/path depending on species)
ggsave("HeightÖM_Conifer_RASE.png", plot = last_plot(), device = NULL, path ="//storage-um.slu.se/restricted$/vfm/Vilt-Skog/Moose-Targets/Results/Joseph/ÖM", 
       scale = 1, width = 16, height = 16, dpi = 300, limitsize = TRUE, units = "cm")
