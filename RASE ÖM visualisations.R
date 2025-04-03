## Load packages
library(ggplot2)
library(ggvis)
library(tidyr)
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
tree_data <- OM_trees_21_25 %>% select(Distrikt, Hagn_id, Behandling, Datum, Art, Medelhöjd)

## Select only "Kontroll" and "Fast" for Hagn_id
tree_data <- tree_data[tree_data$Hagn_id %in% c("Fast", "Kontroll"), ]


# Select only Tall and Gran
Barrtrad <- tree_data %>%
  filter(Art %in% c("Tall", "Gran")) %>%
  group_by(Distrikt, Hagn_id, Behandling, Datum) %>%
  slice_max(Medelhöjd, n = 1) %>%
  mutate(Art = "Barrträd")  # Rename Art to "Barrträd"

## create data subsets for RASE species
Rönn <- tree_data [tree_data$Art == "Rönn", ]
Asp <- tree_data [tree_data$Art == "Asp", ]
Salix <- tree_data [tree_data$Art == "Salix", ]
Ek <- tree_data [tree_data$Art == "Ek", ]

## Combine the two data frames
tree_2 <- rbind(Barrtrad, Ek)
tree_h <- summarySE(tree_2, measurevar="Medelhöjd", groupvars = c("Datum", "Hagn_id", "Art"))
## Plot
tree_height <- ggplot(tree_h, aes(x= Datum, y= Medelhöjd, color = Art, linetype = Hagn_id)) +
  geom_point(size= 1.7, position = position_dodge(0.0))+
  geom_line(aes(linetype = Hagn_id), position = position_dodge(0.0))+
  scale_linetype_manual(values = c(1,2,3,4))+
  scale_shape_manual(values=c(19,21))+
  scale_color_manual(values = c("Barrträd" = "black", "Ek" = "purple"), 
                      breaks = c("Barrträd", "Ek")) +
  theme_classic()+
  theme(legend.position="bottom")+
  theme(plot.title = element_text(family = "", face = "bold", colour = "black", size = "", hjust = 0.5, vjust = 0))+
  geom_errorbar(aes(ymin= Medelhöjd-se, ymax= Medelhöjd+se, linetype = Hagn_id), width = 0, 
                position = position_dodge(0.0), linetype = 1)+
  labs(title = "", colour = "Trädslag", linetype = "Behandling", x = "", y = "Medelhöjd (cm)")+
  scale_x_continuous(breaks=c(2012,2013,2014,2015,2016,2017,2018,2019,2020,2021,2022,2023,2024,2025))+
  guides(linetype = guide_legend(order = 1),  # Behandling first
       colour = guide_legend(order = 2))  # Trädslag second
print(tree_height)

## Export with ggsave (change file name/path depending on species)
ggsave("HeightÖM_Conifer_Oak.png", plot = last_plot(), device = NULL, path ="//storage-um.slu.se/restricted$/vfm/Vilt-Skog/Moose-Targets/Results/Joseph/ÖM", 
       scale = 1, width = 14, height = 14, dpi = 300, limitsize = TRUE, units = "cm")
