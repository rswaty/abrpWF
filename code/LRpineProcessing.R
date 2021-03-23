# getting pine data ready for use in charts

## install.packages("pacman")

## use pacman to load list of packages, install if missing
pacman::p_load(tidyverse, ggsci, ggtext, readr, dyplr, DT)

#set theme for all charts
theme_set(theme_bw())

# reading in data

#canopy <-read.csv("C:\\Users\\Laura\\Documents\\ABRP\\Cleanup\\Canopy_Cleanup\\Canopy_for_R.csv", header=T, sep=",")

#original
canopy <- read_csv("data/Canopy_for_R.csv")
view(canopy) #original= View(pine)

#events <- read.csv("data/events.csv")
#View(events)

################## Explore a bit #########################
## number of total observations per year
## number of plots per BU per year (colored line chart)

# observations per year no grouping or separation by BU

yearObs <-
canopy %>%
  count(MON_YEAR)

yearObsChart <-
  yearObs %>%
  ggplot(aes(x = MON_YEAR, y = n)) +
  geom_line(size = 1.5) +
  labs(x = "Year",
       y = "Number of rows",
       title = "Number of observations per year",
       caption = "Data from Florida Chapter")  +
  # geom_vline(xintercept = events$yearTreated,
  #            color = "red",
  #            linetype = "longdash") +

  #Adjust the axes
  theme(plot.title = element_text(face="bold", size =14),
        axis.title.x = element_text(face="bold", size = 12),
        axis.title.y = element_text(face="bold", size = 12),
        legend.title = element_text(face="bold", size = 12))


yearObsChart

# number of observations per year, by BU

yearObsBU <- canopy %>%
  count (MON_YEAR, BU)

# fix BU data type
yearObsBU %>%
  mutate(BU = as.character(BU))

yearObsBUChart <-
  yearObsBU %>%
  ggplot(aes(x = MON_YEAR, y = n, group =  BU, color = as.factor(BU))) +
  geom_line(size=1.5, position = position_jitter(width = 0.0001, height = 1.0)) +
  #geom_point(size =  2.0) +

  # My top color palette
  scale_color_simpsons(name = "BU") +
  # labels
  labs(x = "Year",
       y = "Number of rows",
       title = "Number of observations per year colored by BU",
       subtitle = "Lines jittered so that they would not totally overlap",
       caption = "Data from Florida Chapter")  +

  #Adjust the axes
  theme(plot.title = element_text(face="bold", size =14),
        axis.title.x = element_text(face="bold", size = 12),
        axis.title.y = element_text(face="bold", size = 12),
        legend.title = element_text(face="bold", size = 12))


yearObsBUChart




################# IND per year with  "canopy" data (NO GROUPING) ##############

#mutate MON_YEAR to be character

canopy %>%
  mutate(MON_YEAR = as.character(MON_YEAR))

INDExplore <-
  canopy %>%
  ggplot(aes(x = MON_YEAR, y = IND, group = MON_YEAR, fill = MON_YEAR)) +
  geom_boxplot(position = position_dodge(width = 0.9)) +
  geom_point(position = position_jitterdodge(seed = 1, dodge.width = 0.9)) +
  theme_bw() +
  labs(x = "Year",
       y = "Number of INDp",
       title = "Total number of rooted stems per plot",
       subtitle = "Dots jittered so that they would not totally overlap",
       caption = "Data from Florida Chapter")  +

  #Adjust the axes, remove legend
  theme(legend.position = "none",
        plot.title = element_text(face="bold", size =14),
        axis.title.x = element_text(face="bold", size = 12),
        axis.title.y = element_text(face="bold", size = 12))



INDExplore



################### number of IND per year averaged within BUs ##########
# need to get average IND per year per BU...averaging the PLOT field

#test
names(canopy)
summary(canopy$IND)
#end test

groupedCanopy <- canopy %>%
  group_by(PRES,  COVER,  MON_YEAR, SPCODE)%>%
  summarise(meanIND = mean(IND))



# make chart

INDCOVERYear <-groupedCanopy %>%

 ggplot(aes(x= MON_YEAR, y= meanIND, group = SPCODE, color = SPCODE)) +
  geom_line(size=1.5) +
  geom_point(size =  2.0) +

  # My top color palette
  scale_color_simpsons() +

  # Theme
  theme_bw() +


  # Update the labels of the axes
  labs(x = "Year",
       y = "Mean number of rooted stems per year at ABRP",
       title = "Summarizing number of rooted stems across cover categories",
       caption = "Data from Florida Chapter",
       color = "IND COVER")  +

  #Adjust the axes
  theme(plot.title = element_text(face="bold", size =14),
        axis.title.x = element_text(face="bold", size = 12),
        axis.title.y = element_text(face="bold", size = 12),
        legend.title = element_text(face="bold", size = 12))

INDCOVERYear

ggsave("INDCOVERYear.png", plot = meanIND, width = 10, scale = 1, units = "in", dpi =300)

##############End Laura's Code###############
# split out by BU


groupedPineBU <- pine %>%
  group_by(PRES,  SIZE,  MON_YEAR, BU)%>%
  summarise(meanSTEMS = mean(STEMS))

# make the chart
groupedPineYearBU <-groupedPineBU %>%

  ggplot(aes(x= MON_YEAR, y= meanSTEMS, group = SIZE, color = SIZE)) +
  geom_line(size=1.0) +
  geom_point(size =  1.5) +
  facet_grid(rows = vars(BU), scales = "free") +
  # Update the labels of the axes
  labs(x = "Year",
       y = "Mean number of stems",
       title = "Stems per year by size class",
       subtitle = "Each facet is a BU; NOTE that scales are different") +


  # My top color palette
  scale_color_simpsons() +

  # Theme
  theme_bw()

  groupedPineYearBU


