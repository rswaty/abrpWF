


## use pacman to load list of packages, install if missing
pacman::p_load(tidyverse, ggsci, ggtext, readr, dyplr, DT, gghighlight)



#set theme for all charts
theme_set(theme_bw())

# reading in data

herbAll <- read_csv("data/herb.csv")
view(herbAll)


# remove number of tree seedlings columns (6-11)
 herbs <- herbAll[-c(6:11)]

 # pivot wide to long

 herbsL <- herbs %>%
   pivot_longer(
     cols = starts_with("%"),
     names_to = "species",
     names_prefix = "%",
     values_to = "percent"
   )


# group by quads, make unique BU-plot column?
 herbsPlots <- herbsL %>%
   group_by(PRES,  BU, PLOT, MONYEAR, species)%>%
   summarise(meanPercent = mean(percent))

 herbsPlots$bup <- paste(herbsPlots$BU, herbsPlots$PLOT, sep = "-")

 # messy herbs facets

 suppLabs <-



 messyPlot <-
   ggplot(data = herbsPlots, aes(x = MONYEAR, y = meanPercent)) +
   geom_jitter(aes(color = species), position= position_jitter(0.2)) +
   geom_line(aes(color = species, group=interaction(species, bup))) +
   geom_smooth(method = "loess", color = "grey14") +
   gghighlight(unhighlighted_colour = alpha("grey", 0.3))+
   facet_wrap(~ species) +



 messyPlot



 # major group
 herbsYear<- herbsL %>%
   group_by(MONYEAR, species)%>%
   summarise(meanPercent = mean(percent))



