library(tidyverse)

Sys.setlocale("LC_TIME","C")
options(stringsAsFactors=FALSE)
theme_set(theme_bw()) # just my preference for plots

df <- read_delim("PROJET-AIR-POLLUTION/Qu6_Yoan/DATA.csv", delim = ";")
variables <- c("O3MAG", "O3CHA", 
               "NO2MAG", "NO2CHA", 
               "PM10MAG", "PM10CHA", 
               "SO2MAG", 
               "TEMPMAG", "TEMPCHA", 
               "PRECMAG", "PRECCHA", 
               "RADMAG", "RADCHA", 
               "WINDDIRMAG", "WINDDIRCHA", 
               "WINDSPEMAG", "WINDSPECHA", 
               "PM2.5MAG")

lf <- pivot_longer(df, all_of(variables), names_to = "variable", values_to = "value")
daily.max <- lf %>%
  group_by(site, year, month, day, season, variable) %>%
  summarize(value = max(value, na.rm=TRUE), .groups = "drop") %>%
  pivot_wider(names_from = variable, values_from = value)

  ggplot(daily.max)+
  facet_grid(site~season)+
  geom_point(aes(TEMP, O3))

(cor.values <- daily.max %>% group_by(site, season) %>%
   summarize(correlation=cor(TEMP, O3, use="pairwise.complete.obs"), .groups = "drop"))

(cor.values.wf <- pivot_wider(cor.values, names_from = site, values_from = correlation))

ggplot(cor.values)+
  geom_col(aes(season, correlation))+
  scale_y_continuous(limits=c(0,1), expand=expansion(mult=c(0, 0.1)))+
  facet_grid(.~site)

  pivot_longer(daily.max, cols = all_of(setdiff(variables, "O3")), names_to = "variable", values_to = "value") %>% # everything but ozone
  ggplot+
  facet_grid(site~variable, scale="free_x")+
  geom_point(aes(value, O3, group=season, color=season), shape=4)

  ggplot(df)+
  facet_grid(site~season)+
  geom_point(aes(CO, NO2))

  library(lattice)

CorrelationValue <- function(x, y, ...) {
  correlation <- cor(x, y, use="pairwise.complete.obs") 
  if(is.finite(correlation)) {
    cpl <- current.panel.limits()
    panel.text(mean(cpl$xlim), mean(cpl$ylim),
               bquote(italic(r)==.(sprintf("%.2f", correlation))),
               adj=c(0.5,0.5), col="blue")
  }
}

daily.max %>%
  filter("LAU" == site) %>%
  with(splom(~cbind(O3, NO2, CO, PM10, TEMP, PREC, RAD) | season,
      upper.panel = CorrelationValue,
      pch=4))

      df %>%
filter(grepl("LAU", site, fixed=TRUE)) %>%
with(splom(~cbind(O3, NO2, CO, PM10, TEMP, PREC,RAD) | season,
      upper.panel = CorrelationValue,
      panel = panel.smoothScatter,
      pch=4, cex=.2, col="gray"))