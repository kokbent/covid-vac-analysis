rm(list=ls())

library(RSQLite)
library(dplyr)
library(ggplot2)
library(stringr)
library(tidyr)

con <- dbConnect(SQLite(), "data/covid_vac_v1.1.sqlite")

dbListTables(con)
met <- dbGetQuery(con, "SELECT * FROM met")
par <- dbGetQuery(con, "SELECT vac, vac_eff, vac_cov, vsd, realization, serial FROM par")

dat <- par %>%
  left_join(met, by = "serial")
head(dat)

#### Separate vac = 0 out
rt_curves0 <- dat %>%
  filter(vac == 0) %>%
  group_by(vac, vsd) %>%
  summarise_at(vars(Rt00:Rt51), mean) %>%
  pivot_longer(-c(vac, vsd), 
               names_to = "week", values_to = "Rt") 

rt_curves0 <- mutate(rt_curves0, 
                      week = as.numeric(str_remove(week, "Rt")))

#### vac = 1
rt_curves1 <- dat %>%
  filter(vac == 1) %>%
  group_by(vac, vac_eff, vac_cov, vsd) %>%
  summarise_at(vars(Rt00:Rt51), mean) %>%
  pivot_longer(-c(vac, vac_eff, vac_cov, vsd), 
               names_to = "week", values_to = "Rt")

rt_curves1 <- mutate(rt_curves1,  
                      week = as.numeric(str_remove(week, "Rt")))

rt_curves_l <- split(rt_curves1, list(rt_curves1$vac_cov, rt_curves1$vsd))

#### Insert corresponding vac = 0 grp into each combination of vac_cov, vsd
for (i in 1:length(rt_curves_l)) {
  if (unique(rt_curves_l[[i]]$vsd) == 0) {
    rt_curves_l[[i]] <- bind_rows(rt_curves0[rt_curves0$vsd == 0,],
                                   rt_curves_l[[i]])
  } else {
    rt_curves_l[[i]] <- bind_rows(rt_curves0[rt_curves0$vsd == 0.2,],
                                   rt_curves_l[[i]])
  }
}

#### Combinations
vac_vsd_combi <- expand.grid(vac_cov = c(0.5, 0.7),
                             vsd = c(0, 0.2))

#### Plot using for loop
for (i in 1:length(rt_curves_l)) {
  outfile <- paste0("fig/Rt_vac_cov_", vac_vsd_combi$vac_cov[i],
                    "_vsd_", vac_vsd_combi$vsd[i],
                    ".png")
  rtc <- rt_curves_l[[i]]
  
  ylim <- range(c(rtc$Rt))
  ylim[1] <- 0
  main <- paste0("Vaccine coverage ", vac_vsd_combi$vac_cov[i] * 100,
                 "%, Social Distancing ", vac_vsd_combi$vsd[i] * 100, "%")
  
  png(outfile, 1600, 1200, res = 200)
  
  cond <- rtc$vac == 0
  plot(rtc[cond, c("week", "Rt")], 
       type = "l",
       ylim = ylim,
       main = main, 
       xlab = "Week", 
       ylab = "Rt",
       col = "red",
       lwd = 2,
       axes = F)
  axis(side = 1, 
       at = 0:5 * 10, 
       labels = (0:5 * 10 + 28) %% 52 + 1)
  axis(side = 2)
  
  cond <- rtc$vac == 1 & rtc$vac_eff == 0.1
  lines(rtc[cond, c("week", "Rt")],
        col = "darkgoldenrod4",
        lwd = 2)
  
  cond <- rtc$vac == 1 & rtc$vac_eff == 0.3
  lines(rtc[cond, c("week", "Rt")],
        col = "cyan4",
        lwd = 2)
  
  cond <- rtc$vac == 1 & rtc$vac_eff == 0.5
  lines(rtc[cond, c("week", "Rt")],
        col = "dodgerblue4",
        lwd = 2)
  
  cond <- rtc$vac == 1 & rtc$vac_eff == 0.7
  lines(rtc[cond, c("week", "Rt")],
        col = "darkblue",
        lwd = 2)
  
  legend("topright",
         legend = c("No Vaccine", "10% Efficacy", "30% Efficacy", 
                    "50% Efficacy", "70% Efficacy"),
         col = c("red", "darkgoldenrod4", "cyan4",
                 "dodgerblue4", "darkblue"),
         lty = 1,
         lwd = 2,
         bty = "n")
  abline(h = 1, lty = 3, lwd = 2)
  
  dev.off()
}

dbDisconnect(con)