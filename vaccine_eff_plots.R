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
epi_curves0 <- dat %>%
  filter(vac == 0) %>%
  group_by(vac, vsd) %>%
  summarise_at(vars(rc00:rc51), mean) %>%
  pivot_longer(-c(vac, vsd), 
               names_to = "week", values_to = "incd") 

epi_curves0 <- mutate(epi_curves0, 
                      incd = incd / 31.27,
                      week = as.numeric(str_remove(week, "rc")))

#### vac = 1
epi_curves1 <- dat %>%
  filter(vac == 1) %>%
  group_by(vac, vac_eff, vac_cov, vsd) %>%
  summarise_at(vars(rc00:rc51), mean) %>%
  pivot_longer(-c(vac, vac_eff, vac_cov, vsd), 
               names_to = "week", values_to = "incd")

epi_curves1 <- mutate(epi_curves1,  
                      incd = incd / 31.27,
                      week = as.numeric(str_remove(week, "rc")))

epi_curves_l <- split(epi_curves1, list(epi_curves1$vac_cov, epi_curves1$vsd))

#### Insert corresponding vac = 0 grp into each combination of vac_cov, vsd
for (i in 1:length(epi_curves_l)) {
  epi_curves_l[[i]] <- left_join(epi_curves_l[[i]],
                                 epi_curves0,
                                 by = c("vsd", "week")) %>%
    rename(vac = vac.x, incd = incd.x, incd_control = incd.y) %>%
    mutate(effectiveness = 1 - (incd / incd_control))
}

vac_vsd_combi <- expand.grid(vac_cov = c(0.5, 0.7),
                             vsd = c(0, 0.2))

for (i in 1:length(epi_curves_l)) {
  outfile <- paste0("fig/eff_vac_cov_", vac_vsd_combi$vac_cov[i],
                    "_vsd_", vac_vsd_combi$vsd[i],
                    ".png")
  epc <- epi_curves_l[[i]]
  
  main <- paste0("Vaccine coverage ", vac_vsd_combi$vac_cov[i] * 100,
                 "%, Social Distancing ", vac_vsd_combi$vsd[i] * 100, "%")
  
  png(outfile, 1600, 1200, res = 200)
  
  ylim <- range(epc$effectiveness)
  ylim[2] <- 1
  
  cond <- epc$vac == 1 & epc$vac_eff == 0.1
  plot(epc[cond, c("week", "effectiveness")], 
       type = "l",
       ylim = ylim,
       main = main, 
       xlab = "Week", 
       ylab = "Overall Weekly Effectiveness",
       col = "darkgoldenrod4",
       lwd = 2,
       axes = F)
  axis(side = 1, 
       at = 0:5 * 10, 
       labels = (0:5 * 10 + 28) %% 52 + 1)
  axis(side = 2)
  abline(h = 0, lty = 3, lwd = 2)
  
  cond <- epc$vac == 1 & epc$vac_eff == 0.3
  lines(epc[cond, c("week", "effectiveness")],
        col = "cyan4",
        lwd = 2)
  
  cond <- epc$vac == 1 & epc$vac_eff == 0.5
  lines(epc[cond, c("week", "effectiveness")],
        col = "dodgerblue4",
        lwd = 2)
  
  cond <- epc$vac == 1 & epc$vac_eff == 0.7
  lines(epc[cond, c("week", "effectiveness")],
        col = "darkblue",
        lwd = 2)
  
  legend("topright",
         legend = c("10% Efficacy", "30% Efficacy", 
                    "50% Efficacy", "70% Efficacy"),
         col = c("darkgoldenrod4", "cyan4",
                 "dodgerblue4", "darkblue"),
         lty = 1,
         lwd = 2,
         bty = "n")
  
  dev.off()
}

dbDisconnect(con)