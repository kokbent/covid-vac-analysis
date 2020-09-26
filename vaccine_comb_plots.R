rm(list=ls())

library(RSQLite)
library(dplyr)
library(ggplot2)
library(stringr)
library(tidyr)
library(lubridate)

#### Initialization and Database ----
totalpop_10k <- 31.27 # Adjust accordingly
con <- dbConnect(SQLite(), "data/covid_vac_v2.0.sqlite")
dbListTables(con)
met <- dbGetQuery(con, "SELECT * FROM met")
vac_day_choice <- dbGetQuery(con, "SELECT DISTINCT vac_day FROM par")$vac_day
vac_day_choice <- sort(vac_day_choice)


#### Functions ----
summ_pivot_tidy <- function (df) {
  # Summarise rc and Rt and then pivot long
  df1 <- df %>%
    group_by(vac, vac_eff, vac_cov, vsd) %>%
    summarise_at(vars(rc00:last_col()), mean) %>%
    pivot_longer(-c(vac, vac_eff, vac_cov, vsd),
                 names_to = c(".value", "week"),
                 names_pattern = "([a-zA-Z]+)([0-9]+)")
  
  df1 <- df1 %>%
    mutate(incd = rc / totalpop_10k,
           week = as.numeric(week))
  
  return(df1)
}

pal <- viridis::viridis(5, begin = 0, end = 0.95)
pal <- c("#000000FF", "#440154FF", "#3C4F8AFF", 
         "#238A8DFF", "#49C16DFF", "#C8CB15FF")

plot_curves <- function (epc, param, legend = F, hline = NULL, ...) {
  
  flag <- ifelse(param == "effectiveness", "n", "l")
  epc$Rt[epc$week > max(epc$week) - 1] <- NA
  
  cond <- epc$vac == 0
  plot(epc[cond, c("week", param)], 
       type = flag,
       col = pal[1],
       lwd = 2,
       axes = F, ...)
  axis(side = 1, 
       at = 0:6 * 10, 
       labels = (0:6 * 10) + 1)
  axis(side = 2)
  
  cond <- epc$vac == 1 & epc$vac_eff == 0.1
  lines(epc[cond, c("week", param)],
        col = pal[2],
        lwd = 2)
  
  cond <- epc$vac == 1 & epc$vac_eff == 0.3
  lines(epc[cond, c("week", param)],
        col = pal[3],
        lwd = 2)
  
  cond <- epc$vac == 1 & epc$vac_eff == 0.5
  lines(epc[cond, c("week", param)],
        col = pal[4],
        lwd = 2)
  
  cond <- epc$vac == 1 & epc$vac_eff == 0.7
  lines(epc[cond, c("week", param)],
        col = pal[5],
        lwd = 2)
  
  cond <- epc$vac == 1 & epc$vac_eff == 0.9
  lines(epc[cond, c("week", param)],
        col = pal[6],
        lwd = 2)
  
  if (legend) {
    legend("topright",
           legend = c("No Vaccine", "10% Efficacy", "30% Efficacy", 
                      "50% Efficacy", "70% Efficacy", "90% Efficacy"),
           col = pal,
           lty = 1,
           lwd = 2,
           bty = "n",
           xpd = NA)
  }
  
  if (!is.null(hline)) abline(h = hline, lty = 3, lwd = 2, xpd = F)
}

make_title <- function (vac_day, vac_cov, vsd) {
  main1 <- paste0("Vaccination at day ", vac_day, " after introduction")
  main2 <- paste0("Vaccine coverage ", vac_cov * 100,
                  "%, Social distancing ", vsd * 100, "%")
  
  return(paste0(main1, "\n", main2))
}


#### Looping through vac_day ----
for (vac_day in vac_day_choice) {
  # Uncomment this if vac_day is implemented
  sql <- paste0("SELECT vac, vac_eff, vac_cov, vsd, realization, serial FROM par
                WHERE vac_day = ", vac_day)
  par <- dbGetQuery(con, sql)
  
  # Comment this if vac_day is implemented
  # par <- dbGetQuery(con, "SELECT vac, vac_eff, vac_cov, vsd, realization, serial FROM par")
  
  ## Joining table
  dat <- par %>%
    left_join(met, by = "serial")
  
  
  #### Separate by vac ----
  #### Separate data into vac == 0 and vac == 1
  ## vac == 0
  epi_curves0 <- dat %>%
    filter(vac == 0) %>%
    summ_pivot_tidy %>%
    ungroup() %>%
    select(-vac_eff, -vac_cov)
  
  ## vac == 1
  epi_curves1 <- dat %>%
    filter(vac == 1) %>%
    summ_pivot_tidy %>%
    ungroup()
  
  
  #### Split, Insert, Effectiveness ----
  #### Split vac == 1 by combination of vac_cov, vsd then insert 
  #### corresponding vac = 0 grp. Then calculate effectiveness.
  epi_curves_l <- split(epi_curves1, list(epi_curves1$vac_cov, epi_curves1$vsd))
  
  for (i in 1:length(epi_curves_l)) {
    vsd <- unique(epi_curves_l[[i]]$vsd)
    epi_curves_l[[i]] <- bind_rows(epi_curves0[epi_curves0$vsd == vsd,],
                                   epi_curves_l[[i]])
    
    epi_curves_l[[i]] <- epi_curves_l[[i]] %>%
      group_by(week) %>%
      mutate(incd_control = incd[vac == 0]) %>%
      arrange(week) %>%
      group_by(vac, vac_eff) %>%
      mutate(effectiveness = 1 - (cumsum(incd) / cumsum(incd_control)))
  }
  
  
  #### Plot using for loop ----
  for (i in 1:length(epi_curves_l)) {
    epc <- epi_curves_l[[i]]
    vac_cov <- unique(epc$vac_cov) %>% na.omit()
    vsd <- unique(epc$vsd) %>% na.omit()
    
    outfile <- paste0("fig/comb-vac_cov-", vac_cov,
                      "-vsd-", vsd,
                      "-vac_day-", vac_day,
                      ".png")
    
    png(outfile, width = 15, height = 21, units = "cm",
        res = 200)
    
    par(mfrow = c(3, 1),
        mar = c(2, 4, 1, 3),
        oma = c(3, 0, 3, 1))
    
    plot_curves(epc, param = "Rt", legend = F, hline = 1, 
                xlab = "",
                ylab = "Rt", 
                ylim = c(0, max(epc$Rt)), 
                main = "")
    plot_curves(epc, param = "incd", legend = T,
                xlab = "",
                ylab = "Weekly Incidence per 10k", 
                ylim = c(0, max(epc$incd)), 
                main = "")
    plot_curves(epc, param = "effectiveness", legend = F, hline = 0, 
                xlab = "",
                ylab = "Cumulative effectiveness", 
                ylim = range(epc$effectiveness), 
                main = "")
    
    main <- make_title(vac_day, vac_cov, vsd)
    
    mtext("Week after first introduction", 1, 1, outer = T)
    mtext(main, 3, 0, outer = T)
    
    dev.off()
  }
  
}

dbDisconnect(con)
