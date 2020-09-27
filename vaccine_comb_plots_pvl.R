rm(list=ls())

library(RSQLite)
library(dplyr)
library(ggplot2)
library(stringr)
library(tidyr)
library(lubridate)

#### Initialization and Database ----
totalpop_10k <- 31.27 # Adjust accordingly
max_week <- 52 # Adjust accordingly, start counting from 1
con <- dbConnect(SQLite(), "data/covid_vac_v2.0.sqlite")
pal <- c("#000000FF", "#440154FF", "#3C4F8AFF", 
         "#238A8DFF", "#49C16DFF", "#C8CB15FF")

dbListTables(con)
met <- dbGetQuery(con, "SELECT * FROM met")
vac_day_choice <- dbGetQuery(con, "SELECT DISTINCT vac_day FROM par")$vac_day
vac_day_choice <- sort(vac_day_choice)


#### Functions ----
calc_eff <- function (incd, incd_control, start_wk) {
  # start_wk = when should the cumulation begin, range: 1 to Nweek
  Nweek <- length(incd)
  eff2 <- rep(NA, Nweek)
  if (start_wk > 1) eff2[1:(start_wk-1)] <- 0
  eff2[start_wk:Nweek] <- 
    (1 - cumsum(incd[start_wk:Nweek]) / cumsum(incd_control[start_wk:Nweek]))
  return(eff2)
}

plot_curves <- function (epc, param, legend = F, hline = NULL, ...) {
  
  flag <- ifelse(startsWith(param, "Eff") | param == "case_avert", 
                 "n", "l")
  ## Remove last week's Rt values (misleading)
  ## Obsolete?
  # epc$Rt[epc$week > max(epc$week) - 1] <- NA
  
  cond <- epc$vac == 0
  plot(epc[cond, c("week", param)], 
       type = flag,
       col = pal[1],
       lwd = 2,
       axes = F, ...)
  axis(side = 1, 
       at = 0:6 * 10, 
       labels = (0:6 * 10))
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
    legend("right",
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
                  "%, Social distancing ", vsd * 100, "% after vaccination")
  
  return(paste0(main1, "\n", main2))
}

#### Looping through vac_day ----
for (vac_day in vac_day_choice) {
  for (pvl in 0:1) {
    ## Read from sqlite
    sql <- paste0("SELECT vac, vac_eff, vac_cov, vsd, realization, serial FROM par
                WHERE vac_day = ", vac_day, " AND pvl = ", pvl)
    par <- dbGetQuery(con, sql)
    if (nrow(par) == 0) next
    
    ## Joining table
    dat <- par %>%
      left_join(met, by = "serial")
    
    #### Pivot longer ----
    ## Using vac == 0, vsd != 0 as control for all other scenarios
    epi_long <- dat %>%
      filter(!(vac == 0 & vsd == 0)) %>%
      pivot_longer(-c(vac, vsd, realization, serial, vac_eff, vac_cov),
                   names_to = c(".value", "week"),
                   names_pattern = "([a-zA-Z]+)([0-9]+)")
    
    epi_long <- epi_long %>%
      mutate(incd = rc / totalpop_10k,
             week = as.numeric(week))
    
    
    #### Calculate effectiveness and case averted ----
    ### Eff1 = Cumulative from beginning
    ### Eff2 = Cumulative since vaccination
    ### We can actually do realization matching here
    
    ## Add incidence of control
    epi_long1 <- epi_long %>%
      group_by(realization, week) %>%
      mutate(incd_control = incd[vac == 0]) %>%
      arrange(realization, week)
    
    ## For each trt combination and realization,
    ## calculate effectiveness & case averted
    start_wk <- ceiling(vac_day/7) + 1
    epi_long1 <- epi_long1 %>%
      group_by(vac, vac_cov, vac_eff, vsd, realization) %>%
      mutate(Eff1 = calc_eff(incd, incd_control, 1),
             Eff2 = calc_eff(incd, incd_control, start_wk),
             case_avert = cumsum(incd_control) - cumsum(incd))
    
    ## Take median of all metrics along all realization
    epi_long1 <- epi_long1 %>%
      group_by(vac, vac_cov, vac_eff, vsd, week) %>%
      summarise(incd = median(incd),
                Rt = median(Rt),
                Eff1 = median(Eff1),
                Eff2 = median(Eff2),
                case_avert = median(case_avert))
    
    #### Plot using for loop ----
    ## Identify unique combinations
    combinations <- epi_long1 %>%
      ungroup() %>%
      filter(vac == 1) %>%
      select(vac_cov, vsd) %>%
      distinct()
    
    ## Looping through each combinations
    for (i in 1:nrow(combinations)) {
      vac_cov_i <- combinations$vac_cov[i]
      vsd_i <- combinations$vsd[i]
      epc <- epi_long1 %>%
        ungroup() %>%
        filter((vac_cov == vac_cov_i & vsd == vsd_i) | vac == 0)
      
      outfile <- paste0("fig/comb-vac_cov-", vac_cov_i,
                        "-vsd-", vsd_i,
                        "-vac_day-", vac_day,
                        "-pvl-", pvl,
                        ".png")
      
      ## Plot start
      png(outfile, width = 15, height = 21, units = "cm",
          res = 200)
      
      par(mfrow = c(5, 1),
          mar = c(2, 4, 1, 1),
          oma = c(3, 0, 3, 8))
      
      plot_curves(epc, param = "Rt", legend = F, hline = 1, 
                  xlab = "",
                  ylab = "Rt", 
                  xlim = c(0, max_week - 1),
                  ylim = c(0, max(epc$Rt)), 
                  main = "")
      plot_curves(epc, param = "incd", legend = F,
                  xlab = "",
                  ylab = "Weekly Incidence per 10k", 
                  xlim = c(0, max_week - 1),
                  ylim = c(0, max(epc$incd)), 
                  main = "")
      plot_curves(epc, param = "Eff1", legend = F, hline = 0, 
                  xlab = "",
                  ylab = "Cumu. effectiveness 1", 
                  xlim = c(0, max_week - 1),
                  ylim = range(c(epc$Eff1, epc$Eff2)), 
                  main = "")
      plot_curves(epc, param = "Eff2", legend = F, hline = 0, 
                  xlab = "",
                  ylab = "Cumu. effectiveness 2", 
                  xlim = c(0, max_week - 1),
                  ylim = range(c(epc$Eff1, epc$Eff2)), 
                  main = "")
      plot_curves(epc, param = "case_avert", legend = F, hline = 0, 
                  xlab = "",
                  ylab = "Cumu. case averted", 
                  xlim = c(0, max_week - 1),
                  ylim = range(epc$case_avert), 
                  main = "")
      
      main <- make_title(vac_day, vac_cov_i, vsd_i)
      
      mtext("Week after first introduction", 1, 1, outer = T)
      mtext(main, 3, 0, outer = T)
      
      ## Additional hack for legend
      par(fig = c(0, 1, 0, 1), 
          oma = c(0, 0, 0, 0), 
          mar = c(0, 0, 0, 0), 
          new = TRUE)
      plot(0, 0, type = "n", bty = "n", xaxt = "n", yaxt = "n")
      legend("right",
             legend = c("No Vaccine", "10% Efficacy", "30% Efficacy", 
                        "50% Efficacy", "70% Efficacy", "90% Efficacy"),
             col = pal,
             lty = 1,
             lwd = 2,
             inset = c(0, 0),
             xpd = T)
      
      dev.off()
    }
  }
}

dbDisconnect(con)
