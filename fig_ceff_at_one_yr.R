#' relies on ggh4x, a non-CRAN package
#' obtain with
#' remotes::install_github("teunbrand/ggh4x")
suppressPackageStartupMessages({
  require(RSQLite)
  require(data.table)
  require(ggplot2)
  require(cowplot)
  # require(ggh4x)
})

.args <- if (interactive()) c(
  "data/covid_vac_v1.2.sqlite", "fig/ceff_at_one_year.png"
) else commandArgs(trailingOnly = TRUE)

dbpath <- .args[1]

con <- dbConnect(SQLite(), dbpath)

stmt <- "SELECT M.*, P.* FROM met M JOIN par P USING(serial) JOIN job J USING(serial) WHERE J.status == 'D';";

dt <- data.table(dbGetQuery(
  con, stmt
))

dbDisconnect(con)

dt$serial <- NULL

casecols <- grep("rc", names(dt), value = TRUE)
keycols <- grep("^(Rt|rc)", names(dt), invert = TRUE, value = TRUE)
long.dt <- melt(dt, id.vars = keycols, measure.vars = casecols)

simkeys <- c(
  setdiff(keycols, c("serial","seed", "realization")),
  "variable"
)

novac <- long.dt[vac==0, .(
  wk = as.integer(gsub("rc", "", variable)),
  cum_cases = cumsum(value)
), keyby=.(realization, vac_day, vsd)]

vac <- long.dt[vac==1, .(
  wk = as.integer(gsub("rc", "", variable)),
  cum_cases = cumsum(value)
), keyby=setdiff(keycols, c("serial","seed", "vac"))]

jt <- vac[novac, on = .(realization, vac_day, vsd, wk)]

oneyear <- jt[wk == 52][,
  ceff := (i.cum_cases - cum_cases)/i.cum_cases
]

qs.dt <- oneyear[,{
  qs <- quantile(ceff, probs = c(0.025, 0.25, 0.5, 0.75, 0.975))
  names(qs) <- c("lo.lo", "lo", "med", "hi", "hi.hi")
  as.list(qs)
}, keyby = c("vac_eff","vac_cov","vac_mech","vac_day","vsd")]

ggplot(qs.dt) +
  aes(x=vac_eff, color = factor(vac_cov), fill = factor(vac_cov), group = vac_cov) +
  facet_grid(vsd ~ vac_day, labeller = labeller(
    vsd = function(v) sprintf("VSD = %s",v),
    vac_day = function(v) sprintf(
      "vaccine day = %s",
      (as.Date("2020-01-01")+c(56,140,200)-1)[as.integer(v)+1]
    )
  )) +
  geom_ribbon(aes(ymin = lo.lo, ymax=hi.hi, color=NULL), alpha = 0.2) +
  geom_ribbon(aes(ymin = lo, ymax=hi, color=NULL), alpha = 0.3) +
  geom_line(aes(y=med)) +
  coord_cartesian(xlim = c(0, 1), ylim = c(0, 1), expand = FALSE) +
  scale_x_continuous("Vaccine Efficacy") +
  scale_y_continuous("Cumulative Effectiveness on 2021-01-01") +
  scale_color_discrete("Vaccine\nCoverage", aesthetics = c("color","fill")) +
  theme_minimal() +
  theme(
    panel.spacing.y = unit(1, "line"),
    panel.spacing.x = unit(1.5, "line")
  )

save_plot(
  tail(.args, 1),
  p,
  ncol = qs.dt[, length(unique(vac_day))],
  nrow = qs.dt[, length(unique(vsd))],
  base_height = 2, base_asp = 2
)
