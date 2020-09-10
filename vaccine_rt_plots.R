#' relies on ggh4x, a non-CRAN package
#' obtain with
#' remotes::install_github("teunbrand/ggh4x")
suppressPackageStartupMessages({
  require(RSQLite)
  require(data.table)
  require(ggplot2)
  require(cowplot)
  require(ggh4x)
})

.args <- if (interactive()) c(
  "data/covid_vac_v1.2.sqlite", "fig/Rt.png"
) else commandArgs(trailingOnly = TRUE)

dbpath <- .args[1]

con <- dbConnect(SQLite(), dbpath)

stmt <- "SELECT M.*, P.* FROM met M JOIN par P USING(serial) JOIN job J USING(serial) WHERE J.status == 'D';";

dt <- data.table(dbGetQuery(
  con, stmt
))

dbDisconnect(con)

dt$serial <- NULL

rtcols <- grep("Rt", names(dt), value = TRUE)
keycols <- grep("^(Rt|rc)", names(dt), invert = TRUE, value = TRUE)
long.dt <- melt(dt, id.vars = keycols, measure.vars = rtcols)

simkeys <- c(
  setdiff(keycols, c("serial","seed", "realization")),
  "variable"
)
qs.dt <- long.dt[,{
  qs <- quantile(value, probs = c(0.025, 0.25, 0.5, 0.75, 0.975))
  names(qs) <- c("lo.lo", "lo", "med", "hi", "hi.hi")
  as.list(qs)
}, keyby = simkeys]

qs.dt[,
  tm := as.integer(gsub("Rt", "", variable))
]

vac_cov_lvls <- qs.dt[, unique(vac_cov)]

ref.dt <- qs.dt[
  vac==0,
  cbind(.SD[
    rep(1:.N, length(vac_cov_lvls))
    ], vac_cov = rep(vac_cov_lvls, each = .N)
  ), .SDcols = -c("vac_cov")
]

interval_alp <- 0.5

p <- ggplot(
  qs.dt[vac==1]
) + aes(
  x=tm,
  color = factor(vac_eff), fill = factor(vac_eff),
  group = vac_eff
) + facet_nested(
    vac_cov + vsd ~ vac_day,
    labeller = labeller(
      vac_cov = function(v) sprintf("Vcov=%s", v),
      vsd = function(v) sprintf("VSD=%s", v),
      vac_day = function(v) sprintf("vac_day=%s", v)
    )
) + geom_ribbon(
    aes(ymin=lo, ymax=hi, fill = "no vaccine"),
    ref.dt,
    alpha = interval_alp
) + geom_line(
  aes(y=med, color = "no vaccine"),
  ref.dt
) + 
  geom_line(aes(y=med)) +
#  geom_ribbon(aes(ymin=lo.lo, ymax=hi.hi, color = NULL), alpha = 0.5) +
  geom_ribbon(
    aes(ymin=lo, ymax=hi, color = NULL),
    alpha = interval_alp
  ) +
  geom_line(aes(y=med)) +
  scale_color_brewer(
    expression(V[eff]),
    type = "seq",
    aesthetics = c("color", "fill")
  ) +
  scale_y_continuous(expression(R[t])) +
  scale_x_continuous("simulation week") +
  coord_cartesian(ylim = c(0,2), xlim = c(0, 50), expand = FALSE) +
  theme_minimal() +
  theme(
    panel.spacing = unit(1, "lines")
  )

save_plot(
  tail(.args, 1),
  p,
  ncol = qs.dt[vac==1, length(unique(vac_day))],
  nrow = qs.dt[vac==1, length(unique(vsd))*length(unique(vac_cov))],
  base_height = 2, base_asp = 2
)
