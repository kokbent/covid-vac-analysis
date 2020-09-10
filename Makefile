
-include local.Makefile

FIGPATH ?= fig
DATPATH ?= data

R = Rscript $^ $@

${FIGPATH}/Rt.png: vaccine_rt_plots.R ${DATPATH}/covid_vac_v1.2.sqlite
	${R}

${FIGPATH}/fig4.png: fig_ceff_at_one_year.R ${DATPATH}/covid_vac_v1.2.sqlite
	${R}

figures: $(patsubst %,${FIGPATH}/%.png,Rt fig4)