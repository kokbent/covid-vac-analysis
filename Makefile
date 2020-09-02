
-include local.Makefile

FIGPATH ?= fig
DATPATH ?= data

R = Rscript $^ $@

${FIGPATH}/Rt.png: vaccine_rt_plots.R ${DATPATH}/covid_vac_v1.2.sqlite
	${R}

figures: $(patsubst %,${FIGPATH}/%.png,Rt)