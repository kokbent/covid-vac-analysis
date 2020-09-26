library(dplyr)
library(ggplot2)
library(viridis)

R = seq(from = 1, to = 4, by = .001)

VC_L = 1
VC_U = (1/.1)*(1 - (1/R))
Tmp1 <- data.frame(R,VC_L,VC_U) %>% mutate(VE="10%")

VC_L = (1/.1)*(1 - (1/R))
VC_U = (1/.3)*(1 - (1/R))
Tmp3 <- data.frame(R,VC_L,VC_U) %>% mutate(VE="30%")

VC_L = (1/.3)*(1 - (1/R))
VC_U = (1/.5)*(1 - (1/R))
Tmp5 <- data.frame(R,VC_L,VC_U) %>% mutate(VE="50%")

VC_L = (1/.5)*(1 - (1/R))
VC_U = (1/.7)*(1 - (1/R))
Tmp7 <- data.frame(R,VC_L,VC_U) %>% mutate(VE="70%")

VC_L = (1/.7)*(1 - (1/R))
VC_U = (1/.9)*(1 - (1/R))
Tmp9 <- data.frame(R,VC_L,VC_U) %>% mutate(VE="90%")

F1 <- rbind(Tmp1, Tmp3) 
F1 <- rbind(F1, Tmp5)
F1 <- rbind(F1, Tmp7)
F1 <- rbind(F1, Tmp9)
F1 <- mutate(F1, VC_L=ifelse(VC_L>1,1,VC_L))

png('fig/vac_crit.png', 5.2, 4, unit='in', res=360)
ggplot() +
  theme_minimal() +
  geom_ribbon(data=F1, aes(x=R, ymin=VC_L, ymax=VC_U, group=VE, fill=VE),alpha=.5)+
  geom_line(data=F1, aes(x=R, y=VC_U, color=VE))+
  theme(legend.title = element_text(face = "italic")) +
  labs(
    x=expression(paste("Effective reproductive number, ", italic("R"))),
    y=expression(paste("Critical vaccination percentage, ", italic(v[c])))) +
  scale_y_continuous(breaks=c(0,.2,.4,.6,.8,1),
                     limits=c(0,1),
                     labels = scales::percent_format())+
  scale_color_viridis_d(aesthetics = c("colour", "fill"))+
  coord_cartesian(ylim=c(0,1),xlim=c(1,4))
dev.off()


