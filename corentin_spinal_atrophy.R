library(tidyverse)
library(data.table)
library(FSA)
options(scipen = 999)


# COmpare the 3 groups -----------------

corentin_inputs <- fread("corentin_inputs.txt")

boxplot <- corentin_inputs %>%
  mutate(Traitement=ifelse(Traitement=="R", "2- [R] Risdiplam",
                           ifelse(Traitement=="S", "1- [S] Nusinersen", "3- [R+S] Risdiplam+Nusinersen"))) %>%
  rename("Treatment"="Traitement") %>%
  ggplot(aes(Treatment, TOTAL, colour=Treatment, fill=Treatment)) +
  geom_boxplot(alpha=0.5, notch = F,outlier.colour = "white",
               outlier.color = "white",
               outlier.fill = "white") +
  geom_jitter(size=4, alpha=0.7, width = 0.3) +
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        legend.position = "right",
        axis.text.x = element_text(angle = 45, hjust = 1)) +
  theme(panel.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.background = element_blank(),
        strip.text = element_blank(),
        axis.line = element_blank(),
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10),
        axis.title.x = element_text(size = 12, vjust = -0.5),
        axis.title.y = element_text(size = 12, vjust = -0.5),
        plot.margin = margin(5, 5, 5, 5, "pt")) +
  xlab("") + ylab("Total MFM [%] \n") +
  scale_fill_manual(values=c("#558EA9", "#A95592", "#271E74")) +
  scale_colour_manual(values=c("#558EA9", "#A95592", "#271E74")) 

ggsave(file="boxplot.svg", plot=boxplot, width=6, height=6)


corentin_inputs %>% group_by(Traitement) %>%
  summarise(mean=mean(TOTAL),
            sd=sd(TOTAL),
            median=median(TOTAL), 
            Q1=quantile(TOTAL, 0.25),
            Q3=quantile(TOTAL, 0.75),
            n=n())

# Traitement  mean    sd median    Q1    Q3     n
# 1 R           31.6  18.7   34.4  22.8  37       7
# 2 S           42.5  27.5   34    22.5  64.5    19
# 3 SR          26.0  14.7   28.6  17.7  34.1     6


kruskal.test(TOTAL  ~ Traitement , data = corentin_inputs)

# Kruskal-Wallis rank sum test
# 
# data:  TOTAL by Traitement
# Kruskal-Wallis chi-squared = 1.4525, df = 2, p-value = 0.4837

dunnTest(TOTAL  ~ Traitement, data = corentin_inputs, method = "bonferroni")
         
# Comparison          Z   P.unadj     P.adj
# 1      R - S -0.5213189 0.6021447 1.0000000
# 2     R - SR  0.5818244 0.5606849 1.0000000
# 3     S - SR  1.1834339 0.2366372 0.7099116

# -------------

# Group both Risdiplam-containing arms as a sanity check ---------------

ignore <- corentin_inputs %>% mutate(Traitement=ifelse(Traitement=="SR", "R", Traitement))

ignore %>% group_by(Traitement) %>%
  summarise(mean=mean(TOTAL),
            sd=sd(TOTAL),
            median=median(TOTAL), 
            Q1=quantile(TOTAL, 0.25),
            Q3=quantile(TOTAL, 0.75),
            n=n())

# Traitement  mean    sd median    Q1    Q3     n
# 1 R           29.0  16.5   33.3  15.6  35.5    13
# 2 S           42.5  27.5   34    22.5  64.5    19

wilcox.test(TOTAL ~ Traitement, data=ignore) 

# Wilcoxon rank sum test with continuity correction
# 
# data:  TOTAL by Traitement
# W = 96, p-value = 0.3001
# alternative hypothesis: true location shift is not equal to 0

boxplot <- ignore %>%
  mutate(Traitement=ifelse(Traitement=="R", "2- [R] Risdiplam",
                           ifelse(Traitement=="S", "1- [S] Nusinersen", "3- [R+S] Risdiplam+Nusinersen"))) %>%
  rename("Treatment"="Traitement") %>%
  ggplot(aes(Treatment, TOTAL, colour=Treatment, fill=Treatment)) +
  geom_boxplot(alpha=0.5, notch = F,outlier.colour = "white",
               outlier.color = "white",
               outlier.fill = "white") +
  geom_jitter(size=4, alpha=0.7, width = 0.3) +
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        legend.position = "right",
        axis.text.x = element_text(angle = 45, hjust = 1)) +
  theme(panel.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.background = element_blank(),
        strip.text = element_blank(),
        axis.line = element_blank(),
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10),
        axis.title.x = element_text(size = 12, vjust = -0.5),
        axis.title.y = element_text(size = 12, vjust = -0.5),
        plot.margin = margin(5, 5, 5, 5, "pt")) +
  xlab("") + ylab("Total MFM [%] \n") +
  scale_fill_manual(values=c("#558EA9", "#A95592")) +
  scale_colour_manual(values=c("#558EA9", "#A95592")) 

ggsave(file="boxplot_2.svg", plot=boxplot, width=6, height=6)

# --------------------

# Correlations --------------------------------------


cor.test(corentin_inputs$Delta_MFM, corentin_inputs$Delta_Satisfaction, method = c( "pearson"))

# Pearson's product-moment correlation
# 
# data:  corentin_inputs$Delta_MFM and corentin_inputs$Delta_Satisfaction
# t = 0.86241, df = 29, p-value = 0.3955
# alternative hypothesis: true correlation is not equal to 0
# 95 percent confidence interval:
#  -0.2078566  0.4852791
# sample estimates:
#       cor 
# 0.1581301 



corplot <- corentin_inputs %>%
  ggplot(aes(Delta_Satisfaction, Delta_MFM)) +
  geom_smooth(method="lm", se=T, colour="firebrick", fill="firebrick", alpha=0.3) +
  geom_jitter(size=4, alpha=0.7,   fill="#271E74", colour="#271E74") +
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        legend.position = "right",
        axis.text.x = element_text(angle = 45, hjust = 1)) +
  theme(panel.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.background = element_blank(),
        strip.text = element_blank(),
        axis.line = element_blank(),
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10),
        axis.title.x = element_text(size = 12, vjust = -0.5),
        axis.title.y = element_text(size = 12, vjust = -0.5),
        plot.margin = margin(5, 5, 5, 5, "pt")) +
  xlab("\n Δ Satisfaction ") + ylab("Δ MFM \n") 

ggsave(file="cormfm.svg", plot=corplot, width=5, height=5)



cor.test(corentin_inputs$Delta_performance, corentin_inputs$Delta_Satisfaction, method = c( "pearson"))

# Pearson's product-moment correlation
# 
# data:  corentin_inputs$Delta_performance and corentin_inputs$Delta_Satisfaction
# t = 2.7008, df = 29, p-value = 0.01143
# alternative hypothesis: true correlation is not equal to 0
# 95 percent confidence interval:
#  0.1117172 0.6926242
# sample estimates:
#       cor 
# 0.4483105 



corplot <- corentin_inputs %>%
  ggplot(aes(Delta_Satisfaction, Delta_performance)) +
  geom_smooth(method="lm", se=T, colour="firebrick", fill="firebrick", alpha=0.3) +
  geom_jitter(size=4, alpha=0.7,   fill="#271E74", colour="#271E74") +
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        legend.position = "right",
        axis.text.x = element_text(angle = 45, hjust = 1)) +
  theme(panel.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.background = element_blank(),
        strip.text = element_blank(),
        axis.line = element_blank(),
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10),
        axis.title.x = element_text(size = 12, vjust = -0.5),
        axis.title.y = element_text(size = 12, vjust = -0.5),
        plot.margin = margin(5, 5, 5, 5, "pt")) +
  xlab("\n Δ Satisfaction ") + ylab("Δ Performance \n") 

ggsave(file="corperf.svg", plot=corplot, width=5, height=5)


cor.test(corentin_inputs$GAS, corentin_inputs$Delta_Satisfaction, method = c( "pearson"))

# Pearson's product-moment correlation
# 
# data:  corentin_inputs$GAS and corentin_inputs$Delta_Satisfaction
# t = 3.285, df = 29, p-value = 0.002668
# alternative hypothesis: true correlation is not equal to 0
# 95 percent confidence interval:
#  0.2040860 0.7387805
# sample estimates:
#       cor 
# 0.5207665 

corplot <- corentin_inputs %>%
  ggplot(aes(Delta_Satisfaction, GAS)) +
  geom_smooth(method="lm", se=T, colour="firebrick", fill="firebrick", alpha=0.3) +
  geom_jitter(size=4, alpha=0.7,   fill="#271E74", colour="#271E74") +
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        legend.position = "right",
        axis.text.x = element_text(angle = 45, hjust = 1)) +
  theme(panel.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.background = element_blank(),
        strip.text = element_blank(),
        axis.line = element_blank(),
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10),
        axis.title.x = element_text(size = 12, vjust = -0.5),
        axis.title.y = element_text(size = 12, vjust = -0.5),
        plot.margin = margin(5, 5, 5, 5, "pt")) +
  xlab("\n Δ Satisfaction ") + ylab("GAS \n") 

ggsave(file="corgas.svg", plot=corplot, width=5, height=5)

# ------------------------------------
