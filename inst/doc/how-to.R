## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  fig.width = 7,
  fig.align = "center"
)

library(knitr)
library(ggplot2)

Plot_Theme <- theme(panel.background = element_blank(), 
        panel.grid.major = element_line(color = "grey60", linetype = "dashed"),
        panel.grid.minor = element_line(color = "grey80", linetype = "dashed"),
        legend.position = c(0.9, 0.85),
        legend.background = element_rect(fill = "white", color = "grey60"))
Plot_Fill <- scale_fill_manual(name = "Technique", values = c("gray30",
                                                              "tomato3"))

## ----prep---------------------------------------------------------------------
library(datplot)
data(Beazley)

## ----preptable, echo = FALSE--------------------------------------------------
knitr::kable(Beazley[sample(seq_len(nrow(Beazley)), 10, replace = FALSE), ])

## ----barplot------------------------------------------------------------------
Beazley$DAT_mean <- (Beazley$DAT_max + Beazley$DAT_min) / 2
library(ggplot2)
ggplot(Beazley, aes(x = DAT_mean, fill = Technique)) +
  geom_histogram(binwidth = 25, position = "dodge") + Plot_Theme + Plot_Fill

## ----warning = FALSE----------------------------------------------------------
system.time(result <- datsteps(Beazley, stepsize = 25))[3]
system.time(result <- datsteps(Beazley, stepsize = 1))[3]

## ----steps1-------------------------------------------------------------------
library(datplot)
result <- datsteps(Beazley, stepsize = 25)
ggplot(result, aes(x = DAT_step, fill = variable)) +
  geom_histogram(binwidth = 25, position = "dodge") + Plot_Theme + Plot_Fill

## ----steps2-------------------------------------------------------------------
result <- datsteps(Beazley, stepsize = "auto")
ggplot(result, aes(x = DAT_step, fill = variable)) + Plot_Theme + Plot_Fill +
  geom_histogram(binwidth = attributes(result)$stepsize, position = "dodge")

## ----stepstable, echo = FALSE-------------------------------------------------
knitr::kable(head(result))

## ----density one--------------------------------------------------------------
result <- datsteps(Beazley, stepsize = 25)
dens <- result
dens <- scaleweight(result, var = "all")
dens <- density(x = dens$DAT_step, weights = dens$weight)
plot(dens)

## ----scaleweight--------------------------------------------------------------
result <- scaleweight(result, var = 2)

## ----scaleweighttable, echo = FALSE-------------------------------------------
knitr::kable(head(result))

## ----ggplot, warning=FALSE----------------------------------------------------
ggplot(data = result, aes(x = DAT_step,
                          fill = variable,
                          weight = weight)) +
  geom_density(alpha = 0.5) +
  xlab("Dating") + Plot_Theme + Plot_Fill

## ----ggplot without weight, warning=FALSE-------------------------------------
ggplot(data = result, aes(x = DAT_step,
                          fill = variable)) +
  geom_density(alpha = 0.5) +
  xlab("Dating") + Plot_Theme + Plot_Fill

## ---- warning = FALSE---------------------------------------------------------
data("Inscr_Bithynia")
Inscr_Bithynia <- na.omit(Inscr_Bithynia[, c(1, 3, 8, 9)])
result_bith <- scaleweight(datsteps(Inscr_Bithynia, stepsize = "auto"),
                           var = "all")

ggplot(result_bith, aes(x = DAT_step)) + Plot_Theme + Plot_Fill +
  geom_density(alpha = 0.5, fill = "grey30") + xlab("Dating")

ggplot(result_bith, aes(x = DAT_step, weight = weight)) +
  Plot_Theme + Plot_Fill +
  geom_density(alpha = 0.5, fill = "grey30") + xlab("Dating")

## ----histogramscale, warning = FALSE, message = FALSE-------------------------
histogramscale <- get.histogramscale(result)

## ----ggplot-combination-------------------------------------------------------
ggplot(result, aes(x = DAT_step, fill = variable)) + Plot_Theme + Plot_Fill +
  stat_density(alpha = 0.5, position = "dodge",
               aes(y = (after_stat(density) * histogramscale), weight = weight)) +
  geom_histogram(alpha = 0.5, binwidth = attributes(result)$stepsize,
                 position = "dodge") +
  labs(y = "maximum number of objects per year", x = "Dating")

## ----cumulative demo, fig.height = 10-----------------------------------------
data("Inscr_Bithynia")
Inscr_Bithynia <- na.omit(Inscr_Bithynia[, c(1, 3, 8, 9)])
Inscr_Bithynia <- Inscr_Bithynia[sample(seq_len(nrow(Inscr_Bithynia)), 5), ]
Inscr_Bithynia_steps <- datsteps(Inscr_Bithynia, 
                                 stepsize = 1, 
                                 calc = "probability", 
                                 cumulative = TRUE)

ggplot(Inscr_Bithynia_steps, aes(x = DAT_step, y = cumul_prob, fill = variable)) + 
  geom_col() + facet_wrap(. ~ ID, ncol = 1) +
  labs(y = "Cumulative Probability", x = "Dating", fill = "Origin") + 
  theme(legend.position = "bottom")

