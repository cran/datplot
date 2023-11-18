## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE, fig.width = 7,
  comment = "#>"
)

## ----message = FALSE----------------------------------------------------------
library(dplyr)
library(stringr)
library(forcats)
library(ggplot2)
library(knitr)
library(datplot)
library(ggridges)
library(reshape2)

## -----------------------------------------------------------------------------
inscriptions <- read.csv(system.file("extdata",
                                     "Bithynia_Inscriptions_ascii.csv",
                                     package = "datplot"))
summary(inscriptions)

## -----------------------------------------------------------------------------
inscriptions$ID <- paste("I_", seq_len(nrow(inscriptions)), sep = "")

## -----------------------------------------------------------------------------
unique(inscriptions$Location)
unique(inscriptions$Language)

## ----message=FALSE------------------------------------------------------------
inscriptions <- inscriptions %>%
  rename(Dating = Chronological.Frame) %>%
  mutate(Dating = na_if(Dating, "---"),
         Language = replace(Language, Language == "Gr/Lat", "Greek/Latin"),
         Language = replace(Language, Language == "Gr / Lat", "Greek/Latin"),
         Language = factor(Language, levels = c("Greek", "Latin",
                                                "Greek/Latin")),
         Location = replace(Location, str_detect(Location, "unknown"),
                            "unknown"),
         Location = replace(Location,
                            Location == "Prusias ad Mare (Keramed)",
                            "Prusias ad Mare"),
         Location = factor(Location))

## -----------------------------------------------------------------------------
summary(inscriptions)

## -----------------------------------------------------------------------------
inscriptions$uncertain_dating <- FALSE
sel <- grep("\\?", inscriptions$Dating)
inscriptions$uncertain_dating[sel] <- TRUE
inscriptions$Dating <- gsub("\\?", "", inscriptions$Dating)

## -----------------------------------------------------------------------------
sel <- grepl("[0-9]", inscriptions$Dating)
periods <- data.frame("Dating" = unique(inscriptions$Dating[which(sel == FALSE)]))
periods$DAT_min <- NA
periods$DAT_max <- NA
#write.csv(periods, file = "../data-raw/periods.csv", fileEncoding = "UTF-8")
# .... Manual editing of the resulting table, saving it as "periods_edit.csv".
join_dating <- read.csv(file = system.file('extdata', 'periods_edit.csv',
                                           package = 'datplot',
                                           mustWork = TRUE),
                        row.names = 1,
                        colClasses = c("character", "character",
                                       "integer", "integer"),
                        encoding = "UTF-8")

## -----------------------------------------------------------------------------
num_dating <- data.frame("Dating" = unique(inscriptions$Dating[which(sel == TRUE)]))
num_dating$DAT_min <- NA
num_dating$DAT_max <- NA

## -----------------------------------------------------------------------------
sel <- grep("^[0-9]{1,3} AD$", num_dating$Dating)
num_dating$DAT_min[sel] <- gsub(" AD", "", num_dating$Dating[sel]) 
num_dating$DAT_max[sel] <- gsub(" AD", "", num_dating$Dating[sel]) 
sel <- grep("^[0-9]{1,3} BC$", num_dating$Dating)
num_dating$DAT_min[sel] <- paste("-", gsub(" BC", "", num_dating$Dating[sel]), 
                                 sep = "")
num_dating$DAT_max[sel] <- paste("-", gsub(" BC", "", num_dating$Dating[sel]), 
                                 sep = "")

## ----echo = FALSE-------------------------------------------------------------
require(knitr)
knitr::kable(na.omit(na.omit(num_dating)[sample(seq_len(nrow(na.omit(num_dating))), 
                                                10), ]))

## -----------------------------------------------------------------------------
join_dating <- rbind(join_dating, num_dating[!is.na(num_dating$DAT_min), ])
num_dating <- num_dating[which(is.na(num_dating$DAT_min)), ]

## -----------------------------------------------------------------------------
num_dating$Dating <- as.character(num_dating$Dating)

## -----------------------------------------------------------------------------
# Values like: 92-120 AD
sel <- grep("^[0-9]{1,3}-[0-9]{1,3} AD", num_dating$Dating)
for (r in sel) {
  split <- strsplit(x = num_dating$Dating[r], split = "-| ")
  num_dating$DAT_min[r] <- split[[1]][1]
  num_dating$DAT_max[r] <- split[[1]][2]
}
# Values like: AD 92-120
sel <- grep("^AD [0-9]{1,3}-[0-9]{1,3}$", num_dating$Dating)
for (r in sel) {
  split <- strsplit(x = num_dating$Dating[r], split = "-| ")
  num_dating$DAT_min[r] <- split[[1]][2]
  num_dating$DAT_max[r] <- split[[1]][3]
}
# Values like: AD 92 - 120
sel <- grep("^AD [0-9]{1,3} - [0-9]{1,3}", num_dating$Dating)
for (r in sel) {
  split <- strsplit(x = num_dating$Dating[r], split = " - | ")
  num_dating$DAT_min[r] <- split[[1]][2]
  num_dating$DAT_max[r] <- split[[1]][3]
}
# Values like: 198/199 AD
sel <- grep("^[0-9]{1,3}/[0-9]{1,3} AD", num_dating$Dating)
for (r in sel) {
  split <- strsplit(x = num_dating$Dating[r], split = "/| ")
  num_dating$DAT_min[r] <- split[[1]][1]
  num_dating$DAT_max[r] <- split[[1]][2]
}
# Values like: 525-75 BC
sel <- grep("^[0-9]{1,3}-[0-9]{1,3} BC", num_dating$Dating)
for (r in sel) {
  split <- strsplit(x = num_dating$Dating[r], split = "-| ")
  num_dating$DAT_min[r] <- 0 - as.numeric(split[[1]][1])
  num_dating$DAT_max[r] <- 0 - as.numeric(split[[1]][2])
}

## ----echo = FALSE-------------------------------------------------------------
knitr::kable(na.omit(na.omit(num_dating)[sample(seq_len(nrow(na.omit(num_dating))),
                                                10), ]))

## -----------------------------------------------------------------------------
join_dating <- rbind(join_dating, num_dating[!is.na(num_dating$DAT_min), ])
num_dating <- num_dating[which(is.na(num_dating$DAT_min)), ]

## -----------------------------------------------------------------------------
sel <- grep("^[0-9]{1}[a-z]{2} c\\. AD$", num_dating$Dating)
for (r in sel) {
  split <- strsplit(x = num_dating$Dating[r], split = "[a-z]{2} c\\.")
  split <- as.numeric(split[[1]][1])
  num_dating$DAT_min[r] <- ((split - 1) * 100)
  num_dating$DAT_max[r] <- ((split - 1) * 100) + 99
}

sel <- grep("^[0-9]{1}[a-z]{2} c\\. BC$", num_dating$Dating)
for (r in sel) {
  split <- strsplit(x = num_dating$Dating[r], split = "[a-z]{2} c\\.")
  split <- as.numeric(split[[1]][1])
  num_dating$DAT_min[r] <- 0-(split * 100) + 1
  num_dating$DAT_max[r] <- 0-((split - 1) * 100)
}


## ----echo = FALSE-------------------------------------------------------------
knitr::kable(na.omit(na.omit(num_dating)[sample(seq_len(nrow(na.omit(num_dating))), 
                                                10), ]))

## -----------------------------------------------------------------------------
join_dating <- rbind(join_dating, num_dating[!is.na(num_dating$DAT_min), ])
num_dating <- num_dating[which(is.na(num_dating$DAT_min)), ]

## -----------------------------------------------------------------------------
sel <- grep("^ca\\. [0-9]{1,3} AD$", num_dating$Dating)
for (r in sel) {
  split <- strsplit(x = num_dating$Dating[r], split = " ")
  split <- as.numeric(split[[1]][2])
  num_dating$DAT_min[r] <- split - 10
  num_dating$DAT_max[r] <- split + 10
}
sel <- grep("^ca\\. [0-9]{1,3} BC$", num_dating$Dating)
for (r in sel) {
  split <- strsplit(x = num_dating$Dating[r], split = " ")
  split <- 0-as.numeric(split[[1]][2])
  num_dating$DAT_min[r] <- split - 10
  num_dating$DAT_max[r] <- split + 10
}

## ----echo = FALSE-------------------------------------------------------------
knitr::kable(na.omit(na.omit(num_dating)))

## -----------------------------------------------------------------------------
join_dating <- rbind(join_dating, num_dating[!is.na(num_dating$DAT_min), ])
num_dating <- num_dating[which(is.na(num_dating$DAT_min)), ]
unique(num_dating$Dating)[1:20]

## -----------------------------------------------------------------------------
join_dating$DAT_min[which(join_dating$DAT_min == 0)] <- 1
join_dating$DAT_max[which(join_dating$DAT_max == 0)] <- -1

## -----------------------------------------------------------------------------
#write.csv(num_dating, file = "../data-raw/num_dating.csv", 
#          fileEncoding = "UTF-8")
num_dating <- read.csv(file = system.file('extdata', 'num_dating_edit.csv',
                                          package = 'datplot', mustWork = TRUE), 
                       encoding = "UTF-8",
                       row.names = 1,
                       colClasses = c("character", "character",
                                      "integer", "integer"))
join_dating <- join_dating %>%
  mutate(DAT_min = as.integer(DAT_min),
         DAT_max = as.integer(DAT_max)) %>%
  rbind(num_dating)

## -----------------------------------------------------------------------------
inscriptions <- left_join(inscriptions, join_dating, by = "Dating")

## ----echo = FALSE-------------------------------------------------------------
knitr::kable(na.omit(inscriptions)[sample(nrow(na.omit(inscriptions)), 15),
                                   c(6, 2, 4, 8, 9)])

## ----warning = TRUE, message = TRUE, out.lines = 20---------------------------
inscr_steps <- inscriptions %>%
  select(ID, Location, DAT_min, DAT_max) %>%
  na.omit() %>%
  as.data.frame() %>%
  datplot::datsteps(stepsize = 5)

## -----------------------------------------------------------------------------
inscriptions %>%
  select(ID, Location, Dating, uncertain_dating, DAT_min, DAT_max) %>%
  na.omit() %>%
  slice(637, 1458) %>%
  kable()

## -----------------------------------------------------------------------------
inscriptions[which(inscriptions$ID == "I_1162"),"DAT_max"] <- 63

## -----------------------------------------------------------------------------
inscriptions[which(inscriptions$ID == "I_2725"),c("DAT_min", "DAT_max")] <-
  inscriptions[which(inscriptions$ID == "I_2725"),c("DAT_max", "DAT_min")]

## ----eval = FALSE-------------------------------------------------------------
#  #write.table(inscriptions, file = "../data-raw/inscriptions.csv",
#  #            fileEncoding = "UTF-8", sep = ";", row.names = FALSE)

## -----------------------------------------------------------------------------
#library(datplot)
data("Inscr_Bithynia")

## -----------------------------------------------------------------------------
inscr_clean <- Inscr_Bithynia %>%
  filter(Dating != "NA",
         Location != "unknown") %>%
  droplevels()

## -----------------------------------------------------------------------------
summary(inscr_clean)

## ----eval = FALSE-------------------------------------------------------------
#  library(datplot)

## ----warning=TRUE, out.lines = 13---------------------------------------------
inscr_steps <- inscr_clean %>%
  select(ID, Location, DAT_min, DAT_max) %>%
  as.data.frame() %>%
  datplot::datsteps(stepsize = 25) %>%
  datplot::scaleweight(var = 2)

## -----------------------------------------------------------------------------
plot(density(inscr_steps$DAT_step))

## -----------------------------------------------------------------------------
ggplot(data = inscr_steps, aes(x = DAT_step, fill = variable,
                               weight = weight)) +
  geom_density(alpha = 0.3)

## -----------------------------------------------------------------------------
ggplot(data = inscr_steps, aes(x = DAT_step, fill = variable)) +
  geom_density(alpha = 0.3)

## -----------------------------------------------------------------------------
ggplot(data = inscr_steps,
       aes(x = DAT_step,
           y = fct_rev(as_factor(variable)),
           fill = variable,
           weight = weight)) +
  geom_density_ridges(aes(height = after_stat(density)),
                      stat = "density", alpha = 0.9) +
  scale_fill_discrete(guide = FALSE)

## -----------------------------------------------------------------------------
bluegreen <- colorRampPalette(c("#8dae25", "#17365c"))

ggplot(data = inscr_steps,
       aes(x = DAT_step,
           y = fct_rev(as_factor(variable)),
           fill = variable,
           weight = weight)) +
  geom_density_ridges(aes(height = after_stat(density)), stat = "density", alpha = 0.9) +
  scale_x_continuous(breaks = seq(from = -800, to = 800, by = 100),
                     limits = c(-800,800), name = "") +
  geom_vline(xintercept = 0, alpha = 0.5, lwd = 1) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5),
        panel.background = element_blank(),
        panel.grid.major.x = element_line(linetype = "dashed",
                                          color = "gray30"),
        panel.grid.minor.x = element_line(linetype = "dotted",
                                          color = "gray80")) +
  scale_fill_manual(guide=FALSE,
                    values = bluegreen(length(unique(inscr_steps$variable)))) +
  labs(title = "Epigraphic Evidence from Bithynia",
       subtitle = "Spatio-temporal distribution",
       y = "administrative centres",
       caption = attributes(inscriptions)$source)

## ----fig.height = 10----------------------------------------------------------
ggplot(data = inscr_steps, aes(x = DAT_step, 
                               fill = variable, weight = weight)) +
  geom_density(alpha = 0.9) +
  scale_fill_discrete(guide = FALSE) +
  facet_wrap(variable ~ ., ncol = 1)

## ----fig.height = 10----------------------------------------------------------
ggplot(data = inscr_steps, aes(x = DAT_step,
                               fill = variable, weight = weight)) +
  geom_density(alpha = 0.9) +
  theme(panel.background = element_blank()) +
  scale_fill_manual(guide=FALSE,
                    values = bluegreen(length(unique(inscr_steps$variable)))) +
  scale_x_continuous(breaks = seq(from = -800, to = 800, by = 100),
                     limits = c(-800,800), name = "") +
  facet_wrap(variable ~ ., ncol = 1) +
  theme(strip.text.x = element_text(size=8),
        strip.background = element_blank(),
        axis.text.x = element_text(angle = 90, vjust = 0.5),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.title.y = element_blank(),
        panel.background = element_blank(),
        panel.grid.major.x = element_line(linetype = "dashed",
                                          color = "gray30"),
        panel.grid.minor.x = element_line(linetype = "dotted",
                                          color = "gray80")) +
  labs(title = "Epigraphic Evidence from Bithynia",
       subtitle = "Spatio-temporal distribution",
       caption = attributes(inscriptions)$source)

## ----fig.height = 6-----------------------------------------------------------
inscr_clean %>% 
  select(ID, Location, DAT_min, DAT_max) %>%
  filter(Location %in% c("Prusias ad Hypium", "Nicomedia", "Apamea")) %>%
  reshape2::melt(id.vars = c("ID", "Location")) %>%
  ggplot(aes(x = value, fill = variable)) +
  geom_histogram(binwidth = 25, position = "dodge") +
  facet_wrap(. ~ Location, ncol = 1) +
  labs(title = "Distribution of Dated Inscriptions in Bithynia (selection)",
       x = "Dating", y = "Number of Inscriptions") +
  theme(legend.position = "top")

## ----fig.height = 6-----------------------------------------------------------
inscr_clean %>% 
  transmute(ID, Location, Language, DAT_mean = ((DAT_min + DAT_max)/2)) %>%
  filter(Location %in% c("Prusias ad Hypium", "Nicomedia", "Apamea")) %>%
  reshape2::melt(id.vars = c("ID", "Location", "Language")) %>%
  ggplot(aes(x = value, fill = Language)) +
  geom_histogram(binwidth = 25) +
  facet_wrap(. ~ Location, ncol = 1) +
  labs(title = "Distribution of Dated Inscriptions in Bithynia (selection)",
       x = "Dating", y = "Number of Inscriptions")

