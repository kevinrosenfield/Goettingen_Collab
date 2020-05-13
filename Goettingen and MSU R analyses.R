# load packages

library(ggplot2); library(tidyr); library(reshape2); library(lmerTest); library(dplyr); library(plotrix); library(effects);library(readxl); library(texreg); library(ICC); library(sjmisc)

# load data now

msu <- read.csv("msu.csv")
c <- c(1:13,15:17)
msu[c] <- lapply(msu[c],as.factor)
msu$Estradiol <- msu$Estrogen

msu_r <- read.csv("msu_r.csv")
c <- c(1:12, 14:16, 19:20)
msu_r[c] <- lapply(msu_r[c],as.factor)
msu_r$Estradiol <- msu_r$Estrogen
msu_r$attr <- rowMeans(msu_r[c("st_attr", "lt_attr")], na.rm =TRUE)

got <- read.csv("got.csv")
c <- c(1:9,11,13:18)
got[c] <- lapply(got[c],as.factor)

got_r <- read_excel("got_r.xlsx")
c <- c(1:5,7:9,11:16,18,20:25)
got_r[c] <- lapply(got_r[c],as.factor)
got_r$rater_ID <- got_r$Male_ID
got_r$attr <- got_r$Rating
got_r$Rating_num <- 1:20

# transformations / data manipulations

f1 <- function(x) {subset(x, type == "rainbow_full" & Progesterone < 500 & Estradiol < 31)}
msu_r <- f1(msu_r)
got_r <- f1(got_r)
msu <- f1(msu)
got <- f1(got)


## transformations

msu <- msu %>%
  mutate(Est_log = log(Estradiol),
         Prog_log = log(Progesterone),
         EP = Estradiol / Progesterone,
         EP_log = log(EP),
         EP_log.gmc = EP_log-mean(EP_log, na.rm = TRUE),
         Prog_log.gmc = Prog_log-mean(Prog_log, na.rm = TRUE),
         Est_log.gmc = Est_log-mean(Est_log, na.rm = TRUE),
         EP.gmc = EP-mean(EP, na.rm = TRUE),
         Prog.gmc = Progesterone-mean(Progesterone, na.rm = TRUE),
         Est.gmc = Estradiol-mean(Estradiol, na.rm = TRUE),
         meanPitch.gmc = meanPitch - mean(meanPitch)) %>%
    group_by(subID) %>%
  mutate(EP_log.cm = mean(EP_log, na.rm = TRUE),
         Prog_log.cm = mean(Prog_log, na.rm = TRUE),
         Est_log.cm = mean(Est_log, na.rm = TRUE),
         EP.cm = mean(EP, na.rm = TRUE),
         Prog.cm = mean(Progesterone, na.rm = TRUE),
         Est.cm = mean(Estradiol, na.rm = TRUE),
         EP_log.cwc = EP_log-EP_log.cm,
         Prog_log.cwc = Prog_log-Prog_log.cm,
         Est_log.cwc = Est_log-Est_log.cm,
         EP.cwc = EP-EP.cm,
         Prog.cwc = Progesterone-Prog.cm,
         Est.cwc = Estradiol-Est.cm) %>%
  ungroup %>%
  mutate(EP_log.cmc = EP_log.cm-mean(EP_log.cm, na.rm = TRUE),
         Prog_log.cmc = Prog_log.cm-mean(Prog_log.cm, na.rm = TRUE),
         Est_log.cmc = Est_log.cm-mean(Est_log.cm, na.rm = TRUE),
         EP.cmc = EP.cm-mean(EP.cm, na.rm = TRUE),
         Prog.cmc = Prog.cm-mean(Prog.cm, na.rm = TRUE),
         Est.cmc = Est.cm-mean(Est.cm, na.rm = TRUE))

msu_r <- msu_r %>%
  mutate(Est_log = log(Estradiol),
         Prog_log = log(Progesterone),
         EP = Estradiol / Progesterone,
         EP_log = log(EP),
         EP_log.gmc = EP_log-mean(EP_log, na.rm = TRUE),
         Prog_log.gmc = Prog_log-mean(Prog_log, na.rm = TRUE),
         Est_log.gmc = Est_log-mean(Est_log, na.rm = TRUE),
         EP.gmc = EP-mean(EP, na.rm = TRUE),
         Prog.gmc = Progesterone-mean(Progesterone, na.rm = TRUE),
         Est.gmc = Estradiol-mean(Estradiol, na.rm = TRUE)) %>%
  group_by(subID) %>%
  mutate(EP_log.cm = mean(EP_log, na.rm = TRUE),
         Prog_log.cm = mean(Prog_log, na.rm = TRUE),
         Est_log.cm = mean(Est_log, na.rm = TRUE),
         EP.cm = mean(EP, na.rm = TRUE),
         Prog.cm = mean(Progesterone, na.rm = TRUE),
         Est.cm = mean(Estradiol, na.rm = TRUE),
         meanPitch.cm = mean(meanPitch, na.rm = TRUE),
         minPitch.cm = mean(minPitch, na.rm = TRUE),
         attr.cm = mean(attr, na.rm = TRUE),
         EP_log.cwc = EP_log-EP_log.cm,
         Prog_log.cwc = Prog_log-Prog_log.cm,
         Est_log.cwc = Est_log-Est_log.cm,
         EP.cwc = EP-EP.cm,
         Prog.cwc = Progesterone-Prog.cm,
         Est.cwc = Estradiol-Est.cm,
         meanPitch.cwc = meanPitch-meanPitch.cm,
         minPitch.cwc = minPitch-minPitch.cm) %>%
  ungroup %>%
  mutate(EP_log.cmc = EP_log.cm-mean(EP_log.cm, na.rm = TRUE),
         Prog_log.cmc = Prog_log.cm-mean(Prog_log.cm, na.rm = TRUE),
         Est_log.cmc = Est_log.cm-mean(Est_log.cm, na.rm = TRUE),
         EP.cmc = EP.cm-mean(EP.cm, na.rm = TRUE),
         Prog.cmc = Prog.cm-mean(Prog.cm, na.rm = TRUE),
         Est.cmc = Est.cm-mean(Est.cm, na.rm = TRUE))

msu_r %>% select(Estradiol, Est.gmc, Est.cwc, Est.cm, Est.cmc) %>%
  summary

got <- got %>%
  group_by(subID) %>%
  mutate(Prog.rank = (order(Progesterone)))

got <- got %>%
  group_by(subID) %>%
  mutate(EP.rank = (order(EP)))

plot(got$meanPitch, got$Prog.rank)


################## ratings must be calculated using by group_by sub_sess, piped into non-ratings sheet, then subject-means for hormones, attr, and acoustics variables need to be piped into ratings df; subject-mean centering also should take place in non-ratings sheet for non-ratings variables, and by sub_sess for ratings, rather than subID

got <- got %>%
  mutate(Est_log = log(Estradiol),
         Prog_log = log(Progesterone),
         EP = Estradiol / Progesterone,
         EP_log = log(EP),
         EP_log.gmc = EP_log-mean(EP_log, na.rm = TRUE),
         Prog_log.gmc = Prog_log-mean(Prog_log, na.rm = TRUE),
         Est_log.gmc = Est_log-mean(Est_log, na.rm = TRUE),
         EP.gmc = EP-mean(EP, na.rm = TRUE),
         Prog.gmc = Progesterone-mean(Progesterone, na.rm = TRUE),
         Est.gmc = Estradiol-mean(Estradiol, na.rm = TRUE)) %>%
  group_by(subID) %>%
  mutate(EP_log.cm = mean(EP_log, na.rm = TRUE),
         Prog_log.cm = mean(Prog_log, na.rm = TRUE),
         Est_log.cm = mean(Est_log, na.rm = TRUE),
         EP.cm = mean(EP, na.rm = TRUE),
         Prog.cm = mean(Progesterone, na.rm = TRUE),
         Est.cm = mean(Estradiol, na.rm = TRUE),
         EP_log.cwc = EP_log-EP_log.cm,
         Prog_log.cwc = Prog_log-Prog_log.cm,
         Est_log.cwc = Est_log-Est_log.cm,
         EP.cwc = EP-EP.cm,
         Prog.cwc = Progesterone-Prog.cm,
         Est.cwc = Estradiol-Est.cm) %>%
  ungroup %>%
  mutate(EP_log.cmc = EP_log.cm-mean(EP_log.cm, na.rm = TRUE),
         Prog_log.cmc = Prog_log.cm-mean(Prog_log.cm, na.rm = TRUE),
         Est_log.cmc = Est_log.cm-mean(Est_log.cm, na.rm = TRUE),
         EP.cmc = EP.cm-mean(EP.cm, na.rm = TRUE),
         Prog.cmc = Prog.cm-mean(Prog.cm, na.rm = TRUE),
         Est.cmc = Est.cm-mean(Est.cm, na.rm = TRUE))

got_r <- got_r %>%
  mutate(Est_log = log(Estradiol),
         Prog_log = log(Progesterone),
         EP = Estradiol / Progesterone,
         EP_log = log(EP),
         EP_log.gmc = EP_log-mean(EP_log, na.rm = TRUE),
         Prog_log.gmc = Prog_log-mean(Prog_log, na.rm = TRUE),
         Est_log.gmc = Est_log-mean(Est_log, na.rm = TRUE),
         EP.gmc = EP-mean(EP, na.rm = TRUE),
         Prog.gmc = Progesterone-mean(Progesterone, na.rm = TRUE),
         Est.gmc = Estradiol-mean(Estradiol, na.rm = TRUE),
         meanPitch.gmc = meanPitch - mean(meanPitch),
         minPitch.gmc = minPitch - mean(minPitch)) %>%
  group_by(subID) %>%
  mutate(EP_log.cm = mean(EP_log, na.rm = TRUE),
         Prog_log.cm = mean(Prog_log, na.rm = TRUE),
         Est_log.cm = mean(Est_log, na.rm = TRUE),
         EP.cm = mean(EP, na.rm = TRUE),
         Prog.cm = mean(Progesterone, na.rm = TRUE),
         Est.cm = mean(Estradiol, na.rm = TRUE),
         meanPitch.cm = mean(meanPitch, na.rm = TRUE),
         minPitch.cm = mean(minPitch, na.rm = TRUE),
         EP_log.cwc = EP_log-EP_log.cm,
         Prog_log.cwc = Prog_log-Prog_log.cm,
         Est_log.cwc = Est_log-Est_log.cm,
         EP.cwc = EP-EP.cm,
         Prog.cwc = Progesterone-Prog.cm,
         Est.cwc = Estradiol-Est.cm,
         meanPitch.cwc = meanPitch-meanPitch.cm,
         minPitch.cwc = minPitch-minPitch.cm) %>%
  ungroup %>%
  mutate(EP_log.cmc = EP_log.cm-mean(EP_log.cm, na.rm = TRUE),
         Prog_log.cmc = Prog_log.cm-mean(Prog_log.cm, na.rm = TRUE),
         Est_log.cmc = Est_log.cm-mean(Est_log.cm, na.rm = TRUE),
         EP.cmc = EP.cm-mean(EP.cm, na.rm = TRUE),
         Prog.cmc = Prog.cm-mean(Prog.cm, na.rm = TRUE),
         Est.cmc = Est.cm-mean(Est.cm, na.rm = TRUE))

got_r %>% select(EP, EP.gmc, EP.cm, EP.cmc, EP.cmc) %>%
  summary

# histograms

v <- c("Estradiol", "Progesterone", "EP", "Est_log", "Prog_log", "EP_log")

par(mfrow=c(4,3),mar=c(5,6,4,1)+.1)

lapply(got[v], FUN = hist)
lapply(msu[v], FUN = hist)

## do hormones predict acoustic variables?

# correlation table
f = function(x, y) {round(cor(subset(x[y], Session = "a"), use = "complete.obs"), 2)}
v = c("meanPitch_z", "jitter_z", "harmonics_z", "Progesterone", "Estradiol", "EP")
f(msu, v)
f(got, v)

#htmlreg(list(msu_p_rawEP, msu_p_gmcEP, msu_p_cwcEP, msu_p_cmcEP), 
#        single.row = T,
#        file = "msu_table.html",
#        stars = numeric(0),
#        caption = "")

var_reduction = function(m0, m1){
  library(tidyverse)
  VarCorr(m0) %>% 
    as.data.frame %>% 
    select(grp, var_m0 = vcov) %>% 
    left_join(VarCorr(m1) %>% 
                as.data.frame %>% 
                select(grp, var_m1 = vcov)) %>% 
    mutate(var_red = 1 - var_m1 / var_m0) 
}

# Create comparison table
cbind(M1 = var_reduction(msu_p0, msu_p_rawEP)[,4],
      M2 = var_reduction(msu_p0, msu_p_gmcEP)[,4],
      M3 = var_reduction(msu_p0, msu_p_cwcEP)[,4],
      M4 = var_reduction(msu_p0, msu_p_cmcEP)[,4]) %>%
  round(2)


## Attractiveness and hormones; main analyses

# gottingen; attr ~ E*P

g_attr_E_P_null <- lmer(attr ~ 1 + (1 | subID) + (1 | rater_ID), data = got_r); summary(g_attr_E_P_null)

g_attr_E_P_raw <- lmer(attr ~ Estradiol + Progesterone + (1 | subID) + (1 | rater_ID), data = got_r); summary(g_attr_E_P_raw)

g_attr_E_P_w <- lmer(attr ~ Est.cwc + Prog.cwc + (1 | subID) + (1 | rater_ID), data = got_r); summary(g_attr_E_P_w)

g_attr_E_P_interactions <- lmer(attr ~ Est.cwc*Prog.cwc + Est.cm*Prog.cm +
                                  Est.cm*Prog.cwc + Est.cwc*Prog.cm + (1 | subID) + (1 | rater_ID),
                                data = got_r); summary(g_attr_E_P_interactions)

# msu; attr ~ E*P

m_attr_E_P_null <- lmer(attr ~ 1 + (1 | subID) + (1 | rater_ID), data = msu_r); summary(m_attr_E_P_null)

m_attr_E_P_raw <- lmer(attr ~ Estradiol + Progesterone + (1 | subID) + (1 | rater_ID), data = msu_r); summary(m_attr_E_P_raw)

m_attr_E_P_w <- lmer(attr ~ Est.cwc + Prog.cwc + (1 | subID) + (1 | rater_ID), data = msu_r); summary(m_attr_E_P_w)

m_attr_E_P_interactions <- lmer(attr ~ Est.cwc*Prog.cwc + Est.cm*Prog.cm +
                                  Est.cm*Prog.cwc + Est.cwc*Prog.cm + (1 | subID) + (1 | sibID) + (1 | rater_ID),
                                data = msu_r); summary(m_attr_E_P_interactions)


## Attractiveness and hormones; robustness using log-transformed hormone values

# gottingen; attr ~ logE*P

g_attr_logE_P_null <- lmer(attr ~ 1 + (1 | subID) + (1 | rater_ID), data = got_r); summary(g_attr_logE_P_null)

g_attr_logE_P_raw <- lmer(attr ~ Est_log + Prog_log + (1 | subID) + (1 | rater_ID), data = got_r); summary(g_attr_logE_P_raw)

g_attr_logE_P_w <- lmer(attr ~ Est_log.cwc + Prog_log.cwc + (1 | subID) + (1 | rater_ID), data = got_r); summary(g_attr_logE_P_w)

g_attr_logE_P_interactions <- lmer(attr ~ Est_log.cwc*Prog_log.cwc + Est_log.cm*Prog_log.cm +
                                     Est_log.cm*Prog_log.cwc + Est_log.cwc*Prog_log.cm + (1 | subID) + (1 | rater_ID),
                                   data = got_r); summary(g_attr_logE_P_interactions)

# msu; attr ~ logE*P

m_attr_logE_P_null <- lmer(attr ~ 1 + (1 | subID) + (1 | rater_ID), data = msu_r); summary(m_attr_logE_P_null)

m_attr_logE_P_raw <- lmer(attr ~ Estradiol + Progesterone + (1 | subID) + (1 | rater_ID), data = msu_r); summary(m_attr_logE_P_raw)

m_attr_logE_P_w <- lmer(attr ~ Est.cwc + Prog.cwc + (1 | subID) + (1 | rater_ID), data = msu_r); summary(m_attr_logE_P_w)

m_attr_logE_P_interactions <- lmer(attr ~ Est_log.cwc*Prog_log.cwc + Est_log.cm*Prog_log.cm +
                                     Est_log.cm*Prog_log.cwc + Est_log.cwc*Prog_log.cm + (1 | subID) + (1 | sibID) + (1 | rater_ID),
                                   data = msu_r); summary(m_attr_logE_P_interactions)



## Attractiveness and hormones; robustness using EP ratio

## gottingen; attr ~ EP

g_attr_EP_null <- lmer(attr ~ 1 + (1 | subID) + (1 | rater_ID), data = got_r); summary(g_attr_EP_null)

g_attr_EP_raw <- lmer(attr ~ EP + (1 | subID) + (1 | rater_ID), data = got_r); summary(g_attr_EP_raw)

g_attr_EP_w <- lmer(attr ~ EP.cwc + (1 | subID) + (1 | rater_ID), data = got_r); summary(g_attr_EP_w)

g_attr_EP_w_b <- lmer(attr ~ EP.cm + EP.cwc + (1 | subID) + (1 | rater_ID), data = got_r); summary(g_attr_EP_w_b)

g_attr_EP_interactions <- lmer(attr ~ EP.cm*EP.cwc + (1 | subID) + (1 | rater_ID),
                               data = got_r); summary(g_attr_EP_interactions)

# gottingen; attr ~ logEP

g_attr_logEP_null <- lmer(attr ~ 1 + (1 | subID) + (1 | rater_ID), data = got_r); summary(g_attr_logEP_null)

g_attr_logEP_raw <- lmer(attr ~ EP_log + (1 | subID) + (1 | rater_ID), data = got_r); summary(g_attr_logEP_raw)

g_attr_logEP_w <- lmer(attr ~ EP_log.cwc + (1 | subID) + (1 | rater_ID), data = got_r); summary(g_attr_logEP_w)

g_attr_logEP_w_b <- lmer(attr ~ EP_log.cm + EP.cwc + (1 | subID) + (1 | rater_ID), data = got_r); summary(g_attr_logEP_w_b)

g_attr_logEP_interactions <- lmer(attr ~ EP_log.cm*EP_log.cwc + (1 | subID) + (1 | rater_ID),
                               data = got_r); summary(g_attr_logEP_interactions)

# msu; attr ~ EP

m_attr_EP_null <- lmer(attr ~ 1 + (1 | subID) + (1 | rater_ID), data = msu_r); summary(m_attr_EP_null)

m_attr_EP_raw <- lmer(attr ~ EP + (1 | subID) + (1 | rater_ID), data = msu_r); summary(m_attr_EP_raw)

m_attr_EP_w <- lmer(attr ~ EP.cwc + (1 | subID) + (1 | rater_ID), data = msu_r); summary(m_attr_EP_w)

m_attr_EP_w_b <- lmer(attr ~ EP.cm + EP.cwc + (1 | subID) + (1 | rater_ID), data = msu_r); summary(m_attr_EP_w_b)

m_attr_EP_interactions <- lmer(attr ~ EP.cm*EP.cwc + (1 | subID) + (1 | rater_ID),
                               data = msu_r); summary(m_attr_EP_interactions)

# msu; attr ~ logEP

m_attr_logEP_null <- lmer(attr ~ 1 + (1 | subID) + (1 | rater_ID), data = msu_r); summary(m_attr_logEP_null)

m_attr_logEP_raw <- lmer(attr ~ EP_log + (1 | subID) + (1 | rater_ID), data = msu_r); summary(m_attr_logEP_raw)

m_attr_logEP_w <- lmer(attr ~ EP_log.cwc + (1 | subID) + (1 | rater_ID), data = msu_r); summary(m_attr_logEP_w)

m_attr_logEP_w_b <- lmer(attr ~ EP_log.cm + EP_log.cwc + (1 | subID) + (1 | rater_ID), data = msu_r); summary(m_attr_logEP_w_b)

m_attr_logEP_interactions <- lmer(attr ~ EP_log.cm*EP_log.cwc + (1 | subID) + (1 | rater_ID),
                               data = msu_r); summary(m_attr_logEP_interactions)




# gottingen; meanPitch; un-transformed E and P

g_f0_EP_null <- lmer(scale(meanPitch) ~ 1 + (1 | subID), data = got); summary(g_f0_EP_null)

g_f0_EP_raw <- lmer(scale(meanPitch) ~ Estradiol + Progesterone (1 | subID), data = got); summary(g_f0_EP_raw)

g_f0_EP_w <- lmer(scale(meanPitch) ~ Est.cwc + Prog.cwc (1 | subID), data = got); summary(g_f0_EP_w)

g_f0_EP_w_b <- lmer(scale(meanPitch) ~ EST.cm + Prog.m + (1 | subID), data = got); summary(g_f0_EP_w_b)

g_f0_EP_interactions <- lmer(scale(meanPitch) ~ Est.cm*Prog.cwc + Est.cwc*Prog.cm + Est.cm*Prog.cm + Est.cwc*Prog.cwc + (1 | subID), data = got); summary(g_f0_EP_interactions)

# gottingen; meanPitch; un-transformed E-to-P ratio

g_f0_EP_null <- lmer(scale(meanPitch) ~ 1 + (1 | subID), data = got); summary(g_f0_EP_null)

g_f0_EP_raw <- lmer(scale(meanPitch) ~ EP + (1 | subID), data = got); summary(g_f0_EP_raw)

g_f0_EP_w <- lmer(scale(meanPitch) ~ EP.cwc + (1 | subID), data = got); summary(g_f0_EP_w)

g_f0_EP_w_b <- lmer(scale(meanPitch) ~ EP.cm + EP.cwc + (1 | subID), data = got); summary(g_f0_EP_w_b)

g_f0_EP_interactions <- lmer(scale(meanPitch) ~ EP.cm*EP.cwc + (1 | subID), data = got); summary(g_f0_EP_interactions)


# gottingen; meanPitch ~ log-transformed E-to-P ratio

g_f0_logEP_null <- lmer(scale(meanPitch) ~ 1 + (1 | subID), data = got); summary(g_f0_logEP_null)

g_f0_logEP_raw <- lmer(scale(meanPitch) ~ EP_log + (1 | subID), data = got); summary(g_f0_logEP_raw)

g_f0_logEP_w <- lmer(scale(meanPitch) ~ EP_log.cwc + (1 | subID), data = got); summary(g_f0_logEP_w)

g_f0_logEP_w_b <- lmer(scale(meanPitch) ~ EP_log.cm + EP_log.cwc + (1 | subID), data = got); summary(g_f0_logEP_w_b)

g_f0_logEP_interactions <- lmer(scale(meanPitch) ~ EP_log.cm*EP_log.cwc + (1 | subID), data = got); summary(g_f0_logEP_interactions)


# msu; meanPitch; un-transformed E-to-P ratio

m_f0_EP_null <- lmer(scale(meanPitch) ~ 1 + (1 | subID), data = msu); summary(m_f0_EP_null)

m_f0_EP_raw <- lmer(scale(meanPitch) ~ EP + (1 | subID), data = msu); summary(m_f0_EP_raw)

m_f0_EP_w <- lmer(scale(meanPitch) ~ EP.cwc + (1 | subID), data = msu); summary(m_f0_EP_w)

m_f0_EP_w_b <- lmer(scale(meanPitch) ~ EP.cm + EP.cwc + (1 | subID), data = msu); summary(m_f0_EP_w_b)

m_f0_EP_interactions <- lmer(scale(meanPitch) ~ EP.cm*EP.cwc + (1 | subID), data = msu); summary(m_f0_EP_interactions)


# msu; meanPitch ~ log-transformed E-to-P ratio

m_f0_logEP_null <- lmer(scale(meanPitch) ~ 1 + (1 | subID), data = msu); summary(m_f0_logEP_null)

m_f0_logEP_raw <- lmer(scale(meanPitch) ~ EP_log + (1 | subID), data = msu); summary(m_f0_logEP_raw)

m_f0_logEP_w <- lmer(scale(meanPitch) ~ EP_log.cwc + (1 | subID), data = msu); summary(m_f0_logEP_w)

m_f0_logEP_w_b <- lmer(scale(meanPitch) ~ EP_log.cm + EP_log.cwc + (1 | subID), data = msu); summary(m_f0_logEP_w_b)

m_f0_logEP_interactions <- lmer(scale(meanPitch) ~ EP_log.cm*EP_log.cwc + (1 | subID), data = msu); summary(m_f0_logEP_interactions)


htmlreg(list(g_f0_EP_raw, g_f0_EP_w, g_f0_EP_w_b, g_f0_EP_interactions, g_f0_logEP_raw,
             g_f0_logEP_w, g_f0_logEP_w_b, g_f0_logEP_interactions), 
        single.row = T,
        file = "got_f0_EP_table.html",
        stars = numeric(0),
        caption = "")


# gottingen; scale(attr) ~ meanPitch

g_attr_f0_null <- lmer(scale(attr) ~ 1 + (1 | subID) + (1 | rater_ID), data = got_r); summary(g_attr_f0_null)

g_attr_f0_raw <- lmer(scale(attr) ~ scale(meanPitch) + (1 | subID) + (1 | rater_ID), data = got_r); summary(g_attr_f0_raw)

g_attr_f0_w <- lmer(scale(attr) ~ scale(meanPitch.cwc) + (1 | subID) + (1 | rater_ID), data = got_r); summary(g_attr_f0_w)

g_attr_f0_w_b <- lmer(scale(attr) ~ scale(meanPitch.cm) + scale(meanPitch.cwc) + (1 | subID) + (1 | rater_ID), data = got_r); summary(g_attr_f0_w_b)

g_attr_f0_interactions <- lmer(scale(attr) ~ scale(meanPitch.cm)*scale(meanPitch.cwc) + (1 | subID) + (1 | rater_ID),
                              data = got_r); summary(g_attr_f0_interactions)

# msu; scale(attr) ~ meanPitch

m_attr_f0_null <- lmer(scale(attr) ~ 1 + (1 | subID) + (1 | rater_ID), data = msu_r); summary(m_attr_f0_null)

m_attr_f0_raw <- lmer(scale(attr) ~ scale(meanPitch) + (1 | subID) + (1 | rater_ID), data = msu_r); summary(m_attr_f0_raw)

m_attr_f0_w <- lmer(scale(attr) ~ scale(meanPitch.cwc) + (1 | subID) + (1 | rater_ID), data = msu_r); summary(m_attr_f0_w)

m_attr_f0_w_b <- lmer(scale(attr) ~ scale(meanPitch.cm) + scale(meanPitch.cwc) + (1 | subID) + (1 | rater_ID), data = msu_r); summary(m_attr_f0_w_b)

m_attr_f0_interactions <- lmer(scale(attr) ~ scale(meanPitch.cm)*scale(meanPitch.cwc) + (1 | subID) + (1 | rater_ID),
                               data = msu_r); summary(m_attr_f0_interactions)

ggplot(msu_r,aes(scale(meanPitch.cm), scale(attr))) + geom_point()




## does E:P ratio predict jitter?

msu_j0 <- lmer(scale(jitter) ~ 1 + (1 | subID) + (1| sibID), data = msu)

msu_j_rawEP <- lmer(scale(jitter) ~ E_P.3 + (1 | subID) + (1| sibID), data = msu); summary(msu_j_rawEP)

msu_j_gmcEP <- lmer(scale(jitter) ~ E_P.gmc + (1| subID) + (1| sibID), data = msu); summary(msu_j_gmcEP)

msu_j_cwcEP <- lmer(scale(jitter) ~ E_P.cwc + (1 | subID) + (1| sibID), data = msu); summary(msu_j_cwcEP)

msu_j_cmcEP <- lmer(scale(jitter) ~ E_P.cm + E_P.cwc + (1 | subID) + (1| sibID), data = msu); summary(msu_j_cmcEP)

htmlreg(list(msu_j_rawEP, msu_j_gmcEP, msu_j_cwcEP, msu_j_cmcEP), 
        single.row = T,
        file = "msu_table.html",
        stars = numeric(0),
        caption = "")

var_reduction = function(m0, m1){
  library(tidyverse)
  VarCorr(m0) %>% 
    as.data.frame %>% 
    select(grp, var_m0 = vcov) %>% 
    left_join(VarCorr(m1) %>% 
                as.data.frame %>% 
                select(grp, var_m1 = vcov)) %>% 
    mutate(var_red = 1 - var_m1 / var_m0) 
}

# Create comparison table
cbind(M1 = var_reduction(msu_j0, msu_j_rawEP)[,4],
      M2 = var_reduction(msu0, msu_j_gmcEP)[,4],
      M3 = var_reduction(msu0, msu_j_cwcEP)[,4],
      M4 = var_reduction(msu0, msu_j_cmcEP)[,4]) %>%
  round(2)


got_j0 <- lmer(scale(jitter) ~ 1 + (1 | subID), data = msu)

got_j_rawEP <- lmer(scale(jitter) ~ E_P.3 + (1 | subID), data = got); summary(got_j_rawEP)

got_j_gmcEP <- lmer(scale(jitter) ~ E_P.gmc + (1| subID), data = got); summary(got_j_gmcEP)

got_j_cwcEP <- lmer(scale(jitter) ~ E_P.cwc + (1 | subID), data = got); summary(got_j_cwcEP)

got_j_cmcEP <- lmer(scale(jitter) ~ E_P.cm + E_P.cwc + (1 | subID), data = got); summary(got_j_cmcEP)

htmlreg(list(got_j_rawEP, got_j_gmcEP, got_j_cwcEP, got_j_cmcEP), 
        single.row = T,
        file = "got_table.html",
        stars = numeric(0),
        caption = "")

# Create comparison table
cbind(M1 = var_reduction(got_j0, got_j_rawEP)[,4],
      M2 = var_reduction(got0, got_j_gmcEP)[,4],
      M3 = var_reduction(got0, got_j_cwcEP)[,4],
      M4 = var_reduction(got0, got_j_cmcEP)[,4]) %>%
  round(2)

# does E:P ratio predict harmonics:noise ratio?

msu_h0 <- lmer(scale(harmonics) ~ 1 + (1 | subID) + (1| sibID), data = msu)

msu_h_rawEP <- lmer(scale(harmonics) ~ E_P.3 + (1 | subID) + (1| sibID), data = msu); summary(msu_h_rawEP)

msu_h_gmcEP <- lmer(scale(harmonics) ~ E_P.gmc + (1| subID) + (1| sibID), data = msu); summary(msu_h_gmcEP)

msu_h_cwcEP <- lmer(scale(harmonics) ~ E_P.cwc + (1 | subID) + (1| sibID), data = msu); summary(msu_h_cwcEP)

msu_h_cmcEP <- lmer(scale(harmonics) ~ E_P.cm + E_P.cwc + (1 | subID) + (1| sibID), data = msu); summary(msu_h_cmcEP)

htmlreg(list(msu_h_rawEP, msu_h_gmcEP, msu_h_cwcEP, msu_h_cmcEP), 
        single.row = T,
        file = "msu_table.html",
        stars = numeric(0),
        caption = "")

var_reduction = function(m0, m1){
  library(tidyverse)
  VarCorr(m0) %>% 
    as.data.frame %>% 
    select(grp, var_m0 = vcov) %>% 
    left_join(VarCorr(m1) %>% 
                as.data.frame %>% 
                select(grp, var_m1 = vcov)) %>% 
    mutate(var_red = 1 - var_m1 / var_m0) 
}

# Create comparison table
cbind(M1 = var_reduction(msu_h0, msu_h_rawEP)[,4],
      M2 = var_reduction(msu_h0, msu_h_gmcEP)[,4],
      M3 = var_reduction(msu_h0, msu_h_cwcEP)[,4],
      M4 = var_reduction(msu_h0, msu_h_cmcEP)[,4]) %>%
  round(2)


got_h0 <- lmer(scale(harmonics) ~ 1 + (1 | subID), data = msu)

got_h_rawEP <- lmer(scale(harmonics) ~ E_P.3 + (1 | subID), data = got); summary(got_h_rawEP)

got_h_gmcEP <- lmer(scale(harmonics) ~ E_P.gmc + (1| subID), data = got); summary(got_h_gmcEP)

got_h_cwcEP <- lmer(scale(harmonics) ~ E_P.cwc + (1 | subID), data = got); summary(got_h_cwcEP)

got_h_cmcEP <- lmer(scale(harmonics) ~ E_P.cm + E_P.cwc + (1 | subID), data = got); summary(got_h_cmcEP)

htmlreg(list(got_h_rawEP, got_h_gmcEP, got_h_cwcEP, got_h_cmcEP), 
        single.row = T,
        file = "got_table.html",
        stars = numeric(0),
        caption = "")

# Create comparison table
cbind(M1 = var_reduction(got_h0, got_h_rawEP)[,4],
      M2 = var_reduction(got_h0, got_h_gmcEP)[,4],
      M3 = var_reduction(got_h0, got_h_cwcEP)[,4],
      M4 = var_reduction(got_h0, got_h_cmcEP)[,4]) %>%
  round(2)

# does E:P ratio predict shimmer?

msu_s0 <- lmer(scale(shimmer) ~ 1 + (1 | subID) + (1| sibID), data = msu)

msu_s_rawEP <- lmer(scale(shimmer) ~ E_P.3 + (1 | subID) + (1| sibID), data = msu); summary(msu_s_rawEP)

msu_s_gmcEP <- lmer(scale(shimmer) ~ E_P.gmc + (1| subID) + (1| sibID), data = msu); summary(msu_s_gmcEP)

msu_s_cwcEP <- lmer(scale(shimmer) ~ E_P.cwc + (1 | subID) + (1| sibID), data = msu); summary(msu_s_cwcEP)

msu_s_cmcEP <- lmer(scale(shimmer) ~ E_P.cm + E_P.cwc + (1 | subID) + (1| sibID), data = msu); summary(msu_s_cmcEP)

htmlreg(list(msu_s_rawEP, msu_s_gmcEP, msu_s_cwcEP, msu_s_cmcEP), 
        single.row = T,
        file = "msu_table.html",
        stars = numeric(0),
        caption = "")

var_reduction = function(m0, m1){
  library(tidyverse)
  VarCorr(m0) %>% 
    as.data.frame %>% 
    select(grp, var_m0 = vcov) %>% 
    left_join(VarCorr(m1) %>% 
                as.data.frame %>% 
                select(grp, var_m1 = vcov)) %>% 
    mutate(var_red = 1 - var_m1 / var_m0) 
}

# Create comparison table
cbind(M1 = var_reduction(msu_s0, msu_s_rawEP)[,4],
      M2 = var_reduction(msu_s0, msu_s_gmcEP)[,4],
      M3 = var_reduction(msu_s0, msu_s_cwcEP)[,4],
      M4 = var_reduction(msu_s0, msu_s_cmcEP)[,4]) %>%
  round(2)


got_s0 <- lmer(scale(shimmer) ~ 1 + (1 | subID), data = msu)

got_s_rawEP <- lmer(scale(shimmer) ~ E_P.3 + (1 | subID), data = got); summary(got_s_rawEP)

got_s_gmcEP <- lmer(scale(shimmer) ~ E_P.gmc + (1| subID), data = got); summary(got_s_gmcEP)

got_s_cwcEP <- lmer(scale(shimmer) ~ E_P.cwc + (1 | subID), data = got); summary(got_s_cwcEP)

got_s_cmcEP <- lmer(scale(shimmer) ~ E_P.cm + E_P.cwc + (1 | subID), data = got); summary(got_s_cmcEP)

htmlreg(list(got_s_rawEP, got_s_gmcEP, got_s_cwcEP, got_s_cmcEP), 
        single.row = T,
        file = "got_table.html",
        stars = numeric(0),
        caption = "")

# Create comparison table
cbind(M1 = var_reduction(got_s0, got_s_rawEP)[,4],
      M2 = var_reduction(got_s0, got_s_gmcEP)[,4],
      M3 = var_reduction(got_s0, got_s_cwcEP)[,4],
      M4 = var_reduction(got_s0, got_s_cmcEP)[,4]) %>%
  round(2)

## do hormones predict vocal attractiveness?

# E:P

msu_a0 <- lmer(scale(st_attr) ~ 1 + (1 | subID), data = msu_r)

msu_a_rawEP <- lmer(scale(st_attr) ~ E_P.3 + (1 | subID), data = msu_r); summary(msu_a_rawEP)

msu_a_gmcEP <- lmer(scale(st_attr) ~ E_P.gmc + (1| subID), data = msu_r); summary(msu_a_gmcEP)

msu_a_cwcEP <- lmer(scale(st_attr) ~ E_P.cwc + (1 | subID), data = msu_r); summary(msu_a_cwcEP)

msu_a_cmcEP <- lmer(scale(st_attr) ~ E_P.cm + E_P.cwc + (1 | subID), data = msu_r); summary(msu_a_cmcEP)

htmlreg(list(msu_a_rawEP, msu_a_gmcEP, msu_a_cwcEP, msu_a_cmcEP), 
        single.row = T,
        file = "msu_table.html",
        stars = numeric(0),
        caption = "")

# Create comparison table
cbind(M1 = var_reduction(msu_a0, msu_a_rawEP)[,4],
      M2 = var_reduction(msu_a0, msu_a_gmcEP)[,4],
      M3 = var_reduction(msu_a0, msu_a_cwcEP)[,4],
      M4 = var_reduction(msu_a0, msu_a_cmcEP)[,4]) %>%
  round(2)

got_a0 <- lmer(scale(attr) ~ 1 + (1 | subID), data = got_r)

got_a_rawEP <- hist(residuals(lmer(scale(attr) ~ E_P.3 + (1 | subID), data = got_r))); summary(got_a_rawEP)

got_a_gmcEP <- lmer(scale(attr) ~ E_P.gmc + (1| subID), data = got_r); summary(got_a_gmcEP)

got_a_cwcEP <- lmer(scale(attr) ~ E_P.cwc + (1 | subID), data = got_r); summary(got_a_cwcEP)

got_a_cmcEP <- lmer(scale(attr) ~ E_P.cm + E_P.cwc + (1 | subID), data = got_r); summary(got_a_cmcEP)

htmlreg(list(got_a_rawEP, got_a_gmcEP, got_a_cwcEP, got_a_cmcEP), 
        single.row = T,
        file = "got_table.html",
        stars = numeric(0),
        caption = "")

# Create comparison table
cbind(M1 = var_reduction(got_a0, got_a_rawEP)[,4],
      M2 = var_reduction(got_a0, got_a_gmcEP)[,4],
      M3 = var_reduction(got_a0, got_a_cwcEP)[,4],
      M4 = var_reduction(got_a0, got_a_cmcEP)[,4]) %>%
  round(2)

# plot

MSU_st_E_plot <- plot(Effect("Estradiol_log_z",MSU_attr_ExP,partial=T))
MSU_st_P_plot <- plot(Effect("Progesterone_log_z",MSU_attr_ExP,partial=T))

MSU_lt_E_plot <- plot(Effect("Estradiol_log_z",MSU_attr_lt_ExP,partial=T))
MSU_lt_P_plot <- plot(Effect("Progesterone_log_z",MSU_attr_lt_ExP,partial=T))

Goettingen_attr_E_plot <- plot(Effect("Estradiol_log_z",Goettingen_attr_EP,partial=T))
Goettingen_attr_P_plot <- plot(Effect("Progesterone_log_z",Goettingen_attr_EP,partial=T))

# E_P

MSU_attr_EP <- lmer(st_attr ~ E_P_log_z + (1 | subID) + (1 | rater_ID) + (1| sibID), data = msu_r); summary(MSU_attr_EP)

MSU_attr_lt_EP <- lmer(lt_attr ~ E_P_log_z + (1 | subID) + (1 | rater_ID) + (1| sibID), data = msu_r); summary(MSU_attr_lt_EP)

Goettingen_attr_EP <- lmer(Rating ~ E_P_log_z + (1 | subID) + (1 | Male_ID), data = got_r); summary(Goettingen_attr_EP)

# plot

MSU_st_EP__plot <- plot(Effect("E_P_log_z",MSU_attr_EP,partial=T))
MSU_lt_EP_plot <- plot(Effect("E_P_log_z",MSU_attr_lt_EP,partial=T))
Goettingen_attr_EP_plot <- plot(Effect("E_P_log_z",Goettingen_attr_EP,partial=T))


library(rsm)

msu_r$st_attr <- scale(msu_r$st_attr)
msu_r <- msu_r[!is.na(msu_r$st_attr) & !is.na(msu_r$Est.gmc) & !is.na(msu_r$Prog.gmc),]
par(mar=c(1.5,0,0,0))
msu_surface_plot.lm <- lm(st_attr ~ poly(Prog.gmc, Est.gmc, degree=2), data=msu_r)
persp(msu_surface_plot.lm, Prog.gmc ~ Est.gmc, theta=50)

got_r$Rating <- scale(got_r$attr)
got_r <- got_r[!is.na(got_r$attr) & !is.na(got_r$Est.gmc) & !is.na(got_r$Prog.gmc),]
par(mar=c(1.5,0,0,0))
goett_surface_plot.lm <- lm(Rating ~ poly(Prog.gmc, Est.gmc, degree=2), data=got_r)
persp(goett_surface_plot.lm, Prog.gmc ~ Est.gmc, theta=50)

## do acoustic variables predict attractiveness?

# multiple linear regression

MSU_attr_acoustic <- lmer(st_attr ~ meanPitch_z + sdPitch_z + voiceBreaks_z + jitter_z + shimmer_z + harmonics_z +
                            (1 | subID) + (1 | rater_ID) + (1| sibID), data = msu_r); summary(MSU_attr_acoustic)

MSU_attr_lt_acoustic <- lmer(st_attr ~ meanPitch_z + sdPitch_z + voiceBreaks_z + jitter_z + shimmer_z + harmonics_z +
                               (1 | subID) + (1 | rater_ID) + (1| sibID), data = msu_r); summary(MSU_attr_lt_acoustic)

Goettingen_attr_acoustic <- lmer(Rating ~ meanPitch_z + sdPitch_z + voiceBreaks_z + jitter_z + shimmer_z + harmonics_z +
                                   (1 | subID) + (1 | Male_ID), data = got_r); summary(Goettingen_attr_acoustic)

# does duration predict attractiveness

MSU_attr_duration <- lmer(st_attr ~ duration_z + (1 | subID) + (1 | rater_ID) + (1| sibID), data = msu_r); summary(MSU_attr_duration)

MSU_attr_lt_duration <- lmer(lt_attr ~ duration_z + (1 | subID) + (1 | rater_ID) + (1| sibID), data = msu_r); summary(MSU_attr_lt_duration)

Goettingen_attr_duration <- lmer(Rating ~ duration_z + (1 | subID) + (1 | Male_ID), data = got_r); summary(Goettingen_attr_duration)

# does mean pitch predict attractiveness

MSU_attr_meanPitch <- lmer(st_attr ~ meanPitch_z + (1 | subID) + (1 | rater_ID) + (1| sibID), data = msu_r); summary(MSU_attr_meanPitch)

MSU_attr_lt_meanPitch <- lmer(lt_attr ~ meanPitch_z + (1 | subID) + (1 | rater_ID) + (1| sibID), data = msu_r); summary(MSU_attr_lt_meanPitch)

Goettingen_attr_meanPitch <- lmer(Rating ~ meanPitch_z + (1 | subID) + (1 | Male_ID), data = got_r); summary(Goettingen_attr_meanPitch)

# does shimmer predict attractiveness

MSU_attr_shimmer <- lmer(st_attr ~ shimmer_z + (1 | subID) + (1 | rater_ID) + (1| sibID), data = msu_r); summary(MSU_attr_shimmer)

MSU_attr_lt_shimmer <- lmer(lt_attr ~ shimmer_z + (1 | subID) + (1 | rater_ID) + (1| sibID), data = msu_r); summary(MSU_attr_lt_shimmer)

Goettingen_attr_shimmer <- lmer(Rating ~ shimmer_z + (1 | subID) + (1 | Male_ID), data = got_r); summary(Goettingen_attr_shimmer)

# does jitter predict attractiveness

MSU_attr_jitter <- lmer(st_attr ~ jitter_z + (1 | subID) + (1 | rater_ID) + (1| sibID), data = msu_r); summary(MSU_attr_jitter)

MSU_attr_lt_jitter <- lmer(lt_attr ~ jitter_z + (1 | subID) + (1 | rater_ID) + (1| sibID), data = msu_r); summary(MSU_attr_lt_jitter)

Goettingen_attr_jitter <- lmer(Rating ~ jitter_z + (1 | subID) + (1 | Male_ID), data = got_r); summary(Goettingen_attr_jitter)

# does harmonics-to-noise ratio predict attractiveness

MSU_attr_HNR <- lmer(st_attr ~ harmonics_z + (1 | subID) + (1 | rater_ID) + (1| sibID), data = msu_r); summary(MSU_attr_HNR)

MSU_attr_lt_HNR <- lmer(lt_attr ~ harmonics_z + (1 | subID) + (1 | rater_ID) + (1| sibID), data = msu_r); summary(MSU_attr_lt_HNR)

Goettingen_attr_HNR <- lmer(Rating ~ harmonics_z + (1 | subID) + (1 | Male_ID), data = got_r); summary(Goettingen_attr_HNR)

# does number of voice breaks ratio predict attractiveness

MSU_attr_voiceBreaks <- lmer(st_attr ~ voiceBreaks_z + (1 | subID) + (1 | rater_ID) + (1| sibID), data = msu_r); summary(MSU_attr_voiceBreaks)

MSU_attr_lt_voiceBreaks <- lmer(lt_attr ~ voiceBreaks_z + (1 | subID) + (1 | rater_ID) + (1| sibID), data = msu_r); summary(MSU_attr_lt_voiceBreaks)

Goettingen_attr_voiceBreaks <- lmer(Rating ~ voiceBreaks_z + (1 | subID) + (1 | Male_ID),
                                    data = got_r); summary(Goettingen_attr_voiceBreaks)
