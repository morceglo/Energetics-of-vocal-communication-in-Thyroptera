#Load required packages
library(readxl)
library(ggpubr)
library(dplyr)
library (nlme)
library (ggplot2)
library(viridis)
library(tidyverse)
library(ggthemes)
library(forcats)
library(hrbrthemes)
library(relimp)
library(relaimpo)
library(lsmeans)
library(corrplot)
library (psych)
library (Hmisc)


#Read data

Data <- read_excel("C:/Users/Gloriana/Dropbox/Publicaciones/Publicaciones en progreso/6_Gasto energético Thyroptera/Analysis R/Energetics of calling in Thyroptera/CostCalling_Thtr.xlsx")
View(Data)



#Summarize basic data

mean(Data$Time_vocal)
sd(Data$Time_vocal)
range(Data$Time_vocal)
sum(Data$Time_vocal == 0)

mean(Data$mlh_wsound)
sd(Data$mlh_wsound)
range(Data$mlh_wsound)

mean(Data$mlh_silent)
sd(Data$mlh_silent)
range(Data$mlh_silent)

mean(Data$KJ_wsound)
sd(Data$KJ_wsound)
range(Data$KJ_wsound)

mean(Data$KJ_silent)
sd(Data$KJ_silent)
range(Data$KJ_silent)


#Determine differences in time spent vocalizing for males and females

group_by(Data, Sex) %>%
  summarise(
    count = n(),
    mean = mean(Response_t, na.rm = TRUE),
    sd = sd(Response_t, na.rm = TRUE)
  )

group_by(Data, Sex) %>%
  summarise(
    count = n(),
    mean = mean(Echo_t, na.rm = TRUE),
    sd = sd(Echo_t, na.rm = TRUE)
  )

group_by(Data, Sex) %>%
  summarise(
    count = n(),
    mean = mean(Other_t, na.rm = TRUE),
    sd = sd(Other_t, na.rm = TRUE)
  )

group_by(Data, Sex) %>%
  summarise(
    count = n(),
    mean = mean(Distress_t, na.rm = TRUE),
    sd = sd(Distress_t, na.rm = TRUE)
  )

group_by(Data, Sex) %>%
  summarise(
    count = n(),
    mean = mean(Mov_ws, na.rm = TRUE),
    sd = sd(Mov_ws, na.rm = TRUE)
  )

group_by(Data, Sex) %>%
  summarise(
    count = n(),
    mean = mean(mlh_wsound, na.rm = TRUE),
    sd = sd(mlh_wsound, na.rm = TRUE)
  )

group_by(Data, Sex) %>%
  summarise(
    count = n(),
    mean = mean(mlh_silent, na.rm = TRUE),
    sd = sd(mlh_silent, na.rm = TRUE)
  )

group_by(Data, Sex) %>%
  summarise(
    count = n(),
    mean = mean(Inst_increase, na.rm = TRUE),
    sd = sd(mlh_silent, na.rm = TRUE)
  )


group_by(Data, Vocal_response) %>%
  summarise(
    count = n(),
    mean = mean(Eedifference_mlh, na.rm = TRUE),
    sd = sd(mlh_silent, na.rm = TRUE)
  )


group_by(Data, Vocal_response) %>%
  summarise(
    count = n(),
    mean = mean(Eedifference_kJ, na.rm = TRUE),
    sd = sd(mlh_silent, na.rm = TRUE)
  )


# Shapiro-Wilk normality test for Male and Female's calling and moving times (trials with sound)

with(Data, shapiro.test(Response_t[Sex == "Male"]))
with(Data, shapiro.test(Response_t[Sex == "Female"]))

with(Data, shapiro.test(Echo_t[Sex == "Male"]))
with(Data, shapiro.test(Echo_t[Sex == "Female"]))

with(Data, shapiro.test(Other_t[Sex == "Male"]))
with(Data, shapiro.test(Other_t[Sex == "Female"]))

with(Data, shapiro.test(Distress_t[Sex == "Male"]))
with(Data, shapiro.test(Distress_t[Sex == "Female"]))

with(Data, shapiro.test(Mov_ws[Sex == "Male"]))
with(Data, shapiro.test(Mov_ws[Sex == "Female"]))

with(Data, shapiro.test(Eedifference_kJ))

with(Data, shapiro.test(Mass))


#Determine the correlation between all calling times, generate a correlation plot

datacalls <- Data[, c(8, 11, 13, 15)]

res <- cor(datacalls, method = "spearman")

res2 <- rcorr(as.matrix(datacalls, method = "spearman"))

res2

corrplot.mixed(res2$r, upper = "ellipse", 
         p.mat = res2$P, sig.level = 0.01, insig = "blank")



# Unpaired Two-Samples Wilcoxon Test

res_mov <- wilcox.test(Mov_ws~Sex, data = Data, exact = FALSE)
res_mov

res_resp <- wilcox.test(Response_t~Sex, data = Data, exact = FALSE)
res_resp

res_echo <- wilcox.test(Echo_t~Sex, data = Data, exact = FALSE)
res_echo

res_other <- wilcox.test(Other_t~Sex, data = Data, exact = FALSE)
res_other

res_distress <- wilcox.test(Distress_t~Sex, data = Data, exact = FALSE)
res_distress




#Chi-squared to determine if there is a difference in the proportion of vocal/non-vocal males and females

chisq.test(Data$Sex, Data$Vocal_response, correct=FALSE)




##Test if there is a difference in energy expenditure by sex (with mass as covariate) 

#Verifying model assumptions: Energetic expenditure by sex, experiments with sound

datws = lm(KJ_wsound ~ Sex, Data)

plot(fitted(datws),residuals(datws))^2

hist(residuals(datws))

qqnorm(residuals(datws))

ggqqplot(Data$KJ_wsound)


#Verifying model assumptions: Energetic expenditure by sex, experiments without sound

dats = lm(KJ_silent ~ Sex, Data)

plot(fitted(dats),residuals(dats))^2

hist(residuals(dats))

qqnorm(residuals(dats))

ggqqplot(Data$KJ_silent)



#First determine if males and females differ in mass (with a t-test)

t.test(Mass ~ Sex, data = Data)

t.test(Inst_increase ~ Sex, data = Data)



#Running linear models to test if there is a difference in energy expenditure by sex (with mass as covariate)

modelws <- lm(KJ_wsound ~ Mass * Sex, data = Data)

anova(modelws)



modelsilent <- lm(KJ_silent ~ Mass * Sex, data = Data)

anova(modelsilent)



#Running linear models to test if the difference in energy expenditure with and without sound is explained by sex and whether bats respond or not (with mass as covariate) (interaction Sex*Vocal_response was added to the model but was non-sig., thus was removed)

modelresp <- lm(Eedifference_kJ ~ Mass + Sex + Vocal_response, data = Data)

summary(modelresp)

anova(modelresp)


modelrespml <- lm(Eedifference_mlh ~ Mass + Sex + Vocal_response, data = Data)

summary(modelrespml)

anova(modelrespml)


#Compare slopes between vocal and non-vocal bats

anova(modelresp)


# Obtain slopes

modelresp$coefficients

m.lst <- lstrends(modelresp, "Vocal_response", var="Eedifference_kJ")

m.lst

# Compare slopes

pairs(m.lst)




#Add a model that considers being vocal for all types of calls (interaction non sig.)

modelall <- lm(Eedifference_kJ ~ Mass + Sex + Vocal, data = Data)

summary(modelall)



#Figure 2. Difference in energy expenditure, measured as the difference in kJ during trials with sound minus kJ during trials without sound, for vocal and non-vocal males and females.


q <- ggplot(Data, aes(x = Sex, y = Eedifference_kJ)) +
  facet_wrap( ~ Vocal_response ) + 
  geom_violin (aes(fill = factor(Sex)), scale = "area", alpha=0.25, position = position_dodge(width = .75), size = 1 , color="black") +
  scale_fill_viridis_d(option = "D") +
  stat_summary (fun.data = "mean_sdl",  fun.args = list(mult = 1), geom = "pointrange", color = "gray20", size = 0.8)  +
  labs( x = "Sex", y = "EE difference (kJ)") + theme(legend.position = "none") +
  theme (panel.background = element_blank(), 
         panel.border = element_rect(colour = "black", fill=NA, size=1)) 

q 

ggsave("Figure2.png", width = 5, height = 4)


# Run linear models to test if energy expenditure (kJ) is influenced by the time spent producing calls


linmod <- lm(KJ_wsound ~ Mass + Mov_ws + Time_vocal, data = Data)
null <- lm(KJ_wsound ~ 1, data = Data)
fit0 <- lm(KJ_wsound ~ Mass + Mov_ws, data = Data)
fit1 <- lm(KJ_wsound ~ Mass + Mov_ws + TV_Resp, data = Data)
fit2 <- lm(KJ_wsound ~ Mass + Mov_ws + TV_Echo, data = Data)
fit3 <- lm(KJ_wsound ~ Mass + Mov_ws + TV_Other, data = Data)
fit4 <- lm(KJ_wsound ~ Mass + Mov_ws + TV_Distress, data = Data)
fit5 <- lm(KJ_wsound ~ Mass + Mov_ws + TV_RespEcho, data = Data)
fit6 <- lm(KJ_wsound ~ Mass + Mov_ws + TV_RespOther, data = Data)
fit7 <- lm(KJ_wsound ~ Mass + Mov_ws + TV_RespDistress, data = Data)
fit8 <- lm(KJ_wsound ~ Mass + Mov_ws + TV_EchoOther, data = Data)
fit9 <- lm(KJ_wsound ~ Mass + Mov_ws + TV_EchoDistress, data = Data)
fit10 <- lm(KJ_wsound ~ Mass + Mov_ws + TV_OtherDistress, data = Data)
fit11 <- lm(KJ_wsound ~ Mass + Mov_ws + Response_t, data = Data)
fit12 <- lm(KJ_wsound ~ Mass + Mov_ws + Echo_t, data = Data)
fit13 <- lm(KJ_wsound ~ Mass + Mov_ws + Other_t, data = Data)
fit14 <- lm(KJ_wsound ~ Mass + Mov_ws + Distress_t, data = Data)
fit15 <- lm(KJ_wsound ~ Mass + Mov_ws + Echo_t + Other_t, data = Data)

#Estimate Akaike's Information Criterion for all models
AIC(linmod)
AIC(null)
AIC(fit0)
AIC(fit1)
AIC(fit2)
AIC(fit3)
AIC(fit4)
AIC(fit5)
AIC(fit6)
AIC(fit7)
AIC(fit8)
AIC(fit9)
AIC(fit10)
AIC(fit11)
AIC(fit12)
AIC(fit13)
AIC(fit14)
AIC(fit15)

#Lowest value for fit7 (compare to null model), then create model with time spent producing echolocation and other calls (fit15)

summary(linmod)
summary(fit7)
summary(fit15)
anova(fit15)
anova(linmod)

anova(fit7, null)
anova(fit15, null)


#Test relative importance of the regressors in the best model (fit15) (https://www.jstatsoft.org/article/view/v017i01)

crlm <- calc.relimp(fit15, type = c("lmg"), rela = TRUE )


plot(crlm)


#Generate bootstrap confidence intervals for ranks

bootresult <- boot.relimp(fit15, b = 1000, type = c("lmg"), fixed = FALSE)

booteval.relimp(bootresult, typesel = c("lmg"), level = 0.9, bty = "perc", nodiff = TRUE)


#Comparison of relative importances for pairs of regressors

eval <- booteval.relimp(bootresult, typesel = c("lmg"), level = 0.9, bty = "perc", norank = TRUE)

eval


#Plotting results of the linear models

par(cex.axis = 0.9)

plot(booteval.relimp(bootresult, typesel = c("lmg"), level = 0.9), names.abbrev = 8, bty = "perc")

ggsave("FigureS1.png", width = 5, height = 4)


#Create scatter plots of energy expenditure and time spent calling

r <- ggplot(Data, aes(Response_t, KJ_wsound)) +
  geom_point(size = 2, color = "#440154FF") +
  geom_smooth(color = "#440154FF", method = lm) +
  labs( x = "Response calls (s)", y = "Energy expenditure (kJ)") +
  scale_color_viridis(discrete = TRUE)+
  scale_fill_viridis(discrete = TRUE) +
  theme (panel.background = element_blank()) 

r    

e <- ggplot(Data, aes(Echo_t, KJ_wsound)) +
  geom_point(size = 2, color = "#39568CFF") +
  geom_smooth(color = "#39568CFF", method = lm) +
  labs( x = "Echolocation calls (s)", y = "Energy expenditure (kJ)") +
  scale_color_viridis(discrete = TRUE)+
  scale_fill_viridis(discrete = TRUE) +
  theme (panel.background = element_blank())

e


o <- ggplot(Data, aes(Other_t, KJ_wsound)) +
  geom_point(size = 2, color = "#1F968BFF") +
  geom_smooth(color = "#1F968BFF", method = lm) +
  labs( x = "Other calls (s)", y = "Energy expenditure (kJ)") +
  scale_color_viridis(discrete = TRUE)+
  scale_fill_viridis(discrete = TRUE) +
  theme (panel.background = element_blank())

o


d <- ggplot(Data, aes(Distress_t, KJ_wsound)) +
  geom_point(size = 2, color = "#95D840FF") +
  geom_smooth(color = "#95D840FF", method = lm) +
  labs( x = "Distress calls (s)", y = "Energy expenditure (kJ)") +
  scale_color_viridis(discrete = TRUE)+
  scale_fill_viridis(discrete = TRUE) +
  theme (panel.background = element_blank())

d


m <- ggplot(Data, aes(Mov_ws, KJ_wsound)) +
  geom_point(size = 2, color = "#404788FF") +
  geom_smooth(color = "#404788FF", method = lm) +
  labs( x = "Movement (min)", y = "Energy expenditure (kJ)") +
  scale_color_viridis(discrete = TRUE)+
  scale_fill_viridis(discrete = TRUE) +
  theme (panel.background = element_blank())

m


g <- ggplot(Data, aes(Mass, KJ_wsound)) +
  geom_point(size = 2, color = "#FDE725FF") +
  geom_smooth(color = "#FDE725FF", method = lm) +
  labs( x = "Mass (g)", y = "Energy expenditure (kJ)") +
  scale_color_viridis(discrete = TRUE)+
  scale_fill_viridis(discrete = TRUE) +
  theme (panel.background = element_blank())

g


figure3 <- ggarrange(r, e, o, d, m, g,
                    labels = c("A", "B", "C", "D", "E", "F"),
                    ncol = 3, nrow = 2)
figure3


ggsave("Figure3.png", width = 9, height = 6)




#Test whether time spent producing response calls is influenced by mass and RMR (no effect of mass, removed from model)

respmodel <- glm(Response_t ~ Sex * KJ_silent + Mass, family = negative.binomial(2), data= Data)

summary (respmodel)

anova(respmodel, test = "Chisq")


#Create scatter plot of RMR and time producing response calls

rmr <- ggplot(Data, aes(KJ_silent, Response_t)) +
  geom_point(aes(colour = Sex), size = 2) +
  geom_smooth(aes(colour = Sex), method = lm) +
  labs(x = expression("Energy expenditure while resting (kJ)"),
       y = expression("Response Calls (s)")) + 
  theme(legend.position = c(0.9, 0.85)) +
  scale_color_viridis(discrete = TRUE)+
  scale_fill_viridis(discrete = TRUE) +
  theme (panel.background = element_blank(), 
         panel.border = element_rect(colour = "black", fill=NA, size=1)) +
  coord_cartesian(xlim=c(1.5,10), ylim=c(-2, 80))

rmr

ggsave("Figure4.png", width = 6, height = 4)




#Compare slopes between males and females

anova(respmodel)

# Obtain slopes

respmodel$coefficients

m.lst <- lstrends(respmodel, "Sex", var="KJ_silent")

m.lst

# Compare slopes

pairs(m.lst)




