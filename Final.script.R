View(Real)
attach(Real)
names(Real)
library(dplyr)
library(dotwhisker)
library(stargazer)
library(estimatr)
library(jtools)

#Renaming columns
names(Real)[names(Real) == "A1"] <- "region"
names(Real)[names(Real) == "A2"] <- "gender"
names(Real)[names(Real) == "A3"] <- "age_int"
names(Real)[names(Real) == "A3_n2"] <- "age"
names(Real)[names(Real) == "A4"] <- "education"
names(Real)[names(Real) == "A5"] <- "mil_serv"
names(Real)[names(Real) == "A6"] <- "mil_serv_rel"
names(Real)[names(Real) == "A7"] <- "religion"
names(Real)[names(Real) == "A8"] <- "rel_freq"
names(Real)[names(Real) == "A9"] <- "pol_id"
names(Real)[names(Real) == "A10"] <- "pol_party"
names(Real)[names(Real) == "A11"] <- "pol_inter"
names(Real)[names(Real) == "A12"] <- "employment"
names(Real)[names(Real) == "A13"] <- "travel"

#test model to see if code is running correctly
model1 <- lm(B4 ~ B16 + A9)
summary(model1)

#dummy variable for military service, 0 is no 1 is yes
Milservdummy=dummy_cols(Mil_Serv)

#creating dummy variables
table(Real$mil_serv)

fd <- Real %>% 
  mutate(., mil_serv_real = as.numeric(mil_serv == 1 ),
         apology = as.numeric(B24%in% c(1, 2, 3)),
         employ = as.numeric(employment%in% c(1, 2, 3, 4)),
         educate = as.numeric(education%in% c(6,7)),
         g.avg  = rowMeans(select(., B1, B2, B3, B4), na.rm = TRUE),
         u.avg = rowMeans(select(., B5, B6, B7, B8), na.rm = TRUE),
         s.avg = rowMeans(select(., B9, B10, B11, B14), na.rm = TRUE)) %>% 
  mutate(., is.g = as.numeric(g.avg <= 3)) %>%
  mutate(., is.u = as.numeric(u.avg <=3)) %>%
  mutate(., is.s = as.numeric(s.avg <=3)) %>%
  mutate(., monetary_comp = as.numeric(B23%in% c(1, 2))) %>%
  mutate(., veteran_comp = as.numeric(B21%in% c(1, 2))) %>%
  mutate(., apology_1 = as.numeric(B18%in% c(1, 2))) %>%
  mutate(., polidbin = as.numeric(pol_id%in% c(1, 2, 3))) %>%
  mutate(., g.avg_r  = rowMeans(select(., B1, B2, B3, B4), na.rm = TRUE),
         u.avg_r = rowMeans(select(., B5, B6, B7, B8), na.rm = TRUE),
         s.avg_r= rowMeans(select(., B9, B10, B11, B14), na.rm = TRUE)) %>%
  mutate(apology_test = as.numeric(B18%in% c(1, 2, 3)))
names(fd)
attach(fd)

#regression with dummy variables template
lm(DV ~ x + y + as.factor(is.g) + as.factor(is.u) + as.factor(is.s))

#regression for g nationalism
g_nati <- lm(apology ~ is.g + educate + employ + gender + mil_serv_real + age + pol_id)
summary(g_nati)

#regression for u nationalism
u_nati <- lm(apology ~ is.u + educate + employ + gender + mil_serv_real + age)  
summary(u_nati)  

#regression for s nationalism
s_nati <- lm(apology ~ is.s + educate +employ + gender + mil_serv_real + age)
summary(s_nati)

#regression with all combined
all_nati <- lm(apology ~ is.u + is.g + is.s + educate + employ + gender + mil_serv_real + age + pol_id)
summary(all_nati)

#regression of second DV
all_nati1 <- lm(monetary_comp ~ is.u + is.g + is.s + educate + employ + gender + mil_serv_real + age + pol_id)
summary(all_nati1)

#regression for third DV
all_nati2 <- lm(veteran_comp ~ is.u + is.g + is.s + educate + employ + gender + mil_serv_real + age + pol_id)
summary(all_nati2)

#regression for fourth DV
all_nati3 <- lm(apology_1 ~ is.u + is.g + is.s + educate + employ + gender + mil_serv_real + age + pol_id)
summary(all_nati3)

all_natit <- lm(apology_test ~ is.u + is.g + is.s + educate + employ + gender + mil_serv_real + age + pol_id)
summary(all_natit)

#regression of apology opp as ordinal
beight1<- lm(B18 ~ is.u + is.g + is.s + educate + employ + gender + mil_serv_real + age + pol_id)
summary(beight1)

#logistic regressions
logit <- glm(apology ~ is.u + is.g + is.s + educate + employ + gender + mil_serv_real + polidbin + as.factor(age), family=binomial)
summary(logit)

logit1 <- glm(monetary_comp ~ is.u + is.g + is.s + educate + employ + gender + mil_serv_real + as.factor(age) + as.factor(pol_id), family = binomial)
summary(logit1)

logit2 <- glm(veteran_comp ~ is.u + is.g + is.s + educate + employ + gender + mil_serv_real + as.factor(age) + as.factor(pol_id), family = binomial)
summary(logit2)

logit3 <- glm(apology_1 ~ is.u + is.g + is.s + educate + employ + gender + mil_serv_real + as.factor(age) + as.factor(pol_id), family = binomial)
summary(logit3)

nat.test <- lm(g.avg_r ~ u.avg_r + s.avg_r)
summary(nat.test)

#robust regressions
robust <- lm_robust(apology ~ is.u + is.g + is.s + educate + employ + gender + mil_serv_real + age + pol_id)
summary(robust)

robust1 <- lm_robust(monetary_comp ~ is.u + is.g + is.s + educate + employ + gender + mil_serv_real + age + pol_id)
summary(robust1)

robust2 <- lm_robust(veteran_comp ~ is.u + is.g + is.s + educate + employ + gender + mil_serv_real + age + pol_id)
summary(robust2)

robust3 <- lm_robust(apology_1 ~ is.u + is.g + is.s + educate + employ + gender + mil_serv_real + age + pol_id)
summary(robust3)

#regressions for not dummy (key independent)
result <- lm_robust(apology ~ g.avg_r + u.avg_r + s.avg_r + educate + employ + gender + mil_serv_real + age + pol_id)
summary(result)

result1 <- lm_robust(monetary_comp ~ g.avg_r + u.avg_r + s.avg_r + educate + employ + gender + mil_serv_real + age + pol_id)
summary(result1)

result2 <- lm_robust(veteran_comp ~ g.avg_r + u.avg_r + s.avg_r + educate + employ + gender + mil_serv_real + age + pol_id)
summary(result2)

result3 <- lm_robust(apology_1 ~ g.avg_r + u.avg_r + s.avg_r + educate + employ + gender + mil_serv_real + age + pol_id)
summary(result3)

#Regressions using averages of key independent variables
stuck <- lm(apology ~ g.avg_r + u.avg_r + s.avg_r + educate + employ + gender + mil_serv_real + age + pol_id)
summary(stuck)

stuck1 <- lm(monetary_comp ~ g.avg_r + u.avg_r + s.avg_r + educate + employ + gender + mil_serv_real + age + pol_id)
summary(stuck1)

stuck2 <- lm(veteran_comp ~ g.avg_r + u.avg_r + s.avg_r + educate + employ + gender + mil_serv_real + age + pol_id)
summary(stuck2)

stuck3 <- lm(apology_1 ~ g.avg_r + u.avg_r + s.avg_r + educate + employ + gender + mil_serv_real + age + pol_id)
summary(stuck3)

stuck3n <- lm(apology_test ~ g.avg_r + u.avg_r + s.avg_r + educate + employ + gender + mil_serv_real + age + pol_id)
summary(stuck3n)

#regression of apology opp as ordinal
beight <- lm(B18 ~ g.avg_r + u.avg_r + s.avg_r + educate + employ + gender + mil_serv_real + age + pol_id)
summary(beight)

generation <- lm(g.avg_r ~ age)
summary(generation)
  
opinion <- lm(B20 ~ g.avg_r + u.avg_r + s.avg_r + educate + employ + gender + mil_serv_real + age + pol_id)
summary(opinion)

pol_nat <- lm(pol_id ~ g.avg_r + u.avg_r + s.avg_r)
summary(pol_nat)

generalnat <- lm(g.avg_r ~ u.avg_r + s.avg_r)
summary(generalnat)

#summary statistics
summ(all_nati3, confint = TRUE, digits = 3)
summ(all_nati1, confint = TRUE, digits = 3)
summ(all_nati, confint = TRUE, digits = 3)
summ(all_nati2, confint = TRUE, digits = 3)

#creation of tables
stargazer(all_nati)
stargazer(all_nati1)
stargazer(all_nati2)
stargazer(all_nati3)

stargazer (all_nati, all_nati1, all_nati2, all_nati3,
           se = starprep(all_nati, all_nati1, all_nati2, all_nati3),
           p = starprep(all_nati, all_nati1, all_nati2, all_nati3, stat = "p.value"),
           omit.stat = "f")
stargazer (stuck, stuck1, stuck2, stuck3,
           se = starprep(stuck, stuck1, stuck2, stuck3),
           p = starprep(stuck, stuck1, stuck2, stuck3, stat = "p.value"),
           omit.stat = "f")
stargazer(stuck, stuck1, stuck2, stuck3)
stargazer(all_nati, all_nati1, all_nati2, all_nati3)

#plot creation
plot_nati <- update(all_nati)
plot_nati1 <- update(all_nati1)
plot_nati2 <- update(all_nati2)
plot_nati3 <- update(all_nati3)
dwplot(list(plot_nati, plot_nati1, plot_nati2, plot_nati3),
       vline = geom_vline(
         xintercept = 0,
         colour = "grey60",
         linetype = 2
       ),
       vars_order = c("is.g", "is.u", "is.s", "educate", "employ", "gender", "mil_serv_real", "age", "pol_id")) %>%
  relabel_predictors(
    c(
      is.g = "General",
      is.u = "Universal",
      is.s = "State",
      educate = "Education",
      employ = "Employment",
      gender = "Gender",
      mil_serv_real = "Mil_Service",
      age = "Age",
      pol_id = "Pol_Id"
    )
  ) +
  ggtitle("Opinion on the Vietnam War") +
  xlab("Coefficient Estimate") + ylab("") +
scale_colour_hue(
  name = "Dependent Variables",
  labels = c("Apology", "Monetary Comp", "Veteran Comp", "Apology Opp")
)


plot_stuck <- update(stuck)
plot_stuck1 <- update(stuck1)
plot_stuck2 <- update(stuck2)
plot_stuck3 <- update(stuck3)
dwplot(list(plot_stuck, plot_stuck1, plot_stuck2, plot_stuck3),
       vline = geom_vline(
         xintercept = 0,
         colour = "grey60",
         linetype = 2
       ),
       vars_order = c("g.avg_r", "u.avg_r", "s.avg_r", "educate", "employ", "gender", "mil_serv_real", "age", "pol_id")) %>%
  relabel_predictors(
    c(
      g.avg_r = "General",
      u.avg_r = "Universal",
      s.avg_r = "State",
      educate = "Education",
      employ = "Employment",
      gender = "Gender",
      mil_serv_real = "Mil_Service",
      age = "Age",
      pol_id = "Pol_Id"
    )
  ) +
  ggtitle("Opinion on the Vietnam War (Key IV Not Binary)") +
  xlab("Coefficient Estimate") + ylab("") +
  scale_colour_hue(
    name = "Dependent Variables",
    labels = c("Apology", "Monetary Comp", "Veteran Comp", "Apology Opp")
  )

#miscellaneous findings
table(Real$B24)
table(Real$B18)


robust.table <- starprep(
  all_nati, all_nati1, all_nati2, all_nati3,
  stat = c("std.error", "statistic", "p.value", "ci", "df"),
  se_type = NULL,
  clusters = NULL,
  alpha = 0.05
)
