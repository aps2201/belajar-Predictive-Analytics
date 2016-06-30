library(arules)
data(Groceries)
rules = apriori(Groceries, parameter =
                  list(support = 0.05, confidence = .1))
inspect(rules)

library(vcdExtra)
data(ICU)
summary(ICU)

ICU2 = ICU[-4]
ICU2$age = cut(ICU2$age, breaks = 4)
ICU2$systolic = cut(ICU2$systolic, breaks = 4)
ICU2$hrtrate = cut(ICU2$hrtrate, breaks = 4)

ICU_tr = as(ICU2, "transactions")

agerec = discretize(ICU$age, method="frequency",
                    categories=4)
rules = apriori (ICU_tr,
                 parameter = list(support = .85, confidence = .95))


IM = interestMeasure(rules, "fishersExactTest", ICU_tr)
round(IM, digits=2)

rulesDeath = apriori(ICU_tr,
                     parameter = list(confidence = 0.3,support=.1),
                     appearance = list(rhs = c("died=Yes"), default="lhs"))
inspect(rulesDeath)

rulesComa = subset(rules, subset = rhs %in%
                     "coma=None")
rulesDeath.df = as(rulesDeath,"data.frame")
rulesDeath.df.sorted =
  rulesDeath.df[order(rulesDeath.df$lift,decreasing = T),]
head(rulesDeath.df.sorted)

