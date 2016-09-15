library(MASS)
set.seed(999)
Covariances = read.table("Data/Data Ch12/Covariances.dat", sep = "\t", header=T)
df = data.frame(matrix(nrow=0,ncol=5))
colnames(df) = c("Hospital","Accomp","Depers","Exhaus","WorkSat")
for (i in 1:17){
  if(i == 1) {start_ln = 1}
  else start_ln = 1+((i-1)*4)
  end_ln = start_ln + 3
  covs = Covariances[start_ln:end_ln, 3:6]
  rownames(covs)=Covariances[start_ln:end_ln,2]
  dat=mvrnorm(n=100, c(rep(0,4)), covs)
  df = rbind(df,dat)
}

df$hosp = as.factor(c(rep(1,100), rep(2,100), rep(3,100),
                      rep(4,100), rep(5,100), rep(6,100),
                      rep(7,100),rep(8,100),rep(9,100),
                      rep(10,100),rep(11,100),rep(12,100),
                      rep(13,100),rep(14,100),rep(15,100),
                      rep(16,100),rep(17,100)))

library(lattice)
attach(df)
xyplot(WorkSat~Depers | hosp, panel = function(x,y) {
  panel.xyplot(x,y)
  panel.lmline(x,y)
})

#multilevel modelling
NursesML = read.table("Data/Data Ch12/nursesML.dat", header = T, sep = " ")
means = aggregate(NursesML[,4], by=list(NursesML[,5]),
                  FUN=mean)[2]

var(unlist(means)) #at the hospital level
var(NursesML[,4]) #at the observation level

library(lme4)
NursesML$hosp = factor(NursesML$hosp)

null = lmer(WorkSat ~ 1 + (1|hosp), data=NursesML)

summary(null)

mean(NursesML[,4])

coef(null)
ranef(null)

#random intercept & fixed slopes
NursesMLtrain = read.table("Data/Data Ch12/NursesMLtrain.dat",
                             header = T, sep = " ")
NursesMLtest = read.table("Data/Data Ch12/NursesMLtest.dat",
                          header = T, sep = " ")
NursesMLtrain$hosp = factor(NursesMLtrain$hosp)
NursesMLtest$hosp = factor(NursesMLtest$hosp)

model = lmer(WorkSat~Accomp+Depers+Exhaust+(1|hosp),data=NursesMLtrain,REML = F)
null = lmer(WorkSat ~ 1 + (1|hosp), data=NursesMLtrain, REML = F)
anova(null, model)

