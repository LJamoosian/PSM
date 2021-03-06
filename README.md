# PSM
Propensity Score Matching


## Step 0
Math Plus is a program that help students to review before taking any basic or intrmediate math class at college(after high school) or review before placment test. The students who participate in this program counts as "treatment group" and non-participants are "control group". Since data is vary and the sample size are different between two groups of "MP" and "Non-MP", for the purpose of making both group balance, we were using PSM approach.

## Step 1
The most important part of any analysis is data preparation and desinging (80% to 90% of work). Be ready for lots of data cleaning and data manipulation. 

## Step 2
Test some independent variables and find significant one and descriptive statistics.

t.test(dt$AgeYears[dt$MathPlus==1],dt$AgeYears[dt$MathPlus==0])
t.test(dt$Cumulative.GPA[dt$MathPlus==1],dt$Cumulative.GPA[dt$MathPlus==0])
dt%>%group_by(MathPlus)%>%summarise(n_students=n(), Proportion_Success_Math152=mean(SUCCESS), STD_Error=sd(SUCCESS)/sqrt(n_students))

dt%>%mutate(test=(SUCCESS-mean(SUCCESS)/sd(SUCCESS)))%>%group_by(MathPlus)%>%summarise(Mean_Int_Alg=mean(test))

##Non-pramatric test for propotion ratio 

btab=table(dt$MathPlus,dt$SUCCESS)
mcnemar.test(btab,correct = F)


## Step 3
Running glm to find significant variables (by checking BIC, AIC, Adjusted R-square,...) and other model selection methods. Another approach is running PCA (Principal Commponent Analysis).
psmol1=glm(dt$SUCCESS ~ MathPlus+GENDER+Ethnicity+URM+AgeYears+Cumulative.GPA+Type,data=dt,family=binomial(link="logit"))
summary(psmol1)



## Step 4

Running some model by "MatchIt" package (Nearest Neighbor and Optimal methods) and "twang" package.

#### Ratio 1:1 treatment:Control
opm.out1=matchit(MathPlus ~  GENDER+ URM+Cumulative.GPA+ + AgeYears + HighestMathPlacement ,data =dt,
                method = "optimal", distance = "logit", ratio=1)
summary(opm.out1)
d1=match.data(opm.out1)
write.xlsx(d1,"D:/opmd1.xls",showNA = F)
##### Ratio 1:2
opm.out2=matchit(MathPlus ~  GENDER+ URM+Cumulative.GPA+ + AgeYears + HighestMathPlacement ,data =dt,
                 method = "optimal", distance = "logit", ratio=2)
summary(opm.out2)
d2=match.data(opm.out2)
write.xlsx(d2,"D://opmd2.xls",showNA = F)
##### Ratio 1:3
opm.out3=matchit(MathPlus ~  GENDER+ URM+Cumulative.GPA+ + AgeYears + HighestMathPlacement ,data =dt,
                 method = "optimal", distance = "logit", ratio=3)
summary(opm.out3)
d3=match.data(opm.out3)
write.xlsx(d3,"D:/opmd3.xls",showNA = F)
#### Ratio 1:4 treatment :Control
opm.out4=matchit(MathPlus  ~  GENDER+ URM+Cumulative.GPA+ + AgeYears + HighestMathPlacement ,data =dt,
                method = "optimal", distance = "logit", ratio=4)
summary(opm.out4)
d4=match.data(opm.out4)
write.xlsx(d4,"D:/opmd4.xls",showNA = F)

##### ATT (average Treatment effect Treated) for MatchIt Optimal
ated1=lm(SUCCESS ~ MathPlus+ GENDER+ URM+Cumulative.GPA + AgeYears + HighestMathPlacement ,data =d1,weights=d1$weights)
summary(ated1)
ated2=lm(SUCCESS ~ MathPlus+ GENDER+ URM+Cumulative.GPA + AgeYears + HighestMathPlacement ,data =d2,weights=d2$weights)
summary(ated2)
ated3=lm(SUCCESS ~ MathPlus+ GENDER+ URM+Cumulative.GPA + AgeYears + HighestMathPlacement ,data =d3,weights=d3$weights)
summary(ated3)
ated4=lm(SUCCESS ~ MathPlus+ GENDER+ URM+Cumulative.GPA + AgeYears + HighestMathPlacement ,data =d4,weights=d4$weights)
summary(ated4)

##### ATT for MatchIt Optimal
ated1=lm(SUCCESS ~ MathPlus+ GENDER+ URM+Cumulative.GPA + AgeYears + HighestMathPlacement ,data =d1,weights=d1$weights)
summary(ated1)
ated2=lm(SUCCESS ~ MathPlus+ GENDER+ URM+Cumulative.GPA + AgeYears + HighestMathPlacement ,data =d2,weights=d2$weights)
summary(ated2)
ated3=lm(SUCCESS ~ MathPlus+ GENDER+ URM+Cumulative.GPA + AgeYears + HighestMathPlacement ,data =d3,weights=d3$weights)
summary(ated3)
ated4=lm(SUCCESS ~ MathPlus+ GENDER+ URM+Cumulative.GPA + AgeYears + HighestMathPlacement ,data =d4,weights=d4$weights)
summary(ated4)

############################# MatchIt ::::: Nearest Neighbor 
####Ratio 1:4 treatment:Control
nn.out=matchit(MathPlus ~  GENDER+ URM+Cumulative.GPA+ + AgeYears + HighestMathPlacement ,data =dt,
                 method = "nearest", distance = "logit",subclass="subclass", ratio=4, group=all)
summary(nn.out)
n4=match.data(nn.out)
write.xlsx(n4,"D:/nn4.xls",showNA = F)
#### ATT for MatchIt Nearest Neighbor
atenn4=lm(SUCCESS ~ MathPlus+GENDER+URM+Cumulative.GPA+ AgeYears+HighestMathPlacement ,data =n4,weights=n4$weights)
summary(atenn4)

###############################   twang
tw.out=ps(MathPlus ~  GENDER+ URM+Cumulative.GPA+ + AgeYears + HighestMathPlacement ,data =dt
          ,verbose = FALSE, estimand = "ATE")
summary(tw.out)

## Step 5
##### After match -test (with new matched data set)
d4%>%group_by(MathPlus)%>%summarise(n_students=n(), Proportion_Success_Math152=mean(SUCCESS), STD_Error=sd(SUCCESS)/sqrt(n_students))

atab=table(d4$MathPlus,d4$SUCCESS)
mcnemar.test(atab,correct = F)
d4%>%mutate(test=(SUCCESS-mean(SUCCESS)/sd(SUCCESS)))%>%group_by(MathPlus)%>%summarise(Mean_Int_Alg=mean(test))


## Step 6 Plot diagnostics through graphics

plot(opm.out3, type="jitter")
plot(opm.out3, type="hist")
plot(opm.out3, type="QQ")

## Step 7 Sensitivity Analysis :: 
Assess if one's estimated based on matching is robust to the possible presence of an unobserved confounder(key assumption of matching).
Signed Rank Test is the non-paraM altr to paired t-test and it designed to evaluate comparision in paired data
how it works? statistics is taking  ranked "absolute differences of paired data" (resistance to outliers and more robust for usual parametric altr)

### Sensitivity Analysis for Optimal of MatchIt packge with Ratio 1:3
y=d3$SUCCESS
trt=d3$MathPlus
ps=glm(trt ~ GENDER+Ethnicity+URM+AgeYears+Cumulative.GPA+Type , data=d3,  family = binomial() )
match=Match(Y=y, Tr=trt, X=ps$fitted, replace=FALSE)

### Rosenbaum Sensitivity Test for Wilcoxon Signed Rank
#install.packages("Matching", dependencies = TRUE)
library(rbounds)
psens(match, Gamma=2, GammaInc=0.1)

### Sensitivity Analysis for Optimal of MatchIt packge with Ratio 1:1
y=d1$SUCCESS
trt=d1$MathPlus
ps=glm(trt ~ GENDER+Ethnicity+URM+AgeYears+Cumulative.GPA+Type , data=d1,  family = binomial() )
match=Match(Y=y, Tr=trt, X=ps$fitted, replace=FALSE)

### Rosenbaum Sensitivity Test for Wilcoxon Signed Rank H0= delta=0
psens(match, Gamma=2, GammaInc=0.1)

## Conclusion:Unconfounded is 0.103 for first analysis and for second one is 0.5, it means in first data set (d3) the distribution of the data under the null hypothesis satisfies exchangeability. There is no hidden bias due to an unobserved confounder
##### Hodges-Lehmann point estimate

hlsens(match, Gamma=1.5, GammaInc = .1,.1)


## Extra Sources: 

http://www-stat.wharton.upenn.edu/~rosenbap/packpaper.pdf

http://www.mattblackwell.org/files/papers/sens.pdf

https://www.researchgate.net/publication/251969933_An_Overview_of_rbounds_An_R_Package_for_Rosenbaum_Bounds_Sensitivity_Analysis_with_Matched_Data




