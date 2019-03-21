
#19.2Multiple Regression 代码

housing <- read.table("housing.csv",
                      sep = ",", header = TRUE,
                      stringsAsFactors = FALSE)
names(housing) <- c("Neighborhood", "Class", "Units", "YearBuilt",
                    "SqFt", "Income", "IncomePerSqFt","Expense",
                    "ExpensePerSqFt","NetIncome", "Value",
                    "ValuePerSqFt", "Boro")
head(housing)
library(ggplot2)
ggplot(housing, aes (x = ValuePerSqFt)) +
  geom_histogram(binwidth = 10) + labs(x = "Value per Square Foot")

ggplot(housing, aes(x = ValuePerSqFt, fill = Boro)) +
  geom_histogram(binwidth = 10) + labs(x = "Value Per Square Foot")

ggplot(housing, aes(x = ValuePerSqFt, fill = Boro)) +
  geom_histogram(binwidth = 10) + labs(x = "Value per Square Foot") +
  facet_wrap(~Boro)

ggplot(housing,aes(x = SqFt)) + geom_histogram()
ggplot(housing, aes(x = Units)) + geom_histogram()

ggplot(housing[housing$Units < 1000,],
       aes(x = SqFt)) + geom_histogram()
ggplot(housing[housing$Units < 1000, ],
       aes(x = Units)) + geom_histogram()

ggplot(housing, aes(x = SqFt, y = ValuePerSqFt)) + geom_point()
ggplot(housing, aes(x = Units, y = ValuePerSqFt)) + geom_point()
ggplot(housing[housing$Units<1000,],aes(x=SqFt,y=ValuePerSqFt))+geom_point()
ggplot(housing[housing$Units<1000,],aes(x=Units,y=ValuePerSqFt))+geom_point()

#  determine the number of buildings with 1000 or more units
sum(housing$Units >= 1000)

#  Remove buildings with 1000 or more units
housing <- housing[housing$Units < 1000, ]

#  plot value per sqft against sq feet
ggplot(housing, aes(x = SqFt, y = ValuePerSqFt)) + geom_point()
ggplot(housing, aes(x = log(SqFt), y = ValuePerSqFt)) + geom_point()
ggplot(housing, aes(x = SqFt, y = log(ValuePerSqFt))) + geom_point()
ggplot(housing, aes(x = log(SqFt), y = log(ValuePerSqFt))) + geom_point()

#  plot valueperSqFt against number of units
ggplot(housing, aes(x = Units, y= ValuePerSqFt)) + geom_point()
ggplot(housing, aes(x = log(Units), y = ValuePerSqFt)) + geom_point()
ggplot(housing, aes(x = Units, y = log(ValuePerSqFt))) + geom_point()
ggplot(housing, aes(x = log(Units), y = log(ValuePerSqFt)))+ geom_point()

house1 <- lm(ValuePerSqFt ~ Units+SqFt + Boro, data = housing)
summary(house1)

house1$coefficients
coef(house1)
coefficients(house1)

library(coefplot)
require(coefplot)
coefplot(house1)

house2 <- lm(ValuePerSqFt ~ Units*SqFt + Boro, data = housing)
house3 <- lm(ValuePerSqFt ~ Units:SqFt + Boro, data = housing)
house2$coefficients
house3$coefficients

coefplot(house2)
coefplot(house3)

house4 <- lm(ValuePerSqFt ~ SqFt*Units*Income, housing)
house4$coefficients
house5 <- lm(ValuePerSqFt ~ Class * Boro, housing)
house5$coefficients
coefplot(house1,sort='mag'+scale_x_continuous(limits=c(-.25,.1)))
coefplot(house1,sort='mag'+scale_x_continuous(limits=c(-.0005,.0005)))

house1.b<-lm(ValuePerSqFt ~ scale(Units)+scale(SqFt)+Boro,data=housing)
coefplot(house1.b,sort='mag')

house6 <- lm(ValuePerSqFt ~ I(SqFt/Units) + Boro, housing)
house6$coefficients
house7 <- lm(ValuePerSqFt ~ (Units + SqFt)^2, housing)
house7$coefficients
house8 <- lm(ValuePerSqFt ~Units* SqFt, housing)
house8$coefficients
house9 <- lm(ValuePerSqFt ~ I(Units+SqFt)^2, housing)
house9$coefficients

#  also from the coefplot package
multiplot(house1, house2, house3)

housingNew <- read.table("housingNew.csv",sep = ",", header = TRUE,
                         stringsAsFactors = FALSE)

housePredict <- predict(house1, newdata = housingNew, se.fit = TRUE,
                        interval = "prediction", level = 0.95)

head(housePredict$fit)

head(housePredict$se.fit)

#20.1 Logistic Regression

acs <- read.table("acs_ny.csv", sep = ",",
                  header = TRUE, stringsAsFactors = FALSE)
acs$Income <- with(acs, FamilyIncome >= 150000)
library(ggplot2)
library(useful)

ggplot(acs,aes(x=FamilyIncome))+geom_density(fill="grey",color="grey")+geom_vline(xintercept=150000)+scale_x_continuous(label=multiple.dollar,limits=c(0,1000000))
head(acs)
income1<-glm(Income ~ HouseCosts+NumWorkers+OwnRent+NumBedrooms+FamilyType,data=acs,family=binomial(link="logit"))
summary(income1)

library(coefplot)
coefplot(income1)

invlogit<-function(x){1/(1+exp(-x))}
invlogit(income1$coefficients)


ggplot(acs,aes(x=NumChildren))+geom_histogram(binwidth=1)
children1<-glm(NumChildren ~ FamilyIncome +FamilyType +OwnRent,data=acs,family=poisson(link="log"))
summary(children1)
coefplot(children1)

z<-(acs$NumChildren - children1$fitted.values)/sqrt(children1$fitted.values)
sum(z^2)/children1$df.residual
pchisq(sum(z^2),children1$df.residual)

children2<-glm(NumChildren ~ FamilyIncome +FamilyType +OwnRent,data=acs,family=quasipoisson(link="log"))
multiplot(children1,children2)

ggplot(acs, aes(x = NumChildren)) + geom_histogram(binwidth = 1)
children1 <- glm(NumChildren ~ FamilyIncome + FamilyType + OwnRent,
                 data = acs, family = poisson(link = "log"))


#20.4 survival Analysis
library(survival)
library(ggplot2)
library(useful)
head(bladder)
bladder[100:105,]
survObject <- with(bladder[100:105, ], Surv(stop, event))
survObject
survObject[, 1:2]

cox1 <- coxph(Surv(stop, event) ~ rx + number + size + enum,
              data = bladder)
summary(cox1)

plot(survfit(cox1), xlab = "Days", ylab = "Survival Rate",
     conf.int = TRUE)
cox2 <- coxph(Surv(stop, event) ~ strata(rx) + number + size + enum,
              data = bladder)
summary(cox2)
plot(survfit(cox2), xlab = "Days", ylab = "Survival Rate",
     conf.int = TRUE, col = 1:2)
legend("bottomleft", legend = c(1, 2), lty = 1, col = 1:2,
       text.col = 1:2, title = "RX")

cox.zph(cox1)
cox.zph(cox2)
head(bladder2)
ag1 <- coxph(Surv(start, stop, event) ~ rx + number + size+enum+ cluster(id), data = bladder2)
ag2 <- coxph(Surv(start, stop, event) ~ strata(rx) + number + size + enum +
               cluster(id), data = bladder2)
plot(survfit(ag1), conf.int = TRUE)
plot(survfit(ag2), conf.int = TRUE, col = 1:2)
legend("topright", legend = c(1, 2), lty = 1, col = 1:2,
       text.col = 1:2, title = "rx")

#21.1 residuals

housing <- read.table("housing.csv", sep = ",", header = TRUE, stringsAsFactors = FALSE)
#  Give the data good names
names(housing) <- c("Neighborhood", "Class", "Units", "YearBuilt",
                    "SqFt", "Income", "IncomePerSqFt", "Expense",
                    "ExpensePerSqFt", "NetIncome", "Value",
                    "ValuePerSqFt", "Boro")
housing<-housing[housing$Units<1000,]
head(housing)
#  eliminate some outliers
housing <- housing[housing$Units < 1000, ]
head(housing)
#  fit a model
house1 <-  lm(ValuePerSqFt ~ Units + SqFt + Boro, data = housing)
summary(house1)
# visualize the model
library(coefplot)
library(ggplot2)
coefplot(house1)
#  see what a fortified lm model looks like
head(fortify(house1))

#  save a plot to an object
#  notice we are using th created columns for the x and y axes
#  they are .fitted and .resid
h1 <- ggplot(aes(x = .fitted, y = .resid), data = house1) +
  geom_point() + 
  geom_hline(yintercept = 0) +
  geom_smooth(se = FALSE) +
  labs(x = "Fitted Value", y = "Residuals")

#  print the plot
h1
h1 <- h1 + geom_point(aes(color = Boro))
h1

#  Basic plot
plot(house1, which = 1)
#  same plot but colored by Boro
# plot(house1, which = 1, col = as.numeric(factor(house1$mode1$Boro)))
legend("topright", legend = levels(factor(house1$model$Boro)), pch = 1,
       col = as.numeric(factor(levels(factor(house1$model1$Boro)))),
       text.col = as.numeric(factor(levels(factor(house1$model1$Boro)))),
       title = "Boro")

plot(house1, which = 2)
ggplot(house1, aes(sample = .stdresid)) + stat_qq() + geom_abline()
ggplot(house1, aes(x = .resid)) + geom_histogram()

#21.2 comparing Models
#  Chapter 18 Model Diagnostics 18.3 Cross Validation
housing <- read.table("housing.csv", sep = ",", header = TRUE, stringsAsFactors = FALSE)
#  Give the data good names
names(housing) <- c("Neighborhood", "Class", "Units", "YearBuilt",
                    "SqFt", "Income", "IncomePerSqFt", "Expense",
                    "ExpensePerSqFt", "NetIncome", "Value",
                    "ValuePerSqFt", "Boro")
#  eliminate some outliers
housing <- housing[housing$Units < 1000, ]
head(housing)
#  fit a model
house1 <-  lm(ValuePerSqFt ~ Units + SqFt + Boro, data = housing)
summary(house1)
# visualize the model
library(coefplot)
coefplot(house1)
library(ggplot2)
#  see what a fortified lm model looks like
head(fortify(house1))
#  save a plot to an object
#  notice we are using th created columns for the x and y axes
#  they are .fitted and .resid
h1 <- ggplot(aes(x = .fitted, y = .resid), data = house1) +
  geom_point() + 
  geom_hline(yintercept = 0) +
  geom_smooth(se = FALSE) +
  labs(x = "Fitted Value", y = "Residuals")
#  print the plot
h1
h1 + geom_point(aes(color = Boro))
#  Basic plot
plot(house1, which = 1)
#  same plot but colored by Boro
# plot(house1, which = 1, col = as.numeric(factor(house1$mode1$Boro)))
legend("topright", legend = levels(factor(house1$model$Boro)), pch = 1,
       col = as.numeric(factor(levels(factor(house1$model1$Boro)))),
       text.col = as.numeric(factor(levels(factor(house1$model1$Boro)))),
       title = "Boro")
plot(house1, which = 2)
ggplot(house1, aes(sample = .stdresid)) + stat_qq() + geom_abline()
ggplot(house1, aes(x = .resid)) + geom_histogram()

#上面和21.1一样，以下不一样
house2 <- lm(ValuePerSqFt ~ Units*SqFt + Boro, data = housing)
house3 <- lm(ValuePerSqFt ~ Units + SqFt*Boro + Class,
             data = housing)
house4 <- lm(ValuePerSqFt ~ Units + SqFt*Boro + SqFt*Class,
             data = housing)
house5 <- lm(ValuePerSqFt ~ Boro + Class, data = housing)

multiplot(house1, house2, house3, house4, house5, pointsize = 2)

anova(house1, house2, house3, house4, house5)

AIC(house1, house2, house3, house4, house5)

BIC(house1, house2, house3, house4, house5)

#21.3 cross-validation
library(boot)
houseG1<-glm(ValuePerSqFt ~ Units+SqFt +Boro,data=housing,family=gaussian(link="identity"))
identical(coef(house1),coef(houseG1))
houseCV1<-cv.glm(housing,houseG1,K=5)
houseCV1$delta

houseG2<-glm(ValuePerSqFt ~ Units *SqFt +Boro,data=housing)
houseG3<-glm(ValuePerSqFt ~ Units +SqFt *Boro+Class,data=housing)
houseG4<-glm(ValuePerSqFt ~ Units +SqFt *Boro+SqFt*Class,data=housing)
houseG5<-glm(ValuePerSqFt ~ Boro +Class,data=housing)

houseCV2<-cv.glm(housing,houseG2,K=5)
houseCV3<-cv.glm(housing,houseG3,K=5)
houseCV4<-cv.glm(housing,houseG4,K=5)
houseCV5<-cv.glm(housing,houseG5,K=5)

cvResults<-as.data.frame(rbind(houseCV1$delta,houseCV2$delta,houseCV3$delta,houseCV4$delta,houseCV5$delta))
names(cvResults)<-c("Error","Adjusted.Error")
cvResults$Model<-sprintf("houseG%s",1:5)
cvResults

cvANOVA<-anova(houseG1,houseG2,houseG3,houseG4,houseG5)
cvResults$ANOVA<-cvANOVA$`Resid.Dev`
cvResults<-AIC(houseG1,houseG2,houseG3,houseG4,houseG5)$AIC
library(reshape2)

library(reshape2)

cvMelt<-melt(cvResults,id.vars="Model",variable.name="Measure",value.name = "Value")
cvMelt
library(ggplot2)
#p316
ggplot(cvMelt,aes(x=Model,y=Value))+geom_line(aes(group=Measure,color=Measure))+facet_wrap(~Measure,scales="free_y")+theme(axis.text.x = element_text(angle=90,vjust=.5))+guides(color=FALSE)

cv.work<-function(fun,k=5,data,cost=function(y,yhat)mean((y-yhat)^2),response="y",...)
{folds<-data.frame(Fold=sample(rep(x=1:k,length.out=nrow(data))),Row=1:nrow(data))
error<-0
for(f in 1:max(folds$Fold))
{theRows<-folds$Row[folds$Fold==f]
  mod<-fun(data=data[-theRows,],...)
  pred<-predict(mod,data[theRows,])
  error<-error+
    cost(data[theRows,response],pred)*
    (length(theRows)/nrow(data))
}
return(error)
}


cv1<-cv.work(fun=lm,k=5,data=housing,response = "ValuePerSqFt",
             formula=ValuePerSqFt ~ Units +SqFt + Boro)
cv2<-cv.work(fun=lm,k=5,data=housing,response = "ValuePerSqFt",
             formula=ValuePerSqFt ~ Units *SqFt + Boro)
cv3<-cv.work(fun=lm,k=5,data=housing,response = "ValuePerSqFt",
             formula=ValuePerSqFt ~ Units +SqFt * Boro+Class)
cv4<-cv.work(fun=lm,k=5,data=housing,response = "ValuePerSqFt",
             formula=ValuePerSqFt ~ Units +SqFt * Boro+SqFt *Class)
cv5<-cv.work(fun=lm,k=5,data=housing,response = "ValuePerSqFt",
             formula=ValuePerSqFt ~ Boro + Class)
cvResults<-data.frame(Model=sprintf("house%s",1:5),
                      error=c(cv1,cv2,cv3,cv4,cv5))
cvResults

#21.4 bootstrap

library(plyr)
library(ggplot2)
baseball<-baseball[baseball$year>=1990,]
head(baseball)

bat.avg<-function(data,indices=1:NROW(data),hits="h",at.bats="ab")
{
  sum(data[indices,hits],na.rm=TRUE)/
    sum(data[indices,at.bats],na.rm=TRUE)
}
bat.avg(baseball)
#p320
avgBoot<-boot(data=baseball,statistic=bat.avg,R=1200,stype="i")
avgBoot
boot.ci(avgBoot,conf=.95,type="norm")
#p321
ggplot()+
  geom_histogram(aes(x=avgBoot$t),fill="grey",color="grey")+
  geom_vline(xintercept=avgBoot$t0+c(-1,1)*2*sqrt(var(avgBoot$t)),
             linetype=2)

#21.5stepwise variable selection

nullModel<-lm(ValuePerSqFt ~1,data=housing)
fullModel<-lm(ValuePerSqFt ~Units+SqFt*Boro+Boro*Class,data=housing)
houseStep<-step(nullModel,
                scope=list(lower=nullModel,upper=fullModel),
                direction="both")
houseStep

#22.1regularization and shrinkage
library(useful)
library(ggplot2)
library(glmnet)
library(Matrix)
library(foreach)
library(parallel)
library(doParallel)
library(iterators)
library(plyr)
library(stringr)
library(reshape2)
acs <-read.table(file ="acs_ny.csv", sep = ",", header = TRUE, stringsAsFactors = FALSE)
#  build a data.frame where the first three columns are numeric, the next two factors
testFrame <- data.frame(First = sample(1:10, 20, replace = TRUE),
                        Second = sample(1:20, 20, replace = TRUE),
                        Third = sample(1:10, 20, replace = TRUE),
                        Fourth = factor(rep(c("Alice", "Bob", "Charlie", "David"), 5)),
                        Fifth = ordered(rep(c("Edward", "Frank", "Georgia", "Hank", "Issac"), 4)),
                        Sixth = rep(c("a", "b"), 10), stringsAsFactors = F)
head(testFrame)
head(model.matrix(First ~Second+Fourth+Fifth,testFrame))

mod.mat <- model.matrix(First ~ Second + Fourth + Fifth, testFrame)
#  always use all levels
test.a <- build.x(First ~ Second + Fourth +Fifth, testFrame)
#  just use all levels for Fourth
test <- build.x(First ~ Second + Fourth + Fifth, testFrame, 
                contrasts = c(Fourth = FALSE, Fifth = TRUE))
#  make a binary incoime variable for building a logistic regression

acs$Income <- with(acs, FamilyIncome >= 150000)
head(acs)
#  build predictor matrix
acsX <- build.x(Income ~ NumBedrooms + NumChildren + NumPeople + NumRooms +NumUnits +
                  NumVehicles + NumWorkers + OwnRent + YearBuilt + ElectricBill + FoodStamp +
                  HeatingFuel + Insurance + Language -1, data = acs, contrasts = FALSE)
class(acsX)
dim(acsX)
topleft(acsX,c=6)
topright(acsX,c=6)
acsY <- build.y(Income ~ NumBedrooms + NumChildren + NumPeople + NumRooms +NumUnits +
                  NumVehicles + NumWorkers + OwnRent + YearBuilt + ElectricBill + FoodStamp +
                  HeatingFuel + Insurance + Language -1, data = acs)
head(acsY)
tail(acsY)
set.seed(1863561)
#  run the cross-validation glmnet
acsCV1 <- cv.glmnet(x = acsX, y = acsY, family = "binomial", nfold = 5)
acsCV1$lambda.min
acsCV1$lambda.1se
plot(acsCV1)
coef(acsCV1, s = "lambda.1se")

#  plot the path
plot(acsCV1$glmnet.fit, xvar = "lambda")
#  add vertical lines for optimal values of lambda
abline(v = log(c(acsCV1$lambda.min,  acsCV1$lambda.1se)), lty = 2)
#  fit the Ridge model
set.seed(71623)
acsCV2 <- cv.glmnet(x = acsX, y = acsY, family = "binomial", nfold = 5, alpha = 0)
#  Look at the lambda values
acsCV2$lambda.min
acsCV2$lambda.1se

#  look at the coefficients
coef(acsCV2, s = "lambda.1se")
#  plot the cross-validation error path
plot(acsCV2)
#  plot the coefficient path
plot(acsCV2$glmnet.fit, xvar = "lambda")
abline(v = log(c(acsCV2$lambda.min, acsCV2$lambda.1se)), lty = 2)

#  set the seed for repeatability for random results
set.seed(2834673)
#  create folds, we want observations to be in the same fold each time it is run
theFolds <- sample(rep(x = 1:5, length.out = nrow(acsX)))
#  make sequence of alpha values
alphas <- seq(from = 0.5, to = 1.0, by = 0.05)

set.seed(5127151)
#  start a cluster with two workers
c1 <- makeCluster(2)
#  register the workers
registerDoParallel(c1)

before<-Sys.time()
#  keep track of timing
#  Windows does not recognize Sys.Time()
#  build foreach loop to run in parallel
acsDouble <- foreach(i = 1:length(alphas), .errorhandling = "remove", .inorder = FALSE,
                     .multicombine = TRUE, .packages = "glmnet") %dopar%
                     {
                       print(alphas[i])
                       cv.glmnet(x = acsX, y = acsY, family = "binomial", nfolds = 5, foldid = theFolds, 
                                 alpha = alphas[i])
                     }
after<-Sys.time()
#  Windows does not recognize Sys.Time()
#  make sure to stop the cluster when done
stopCluster(c1)
after-before
#  time difference not computed due to Windows limitation
sapply(acsDouble, class)
#  function for extracting info from glmnet object
extractGlmnetInfo <- function(object)
{
  #  find lambda's
  lambdaMin <- object$lambda.min
  lambda1se <- object$lambda.1se
  # figure out where lambda's fall in the path
  whichMin <- which(object$lambda == lambdaMin)
  which1se <- which(object$lambda == lambda1se)
  #  build a one line data.frame with each of the selected lambda's
  #  and its corresponding error values
  data.frame(lambda.min = lambdaMin, error.min = object$cvm[whichMin],
             lambda.1se = lambda1se, error.1se = object$cvm[which1se])
}
#  apply that function to each element of the list
#  combine it all into a data.frame
alphaInfo <- Reduce(rbind, lapply(acsDouble, extractGlmnetInfo))
#  could also be do with ldply from pylr
alphaInfo2 <- plyr::ldply(acsDouble, extractGlmnetInfo)
identical (alphaInfo, alphaInfo2)

alphaInfo$Alpha <- alphas
alphaInfo

alphaMelt <- melt(alphaInfo, id.vars = "Alpha", value.name = "Value",
                  variable.name = "Measure")
alphaMelt$Type <- str_extract(string = alphaMelt$Measure,
                              pattern = "(min|1se)")
#  some housekeeping
alphaMelt$Measure <- str_replace(string = alphaMelt$Measure, 
                                 pattern = "\\.(min|1se)", replacement = "")
alphaCast <- dcast(alphaMelt, Alpha + Type ~ Measure,
                   value.var = "Value")
ggplot(alphaCast, aes(x = Alpha, y = error)) +
  geom_line(aes(group = Type)) +
  facet_wrap(~Type, scales = "free_y", ncol = 1) +
  geom_point(aes(size = lambda))
set.seed(5127151)
acsCV3 <- cv.glmnet(x = acsX, y = acsY, family = "binomial", nfold = 5,
                    alpha = alphaInfo$Alpha[which.min(alphaInfo$error.1se)])
plot(acsCV3)
plot(acsCV3$glmnet.fit, xvar = "lambda")
abline(v = log(c(acsCV3$lambda.min, acsCV3$lambda.1se)), lty = 2)

theCoef <- as.matrix(coef(acsCV3, s = "lambda.1se"))
coefDF <- data.frame(Value = theCoef, 
                     Coefficient = rownames(theCoef))
coefDF <- coefDF[nonzeroCoef(coef(acsCV3, s = "lambda.1se")), ]
ggplot(coefDF, aes(x = X1, y = reorder(Coefficient, X1))) + 
  geom_vline(xintercept = 0, color = "grey", linetype = 2) +
  geom_point(color = "blue") + labs(x = "Value",
                                    y = "Coefficient", title = "Coefficient Plot")
#22.2 bayesian shrinkage

#download.data('http://jaredlander.com/data/ideo.rdata','data/ideo.rdata')
ideo<- read.table("ideo.rdata",
                  )
head(ideo)
results<-ideo%>%
  group_by(Year)%>%
  do