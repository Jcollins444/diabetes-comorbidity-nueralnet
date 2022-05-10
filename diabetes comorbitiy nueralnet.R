########################################################################
dat <- read.csv(file=
                    "https://archive.ics.uci.edu/ml/machine-learning-databases/00529/diabetes_data_upload.csv",
                  header=TRUE)
dim(dat)
head(dat)
anyNA(dat)

############### EDA ###########################################
###############################################################
##################### checking type, missing, unique values #######
### data type change



dat[dat$Gender == "Male", ]$Gender <- "1"
dat[dat$Gender == "Female", ]$Gender <- "0"

dat$Gender <- as.integer(dat$Gender)
#####################################################################

dat[dat$Polyuria == "Yes", ]$Polyuria <- "1"
dat[dat$Polyuria == "No", ]$Polyuria <- "0"

dat$Polyuria <- as.integer(dat$Polyuria)

#####################################################################

dat[dat$Polydipsia == "Yes", ]$Polydipsia <- "1"
dat[dat$Polydipsia == "No", ]$Polydipsia <- "0"

dat$Polydipsia <- as.integer(dat$Polydipsia)

#####################################################################


dat[dat$sudden.weight.loss == "Yes", ]$sudden.weight.loss <- "1"
dat[dat$sudden.weight.loss == "No", ]$sudden.weight.loss <- "0"

dat$sudden.weight.loss <- as.integer(dat$sudden.weight.loss)


#####################################################################



dat[dat$weakness == "Yes", ]$weakness <- "1"
dat[dat$weakness == "No", ]$weakness <- "0"

dat$weakness <- as.integer(dat$weakness)


#####################################################################

dat[dat$Polyphagia == "Yes", ]$Polyphagia <- "1"
dat[dat$Polyphagia == "No", ]$Polyphagia <- "0"

dat$Polyphagia <- as.integer(dat$Polyphagia)

#####################################################################

dat[dat$Genital.thrush == "Yes", ]$Genital.thrush <- "1"
dat[dat$Genital.thrush == "No", ]$Genital.thrush <- "0"

dat$Genital.thrush <- as.integer(dat$Genital.thrush)



#####################################################################

dat[dat$visual.blurring == "Yes", ]$visual.blurring <- "1"
dat[dat$visual.blurring == "No", ]$visual.blurring <- "0"

dat$visual.blurring <- as.integer(dat$visual.blurring)


#####################################################################

dat[dat$Itching == "Yes", ]$Itching <- "1"
dat[dat$Itching == "No", ]$Itching <- "0"

dat$Itching <- as.integer(dat$Itching)
#####################################################################

dat[dat$Irritability == "Yes", ]$Irritability <- "1"
dat[dat$Irritability == "No", ]$Irritability <- "0"

dat$Irritability <- as.integer(dat$Irritability)
#####################################################################

dat[dat$delayed.healing == "Yes", ]$delayed.healing <- "1"
dat[dat$delayed.healing == "No", ]$delayed.healing <- "0"

dat$delayed.healing <- as.integer(dat$delayed.healing)
#####################################################################

dat[dat$partial.paresis == "Yes", ]$partial.paresis <- "1"
dat[dat$partial.paresis == "No", ]$partial.paresis <- "0"

dat$partial.paresis <- as.integer(dat$partial.paresis)

#####################################################################

dat[dat$muscle.stiffness == "Yes", ]$muscle.stiffness <- "1"
dat[dat$muscle.stiffness == "No", ]$muscle.stiffness <- "0"

dat$muscle.stiffness <- as.integer(dat$muscle.stiffness)

#####################################################################

dat[dat$Alopecia == "Yes", ]$Alopecia  <- "1"
dat[dat$Alopecia  == "No", ]$Alopecia  <- "0"

dat$Alopecia  <- as.integer(dat$Alopecia )

#####################################################################

dat[dat$Obesity == "Yes", ]$Obesity  <- "1"
dat[dat$Obesity  == "No", ]$Obesity  <- "0"

dat$Obesity  <- as.integer(dat$Obesity )

#####################################################################

dat[dat$class == "Positive", ]$class  <- "1"
dat[dat$class  == "Negative", ]$class  <- "0"

dat$class  <- as.integer(dat$class )

#####################################################################


vnames <- colnames(dat)
n <- nrow(dat)
out <- NULL
for (j in 1:ncol(dat)){
  vname <- colnames(dat)[j]
  x <- as.vector(dat[,j])
  n1 <- sum(is.na(x), na.rm=TRUE) 
  n2 <- sum(x=="NA", na.rm=TRUE) 
  n3 <- sum(x==" ", na.rm=TRUE) 
  nmiss <- n1 + n2 + n3
  nmiss <- sum(is.na(x))
  ncomplete <- n-nmiss
  out <- rbind(out, c(col.num=j, v.name=vname, mode=mode(x), n.level=length(unique(x)),
                      ncom=ncomplete, nmiss= nmiss, miss.prop=nmiss/n))
}
out <- as.data.frame(out) 
row.names(out) <- NULL
out

################ missing check ############ check only NA
colMeans(is.na(dat))



########  Missing values by visualization ###############
library(naniar)
vis_miss(dat)
gg_miss_var(dat)





#####################################################################
#####################################################################
#####################################################################
############## Frequency distribution of target variable ########


hist(dat$Age, col="red")
hist(dat$Gender, col="red")
hist(dat$Polyuria, col="red")
hist(dat$Polydipsia, col="red")
hist(dat$sudden.weight.loss, col="red")
hist(dat$weakness, col="red")
hist(dat$Polyphagia, col="red")
hist(dat$Genital.thrush, col="red")
hist(dat$visual.blurring, col="red")
hist(dat$Itching, col="red")
hist(dat$Irritability, col="red")
hist(dat$delayed.healing, col="red")
hist(dat$partial.paresis, col="red")
hist(dat$muscle.stiffness, col = "blue")
hist(dat$Alopecia, col = "blue")
hist(dat$Obesity, col = "blue")
hist(dat$class, col = "blue")



#####################################################################
#####################################################################
################################run the test on each predictor relative to class#####################################

wilcox.test(dat$Age~dat$class, dat=dat, alternative = "two.sided")
t.test(dat$Age~dat$class, dat=dat, alternative = "two.sided")
#####################################################################
wilcox.test(dat$Gender~dat$class, dat=dat, alternative = "two.sided")
t.test(dat$Gender~dat$class, dat=dat, alternative = "two.sided")
#####################################################################
wilcox.test(dat$Polyuria~dat$class, dat=dat, alternative = "two.sided")
t.test(dat$Polyuria~dat$class, dat=dat, alternative = "two.sided")
#####################################################################
wilcox.test(dat$Polydipsia~dat$class, dat=dat, alternative = "two.sided")
t.test(dat$Polydipsia~dat$class, dat=dat, alternative = "two.sided")
#####################################################################
wilcox.test(dat$sudden.weight.loss~dat$class, dat=dat, alternative = "two.sided")
t.test(dat$sudden.weight.loss~dat$class, dat=dat, alternative = "two.sided")
#####################################################################

wilcox.test(dat$Polyphagia~dat$class, dat=dat, alternative = "two.sided")
t.test(dat$Polyphagia~dat$class, dat=dat, alternative = "two.sided")
#####################################################################

wilcox.test(dat$Genital.thrush~dat$class, dat=dat, alternative = "two.sided")
t.test(dat$Genital.thrush~dat$class, dat=dat, alternative = "two.sided")
#####################################################################
wilcox.test(dat$visual.blurring~dat$class, dat=dat, alternative = "two.sided")
t.test(dat$visual.blurring~dat$class, dat=dat, alternative = "two.sided")
#####################################################################
wilcox.test(dat$Itching~dat$class, dat=dat, alternative = "two.sided")
t.test(dat$Itching~dat$class, dat=dat, alternative = "two.sided")
#####################################################################

wilcox.test(dat$Irritability~dat$class, dat=dat, alternative = "two.sided")
t.test(dat$Irritability~dat$class, dat=dat, alternative = "two.sided")
#####################################################################
wilcox.test(dat$delayed.healing~dat$class, dat=dat, alternative = "two.sided")
t.test(dat$delayed.healing~dat$class, dat=dat, alternative = "two.sided")
#####################################################################
wilcox.test(dat$partial.paresis~dat$class, dat=dat, alternative = "two.sided")
t.test(dat$partial.paresis~dat$class, dat=dat, alternative = "two.sided")
#####################################################################
wilcox.test(dat$muscle.stiffness~dat$class, dat=dat, alternative = "two.sided")
t.test(dat$muscle.stiffness~dat$class, dat=dat, alternative = "two.sided")
#####################################################################
wilcox.test(dat$Alopecia~dat$class, dat=dat, alternative = "two.sided")
t.test(dat$Alopecia~dat$class, dat=dat, alternative = "two.sided")
#####################################################################
wilcox.test(dat$Obesity~dat$class, dat=dat, alternative = "two.sided")
t.test(dat$Obesity~dat$class, dat=dat, alternative = "two.sided")
#####################################################################
#####################################################################
#####################################################################
set.seed(123)
n <- nrow(dat)
split_data <- sample(x=1:2, size = n, replace=TRUE, prob=c(0.67, 0.33))
train1 <- dat[split_data == 1, ]
test1 <- dat[split_data == 2, ]
yobs <- test1$class
test1 <- test1[ , -19]

fisher.test(table(dat$Gender, dat$class))
fisher.test(table(dat$Polyuria, dat$class))
fisher.test(table(dat$Polydipsia, dat$class))
fisher.test(table(dat$sudden.weight.loss, dat$class))
fisher.test(table(dat$weakness, dat$class))
fisher.test(table(dat$Polyphagia, dat$class))
fisher.test(table(dat$Genital.thrush, dat$class))
fisher.test(table(dat$visual.blurring, dat$class))
fisher.test(table(dat$Itching, dat$class))
fisher.test(table(dat$Irritability, dat$class))
fisher.test(table(dat$delayed.healing, dat$class))
fisher.test(table(dat$partial.paresis, dat$class))
fisher.test(table(dat$muscle.stiffness, dat$class))
fisher.test(table(dat$Alopecia, dat$class))
fisher.test(table(dat$Obesity, dat$class))

########## Fitting the model with 1 hidden layer ################################

library(neuralnet)
#https://www.rdocumentation.org/packages/neuralnet/versions/1.44.2/topics/neuralnet
options(digits=3)
net1 <- neuralnet(class ~ ., 
                  data = train1, 
                  hidden=2, #1 hidden layer, 2 neurons
                  act.fct='logistic', err.fct="sse", linear.output=F, likelihood=TRUE)


# PLOT THE MODEL
plot(net1, rep="best", show.weights=T, dimension=6.5, radius=.15,
     col.hidden="red", col.hidden.synapse="black", lwd=1, fontsize=9)


########## Fitting the model with 2 hidden layers and 3 nuerons ################################

library(neuralnet)
#https://www.rdocumentation.org/packages/neuralnet/versions/1.44.2/topics/neuralnet
options(digits=5)
net1 <- neuralnet(class ~ ., 
                  data = train1, 
                  hidden=3, #2 hidden layer, 3 neurons
                  act.fct='logistic', err.fct="sse", linear.output=F, likelihood=TRUE)


# PLOT THE MODEL
plot(net1, rep="best", show.weights=T, dimension=6.5, radius=.15,
     col.hidden="red", col.hidden.synapse="black", lwd=1, fontsize=9)



########## Fitting the model with 9 hidden layer and 13 nuerons ################################

library(neuralnet)
#https://www.rdocumentation.org/packages/neuralnet/versions/1.44.2/topics/neuralnet
options(digits=22)
net1 <- neuralnet(class ~ ., 
                  data = train1, 
                  hidden=13, #9 hidden layer, 13 neurons
                  act.fct='logistic', err.fct="sse", linear.output=F, likelihood=TRUE)


# PLOT THE MODEL
plot(net1, rep="best", show.weights=T, dimension=6.5, radius=.15,
     col.hidden="red", col.hidden.synapse="black", lwd=1, fontsize=9)


# PREDICTION
ypred <- compute(net1, covariate=test1)
ypred
ypred <- compute(net1, covariate=test1)$net.result

MSE.c <- mean((yobs-ypred)^2)
MSE.c

ypred <- ifelse(ypred>0.5,1,0)


library(cvAUC)
rf_AUC <- ci.cvAUC(predictions = ypred, labels =yobs, folds=1:NROW(test1), confidence = 0.95)
rf_AUC
(rf_auc.ci <- round(rf_AUC$ci, digits = 3))

library(verification)
mod.nn <- verify(obs = yobs, pred = ypred)
roc.plot(mod.nn, plot.thres=NULL)
text(x=0.7, y=0.2, paste("Area under ROC = ", round(rf_AUC$cvAUC, digits = 3), "with 95% CI (",
                         rf_auc.ci[1], ",", rf_auc.ci[2], ").", sep = " "), col="blue", cex =1.2)

