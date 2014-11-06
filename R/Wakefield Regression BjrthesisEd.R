# Wakefield Modeling Project Master's Thesis Edition:
# A Ben Rogers Joint
# Started 4/9/2014
# Last Edited 10/22/2014
 

rm(list = ls())
ptr <- proc.time()


# function section

critergen <- function( predicted, measured, fulltabl = FALSE ) {#Critergen is a function that when fully operational, will generate different criteria for how well data is predicted by different values.
#inputs: the predictions made by the predict() function and the actual values measured as 1d arrays.
predictedRes <- ifelse(predicted >= .5, 1,0)   

if (fulltabl == TRUE) return( prop.table(table(predictedRes == measured, exclude = NULL))) else return( prop.table(table(predictedRes == measured, exclude = NULL))['TRUE']) #output: % true in table of elastic net's predictions on test set
}


#Go to correct file location.  

inputwd <- "/Users/bjr/Google Drive/Activate/Wakefield Modelling Project/Wakefield Input 2"
setwd(inputwd)
outputwd <- "/Users/bjr/Google Drive/Activate/Wakefield Modelling Project/Wakefield Output 2/Wakefield Regression Outputs"
dateUnformed <-  date()
dateFormed <-  strsplit(dateUnformed, split = ' ')
dateFormed <- paste(dateFormed[[1]][c(2,4)], collapse = ' ')

maindf <-  readRDS('maindf.RDS')
Vandat <- readRDS('Vandat.RDS')


#Load relevant packages

library(car)
library(xlsx)
library(gdata)
library(rockchalk)
library(caret) #loads ggplot2
library(leaps)
library(glmnet)
library(mice)
library(mgcv) 
library(randomForest)
library(rpart) 
 
maindf2 <-  readRDS('maindf2.rds')
str(maindf2)

ifilename <-  'imputation3.rds' #Name of file where imputation is stored:
mickey <- readRDS(file = ifilename)
maindf2 <- complete(mickey)

colnames(maindf2)

# Preproc for maindf2
Agebreaks <- c(17, 30, 45, 64, 200)
Agelabels <- c('18-30', '31-45', '46-64', '65+')
maindf2$cat_age <- cut(maindf2$Age, breaks = Agebreaks , labels = Agelabels)

maindf2$SpecVotes <- 0
maindf2[ which(maindf2$voted_08g %in% c(1) & maindf2$voted_12g %in% c(1)), 'SpecVotes'] <- 1
maindf2[maindf2$voted_10g %in% c(1), 'SpecVotes'] <- 0

maindf2$TenDummy <- 0
maindf2[maindf2$voted_10g %in% c(1), 'TenDummy'] <- 1




# app


varsToNotInclude <- c("prospectid", 'attemptcount', 'zipcode', 'county', 'voterid','VANID', 'Notes','PollingAddress', 'PollingLocation', 'PrecinctName', 'block_group', 'X2012.ClarityTurnout','Phone','PersonID', 'votebuilder_identifier', 'BoardOfEducationCode','namecheck', 'datelastcalled', 'datasource_name','agent_name', 'firstname', 'lastname')

maindf2 <- maindf2[, !colnames(maindf2) %in% varsToNotInclude]

maindf2[, lapply(maindf2, is.character)==T] <- lapply(maindf2[, lapply(maindf2, is.character)==T], as.factor)
head(maindf2)



# pull out a tenth of the data for control purposes
set.seed(12345)
tenperc <-  createFolds(1:nrow(maindf2), k = 10)

controldf2 <- maindf2[tenperc$Fold06,] #Order Matters: previously this data had not been fully recorded, as parts were cut out due to the redefininition of maindf2 in the line below
maindf2 <- maindf2[-tenperc$Fold06,]

rownames(maindf2[tenperc$Fold10,])


# junkermod <-  glm(sp03 ~. , family ='binomial',  data = maindf2)
# model.matrix(maindf2)




#BEGIN LOOP HERE!
 i <- 1
  L <- 1
NIVs <- 1 #The number of IVs, this will eventually serve as the building block of the outer loop
MAXIVs <- 10 # Number of Go rounds for IV selection

# nFold <- 2 #number of folds to use for kfold cross validation method of IV selection


deevlist <- c('sp03', 'sp04','sp05','sp06','sp08') 
 # deevlist <- 'sp03SuppRec' #for when we want to test a feature, but are overwhelmed by the number of regressions
 #Select things that we don't want in our IV list below
 dvcols <- c('sp03', 'sp04','sp05','sp06','sp08',  'deevdiv', 'deevfac') #anything that deals with a DV
 # rescols <- c('Preds_for_sp03Rec2', 'Recoded_sp03Rec2','Preds_for_sp04Rec2', 'Recoded_sp04Rec2', 'Preds_for_sp05Rec2', 'Recoded_sp05Rec2', 'Preds_for_sp06Rec2', 'Recoded_sp06Rec2', 'Preds_for_sp08Rec2', 'Recoded_sp08Rec2') #Any thing that resembles a dv or is a stored dv. 
extraagevars <-  colnames(maindf2)[grep("age_", colnames(maindf2))]
gendervars <- colnames(maindf2)[grep("male", colnames(maindf2))]

redundant <- c('reg_party_dem','others_num_dem', 'others_num_rep', 'reg_party_ind', 'clarity_party', 'cons_dbi_educ_nohs', 'cat_ageDe', extraagevars, gendervars) #Age sex and Party aren't actually redundant, but since we hard code them into the loop, this is a good place to tell the code, hey we don't need you included in the boostrapping selection process 
zerovar <- c('voted_12p_dem', 'voted_10p_dem')
mostlyNAs <- c('cons_on_outside_list')
ActivateVars <- c('prospectid', 'PersonID', 'zipcode')
 # VANVars <- c('VANID')
 # VANVars <-  colnames(Vandat)
clarityvars <- colnames(maindf2)[grep('clarity', colnames(maindf2))]

possAIVs <- c('SpecVotes', 'TenDummy','cat_age', 'Age','Sex','Party')

colsnottouse <- c(dvcols,"sp07Rec", "sp07.2Rec", "sp08Rec", 'reg_party_rep','attemptcount', 'voterid', 'votebuilder_identifier', 'dsnRec', 'cen10_asian', 'namecheck', 'phone_primary_cell',zerovar, redundant, mostlyNAs, ActivateVars, clarityvars, colnames(maindf2)[nearZeroVar(maindf2)])


# maindf2 <- maindf2[, c(which(colnames(maindf2) %in% possAIVs), which(!colnames(maindf2) %in% possAIVs))] #rearrange maindf2 s.t. columns of interest come first
colnames(maindf2)
#Imputation section
 # impdata <- maindf2[,c(deevlist, colnames(maindf2)[ !colnames(maindf2) %in% colsnottouse],  colnames(maindf2)[colnames(maindf2) %in% redundant])] #Put data to impute in a variable
 # colnames(impdata)
# apply( apply(impdata, 2, is.na), 2, sum)
# # colnames(impdata)[ unique(which(is.na(impdata[,1:20] ) == T, arr.ind = T)[,2])]

# str(maindf2, list.len = ncol(maindf2))
# lastn <- 225
# print(colnames(maindf2)[lastn])


# library(parallel)
# clus <-  makeCluster(6, type = "PSOCK")
# getOption('mc.cores', 2L)
# for(u in 1:5){
	# mickey <-  mice(impdf)
	# summary(mickey)
	# saveRDS(object = mickey, file = paste('imputation', u ,'.rds', sep = '' ))
	# rm(mickey)
# }
 
# #Recode age and sex in our data to use the complete columns.
# colnames(impdata)
# impdata$Age <- impdata$age_years
# impdata$age_years <- NULL

# #Get gender_male into the same format as sex, and replace the old version
# data.frame( impdata$Sex, impdata$gender_male)
# impdata$gender_male <- factor(impdata$gender_male)
# levels(impdata$gender_male) <- c( levels(impdata$gender_male), 'M', 'F')
# impdata$gender_male[ impdata$gender_male == 1] <- 'M'
# impdata$gender_male[ impdata$gender_male == 0] <- 'F'
# impdata$gender_male <- factor(impdata$gender_male)


# which(impdata$Sex != impdata$gender_male) #All are now the same w/recoding, except for NA's
# impdata$Sex <- impdata$gender_male
# impdata$gender_female <- NULL
# impdata$gender_male <- NULL
# impdata$others_num_male <- NULL

# #get rid of redundant party variables
# impdata$clarity_party <- NULL 
# impdata$others_num_dem <- NULL
# impdata$others_num_rep <- NULL
# impdata$reg_party_dem <- NULL

# # impdata$age_years[606] <- NA

# #While we can deal with sex and age, this 3+ planetrips needs to be considered separately
# impcor <-  cor(model.matrix(sp03~., impdata))
# sort(impcor[,'cons_dbi_travel_vacation_3plusplanetrips'])  #When you look at it like this, it's very clear that cons_dbi_travel_vacation_air is so close as to completely track cons_dbi_travel_vacation_3plusplanetrips

# data.frame('ThreePlus' = impdata$cons_dbi_travel_vacation_3plusplanetrips, 'airtravel' = impdata$cons_dbi_travel_vacation_air, 'diff' = impdata$cons_dbi_travel_vacation_3plusplanetrips- impdata$cons_dbi_travel_vacation_air )
# impdata$cons_dbi_travel_vacation_air <- NULL #use cons_dbi_travel_vacation_3plusplanetrips, as its meaning is much clearer.

 # morty <-  mice(impdata, m = 1, maxit = 1) # so it looks like some columns are just getting switched under the radar. Not sure why, but a full run ignores 3 columns; Age, Sex, and cons_dbi_travel_vacation_3plusplanetrips.
 # We have learned why. It turns out that mice ignores any column it believes is similar enough to others
# plot(Age ~ age_years, data = impdata)

# fit <-  with(data = morty, exp = lm(sp03 ~ Sex))
# fitpool <- pool(fit)

# View(complete(morty, action = 1)[606:607,])
# impdata[, c('Sex', 'Age', 'cons_dbi_travel_vacation_3plusplanetrips')]

# saveRDS(object = morty, file = 'imputationtest.rds')
 
# ifilename <-  'imputation4.rds' #Name of file where imputation is stored:
# mickey <- readRDS(file = ifilename)

# str(mickey)
#Dataframe appears to have been saved in mickey$pad$data
# But isn't imputed...
# We can get that using complete(mickey)
# str(mickey)
 # complete(mickey)[, !colnames(complete(mickey)) %in% c('sp03', 'sp04', 'sp05', 'sp06', 'sp08')]

# mousesample <- sample(1:nrow(complete(mickey)), 9 * nrow(complete(mickey))/10)
xdata <- model.matrix(sp08 ~ . - sp04 - sp05 -sp06 -sp03 -reg_earliest_month - cons_childcnt- others_num_female, data = maindf2)
# colnames(xdat) 
# length( xdat)
# str(xdata) #why do 130 rows seem to just disappear? Identical values elsewhere? No. There were still some NAs in our data
# dim(xdata)
# head(xdata)

#Until we've got a perfect xdat, we'll need to make certain x and y data line up. We can do this by
# ydata <- complete(mickey)$sp08[ 	-which( is.na(complete(mickey)) == T, arr.ind = T)[,1]]
ydata <- maindf2[,'sp08']
# grid <- 10^seq(10, -2, length = 100) #borrowing this directly from islr
# testglmnet <-  glmnet( 
# x = xdata,
# y = ydata,
# family = 'binomial',
# alpha = 1, #Perform the lasso!
# lambda = grid
# ) 


cvtest <- cv.glmnet(x = xdata, y = ydata, alpha = 1) #Find best value of penalty for our imputed data
plot(cvtest) #plot the value, because it looks cool. 

bestlasso<-  glmnet( 
x = xdata,
y = ydata,
family = 'binomial',
alpha = 1, #Perform the lasso!
lambda = cvtest$lambda.min
) 

coef(bestlasso) 


library(glmnetcr) 
print(ifilename)
data.frame(coefs = coef(bestlasso)[which(coef(bestlasso) != 0)], odds.ratios = exp( coef(bestlasso)[which(coef(bestlasso) != 0)]), row.names = rownames(coef(bestlasso))[which(coef(bestlasso) != 0)])
nonzerocoefsfr <-  data.frame(coefs = coef(bestlasso)[which(coef(bestlasso) != 0)], odds.ratios = exp( coef(bestlasso)[which(coef(bestlasso) != 0)]), row.names = rownames(coef(bestlasso))[which(coef(bestlasso) != 0)])
print(nonzerocoefsfr)
nrow(nonzerocoefsfr) - 1 #according to Zou Hastie, and Tibshirani (2007) The number of nonzero coefficients in the lasso is an unbiased estimator of the Effective Degrees of Freedom in the lasso. I wonder about the elastic net...

# mickey.test <- complete(mickey)[-mousesample,]
# mickey.test$sp08.2 <- mickey.test$sp08
# mickey.test$sp08 <- NULL

# newxval <- data.matrix(mickey.test)[, which(colnames(complete(mickey)) != 'sp08')]
# newxval <- data.matrix(mickey.test)

# W/Dr. Johnson
blpreds <-  predict(bestlasso, newx = xdata, lambda = cvtest$lambda.min, type = 'response' )
#Craft PCP Function

critergen(blpreds, maindf2[,'sp08'], fulltabl = T)

# cbind(blpreds, ydata)
# ydatpred <-  ifelse(blpreds > 0.5, 1, 0)
# nrow(maindf2)
# length(ydatpred)
# prop.table(table(ydatpred ==  maindf2[,'sp08'], exclude = NULL))

newxdata <- model.matrix(sp08 ~ . - sp04 - sp05 -sp06 -sp03 - reg_earliest_month - cons_childcnt- others_num_female, data = controldf2)
colnames(newxdata)
lassopreds <- predict(bestlasso, newx = xdata, lambda = cvtest$lambda.min, type = 'response')
lassopredsCont <- predict(bestlasso, newx = newxdata, lambda = cvtest$lambda.min, type = 'response')

# lassopredsRes <-  ifelse( lassopreds >= .5, 1,0)
# lassopredsContRes <-  ifelse( lassopredsCont >= .5, 1,0)


#Work on an elastic net Implementation
netalphaval <- seq(0,1, by = .1) #create a sequence for alpha that can be switched out as needed
for(n in 1:length(netalphaval)){
	cvtestelnet <- cv.glmnet(x = xdata, y = ydata, alpha = netalphaval[n]) #Find best value of penalty for our imputed data
	if(n==1) aldf <- data.frame(al = netalphaval[n], lam.min = cvtestelnet$lambda.min, err.min = min(cvtestelnet$cvm)) else aldf <- rbind(aldf, data.frame(al = netalphaval[n],lam.min = cvtestelnet$lambda.min, err.min = min(cvtestelnet$cvm))) #If n == 1, save just the first three values in a data frame. Otherwise, add the values onto the data frame
	
}

plot(cv.glmnet(x = xdata, y = ydata, alpha = netalphaval[which(aldf$err.min == min(aldf$err.min))]))

#perform elastic net regression
bestnet <-  glmnet( 
x = xdata,
y = ydata,
family = 'binomial',
alpha = aldf$al[ which(aldf$err.min == min(aldf$err.min))], #Perform the elastic net with alpha that minimized crossvalidation error
lambda = aldf$lam.min[ which(aldf$err.min == min(aldf$err.min))] # and its corresponding lambda value
) 

netpreds <- predict(bestnet, newx = xdata, lambda = aldf$lam.min[ which(aldf$err.min == min(aldf$err.min))], type = 'response')
# netpredsRes <- ifelse(netpreds >= .5, 1,0)

netpredsCont <- predict(bestnet, newx = newxdata, lambda = aldf$lam.min[ which(aldf$err.min == min(aldf$err.min))], type = 'response')
# netpredsContRes <- ifelse(netpredsCont >= .5, 1,0)


# Work on getting trees included in sample
treetest <-  randomForest(x = xdata, y = factor(ydata), ntree = 500, mtry = 30)



summary(treetest)
print(treetest)
# imporder <- order(treetest$importance, decreasing = T)
# data.frame(rownames(treetest$importance)[imporder], MeanDecreaseGini= treetest$importance[imporder])
plot(treetest) # Plots error rates or MSE of the randomForest object
prop.table(table(ydata == treetest$predicted))

forestpredsCont <-  predict(treetest, newxdata)

# traintreetest <- train(x = xdata, y = factor(ydata), method = "rf")
# print(traintreetest) #suggests best value for mtry is 102
incxdata <- model.matrix(sp08 ~ . - sp04 - sp05 -sp06 -sp03 - reg_earliest_month - cons_childcnt- others_num_female, maindf[,!colnames(maindf) %in% varsToNotInclude])

randomForest(x = maindf[,!colnames(maindf) %in% c('sp08', 'sp04', 'sp05', 'sp06', 'sp03', 'reg_earliest_month', 'cons_childcnt','others_num_female')], y = factor(maindf$sp08)) #NA Not permitted in predictors

besttreetest <-  randomForest(x = xdata, y = factor(ydata), ntree = 1500, mtry = 102) #Let's check that, shall we?
plot(besttreetest)
prop.table(table(ydata == besttreetest$predicted))
prop.table(table(controldf2$sp08 == as.numeric(as.character( predict(besttreetest,newxdata))) ))

# implement k-nearest neighbor classification. It does horribly
# knntest <-  train(x = xdata, y = ydata, method = 'knn')
# prop.table(table( ydata == round(predict(knntest, xdata))))
# prop.table(table(controldf2$sp08 == round( predict(knntest,newxdata)))) 

is.matrix(xdata)

#Work on putting together adaboost
adatrainer <-  train(x = xdata, y = ydata, method = 'ada')
adatest <-  ada(xdata, ydata)

adatest2 <- ada(x = maindf[,!colnames(maindf) %in% c(colsnottouse, varsToNotInclude, 'dispositionid','sp08', 'sp04', 'sp05', 'sp06', 'sp03', 'reg_earliest_month', 'cons_childcnt','others_num_female')], y = maindf$sp08)

head(maindf)

prop.table(table( ydata == predict(adatest, newdata = data.frame(xdata))))
prop.table(table( controldf2$sp08 == predict(adatest, newdata = data.frame(newxdata))))


# teedat <-  data.frame(y =c(1,1,1,1), x1 = c(1,1,1,1), x2 = c(0,0,0,1))

# model.matrix(y~., data = teedat)

#End IV Removal

# maindf2[,rescols] <- NULL #make sure the recursive elements can't harm reruns by nullifying them pre-loop. 
 
# age, gender party registration, county, and income, if we have it on everyone. 

colnames(maindf2)

Rprof('bensprof.txt')
for(L in 1:1) { #begin DV loop
	deev <- 'sp08'
	maindf2$deevdiv <- maindf2[,deev] #for this one, we don't need to do anything to deevdiv to make it work. Unfortunately, it's pretty much everywhere instead of deev, so I'm just passing it on here.
	controldf2$deevdiv <- controldf2[,deev] #ditto.
	# colnames(maindf2)
	
	
	
	#chose columns for our sample
	numsforsample <- 1:ncol(maindf2)
	
	initlooplength <-  ncol(maindf2) - length( colsnottouse)
	
	# colnames(maindf2)
	
	colsnumstoavoid <-  which(colnames(maindf2) %in% colsnottouse)
	colnumstouse <- which(!colnames(maindf2) %in% colsnottouse)  # get all the columns we want to use as IVs
	
	write.csv( colnames(maindf2[,colnumstouse]), '/Users/bjr/GitHub/bjrThesis/R/colnumsused.csv')
	
	#create variable selecting loop
	# This loop will select a single variable and regress it against deev
	currentReg <- NULL # This will hold the regression of the moment
	bestReg <- NULL
	bestVarResSt <- NULL
	# priorIVs <- c('cat_age', 'Sex', 'PartyRec', 'cen00_medianincomeRec', "CountyName")
	# priorIVs <- c('cat_age', 'Sex', 'PartyRec', 'CountyName','cen10_densityRec','cen00_medianincomeRec')
	
	# priorIVs <- c('cat_age', 'Sex', 'Party', 'CountyName', 'SpecVotes', 'TenDummy', 'cen10_density', 'cen00_medianincome') #Mark's IV's
	# priorIVs <- c('cat_age', 'Sex', 'Party', 'SpecVotes', 'TenDummy')
	 # priorIVs <- NULL #for when we want to run it without initializing. Surprisingly, this does two things. 1. It performs more poorly than if we add AIVs (that is, overall lower criterion values), 2. It doesn't select any of the AIVs we think are relevant. 3. It doesn't 
	
	  priorIVs <- NULL #For when we want to do something without prior IVs
	
	for(NIVs in 1:MAXIVs){ #Outer Loop Begins
		metabreaker <- 999 #set metabreaker to some nonzero value. 999, in honor of the current greatest troll of all time. 
		# print( paste( 'NIVs =', NIVs))
		print(paste('dv =',deev))
		# if(NIVs == 1) priorIVs <- c('cat_age', 'Sex', 'Party')
		
		
		# Need to figure out how to make it so that If at no point does something happen in the loop below, break out of the outermost loop
		
		for(i in 1:initlooplength) {#Inner Loop Begins
			# for(i in 1:length) {#Inner Loop Begins
			
			iloopbreaker <- 1 #iloopbreaker begins as 1
			
			 ivstouse <- c(priorIVs, colnames(maindf2)[colnumstouse[i]])
			
			# ivstouse <- priorIVs
			
			# if(NIVs == 1) ivformed <-  ivstouse  else ivformed <-  do.call(paste,c(as.list( ivstouse), sep = ' + ')) # if NIVs is not one, we need to form a list of IVs to place into formed eqn. If it isn't then we can just use the text from ivstouse
			# ivformed <-  do.call(paste,c(as.list( ivstouse), sep = ' + ')) #Great if you want to use do.call, but I found something better below
			ivformed <-  paste(ivstouse, collapse = ' + ')
			formedeqn <- as.formula(paste('deevdiv', " ~ ", ivformed)) #Form our equation. In this version, we're going to need to figure out the DV's structure before we start these loops
			
			
			traindf2 <- maindf2[complete.cases(maindf2$deevdiv), c('deevdiv', ivstouse)]
			
			# KFold cross-validation method of iv selection
			# trainerFolds <- createFolds(traindf2[, 'deevdiv'], k = nFold, list = T)
			
			# for(r in 1:nFold){ #Begin Kfold cross-validation loop
			 # traindf3 <- na.omit(traindf2[-trainerFolds[[r]],]) #Create training data
			
			# testdf3 <-  na.omit(traindf2[trainerFolds[[r]],]) #create test data
			 
			# currentReg <-  multinom(formula = formedeqn, data = traindf3) #perform a beta regression and store it in currentReg
			
			# testdf3$testpreds <-  predict(currentReg, testdf3) #predict the test data's results, and store it in the test data's dataframe
			# testdf3$testpreds <- as.numeric(levels(testdf3$testpreds)[testdf3$testpreds])
			
			
			# if (r == 1) currentVarResidST <-  var(na.omit( testdf3$deevdiv - testdf3$testpreds)) else currentVarResidST <-  c(currentVarResidST, var(na.omit( testdf3$deevdiv - testdf3$testpreds)) ) #store the variance of the residuals; if it's the first go-round, put it them into a new variable called currentVarResidST. If it isn't, store them there using the c() function
			
			
			# }
			# currentVarResid <- mean(currentVarResidST)
			
			
			# currentReg <-  betareg(formula = formedeqn, data = traindf2, link = 'logit') #perform a beta regression and store it in currentReg
			
			currentReg <-  glm(formula = formedeqn, data = maindf2, family = 'binomial') #perform a regression and store it in currentReg
			# summary(currentReg)
			predict(currentReg)
			
			 # bestReg <-  glm(formula = formedeqn, data = maindf2, family = 'binomial') #perform a logit regression and store it in currentReg
			
			
			 # currentVarResid <-  var( currentReg$resid)  #Get currentReg's residuals, and store their variance
			traindf2$currentpreds <- predict(currentReg, traindf2, 'response') 
			controldf2$currentpreds <- predict(currentReg, controldf2, 'response') 
			
			# rtPredRat <-  (length(which(traindf2$deevdiv == 0 & traindf2$currentpreds <.5)) + length(which(traindf2$deevdiv == 1 & traindf2$currentpreds >.5)) )/length(traindf2$deevdiv)   #rtPredRat gives us the ratio of the number of predictions which are less than 50 and equal zero and the number greater than 50 that actually equal 1, and
			 
			 #criter (I'm not typing criterion throughout this) gives us the ratio of the number of predictions which are less than 50 and equal zero and the number greater than 50 that actually equal 1 as a ratio to the total number of possible values that can be predicted in the control group. 
			 #New criterion for judging model: 
			
			# # controldf3 <- controldf2[which( is.na(controldf2$deevdiv) == F), c('deevdiv', 'currentpreds')] 
			
			# onOnePreds <- length(which(controldf3$deevdiv == 1 & controldf3$currentpreds >=.5)) #Number of times the control group predicts ones correctly
			# onZeroPreds <- length(which(controldf3$deevdiv == 0 & controldf3$currentpreds <.5)) #Number of times controlgroup predicts zeros correctly
			# Npreds <- length(controldf3$deevdiv) #number of values that can be predicted
			
			
			# criter <-  (onOnePreds + onZeroPreds )/Npreds  
			# fullpreds <-  ifelse(traindf2$currentpreds > 0.5, 1, 0)
			
			# perctrue <-  prop.table(table(fullpreds ==  traindf2$deevdiv, exclude = NULL))['TRUE']
			
			 
			criter <- critergen(traindf2$currentpreds, traindf2$deevdiv)
			 
			 
			# # # print(currentVarResid)
			
			if(i == 1 & NIVs == 1 ){ #On the very first pass,
				bestReg <- currentReg #The First regression is the best regression.
				bestRtPredRat <- criter
				# bestVarResid <- currentVarResid #Same with the residual variance
				# bestVarResST <- bestVarResid #and thus we put it in storage
				# print(paste('Number of correctly predicted Zeros =', onZeroPreds))
				# print(paste('Number of correctly predicted Ones =', onOnePreds))
			
				print(paste('Rate to beat =',round( criter, 6)))
				bestIV <- colnames(maindf2)[colnumstouse[i]]
				 } #keep them always if i == 1
			
			# print( paste('currentVarResid =', currentVarResid ))
			# print(paste('bestVarResid =', bestVarResid))
			
			# residdif <- currentVarResid - bestVarResid
			# # print(paste('currentVarResid - bestVarResid = ', round(currentVarResid, 4), '-', round(bestVarResid, 4), '≈', round(residdif, 4 )) )
			
				# if(bestVarResid > currentVarResid){ #if the currentVarResid is lower than the bestVarresid
					
					if(criter > bestRtPredRat){ #if the current prediction ratio is bigger than the best one so far
					bestReg <- currentReg #put the current reg as best reg		
					bestRtPredRat <- criter # Put the current right prediction ratio as the best one, 
					bestIV <- colnames(maindf2)[colnumstouse[i]] #and save the bestIV for storage in priorIVs
					print(paste('Best Criterion Value =',round( bestRtPredRat,6)))
					metabreaker <- 0 #make the metabreaker variable = 0
					} # if the best regressions residual variance is bigger than the current regression's residual variance, the current regression replaces the prior best regression. 	
				
			
			# print(paste('i =', i))
			# rm(rtPredRat)
			rm(criter)# rm(contPredRat)
			
			if(metabreaker != 0) iloopbreaker <- 0 #if metabreaker is still equal to its original nonzero value, break the loop.
			
		}# Inner Loop Ends
		# 
		# print( paste( 'NIVs =', NIVs))
		priorIVs <- c(priorIVs, bestIV) #The bestIV now becomes part of the prior IVs
		print(priorIVs)
		
		#Executive Decision: This program must become more aggressive at countering multicollinearity, if only because the code runs at a snail's pace. 
		#Also, how to tell whether the IV's that survive the KFold cross validation are actually liable to reduce overall variance of residuals, or just lucky? Preferably without adding on to computational time? 
		
		
		
		 #get the names of all IVs in the best Regression (excluding the intercept)
		 length(priorIVs) == NIVs
		 initlooplength <- initlooplength - 1 # since one of our variables is lost to the best regression, reduce the length of our internal loop by one
		
		#Now, we need to figure out how to make sure it's not going to repeatedly grab the same variable, eliminating it from colnumstouse
		
		
		
		 IVcols <-  which(colnames(maindf2) %in% priorIVs) #get numbers of columns that have been used so far
		
		
		
		 colnumstouse <- colnumstouse[ !colnumstouse %in% IVcols] #get rid of the columns used previously by taking them out of colnums to use. 
		 
		 if(iloopbreaker == 0) break #if I loopbreaker is 0, break out of the iv loop and move onto the next dv
	} #Outer Loop ends
	# print(data.frame(controldf3$deevdiv, controldf3$currentpreds))
	#Print outer loop output
	print('')
	print('')
	print(deev)
	print( summary(bestReg))
	# str(bestReg)
	print(paste('nVal =', length(bestReg$residuals)))
	# summary(bestReg$residuals)
	# hist(bestReg$residuals)
	
	# print(var(bestReg$residuals))
	print('')
	
	
	#note to self. What if it doesn't select any after a certain point? Is there a danger that it might start arbitrarily pulling stuff out? I'll say it does, and this is something we have to watch for. 
	
	
	# summarize(maindf2[,IVcols])
	
	deevpreds <-  predict(bestReg, maindf2, 'response')
	contpreds <- predict(bestReg, controldf2, 'response')
	# finpreds <- predict(bestReg, Vandat, 'response')
	# str(bestReg)
	
	
	#create titles for our new variables, and add them to maindf2
	predtitler <- paste('Scoring on ', deev, sep = '') #title deevpreds for our export
	rectitler <- paste('Voter choice of ',deev, sep = '')
	# data.frame(c(names(deevpreds)), c( rownames(maindf2)))
	maindf2 <-  cbind(maindf2,  deevpreds)
	controldf2 <- cbind(controldf2, contpreds)
	# Vandat <- cbind(Vandat, 'torep' = finpreds)
	
	colnames(maindf2)[colnames(maindf2) == 'deevdiv'] <- rectitler
	colnames(maindf2)[colnames(maindf2) == 'deevpreds'] <- predtitler
	colnames(controldf2)[colnames(controldf2) == 'deevdiv'] <- rectitler
	colnames(controldf2)[colnames(controldf2) == 'contpreds'] <- predtitler
	
	# colnames(Vandat)[colnames(Vandat) == 'torep'] <- predtitler
	
	#Fix our predictions to the scale of the original queries
	
	 maindf2[, rectitler] <- round(maindf2[,rectitler], 2) #
	 maindf2[, predtitler] <- round(maindf2[,predtitler]*100, 5)
	
	 controldf2[, rectitler] <- round(controldf2[, rectitler], 2)
	 controldf2[, predtitler] <- round(controldf2[, predtitler] *100, 5)
	
	
	# Vandat[, predtitler] <- round(Vandat[, predtitler] *100, 2)
	
	
	
	
	# plot(log(1:length(bestVarResSt), 10), log(bestVarResSt, base = 10), type = 'l')
	# plot(1:length(bestVarResSt), bestVarResSt, type = 'l')
	
	 if(L == 1) bestRegST <- bestReg else bestRegST <- c(bestRegST, bestReg)
	
	#Clean up before next iteration
	
	rm( deev, colnumstouse, bestRtPredRat)
	} # End DV Loop
	
	
Rprof(NULL)
	
summaryRprof('bensprof.txt')
	
critergen(predict(bestReg, maindf2, 'response'), maindf2$sp08, fulltabl = T) #true % of BeSiVa's Predictions on training set

critergen( lassopreds, maindf2$sp08, fulltabl = T) #table of Lasso's predictions on training set.

critergen(netpreds, maindf2$sp08, fulltabl = T) #table of elastic net's predictions on training set

critergen(as.numeric(as.character(treetest$predicted)), maindf2$sp08, fulltabl = T)


#Begin looking at test set
	
critergen(predict(bestReg, controldf2, 'response'), controldf2$sp08, fulltabl = T) #table of BeSiVa's Predictions on test set. 

critergen(lassopredsCont, controldf2$sp08, fulltabl = T) #table of Lasso's predictions on test set.

critergen(netpredsCont, controldf2$sp08, fulltabl = T) #table of elastic net's predictions on test set

critergen(as.numeric(as.character(forestpredsCont)), controldf2$sp08, fulltabl = T)

# so it looks like it can beat lasso when it comes to predicting training data, but throw in test data, and it's actually worse than the lasso

	
	# system('say Done!')
#So it turns out that order totally matters. when it gets party, party may as well have been the only variable in the entire data set, judging from the way that the data jumps. However, it looks like 
