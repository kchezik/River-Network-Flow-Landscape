library("SSN") # Import the SSN library.
lsn = importSSN(filepath = "~/Documents/Simon_Fraser_University/PhD_Research/Projects/River-Network-Flow-Landscape/lsn.ssn",predpts = "preds", o.write = F) #Load the .lsn dataset into R.

names(lsn)$Obs #Look at observations. This is where the predictor and response variables are stored.
head(lsn@data) #Quick look at the data.
plot(lsn,"Y_DOY2", lwdLineCol = "afvArea", lwdLineEx = 10, lineCol = "black", xlab =  "UTM East" , ylab =  "UTM North", asp=1) #Plot a variable.

createDistMat(lsn, predpts = "preds", amongpreds = TRUE) # Create the distance matrix between points.

#### Torgegram Plots For Each Response Variable ####
# 1.) Flow-Unconnected Autocorrelation Covariates: elevation or distance upstream. Tail-up.
# 2.) Flow-Unconnected Autocorrelation Covariates: elevation or distance upstream. Oddly, flow-connected sites autocorrelation becomes greater with distance. Tail-up.
# 3.) Tail-up model.
Y_DOY2.Torg = Torgegram(lsn, "Y_DOY2") #1
Y_Max.Torg = Torgegram(lsn, "Y_DOY2") #1
Y_Min.Torg = Torgegram(lsn, "Y_DOY2") #1
Y_DOY2_SE.Torg = Torgegram(lsn, "Y_DOY2_SE") #1
Y_Max_SE.Torg = Torgegram(lsn, "Y_Max_SE") #3
Y_Min_SE.Torg = Torgegram(lsn, "Y_MIN_SE") #3
plot(Y_DOY2.Torg)


#### Y_DOY2 Analysis & Model Selection ####

#Null Model
Non_Space = glmssn(Y_DOY2 ~ mean_diff_ + Glacier_Km + WB_Km2 + North_Km2 + East_Km2 + South_Km2 + West_Km2 + upDist + UTM_N + UTM_E, lsn, CorModels = NULL, use.nugget = T, EstMeth = "REML")
summary(Non_Space)
#Non_Space.resid = residuals(Non_Space)
#names(getSSNdata.frame(Non_Space.resid))
#plot(Non_Space.resid, inflcol = "_resid.stand_")
#par(mfrow = c(1,2)); hist(Non_Space.resid); hist(lsn,"Y_DOY2");par(mfrow = c(1,1))


# Linear Models
Euclid.Sph = glmssn(Y_DOY2 ~ Glacier_Km + WB_Km2 + North_Km2 + NE_Km2 + East_Km2 + SE_Km2 + South_Km2 + SW_Km2 + West_Km2 + NW_Km2 + upDist + UTM_N + UTM_E, lsn, CorModels = "Spherical.Euclid", addfunccol = "afvArea", EstMeth = "REML")
summary(Euclid.Sph)
#Euclid.resid = residuals(Euclid.Sph)
#par(mfrow = c(1,2)); hist(Euclid.resid); hist(lsn,"Y_DOY2");par(mfrow = c(1,1))

Euclid.Gaus = glmssn(Y_DOY2 ~ Glacier_Km + WB_Km2 + North_Km2 + NE_Km2 + East_Km2 + SE_Km2 + South_Km2 + SW_Km2 + West_Km2 + NW_Km2 + upDist + UTM_N + UTM_E, lsn, CorModels = "Gaussian.Euclid", addfunccol = "afvArea", EstMeth = "REML")
summary(Euclid.Gaus)
#Euclid.resid = residuals(Euclid.Gaus)
#par(mfrow = c(1,2)); hist(Euclid.resid); hist(lsn,"Y_DOY2");par(mfrow = c(1,1))

Euclid.Exp = glmssn(Y_DOY2 ~ Glacier_Km + WB_Km2 + North_Km2 + NE_Km2 + East_Km2 + SE_Km2 + South_Km2 + SW_Km2 + West_Km2 + NW_Km2 + upDist + UTM_N + UTM_E, lsn, CorModels = "Exponential.Euclid", addfunccol = "afvArea", EstMeth = "REML")
summary(Euclid.Exp)
#Euclid.resid = residuals(Euclid.Exp)
#par(mfrow = c(1,2)); hist(Euclid.resid); hist(lsn,"Y_DOY2");par(mfrow = c(1,1))

Euclid.Cau = glmssn(Y_DOY2 ~ Glacier_Km + WB_Km2 + North_Km2 + NE_Km2 + East_Km2 + SE_Km2 + South_Km2 + SW_Km2 + West_Km2 + NW_Km2 + upDist + UTM_N + UTM_E, lsn, CorModels = "Cauchy.Euclid", addfunccol = "afvArea", EstMeth = "REML")
summary(Euclid.Cau)
#Euclid.resid = residuals(Euclid.Cau)
#par(mfrow = c(1,2)); hist(Euclid.resid); hist(lsn,"Y_DOY2");par(mfrow = c(1,1))

options(digits = 5)
InfoCritCompare(list(Non_Space,Euclid.Sph,Euclid.Gaus,Euclid.Exp,Euclid.Cau))

#Tail-Up Models
Tail_Up = glmssn(Y_DOY2 ~ Glacier_Km + upDist + UTM_N + UTM_E, lsn, CorModels = "LinearSill.tailup", addfunccol = "afvArea", EstMeth = "REML")
summary(Tail_Up)
Tail_Up.resid = residuals(Tail_Up)
plot(Tail_Up.resid)
par(mfrow = c(1,2)); hist(Tail_Up.resid); hist(lsn,"Y_DOY2");par(mfrow = c(1,1))

AIC(Non_Space)
AIC(Euclid)
AIC(Tail_Up) #Better AIC by 7.46 over the no spatial relationship model and 9.5 better than the Euclid model.

cv.out = CrossValidationSSN(Tail_Up)
par(mfrow = c(1,2))
plot(Tail_Up$sampinfo$z, cv.out[, "cv.pred"], pch = 19, xlab = "Observed Data", ylab = "LOOCV Prediction")
abline(0,1)
plot(na.omit(getSSNdata.frame(lsn)[,"Y_DOY2"]), cv.out[,"cv.se"],pch = 19, xlab = "Observed Data", ylab = "LOOCV Prediction SE")

varcomp(Tail_Up) #Most of the variation is not explained by the covariates as the tail-up model is contributing 76% of the explanatory power.








