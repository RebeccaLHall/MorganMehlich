#Al alt log ratio

#Install Ranger for random Forest

install.packages("ranger")
library(ranger)

#partitian data 
### Perform kennard stone to separate data into 70% calibration and 30% validation sets

#install.packages("prospectr")
library(prospectr)


#install.packages("spectacles")
library(spectacles)

#install.packages("randomForest")
library(randomForest)

# creating a new x because the Kenstone function only accept numbers, I will use
# cbind for this
temp_x  <- cbind(M3P$pH,M3P$Morgan,M3P$Al, M3P$Ca, M3P$Fe, M3P$P)

index <- kenStone(temp_x,as.integer(0.7*NROW(M3P$Morgan)),metric="mahal",pc=5)

index <- kenStone(ALR_p,as.integer(0.7*NROW(ALR_p$P_Morgan_m)),metric="mahal",pc=5) #for data with no categorical variables 
# calibration samples

x_cal <- ALR_p[index$model,]
x_val <- ALR_p[index$test,]

rf_model <- randomForest(x_cal[-4,], x_cal$P_Morgan_m, importance=TRUE, localImp=TRUE)

p_morgan_pred_cal <- predict(rf_model, x_cal)
p_morgan_pred_val <- predict(rf_model, x_val)

# ploting the predicted results

ref_pred_plot_cal(x_cal$P_Morgan_m,p_morgan_pred_cal, 'P Morgan')
ref_pred_plot_val(x_val$P_Morgan_m,p_morgan_pred_val, 'P Morgan')
ref_pred_plot_cal_val(x_cal$P_Morgan_m,p_morgan_pred_cal, 'P Morgan', x_val$P_Morgan_m,p_morgan_pred_val)

#MLR 

help(lm)


MLR <- lm(P_Morgan_m ~ Landuse + Season + pH + Al_mgkg + Ca_mgkg + Fe_mgkg + P_mgkg, data = x_cal)
summary(MLR)

MLR_pred_cal <- predict(MLR, x_cal)
MLR_pred_val <- predict(MLR, x_val)


ref_pred_plot_cal(x_cal$P_Morgan_m,MLR_pred_cal, 'P Morgan')
ref_pred_plot_val(x_val$P_Morgan_m,MLR_pred_val, 'P Morgan')
ref_pred_plot_cal_val(x_cal$P_Morgan_m,MLR_pred_cal, 'P Morgan', x_val$P_Morgan_m,MLR_pred_val)

# CORRELATION MATRIX

install.packages("psych")
library(psych)

pairs.panels(x_cal[3:8], lm=TRUE, stars = TRUE, ci=TRUE, smooth =TRUE)
pairs.panels(x_val[3:8], lm=TRUE, stars = TRUE)
pairs.panels(extval, lm=TRUE, stars = TRUE)
