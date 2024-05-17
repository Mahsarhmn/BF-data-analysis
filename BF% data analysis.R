######################################
###BF% data analysis
######################################
library(MASS)
library(tidyverse)
library(ggplot2)
library(GGally)
library(glmnet) ###Lasso
library (pls)   ###PCA
library(openxlsx)
library(rstatix)
library(xtable)
library(kableExtra)
library(arsenal)

###Load data


load(file = "salamat")
dsl  = salamat %>% select("sex","age", "weight", "height", "wc", "hc", "ac", "BF%", "tricept(left)", "tricept(right)",
                          "abdominal(left)", "abdominal(right)", "subscapular(left)", "subscapular(right)",
                          "obesity(family history)")%>%
                           rowwise() %>%
                           mutate(tricept = mean(c(`tricept(left)`, `tricept(right)`))) %>%
                           mutate(abdominal = mean(c(`abdominal(left)`, `abdominal(right)`))) %>%
                           mutate(clavicle = mean(c(`subscapular(left)`, `subscapular(right)`))) %>%
                           mutate(gender = case_when(sex == 2 ~ "female",
                                                     sex == 1 ~ "male")) %>%
                           mutate(bmi = weight/((height/100)^2)) %>%
                           select(gender, age, bmi, BF = "BF%", wc, hc, ac, tricept, abdominal, clavicle) %>%
                           drop_na()


#write.xlsx(dsl, file = "C:/example/BF% paper/BF% data analysis/dsl.xlsx")

###########################
### descriptive statistics
###########################

tab1 = tableby(gender ~ ., data = dsl, numeric.stats = c("mean","q1q3", "sd"))
summary(tab1)



#################
###group by sex
################

mdsl = dsl %>% filter(gender == "male") %>% #n =426
               filter(!age >19.5, bmi <50 & bmi >15 & !bmi>40, BF >0 & BF <50,
                      wc>50, hc>70, ac<40 & ac>16,
                      tricept<40, abdominal<60 & abdominal >5, clavicle<50 ) %>%
               mutate(BFsq = BF^2) %>%
               drop_na()



fdsl = dsl %>% filter(gender == "female") %>%     ##n = 674
               filter(!age>19.5, !bmi>35 & bmi >15, BF<50 & BF>5,
                      wc>50 & !wc>110 , hc >70 & hc <115, ac<38 & ac >10,
                      tricept<40 & tricept >5, abdominal<45, clavicle<41 & clavicle>5) %>%
               mutate(BFsq = BF^2) %>%
               drop_na()




newdsl = rbind(fdsl, mdsl)


###Hist plot
png("hp.png", width = 1392, height = 772)
hp = newdsl %>% group_by(gender) %>% drop_na() %>%
  gather(key = "variable", value = "measurement", -gender) %>%
  ggplot( aes(x = measurement, color = gender)) +
  geom_histogram( color = "#efe9ef", alpha = 0.6, position = 'identity') +
  facet_grid(gender~variable, scale = "free_x")

hp
dev.off()

###Normality test

nt_girls = dsl %>% drop_na() %>%
           filter(gender == "female") %>% select (-c(gender)) %>%
           gather(key = "variable", value = "value") %>% group_by(variable) %>%
           nest() %>%
           mutate(ks_test = map2(data,"pnorm", stats::ks.test)) %>%
           mutate(ks_stat = map_dbl(ks_test, "p.value"))
nt_girls


nt_boys = dsl %>% drop_na() %>%
          filter(gender == "male") %>% select (-c(gender)) %>%
          gather(key = "variable", value = "value") %>% group_by(variable) %>%
          nest() %>%
          mutate(ks_test = map2(data,"pnorm", stats::ks.test)) %>%
          mutate(ks_stat = map_dbl(ks_test, "p.value"))
nt_boys

###Log transformation

tdsl =  dsl %>%
        mutate(across(
                      .cols = contains(variable.names(dsl)[-1]),
                      .fns = sqrt))


###Pairs plot
   png("pp.png", width = 1392, height = 772)
   pp = newdsl %>% group_by(gender) %>%
        ggpairs(aes(colour = gender, alpha = 0.4))
   pp
   dev.off()

#################################
### Box-cox tranformation for BF%
#################################

  fdsl = dsl %>% filter(gender == "female")
     fit_BF_f = lm(BF ~ 1, data = fdsl)
     b_BF_f = boxcox(lm(BF ~ 1, data = fdsl))
     lambda_BF_f = b_BF_f$x[which.max(b_BF_f$y)]
     lambda_BF_f  ## 0.99
     ks.test((fdsl$BF ^ lambda_BF_f - 1) / lambda_BF_f,"pnorm")

  mdsl = dsl %>% filter(gender == "male")
    fit_BF_m = lm(BF ~ 1, data = mdsl)
    b_BF_m = boxcox(lm(BF ~ 1, data = mdsl))
    lambda_BF_m = b_BF_m$x[which.max(b_BF_m$y)]
    lambda_BF_m  ## 0.06
    ks.test((mdsl$BF ^ lambda_BF_m - 1) / lambda_BF_m,"pnorm")






#################################
### Box cox tranformation for age
#################################


  fit_age_f = lm(age ~ 1, data = fdsl)
  b_age_f = boxcox(lm(age ~ 1, data = fdsl))
  lambda_age_f = b_age_f$x[which.max(b_age_f$y)]
  lambda_age_f  ## -2
  ks.test((fdsl$age ^ lambda_age_f - 1) / lambda_age_f,"pnorm")


  fit_age_m = lm(age ~ 1, data = mdsl)
  b_age_m = boxcox(lm(age ~ 1, data = mdsl))
  lambda_age_m = b_age_m$x[which.max(b_age_m$y)]
  lambda_age_m  ## -2
  ks.test((mdsl$age ^ lambda_age_m - 1) / lambda_age_m,"pnorm")


####################################
### Box cox tranformation for height
####################################

  fit_height_f = lm(height ~ 1, data = fdsl)
  b_height_f = boxcox(lm(height ~ 1, data = fdsl))
  lambda_height_f = b_height_f$x[which.max(b_height_f$y)]
  lambda_height_f  ## -0.02
  ks.test((fdsl$height ^ lambda_height_f - 1) / lambda_height_f,"pnorm")


  fit_height_m = lm(height ~ 1, data = mdsl)
  b_height_m = boxcox(lm(height ~ 1, data = mdsl))
  lambda_height_m = b_height_m$x[which.max(b_height_m$y)]
  lambda_height_m  ## 1.96
  ks.test((mdsl$height ^ lambda_height_m - 1) / lambda_height_m,"pnorm")

  fdsl = fdsl %>% mutate(BF_n = (fdsl$BF ^ lambda_BF_f - 1) / lambda_BF_f,
                         age_n = (fdsl$age ^ lambda_age_f - 1) / lambda_age_f,
                         height_n = (fdsl$height ^ lambda_height_f - 1) / lambda_height_f)
  mdsl = mdsl %>% mutate(BF_n = (mdsl$BF ^ lambda_BF_m - 1) / lambda_BF_m,
                         age_n = (mdsl$age ^ lambda_age_m - 1) / lambda_age_m,
                         height_n = (mdsl$height ^ lambda_height_m - 1) / lambda_height_m)

  dsl = rbind(fdsl, mdsl)

##############################
### Variable selection (lasso)
##############################
                                  ###girls###

x_f = model.matrix (scale(BFsq) ~., fdsl[, -c(1,2,4)])[,-1]
y_f = scale(fdsl$BFsq)

set.seed (1)
 train_f = sample (1: nrow(x_f), .7*nrow(x_f))
 test_f = (- train_f )
 y.test_f = y_f[test_f]



lasso.mod_f = glmnet (x_f[train_f ,], y_f[train_f], alpha = 1)

set.seed (1)
  cv.out_f =  cv.glmnet (x_f[train_f ,], y_f[train_f], alpha = 1)
  plot(cv.out_f)
  bestlam_f = cv.out_f$lambda.min

  lasso.pred_f = predict (lasso.mod_f, s = bestlam_f, newx = x_f[test_f ,])
  mean(( lasso.pred_f -y.test_f)^2) ## 0.33

  out_f = glmnet (x_f, y_f, alpha =1, lambda = bestlam_f)
  lasso.coef_f = predict (out_f, type = "coefficients", s = bestlam_f )
  lasso.coef_f
  lasso.coef_f[lasso.coef_f != 0] ###  coefficients of  ac and age were zero



  SS.residual = sum(( lasso.pred_f -y.test_f)^2)
  SS.regression = sum((lasso.pred_f - mean(y.test_f))^2)
  SS.total = (SS.regression+SS.residual)
  test.rsq = 1 - SS.residual/SS.total
  test.rsq ### 0.67


  BF_n_f_sep = sqrt(sum((y.test_f - lasso.pred_f)^2)/length(y.test_f))
  BF_n_f_sep # 5.28

  BF_n_f_mpe = mean(abs((y.test_f - lasso.pred_f)/y.test_f))*100
  BF_n_f_mpe #29.31

                               ###boys###



   x_m = model.matrix (scale(BFsq) ~., mdsl[, -c(1,4)])[,-1]
   y_m = scale(mdsl$BFsq)

set.seed (1)
  train_m = sample (1: nrow(x_m), .7*nrow(x_m))
  test_m = (- train_m )
  y.test_m = y_m[test_m]


  lasso.mod_m = glmnet (x_m[train_m ,], y_m[train_m], alpha = 1)

set.seed (1)
  cv.out_m =  cv.glmnet (x_m[train_m ,], y_m[train_m], alpha = 1)
  plot(cv.out_m)
  bestlam_m = cv.out_m$lambda.min

  lasso.pred_m = predict (lasso.mod_m, s = bestlam_m, newx = x_m[test_m ,])
  mean(( lasso.pred_m -y.test_m)^2) ###MSE


  out_m = glmnet (x_m, y_m, alpha =1, lambda = bestlam_m)
  lasso.coef_m = predict (out_m, type = "coefficients", s = bestlam_m )
  lasso.coef_m
  lasso.coef_m[lasso.coef_m != 0] ###  coefficients of hc were zero


  SS.residual = sum(( lasso.pred_m -y.test_m)^2)
  SS.regression = sum((lasso.pred_m - mean(y.test_m))^2)
  SS.total = (SS.regression+SS.residual)
  test.rsq = 1 - SS.residual/SS.total  ### 0.81


  BF_n_m_sep = sqrt(sum((y.test_m - lasso.pred_m)^2)/length(y.test_m))
  BF_n_m_sep # 0.32

  BF_n_m_mpe = mean(abs(y.test_m - lasso.pred_m)/y.test_m)*100
  BF_n_m_mpe#8.83

###########################
### lm fit
###########################

### girls ###
lm_f = lm(scale(BFsq) ~ bmi + hc + wc + tricept + abdominal + clavicle,
          data = as.data.frame(fdsl[train_f, ])[-c(68, 438, 175, 21, 424, 77, 169, 247,
                                                   213, 5, 163, 236, 16, 321, 55, 304,
                                                   86, 194, 237, 396, 269, 265),])


  test.pred = predict(lm_f)
  test.y = scale(fdsl_n$BFsq)
summary(lm_f)
par(mfrow = c(2,2))
plot(lm_f)




fdsl_n = as.data.frame(fdsl[train_f, ])[-c(68, 438, 175, 21, 424, 77, 169, 247,
                                           213, 5, 163, 236, 16, 321, 55, 304,
                                           86, 194, 237, 396, 269, 265), ]

set.seed(1)
rf = fdsl_n[sample(nrow(fdsl_n), nrow(fdsl_n), replace = FALSE),]
spf = split(1:nrow(fdsl_n), cut(1:nrow(fdsl_n), quantile(1:nrow(fdsl_n), prob = 0:7/ 7, names = FALSE), include = TRUE))
cv_f = matrix(0, nrow = 7, ncol = 3)
colnames(cv_f) = c("R2", "SEP", "MAPE")
for(i in 1:7){
  str = spf[[i]]
  dva = rf[str, ]
  dtr = rf[-str,]
  f = lm(scale(BFsq) ~ bmi + hc + wc + tricept + abdominal + clavicle, data = dtr)
  test.pred = predict(f, newdata = dva)
  test.y = dva$BFsq
  SS.total = sum((test.y - mean(test.y))^2)
  SS.residual = sum((test.y - test.pred)^2)
  SS.regression = sum((test.pred - mean(test.y))^2)
  SS.total = (SS.regression + SS.residual)
  test.rsq = 1 - SS.residual/SS.total

  test_sep = sqrt(sum((test.y - test.pred)^2)/length(test.y))

  test_mpe = mean(abs(test.y - test.pred)/test.y)*100

  cv_f[i,1] = test.rsq
  cv_f[i,2] = test_sep
  cv_f[i,3] = test_mpe
}

test_score_f = apply(cv_f, 2, mean)
round(test_score_f, 2)   ## .70, 0.54, 18.91


### boys ###

lm_m = lm(scale(BFsq) ~ bmi + wc + ac + tricept + abdominal + clavicle,
          data = as.data.frame(mdsl[train_m, ])[-c(288, 224, 285, 147, 296, 297,
                                                   282, 239, 92, 97, 30, 96, 233,
                                                   74, 179),])

summary(lm_m)
par(mfrow = c(2,2))
plot(lm_m)


mdsl_n = as.data.frame(mdsl[train_m, ])[-c(288, 224, 285, 147, 296, 297,
                                           282, 239, 92, 97, 30, 96, 233,
                                           74, 179),]


set.seed(3)
rm = mdsl_n[sample(nrow(mdsl_n), nrow(mdsl_n), replace = FALSE),]
spm = split(1:nrow(mdsl_n), cut(1:nrow(mdsl_n), quantile(1:nrow(mdsl_n), prob = 0:10/ 10, names = FALSE), include = TRUE))
cv_m = matrix(0, nrow = 10, ncol = 3)
colnames(cv_m) = c("R2", "SEP", "MAPE")
for(i in 1:10){
  str = spm[[i]]
  dva = rm[str, ]
  dtr = rm[-str,]
  f = lm(scale(BFsq) ~ bmi + wc + ac + tricept + abdominal + clavicle, data = dtr)
  test.pred = predict(f, newdata = dva)
  test.y = scale(dva$BFsq)
  SS.total = sum((test.y - mean(test.y))^2)
  SS.residual = sum((test.y - test.pred)^2)
  SS.regression = sum((test.pred - mean(test.y))^2)
  SS.total = (SS.regression + SS.residual)
  test.rsq = 1 - SS.residual/SS.total

  test_sep = sqrt(sum((test.y - test.pred)^2)/length(test.y))

  test_mpe = mean(abs(test.y - test.pred)/test.y)*100

  cv_m[i,1] = test.rsq
  cv_m[i,2] = test_sep
  cv_m[i,3] = test_mpe
}

test_score_m = apply(cv_m, 2, mean)
round(test_score_m, 2)   ## .81, 4.24, 7.27


##################################
### Variable selection (PCA)
##################################
library("FactoMineR")
library("factoextra")

### pca for girls ###

fpca = PCA(fdsl[, c(5:10)], graph = FALSE)
summary(fpca)
f.eig.val = get_eigenvalue(fpca)
fviz_eig(fpca, addlables = TRUE, ylim = c(0, 80))

f.var_pca = get_pca_var(fpca)
head(f.var_pca$cos2)

fviz_contrib(fpca, choice = "var", axes = 1)
fviz_contrib(fpca, choice = "var", axes = 2)

fviz_pca_var(fpca, col.var = "black")

fviz_pca_var(fpca, col.var = "cos2",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE # Avoid text overlapping
)


fbp = fviz_pca_biplot(fpca, col.var = "cos2",
                     gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
                     repel = TRUE, # Avoid text overlapping
                     geom = c("point"),
                     title = "PCA - Biplot for Girls",
                     col.ind = "gray90",
                     addEllipses = F
)

plot(fbp)



#Add PC to data
fdslpca = cbind(fdsl[,c(3,11)], fpca$ind$coord[,c(1:2)])
names(fdslpca)[3:4] = c("PC1", "PC2")
head(fdslpca)


#Train & Test

x_fpca = model.matrix (BFsq ~., fdslpca)[,-1]
y_fpca = fdslpca$BFsq

set.seed (1)
train_fpca = sample (1: nrow(x_fpca), .7*nrow(x_fpca))
test_fpca = (- train_fpca )
y.test_fpca = y_fpca[test_fpca]

lm_fpca = lm(scale(BFsq) ~ bmi + PC1 + PC2, data = as.data.frame(fdslpca[train_fpca,],  row.names = c(1:nrow(fdslpca[train_fpca,])))[-c(175, 68, 213, 194, 24, 438,
                                                                                                                                        169, 321, 236, 424, 21, 83,
                                                                                                                                        16, 55, 247, 180, 5, 388, 1,
                                                                                                                                        237, 86, 360),])

                                                               #  row.names = c(1:nrow(fdslpca[train_fpca,])))[-c(132, 275, 99,353,353,191,74,
                                                                   #                                                                          166, 244, 425, 311, 27, 460, 226),])

   #
summary(lm_fpca)
par(mfrow = c(2,2))
plot(lm_fpca)

#par(mfrow = c(1,2))
#hist(fdslpca$BFsq, main = "BF^2")
#hist(sqrt(fdslpca$BFsq), main = "BF")


fdslpca_n = as.data.frame(fdslpca[train_fpca,],
                          row.names = c(1:nrow(fdslpca[train_fpca,])))[-c(175, 68, 213, 194, 24, 438,
                                                                          169, 321, 236, 424, 21, 83,
                                                                          16, 55, 247, 180, 5, 388, 1,
                                                                          237, 86, 360),]


set.seed(1)
rfpca = fdslpca_n[sample(nrow(fdslpca_n), nrow(fdslpca_n), replace = FALSE),]
spfpca = split(1:nrow(fdslpca_n), cut(1:nrow(fdslpca_n), quantile(1:nrow(fdslpca_n), prob = 0:8/ 8, names = FALSE), include = TRUE))
cv_fpca = matrix(0, nrow = 8, ncol = 3)
colnames(cv_fpca) = c("R2", "SEP", "MAPE")
for(i in 1:8){
  str = spfpca[[i]]
  dva = rfpca[str, ]
  dtr = rfpca[-str,]
  f = lm(scale(BFsq) ~ bmi + PC1 + PC2, data = dtr)
  test.pred = sqrt(predict(f, newdata = dva)*sd(dtr$BFsq)+mean(dtr$BFsq))
  test.y = sqrt(dva$BFsq)
  SS.total = sum((test.y - mean(test.y))^2)
  SS.residual = sum((test.y - test.pred)^2)
  SS.regression = sum((test.pred - mean(test.y))^2)
  SS.total = (SS.regression + SS.residual)
  test.rsq = 1 - SS.residual/SS.total

  test_sep = sqrt(sum((test.y - test.pred)^2)/length(test.y))

  test_mpe = mean(abs(test.y - test.pred)/test.y)*100

  cv_fpca[i,1] = test.rsq
  cv_fpca[i,2] = test_sep
  cv_fpca[i,3] = test_mpe
}

test_score_fpca = apply(cv_fpca, 2, mean)
round(test_score_fpca, 2)   ## 0.68,  5.02, 22.89


### pca for boys ###

mpca = PCA(mdsl[, c(5:10)])
m.eig.val = get_eigenvalue(mpca)
fviz_eig(mpca, addlables = TRUE, ylim = c(0, 80))

m.var_pca = get_pca_var(mpca)
head(m.var_pca$cos2)

fviz_pca_var(mpca, col.var = "black")

fviz_pca_var(mpca, col.var = "cos2",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE # Avoid text overlapping
)


mbp = fviz_pca_biplot(mpca, col.var = "cos2",
                     gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
                     repel = TRUE, # Avoid text overlapping
                     geom = c("point"),
                     title = "PCA - Biplot for Boys",
                     col.ind = "gray90",
                     addEllipses = F
)

plot(mbp)




#Add PC to data
mdslpca = cbind(mdsl[,c(3,11)], mpca$ind$coord[,c(1:2)])
names(mdslpca)[3:4] = c("PC1", "PC2")
head(mdslpca)

#Train & Test

x_mpca = model.matrix (BFsq ~., mdslpca)[,-1]
y_mpca = mdslpca$BFsq

set.seed (1)
train_mpca = sample (1: nrow(x_mpca), .7*nrow(x_mpca))
test_mpca = (- train_mpca )
y.test_mpca = y_mpca[test_mpca]

lm_mpca = lm(scale(sqrt(BFsq)) ~ bmi + PC1 + PC2, data = as.data.frame(mdslpca[train_mpca,],
                                                          row.names = c(1:nrow(mdslpca[train_mpca,])))[-c(211, 3, 146, 83, 30, 297, 261,
                                                                                                          93, 29, 80, 145, 151),])




summary(lm_mpca)
par(mfrow = c(2,2))
plot(lm_mpca)


#par(mfrow = c(1,3))
#hist(mdslpca$BFsq, main = "BF^2")
#hist(sqrt(mdslpca$BFsq), main = "BF")
#hist(log(sqrt(mdslpca$BFsq)), main = "log_BF")

mdslpca_n = as.data.frame(mdslpca[train_mpca,],
                          row.names = c(1:nrow(mdslpca[train_mpca,])))[-c(211, 3, 146, 83, 30, 297, 261,
                                                                          93, 29, 80, 145, 151),]




set.seed(1)
rmpca = mdslpca_n[sample(nrow(mdslpca_n), nrow(mdslpca_n), replace = FALSE),]
spmpca = split(1:nrow(mdslpca_n), cut(1:nrow(mdslpca_n), quantile(1:nrow(mdslpca_n), prob = 0:7/ 7, names = FALSE), include = TRUE))
cv_mpca = matrix(0, nrow = 7, ncol = 3)
colnames(cv_mpca) = c("R2", "SEP", "MAPE")
for(i in 1:7){
  str = spmpca[[i]]
  dva = rmpca[str, ]
  dtr = rmpca[-str,]
  f = lm(scale(sqrt(BFsq)) ~ bmi + PC1 + PC2, data = dtr)
  test.pred = predict(f, newdata = dva)*sd(sqrt(dtr$BFsq))+mean(sqrt(dtr$BFsq))
  test.y = sqrt(dva$BFsq)
  SS.total = sum((test.y - mean(test.y))^2)
  SS.residual = sum((test.y - test.pred)^2)
  SS.regression = sum((test.pred - mean(test.y))^2)
  SS.total = (SS.regression + SS.residual)
  test.rsq = 1 - SS.residual/SS.total

  test_sep = sqrt(sum((test.y - test.pred)^2)/length(test.y))

  test_mpe = mean(abs(test.y - test.pred)/test.y)*100

  cv_mpca[i,1] = test.rsq
  cv_mpca[i,2] = test_sep
  cv_mpca[i,3] = test_mpe
}

test_score_mpca = apply(cv_mpca, 2, mean)
round(test_score_mpca, 2)   ## 0.88,  2.73, 17.27


### predicted vs actual values

ly = layout(matrix(c(1,2,3,3), nr=2, byrow = TRUE))
#par(mgp = c(3,.5,0), mar = c(1.5,1,1,1), oma = c(1,3,1.5,3), lwd = .3)
layout.show(ly)


mdslpca_n
p1 = plot(bbc2[-c(80,85,61,47,41,74,20,114),]$TrF,
          exp(b2.TrFp*sd(log(bbc2[-c(80,85,61,47,41,74,20,114),]$TrF))
              + mean(log(bbc2[-c(80,85,61,47,41,74,20,114),]$TrF))),
          xlab = "", ylab = "", main = "Trunk Fat",
          xlim = c(1000, 12000), ylim = c(500, 10000),
          cex.main  = .7,pch = 16,
          cex = .5, col = "#00ffff", cex.axis=.4)


points(bbc1[-c(3,17,47,73,94,100,83,63,39,79),]$TrF,
       exp(b1.TrFp*sd(log(bbc1[-c(3,17,47,73,94,100,83,63,39,79),]$TrF))
           + mean(log(bbc1[-c(3,17,47,73,94,100,83,63,39,79),]$TrF))),
       pch = 17, cex = .5, col = "#ff0080")











