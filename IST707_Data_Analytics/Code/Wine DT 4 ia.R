


# DT WINE===============================================================

# load data=======================================================
setwd('D:/documents/888_school/06_IST_707/final_project/')
library(partykit)
library(DescTools)
wide_wine = read.csv('input/wide_wine_1.csv')
Wine <- read.csv('input/winemag-data-130k-v3.csv')

# create labels only df
labels_df <- Wine[,c(1,13)]
head(labels_df)

ww_bin <- wide_wine
# transform frequencies to binary=============================================

ww_bin[ww_bin> 0] <- 1
head(ww_bin)

# solution sourced from https://stackoverflow.com/questions/9251326/convert-data-frame-column-format-from-character-to-factor

# MATCHING LABELS ========================================================
# To do it for all names
ww_bin[] <- lapply( ww_bin, factor) # the "[]" keeps the dataframe structure
col_names <- names(ww_bin)
# do do it for some names in a vector named 'col_names'
ww_bin[col_names] <- lapply(ww_bin[col_names] , factor)


ww_bin$document <- wide_wine$document




library(sqldf)
library(dplyr)

labels_df$X <- as.numeric(labels_df$X)

# 1. fix 0 index
labels_df$X <- labels_df$X+1
labels_df$variety <- as.character(labels_df$variety)

# ww_bin$document

# ww_bin$document <- as.character(ww_bin$document)

ww_bin_labels <- inner_join(labels_df,ww_bin, by=c('X' = 'document'))

ww_bin_labels$variety

ww_bin_labels2 <- ww_bin_labels[, -1]
str(ww_bin_labels2)

cn <- colnames(ww_bin_labels)
ww_bin_labels$X
match(c("X", "variety"),cn)


# 2. limit Wine to the top 20 wines
# "Red Blend","Bordeaux Style Red Blend", "Sauvignon Blanc", "Syrah", "Rose", "Merlot"

top_wines <- c("White", "Red")


# set labels
labels <- ww_bin_labels$variety

ww_bin_labels_topwines <- ww_bin_labels[ww_bin_labels$variety %in% top_wines,]


#
#
# RED FRAME
#

# 
# ww_bin_labels_topwines_safe_copy <- ww_bin_labels_topwines
# ww_bin_labels_topwines$variety <- arr
# ww_bin_labels_topwines$variety <- as.factor(ww_bin_labels_topwines$variety)

table(ww_bin_labels_topwines$variety)
head(ww_bin_labels_topwines)
ww_bin_labels_topwines2 <- ww_bin_labels_topwines[,-1]

class(ww_bin_labels_topwines$variety)
dim(ww_bin_labels_topwines)

ww_bin_labels2$variety <- as.factor(ww_bin_labels2$variety)
ww_bin_labels_topwines2$variety <- as.factor(ww_bin_labels_topwines2$variety)
# ww_bin_labels_topwines$variety <- as.factor(ww_bin_labels_topwines$variety)
str(ww_bin_labels_topwines2)

table(ww_bin_labels_topwines2$variety)
set.seed(210)


smp_size <- floor(0.66 * nrow(ww_bin_labels_topwines))

dim(ww_bin_labels_topwines)
## set the seed to make your partition reproducible
# set.seed(1248) 
train_ind <- sample(seq_len(nrow(ww_bin_labels_topwines)), size = smp_size,replace = F)
length(train_ind) # 27771

train <- ww_bin_labels_topwines[train_ind, ]
test <- ww_bin_labels_topwines[-train_ind, ]
# create a place to parse train index
parse_at<- length(row.names(train))/2
test_ind_g2 <-as.numeric(row.names(train)[1:parse_at])
test_ind_g2
test_ind_g3 <- as.numeric(row.names(train))[parse_at:length(row.names(train))]
sum(test_ind_g2 %in% test_ind_g3)
sum(as.numeric(rownames(test)) %in% test_ind_g2)
sum(as.numeric(rownames(test)) %in% test_ind_g3)

test$X



# create 2 other training sets from test
train2<- ww_bin_labels_topwines[-test_ind_g2, ]
t2_pn <- train2[train2$variety=='Red',]
t2_pn <- t2_pn[1:nrow(t2_rs),]
t2_char <- train2[train2$variety=='White',]
t2_char <- t2_char[1:nrow(t2_rs),]

df <-as.data.frame(c(t2_pn, t2_char))

dim(df)
# sample(df)
df <- df[sample.int(nrow(df)),]
head(df)


table(train2$variety)
test2 <- ww_bin_labels_topwines[as.numeric(row.names(ww_bin_labels_topwines)) %in% test_ind_g2, ]
head(test2)
train3 <- ww_bin_labels_topwines[-test_ind_g3, ]
test3 <- "a"
ww_bin_labels_topwines[1, ]




# model #1: try all wines (no prediction)=================================================
library(rpart)
library(rpart.plot)
library(rattle)
library(RColorBrewer)
library(party)
library(partykit)
library(caret)



df= ww_bin_labels_topwines[,-1]

dt_fit <- rpart(variety ~., method="class", parms = list(split= 'information'), 
                data=ww_bin_labels_topwines, control = rpart.control(cp=.003, minbucket = 300) )
summary(dt_fit)

rpart.plot(dt_fit, cex=.7,extra = 0, fallen.leaves = F)
dt_fit_xval5 <- rpart(variety ~., method="class", parms = list(split= 'information'), 
                data=ww_bin_labels_topwines, control = rpart.control(cp=.003, minbucket = 300, xval = 3) )
table(ww_bin_labels_topwines$variety)
str(ww_bin_labels_topwines)


prp(dt_fit,box.palette = 'auto',extra=104, main = "Decision tree: predict wine based on word frequency.")

prp(dt_fit,box.palette = 'auto',extra=104, main = "Decision tree: predict wine based on word frequency.")

# but the xerror is not hitting a low point.
a <- as.data.frame(printcp(dt_fit))
plot(a$nsplit,a$xerror, pch=16, col='dodgerblue', main='DT-1 X-Error by split')

printcp(dt_fit_xval5)
prune_dt <- prune(dt_fit, 0.0030000+0.0047929)
prune_dt
barplot(dt_fit$variable.importance, main='DT-1 variable importance.') 

dt_ff_1 <- ww_bin_labels_topwines %>% select(variety, black, noir, red, 
                                             lime, white, yellow, tannic, plum, crisp, dark, stone, off, herbal, light, wild, fresh, now, firm, tart)

dt_ff_1


dt_fit_f <- rpart(variety ~., method="class", parms = list(split= 'information'), 
                data=dt_ff_1, control = rpart.control(cp=.003, minbucket = 300) )


printcp(dt_fit_f)

prp(prune_dt,box.palette = 'auto',extra=104, nn = T, main = "Pruned decision tree #1.")





# + CONFUSION MATRIX================================
					
levels(pred_cat_list)


nrow(test)
length(train_ind)
nrow(train2)
nrow(test2)
(length(train_ind2))
1: (length(train_ind)/2)
length(test$X)
# train$label <- label_x
dim(train)# 27771   168

length(label_x) 

head(test)

dt1_cf <- as.data.frame(table(acc_df))
levels(pred_cat_list)

library(ggplot2)
ggplot(data =  dt1_cf, mapping = aes(x = label, y = method)) +
  
train_labels <- train[,c(1:2)]
train_labels2 <- train2[,c(1:2)]

train_X <- train[,-1]
train_X2 <- train2[,-1]

train_X
test_X <- test[,-c(1:2)]
test_X2 <- test2[,-c(1:2)]
test_Y <- test$variety
test_Y2 <- test2$variety

# model #1: predict labels using 70/30 split========
dt_fitted <- rpart(variety ~., method="class", parms = list(split= 'information'), 
                data=train_X, control = rpart.control(cp=.000002, minbucket = 200) )
(dt_fitted)
rpart.plot(dt_fitted, cex=.7,extra = 0, fallen.leaves = F)

prp(dt_fitted,box.palette = 'auto',extra=100, main = "Decision tree: predict wine based on word frequency.")


summary(dt_fitted)
# but the xerror is not hitting a low point.
xerror_df <-printcp(dt_fitted)
xerror_df <-as.data.frame(xerror_df)
# model 1.1 prune model only ==========
# pru

# find xerror min
min_err <- xerror_df[xerror_df$xerror == min(xerror_df$xerror),]

prune_dt <- prune(dt_fit, min_err$CP + min_err$xstd)

prp(prune_dt,box.palette = 'auto',extra=104, main = "Pruned decision tree: predict wine based on word frequency.")

# test pred
# predict 1
preds <- predict(dt_fitted, test_X )
preds
test_Y
table(preds, test_Y)


preds <- as.data.frame(preds)



rm(pred_cat_list)
rm(max_vec)
pred_cat_list = vector()
max_vec = vector()
for (row_index in 1:nrow(preds)){
  cat = which.max(preds[row_index,])
  maxx = max(preds[row_index,])
  max_vec <- c(max_vec, maxx)
  pred_cat_list <- c(pred_cat_list, names(cat))
}

pred_cat_list <- as.factor(pred_cat_list)
table(pred_cat_list, test_Y)



# model 2==========================================
dt_fitted2 <- rpart(variety ~., method="class", parms = list(split= 'information'), 
                   data=train, control = rpart.control(cp=.0002, minbucket = 20) )
dt_fitted2
View(train)

rpart.plot(dt_fitted2, cex=.9,extra = 0, fallen.leaves = F)

prp(dt_fitted2,box.palette = 'auto',extra=100, main = "Decision tree #2: predict wine based on word frequency.")

# but the xerror is not hitting a low point.
printcp(dt_fitted2)

prune_dt2 <- prune(dt_fitted2, 0.00056792+0.0039410)
prune_dt2
prp(prune_dt2,box.palette = 'auto',extra=104, main = "Decision tree #2 pruned: predict wine based on word frequency.")


# model 3=======================================================
dim(ww_bin_labels_topwines)
dim(train)
cut_at <- floor(nrow(df)*.66)
train3 <- df[1:cut_at, ]
test3 <- df[(cut_at+1):length(df),]
library(partykit)

dt_fitted3 <- rpart(variety ~., method="class", parms = list(split= 'information'), 
                    data=train[,-c(1)], control = rpart.control(cp=.00002, minbucket = 20,xval=3 ))
dt_fitted3
rpart.plot(dt_fitted3, cex=.5,extra = 100, fallen.leaves = F)
prp(dt_fitted3,box.palette = 'auto',cex=.6,extra=100, main = "Model #12 grown tree:\npredict wine based on description.",sub="cp=.00002, minbucket = 20,xval=3")
 printcp(dt_fitted3)


a <- as.data.frame(printcp(dt_fitted3))


minat = a[a$xerror ==min(a$xerror),]
pruneat <- minat$CP+minat$xstd

plot(a$nsplit,a$xerror, pch=16, col='purple', main='Prune by Split X-Error')
abline(v=29, col="red", lty=2)
# abline(h=min(a$xerror), col="yellow")

prun_3 <-prune(dt_fitted3, 3.5923e-04+0.0055996)
prun_4 <- prune(dt_fitted3,pruneat)
prp(prun_3,box.palette = 'auto', roundint=FALSE, cex=.8,extra=100, main = "Model #12 Pruned:\npredict wine based on description.",sub="cp=.00596, minbucket = 20,xval=3")
# train3 <- train3[,-1]
# ctrl<- ctree_control( mincriterion = 0.09, minsplit = 400, minbucket = 109)
# dt3 <-ctree(variety ~ .,data = train3, controls = ctrl)
# plot(dt3)

table(test3$variety)
pred3 <-predict(prun_3, test[,-c(1:2)],type="class")
pred3
# 
pred4 <-predict(prun_3, test[,-c(1:2)])
pred4


# as.factor(test$variety) -> test$variety
# length(test$variety)
# length(pred3)

(6194+4558)/ (6194+4558 + 1496 + 1241)
plot(t, col="dodgerblue",  main = "confusion matrix")


# WHITE RED FINAL MODEL.===============================================================================
t <- table(as.factor(pred3),as.factor(test$variety))
t

plotROC(truth=as.factor(test$variety),predicted = pred3)
confusionMatrix(predicted = t$as.factor.pred3., actual = t$as.factor.test.variety.)
  


m<- cbind(as.factor(pred3),as.factor(test$variety))
dfm <- as.data.frame(m)
confusionMatrix(as.factor(pred3),test$variety)
str(pred3)
str(test$variety)
levels(pred3)
levels(test$variety2)
table(pred3, test$variety2)
test$variety2 <- as.factor(test$variety)
CFM_BEST <- confusionMatrix(pred3, test$variety2)
CFM_BEST
class(pred3)
pred3
class(test['variety'])
as.character(test$variety) -> actuals_y
as.factor(actuals_y) -> y
levels.default(pred)

confusionMatrix(pred3, y)
xxx <-droplevels(y)
y <-as.factor(xxx)

confusionMatrix(pred3, y)


plotROC(predicted = pred4[,2], test$variety, main="Red/White Receiver Operating Characteristic")
dfm

auc(dfm$pred3, dfm$V2)

#------------------------------* * * -----------------------------------------------

# loop thru
rm(pred_cat_list)
rm(max_vec)
pred_cat_list = vector()
max_vec = vector()
for (row_index in 1:nrow(preds)){
  cat = which.max(preds[row_index,])
  maxx = max(preds[row_index,])
  max_vec <- c(max_vec, maxx)
  pred_cat_list <- c(pred_cat_list, names(cat))
}
# test3_Y <- as.factor(test3$variety)
pred_cat_list <- as.factor(pred_cat_list)
acc_df3 <- cbind(pred_cat_list, test[,-c(1:2)])
acc_df3 <- as.data.frame(acc_df3)
1340+2713+1029+1075
sum(acc_df3)
table(acc_df3)

acc_df3
# make confusion matrix for test============================
library(caret)
library(ModelMetrics)

## CFM-1======================================
table(acc_df)
rm(pred_cat_list)
rm(max_vec)
pred_cat_list = vector()
max_vec = vector()
for (row_index in 1:nrow(preds)){
  cat = which.max(preds[row_index,])
  maxx = max(preds[row_index,])
  max_vec <- c(max_vec, maxx)
  pred_cat_list <- c(pred_cat_list, names(cat))
}
## CFM-2=====================================
preds2 <- predict(prune_dt2, test2)
preds

rm(pred_cat_list)
rm(max_vec)
pred_cat_list = vector()
max_vec = vector()
for (row_index in 1:nrow(preds2)){
  cat = which.max(preds2[row_index,])
  maxx = max(preds2[row_index,])
  max_vec <- c(max_vec, maxx)
  pred_cat_list <- c(pred_cat_list, names(cat))
}
pred_cat_list <- as.factor(pred_cat_list)
test2_Y = as.factor(test2$variety)
acc_df2 <- cbind(pred_cat_list, test2_Y)
acc_df2 <- as.data.frame(acc_df2)


table(acc_df2)
# model 1.2: prun and subset to words used===================


cat_list[4]
levels(test_Y)
levels(pred_cat_list)

acc_df <- cbind(pred_cat_list, test_Y)
as.data.frame(acc_df) -> acc_df
# acc_df$true <- acc_df$pred_cat_list==acc_df$test_Y
acc_df
# cf matrix 4 category 
cfm <- table(pred_cat_list , test_Y)
cfm
confusionMatrix(cat_list, test_Y)
# class 1 
str(pred_cat_list)

library(ROCR)


apply
plotROC(as.factor(test$variety), pred4)
plotROC <- function(truth, predicted, ...){
  pred <- prediction(abs(predicted), truth)    
  perf <- performance(pred,"tpr","fpr")
  
  plot(perf, ...)
}

head(acc_df)
## CFM-1======================================
acc_df_1 <- acc_df
acc_df_1[acc_df_1 > 1] <- F
acc_df_1[acc_df_1 == 1] <- T
table(acc_df)

levels(test_Y)[1]
plotROC(acc_df_1$test_Y, acc_df_1$pred_cat_list, main = "Cabernet Sauvignon Receiver Operating Characteristic", sub='Wine Adjectives, adverbs, stopwords.')

plotROC(truth=test$variety2, predicted=pred4[,2], main="Model #12: Receiving Operator Characteristic.")

acc_df_2 <- acc_df
acc_df_2[acc_df_2 != 2] <- F
acc_df_2[acc_df_2 == 2] <- T
# acc_df_2[acc_df_2 == 0] <- 2
table(acc_df)

(3054+4850)/(4850+3054+3522+476)
# ROC cat 2
levels(test_Y)[2]
prc <-plotROC(acc_df_2$test_Y, acc_df_2$pred_cat_list, main = "Chardonnay Receiver Operating Characteristic", sub='Wine Adjectives, adverbs, stopwords.')

auc(prc)

# ROC cat 3
acc_df_3 <- acc_df
acc_df_3[acc_df_3 != 3] <- F
acc_df_2[acc_df_2 == 2] <- T
# acc_df_2[acc_df_2 == 0] <- 2
table(acc_df)
plotROC()




library(PRROC)
library(auc)
data(churn)
y = test['variety']
plot(pr.curve(pred3, y, curve=TRUE))



plotROC(predicted = preds$`Pinot Noir`, test_Y=='Pinot Noir', main="Pinot Noir Receiver Operating Characteristic")
plotROC(predicted = preds$Chardonnay, test_Y=='Chardonnay', main="Chardonnay Receiver Operating Characteristic")
plotROC(predicted = preds$Riesling, test_Y=='Riesling', main="Riesling Receiver Operating Characteristic")

cab <- roc(preds$`Cabernet Sauvignon`, test_Y)
pinot <- roc(preds$`Pinot Noir`, test_Y=='Pinot Noir')
cab$cutoffs
#AUC

table(acc_df2)

plot(roc(predictions = pred3, y))
preds$$cabY <- test_Y=="Cabernet Sauvignon"
library(pROC)
roc_rose <- plot(roc(), print.auc = TRUE, col = "blue")

par(mfrow=c(2,2))

library(ROCR)
data(ROCR.simple)
pred_ries <- prediction(preds$Riesling, test_Y=='Riesling')
pred_cab <- prediction(preds$Riesling, test_Y=='Cabernet Sauvignon')
pred_pinot <-  prediction(preds$`Pinot Noir`, test_Y=='Pinot Noir')
pred_chard <- prediction (preds$Chardonnay, test_Y=='Chardonnay')
cbind(pred_cab, pred_pinot)
perf <- performance(pred,"tpr","fpr")
plot(perf)
## precision/recall curve (x-axis: recall, y-axis: precision)
perf1 <- performance(pred, "prec", "rec")
plot(perf1)





ww_bin_labels_topwines$variety <- as.factor(ww_bin_labels_topwines$variety)

# model2=====================================================================
fit2 <- rpart(variety ~., method="class", parms = list(split= 'gini'), 
                data=ww_bin_labels_topwines2, control = rpart.control(),
)
rpart.plot(fit2, cex=.5)

prune.rpart(fit2)

printcp(fit2)

#
# trControl=trctrl,
# tuneLength = 10)
# ?train()

# Model 3==============================================
ctrl<- ctree_control( mincriterion = 0.0001, minsplit = 4, minbucket = 10, multiway = T)

fit <- ctree(variety~., data=ww_bin_labels_topwines2, control=ctrl)
plot(fit)



# run gini==================================================
# function
# gini_fun = function(x){
#   xx = Gini(x)
#   
#   return (xx)
#   
# }
# 
# g_vec = vector()
# sum_vec = vector()
# col_n = colnames(ww2)
# 
# col_keep_gini = vector()
# 
# for(i in 1:ncol(ww2)){
#   x <- ww2[,i]
#   
#   ginix <- gini_fun(x)
#   
#   if (ginix < .95){
#     print(col_n[i])
#     print(ginix)
#     print('\n --------')
#     col_keep_gini = c(col_keep_gini,col_n[i])
#   }
#   
# }
# 
# rm(sum_vec)
# rm(g_vec)

# X  <- ww2[, col_keep_gini]
# X <- ww2[]
# dim(X)

# smp_size <- floor(0.7 * nrow(X))
# 
# ## set the seed to make your partition reproducible
# set.seed(1248)
# train_ind <- sample(seq_len(nrow(X)), size = smp_size)
# 
# train <- X[train_ind, ]
# test <- ww2[-train_ind, ]
# # train$label <- label_x
# dim(train)
# length(label_x)
# # run dt
# train

# set labels
# label <- b$variety
# label_x <- label[train_ind]
# label_test <- label[-train_ind]
# # these should all be the same length.
# length(train$black)
# length(b$document)
# length(label_x)
# length(label)
# XX <- cbind(label_x, train)

# 
# lx <- label_x[2:length(label_x)]
# length(label_x)
# dim(train)

