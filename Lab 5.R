library(rpart)
test = as.data.frame(read.csv("LoanStats3d.csv", skip = 1))
train = as.data.frame(read.csv("LoanStats3c.csv", skip = 1))

train = train[-(nrow(train)),]
train = train[-(nrow(train)),]

test = test[-(nrow(test)),]
test = test[-(nrow(test)),]


train$highgrade = (train$grade == "A" | train$grade == "B") 
med = sum(train$highgrade)/nrow(train)
med
medianincome = median(train$annual_inc, na.rm=T)
highincome = train$annual_inc>medianincome
income_test = t.test(train$highgrade~highincome)
income_test
medianloan = median(train$loan_amnt, na.rm=T)
highloanamount = train$loan_amnt>medianloan
loan_test = t.test(train$highgrade~highloanamount)
loan_test
renters = train$home_ownership=="RENT"
home_test = t.test(train$highgrade~renters)
home_test

model = glm(train$highgrade ~ train$annual_inc + train$home_ownership + train$loan_amnt, family = binomial)
summary(model)
predictedy = predict(model, type = "response")
classi= predictedy > .5
table(train$highgrade, classi)


fit = rpart(train$highgrade ~ train$annual_inc + train$home_ownership + train$loan_amnt, method = "class")
aa = predict(fit, type="class")
table(train$highgrade, aa)
plot(fit)
text(fit)

highgrade = test$grade == "A" | test$grade == "B"
logisticmodel = glm(highgrade ~ test$annual_inc + test$home_ownership + test$loan_amnt, family = binomial)
treemodel = rpart(highgrade ~ test$annual_inc + test$home_ownership + test$loan_amnt, method = "class")
treemodel
lmodel_predicted = predict(logisticmodel, type = "response")
lmodel_classified = lmodel_predicted>.5
treemodel_classified = predict(treemodel, type="class")
table(highgrade, lmodel_classified)
table(highgrade, treemodel_classified)

