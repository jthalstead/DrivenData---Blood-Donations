### DrivenData
### Warm Up: Predicting Blood Donations
### Best Score - 0.4259
### Current Rank - 31

### init
rm(list = ls())
library(caret)

### obj
logloss = function(act, pred){
  ll = -1/length(act) * (sum((act * log(pred) + (1 - act) * log(1 - pred))))
  return(ll)
}

### read
all = read.csv('transfusion.data')
test = read.csv('blood_test.csv')

### munge
colnames(all) = c('msld', 'nd', 'vd', 'msfd', 'y')
all$y = as.factor(all$y)
all$vd = NULL
test.id = test$X; test$X = NULL;
colnames(test) = c('msld', 'nd', 'vd', 'msfd')

### feat
all$r = sqrt(all$msfd - all$msld)
test$r = sqrt(test$msfd - test$msld)

### split
target = all$y; all$y = NULL;
index = sample(1:nrow(all), nrow(all)*0.6)
train = all[index, ]
train.y = target[index]
valid = all[-index, ]
valid.y = target[-index]

### fit
fit.glm = glm(train.y ~ ., data = train, family = binomial(link = 'logit'))

### valid
valid.pred = predict(fit.glm, valid, type = 'response')
confusionMatrix(round(valid.pred, 0), valid.y)
logloss((as.numeric(valid.y)-1), valid.pred)

### submit
test.pred = predict(fit.glm, test, type = 'response')
submit = data.frame(test.id, test.pred)
colnames(submit) = c('', 'Made Donation in March 2007')
write.csv(submit, 'blood_submit.csv', row.names = F)