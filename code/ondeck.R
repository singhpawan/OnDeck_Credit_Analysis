library(dplyr)
library(markovchain)
library(gridExtra)
library(questionr)
library(ggplot2)
library(caret)
library(forecast)
library(knitr)
library(memisc)



data <- read.csv('/home/pawan/Downloads/OnDeck Analytics Asssignment.csv')
df <- data.frame(data)
head(df)
summary(df)

df$days_delinquent_old_factor <- cut(df$days_delinquent_old, c(0,1,5,10,30,60,180), labels = c("0","1-5","5-10","10-30","30-60","60+"))
df$days_delinquent_new_factor <- cut(df$days_delinquent_new, c(0,1,5,10,30,60,180), labels = c("0","1-5","5-10","10-30","30-60","60+"))

df$ratio <- df$new_outstanding_principal_balance / df$average_bank_balance__c 
df$percent_loan_paid <- ((df$initial_loan_amount - df$new_outstanding_principal_balance) / df$initial_loan_amount) * 100
df$delinquency_rate <- df$days_delinquent_new / df$days_delinquent_old
df$movement <- ifelse(df$days_delinquent_new > df$days_delinquent_old, "increase", "same/decreased")
df$as_of_date <- as.Date(df$as_of_date)
df$last_cleared_payment_date <- as.Date(df$last_cleared_payment_date)
df$days_since_last_payment <- as.Date("2012-12-1") - df$last_cleared_payment_date
df$target <- ifelse(df$movement == "increase", 1, 0)


p1 <- ggplot(data = df, aes(x = days_delinquent_new, y = fico)) + geom_point(aes(color=df$movement)) + scale_x_continuous(limits=c(0,180), breaks=c(1,5,10,30,60,180))
p2 <- ggplot(data = df, aes(x = days_delinquent_old, y = fico)) + geom_point(aes(color=df$movement)) + scale_x_continuous(limits=c(0,180), breaks=c(1,5,10,30,60,180))
grid.arrange(p1, p2, ncol = 1)
p3 <- ggplot(data = df, aes(x =percent_loan_paid, y = delinquency_rate )) + geom_point(aes(color=df$sales_channel__c))
p3
p4 <- ggplot(data = df, aes(x =percent_loan_paid, y = delinquency_rate )) + geom_point(aes(color=df$current_collection_method))
p4
p5 <- ggplot(data = df, aes(x = days_delinquent_new, y = fico)) + geom_point(aes(color=df$days_delinquent_old_factor)) + scale_x_continuous(limits=c(0,180), breaks=c(1,5,10,30,60,180))
p6 <- ggplot(data = df, aes(x = days_delinquent_old, y = fico)) + geom_point(aes(color=df$days_delinquent_old_factor)) + scale_x_continuous(limits=c(0,180), breaks=c(1,5,10,30,60,180))
grid.arrange(p5, p6, ncol = 1)
p7 <- ggplot(data = df, aes(x =percent_loan_paid, y = delinquency_rate )) + geom_point(aes(color=df$days_delinquent_new_factor))
p8 <- ggplot(data = df, aes(x =percent_loan_paid, y = delinquency_rate )) + geom_point(aes(color=df$days_delinquent_old_factor))
grid.arrange(p7, p8, ncol=1)


by(df$new_outstanding_principal_balance, df$sales_channel__c, mean)
by(df$new_outstanding_principal_balance,df$sales_channel__c, median)

by(df$days_delinquent_old,df$sales_channel__c,mean)
by(df$days_delinquent_old,df$sales_channel__c,median)

by(df$days_delinquent_new,df$sales_channel__c,mean)
by(df$days_delinquent_new,df$sales_channel__c,median)

by(df$ratio,df$sales_channel__c,mean)
by(df$ratio,df$sales_channel__c,median)

by(df$ratio, df$current_collection_method, mean)
by(df$ratio, df$current_collection_method, median)

by(df$new_outstanding_principal_balance, df$current_collection_method, mean)
by(df$new_outstanding_principal_balance, df$current_collection_method, median)

by(df$average_bank_balance__c, df$current_collection_method, mean)
by(df$average_bank_balance__c, df$current_collection_method, median)

by(df$percent_loan_paid, df$sales_channel__c, mean)
by(df$percent_loan_paid, df$sales_channel__c, median)

by(df$percent_loan_paid, df$current_collection_method, mean)
by(df$percent_loan_paid, df$current_collection_method, median)

by(df$delinquency_rate, df$sales_channel__c, mean)
by(df$delinquency_rate, df$sales_channel__c, median)

by(df$delinquency_rate, df$current_collection_method, mean)
by(df$delinquency_rate, df$current_collection_method, median)


transmat <- wtd.table(df$days_delinquent_old_factor, df$days_delinquent_new_factor,na.rm = TRUE)
transmat_prob <- transmat/rowSums(transmat)
transmat_prob
transmat_norm <- wtd.table(df$days_delinquent_old_factor, df$days_delinquent_new_factor, weights = df$new_outstanding_principal_balance - mean(df$new_outstanding_principal_balance)/sd(df$new_outstanding_principal_balance),na.rm = TRUE)
transmat_norm_prob <- transmat_norm/rowSums(transmat_norm)
transmat_norm_prob

###########################################################################################################################
#To implement a logistic regression model. This model is good for binary diagnosis.and is good at predicting the odds of 
# a loan changing its credit rating
m1 <- glm(target ~ ratio, data = df, family = "binomial")
m2 <- update(m1,  ~ . + percent_loan_paid)
m3 <- update(m2, ~ . + lender_payoff)
m4 <- update(m3, ~ . + fico)
m5 <- update(m4, ~ . + sales_channel__c)
m6 <- update(m5, ~ . + type)
m7 <- update(m6, ~ . + days_since_last_payment)
mtable(m1, m2, m3,m4,m5,m6,m7)

training <- df[1:420,]
testing <- df[421:477,]
head(training)
head(testing)
ma <- glm(target ~ ratio, data = training, family = "binomial")
mb <- update(ma,  ~ . + percent_loan_paid)
mc <- update(mb, ~ . + lender_payoff)
md <- update(mc, ~ . + fico)
me <- update(md, ~ . + sales_channel__c)
mf <- update(me, ~ . + type)
mg <- update(mf, ~ . + days_since_last_payment)
mtable(ma, mb, mc,md,me,mf,mg)

##################Testign part###################################################################################
testing$predict <- predict(mg, newdata=testing, type= "response")
head(testing)
testing[,c('target', 'predict')]

testing$predict <- ifelse(testing$predict > 0.33, 1, 0 )
testing[,c('target', 'predict')]

total = nrow(testing)
acc <- accuracy(testing$predict, testing$target)
acc
confusionmatrix <- table(testing$predict, testing$target)
confusionmatrix

