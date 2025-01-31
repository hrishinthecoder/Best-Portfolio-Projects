# logistic regression

# loading the dataset
data <- read.csv(file.choose(), header = T)
str(data)
data$admit <- as.factor(data$admit)
data$rank <- as.factor(data$rank)

# Two way table of factor variables
xtabs(~admit + rank, data = data)

# Partition the data - 80% test and 20% train
set.seed(1234)
ind <- sample(2, nrow(data), replace = T, prob = c(0.8,0.2))
train <- data[ind==1,]
test <- data[ind==2,]

# Logistic Regression Model
model <- glm(admit ~ gpa + rank, data = train, family = "binomial")
summary(model)

# Prediction
p1 <- predict(model, train, type = 'response')
head(p1) 

# Misclassification error - train data
pred1 <- ifelse(p1>0.5,1,0)
tab1 <- table(Predicted = pred1, Actual = train$admit)
tab1
1 - sum(diag(tab1))/sum(tab1)

# Misclassification error - test data
p2 <- predict(model, test, type = 'response')
pred2 <- ifelse(p2>0.5,1,0)
tab2 <- table(Predicted = pred2, Actual = test$admit)
tab2
1 - sum(diag(tab2))/sum(tab2)

# Goodness of Fit test
with(model, pchisq(null.deviance - deviance, df.null-df.residual, lower.tail = F))
