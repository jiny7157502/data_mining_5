#데이터마이닝 기말과제 5번 코드

# 1. 데이터를 획득하고 모델을 적용하기 위한 준비과정
ucla = read.csv('https://stats.idre.ucla.edu/stat/data/binary.csv')

ucla$admit = factor(ucla$admit)
str(ucla)

# 2. 학습데이터와 테스트데이터 분리과정
# ucla의 학습데이터를 전체 데이터 60%를 랜덤하게 샘플리하고 테스트데이터는 먼저 뽑은 60% 학습데이터의 나머지 값으로 저장하여 분리
n = nrow(ucla)
i = 1:n
train_list = sample(i, n*0.6)
test_list = setdiff(i, train_list)
ucla_train = ucla[train_list, ]
ucla_test = ucla[test_list, ]

# 3. 학습데이터로 모델을 만드는 과정(모델링)
m = glm(admit ~ gre + gpa + rank, data = ucla_train, family = binomial)
coef(m)

# 4. 테스트데이터로 예측하고, 예측결과를 혼동행렬로 출력하는 과정

# 4-1. 결정트리
library(rpart)
library(caret)

r = rpart(admit~., data = ucla_train)
newd = data.frame(gre=ucla_test$gre, gpa=ucla_test$gpa, rank=ucla_test$rank)
predict(r, newdata = newd)
r_pred = predict(r, newd, type='class')
confusionMatrix(r_pred, ucla_train$admit)

table(r_pred, ucla_train$admit)

# 4-2. 랜덤포레스트(트리 개수 50개)
library(randomForest)
small_forest = randomForest(admit~., data = ucla_train, ntree = 50)
p = predict(small_forest, newdata=ucla_test)
table(p, ucla_train$admit)

# 4-3. 랜덤포레스트(트리 개수 1000개)
large_forest = randomForest(admit~., data = ucla_train, ntree = 1000)
p = predict(large_forest, newdata=ucla_test)
table(p, ucla_train$admit)

# 4-4. K-NN
library(class)
k = knn(ucla_train, ucla_test, ucla_train$admit, k = 5)
str(k)
str(ucla_train)
p = predict(k, ucla_train$admit)
table(k, ucla_train$admit)

# 4-5. SVM(radial basis)
library(e1071)
s = svm(admit~., data = ucla_train)
p = predict(s, ucla_train)
table(p, ucla_train$admit)

# 4-6. SVM(polynomial)
s = svm(admit~., data = ucla_train, kernel = 'polynomial')
p = predict(s, ucla_train)
table(p, ucla_train$admit)
