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

# 데이터 및 개수 확인
ucla_train
nrow(ucla_train)

ucla_test
nrow(ucla_test)

# 3. 학습데이터로 모델을 만드는 과정(모델링)

# 3-1. 결정트리 모델
library(rpart)
library(caret)
r = rpart(admit~., data = ucla_train)

# 3-2. 랜덤포레스트(트리 개수 50개) 모델
library(randomForest)
small_forest = randomForest(admit~., data = ucla_train, ntree = 50)

# 3-3. 랜덤포레스트(트리 개수 1000개) 모델
large_forest = randomForest(admit~., data = ucla_train, ntree = 1000)

# 3-4. K-NN 모델
library(class)
k = knn(ucla_train, ucla_test, ucla_train$admit, k = 5)

# 3-5. SVM(radial basis) 모델
library(e1071)
s = svm(admit~., data = ucla_train)

# 3-6. SVM(polynomial) 모델
sp = svm(admit~., data = ucla_train, kernel = 'polynomial')

# 4. 테스트데이터로 예측하고, 예측결과를 혼동행렬로 출력하는 과정

# 4-1. 결정트리 테스트 데이터 예측
newd = data.frame(gre=ucla_test$gre, gpa=ucla_test$gpa, rank=ucla_test$rank)
predict(r, newdata = newd)
r_pred = predict(r, newd, type = 'class')
table(r_pred, ucla_test$admit)

# 4-2. 랜덤포레스트(트리 개수 50개) 테스트 데이터 예측
p = predict(small_forest, newdata=ucla_test)
table(p, ucla_test$admit)

# 4-3. 랜덤포레스트(트리 개수 1000개) 테스트 데이터 예측
p = predict(large_forest, newdata=ucla_test)
table(p, ucla_test$admit)

# 4-4. K-NN 테스트 데이터 예측
k = knn(ucla_train, ucla_test, ucla_train$admit, k = 5)
table(k, ucla_test$admit)

# 4-5. SVM(radial basis) 테스트 데이터 예측
p = predict(s, newdata = ucla_test)
table(p, ucla_test$admit)

# 4-6. SVM(polynomial) 테스트 데이터 예측
p = predict(sp, newdata =  ucla_test)
table(p, ucla_test$admit)
