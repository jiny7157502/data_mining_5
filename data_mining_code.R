#데이터마이닝 기말과제 5번 코드

# 1. 데이터를 획득하고 모델을 적용하기 위한 준비과정
ucla = read.csv('https://stats.idre.ucla.edu/stat/data/binary.csv')
ucla$rank = factor(ucla$rank)
str(ucla)

# 2. 학습데이터와 테스트데이터 분리과정
# ucla의 학습데이터를 전체 데이터 60%를 뽑고 테스트데이터는 먼저 뽑은 60% 학습데이터의 나머지 값으로 저장하여 분리
library(dplyr)

# UCLA 학습데이터 분리
ucla_trainData = ucla[sample(NROW(ucla),NROW(ucla)*0.6),]
ucla_trainData = arrange(ucla_trainData)
ucla_trainData

# UCLA 테스트데이터 분리
ucla_testData = ucla[-sample(NROW(ucla),NROW(ucla)*0.6),]
ucla_testData = arrange(ucla_testData)
ucla_testData

# 3. 학습데이터로 모델을 만드는 과정(모델링)
m = glm(admit ~ gre + gpa + rank, data = ucla_trainData, family = binomial)
coef(m)

# 4. 테스트데이터로 예측하고, 예측결과를 혼동행렬로 출력하는 과정

# 4-1. 결정트리