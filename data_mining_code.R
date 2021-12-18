#데이터마이닝 기말과제 5번 코드

# 1. 데이터를 획득하고 모델을 적용하기 위한 준비과정
ucla = read.csv('https://stats.idre.ucla.edu/stat/data/binary.csv')
ucla
str(ucla)
levels(factor(ucla$rank))
head(ucla)
