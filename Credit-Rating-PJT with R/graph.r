## 이상치를 제거한 후 등급이 정규분포에 따르는지 확인하는 코드입니다.
library(ggplot2)

df_<- read.csv("df_final.csv") # 이상치 제거 후
head(df_)

hist(df_$Label, breaks = 10,
     main = '이상치 제거 후 부여한 등급에 대한 히스토그램',
     xlab = '등급',
     
     ylab = '빈도 수') # 등급에 대한 데이터의 빈도를 알아보기 위해 히스토그램을 그려본다.
ggplot(data = df_, aes(Label)) +
  ggtitle('이상치 제거 후 정규 분포') +
  xlab('등급') +
  theme(text = element_text(size = 12, family = 'AppleGothic')) +
  stat_function(fun = dnorm, args = list(mean = mean(df_$Label), 
                                         sd = sd(df_$Label)))

df__ <- read.csv('df_final_.csv') # 나이스 올크레딧
head(df__)

hist(df__$NICE, breaks = 10,
     main = '나이스 신용평가사 기준으로 부여한 등급에 대한 히스토그램',
     xlab = '등급',
     ylab = '빈도 수')

ggplot(data = df__, aes(NICE)) +
  ggtitle('나이스 신용평가사 기준으로 부여한 등급에 대한 정규 분포') +
  xlab('등급') +
  theme(text = element_text(size = 12, family = 'AppleGothic')) +
  stat_function(fun = dnorm, args = list(mean = mean(df__$NICE), 
                                         sd = sd(df__$NICE)))
                                                                                 
hist(df__$KCB, breaks = 10,
     main = '올크레딧 신용평가사 기준으로 부여한 등급에 대한 히스토그램',
     xlab = '등급',
     ylab = '빈도 수')

ggplot(data = df__, aes(KCB)) +
  ggtitle('올크레딧 신용평가사 기준으로 부여한 등급에 대한 정규 분포') +
  xlab('등급') +
  theme(text = element_text(size = 12, family = 'AppleGothic')) +
  stat_function(fun = dnorm, args = list(mean = mean(df__$KCB), 
                                         sd = sd(df__$KCB)))
