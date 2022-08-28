### final_project ###
### 휴먼지능정보공학전공 안재우(201915036), 황정원(202010833)
## 전처리된 데이터로 가설검정, 데이터 확인, 이상치 제거, 정규분포화하는 코드입니다.
df <- read.csv("df_R_.csv")
head(df)

## 가설 1.
# 나이가 많을수록 카드 한도가 높을지 확인
# -> 노동 인구 구간이 확실히 카드 한도가 높다.
table(df$BTH_YR) 
year <- c(1935:2009) # 태어난 년도 벡터 생성
year_l <- c(1:75) # 태어난 년도 별로 카드 한도를 저장할 벡터 생성

for (i in 1:length(year)){ 
  c <- 0 # 태어난 년도 별 카드 한도의 평균을 구하기 위해 누적된 카드 한도의 수를 저장할 빈 벡터 생성
  year_l[i] <- 0 # 앞서 생성한 카드 한도를 저장할 벡터 초기화
  for (j in 1:length(df$BTH_YR)){
    if (year[i] == df$BTH_YR[j]){ # 태어난 년도와 카드 한도 정보가 담겨있는 데이터프레임의 년도가 같다면
      year_l[i] <- year_l[i] + df$CD_USG_LMT[j] # 한도를 저장할 벡터에 카드 한도 데이터를 저장
      c <- c+1 # 한도를 저장했다면 c를 하나 증가
    }
  }
  year_l[i] <- year_l[i] / c # 같은 년도의 한도를 모두 저장한 뒤 평균을 구하기 위해 누적된 카드 한도를 c로 나눠준다.
  c <- 0 # 평균을 구해 year_l 벡터에 저장하였으므로 c벡터를 초기화
}

# year_l
barplot(year_l, year, # year_l(같은 년도의 카드 한도 평균), year(태어난 년도) 를 활용하여 barplot을 생성
        names = year,
        col = 'skyblue',
        main = '년도 별 카드 한도 평균',
        xlab = '출생 년도',
        ylab = '카드 한도의 평균')


## 가설 2.
# 카드의 개수가 많을 수록 카드 사용금액이 높은지 확인
table(df$Card_CNT) # 데이터 프레임 안에 카드 개수의 분포를 확인한다.
card_cnt <- c(1:165, 167:171, 174, 175, 177, 181, 183, 185, 186, 189, 193, 195, 197, 200, 202, 228) # 구한 카드 개수를 빈 벡터에 저장한다.
tail(card_cnt) # 카드 개수가 올바른지 확인
length(card_cnt)

card_l <- c(1:184) # 카드 사용 금액을 저장할 빈 벡터를 생성한다.

for (i in 1:length(card_cnt)){
  c <- 0 # 카드 개수 별 카드 한도의 평균을 구하기 위해 카드 사용 금액을 저장할 빈 벡터를 생성한다.
  card_l[i] <- 0 # 카드 사용 금액을 저장할 벡터를 초기화 한다.
  for (j in 1:length(df$Card_CNT)){
    if (card_cnt[i] == df$Card_CNT[j]){ # 만약 카드의 개수와 데이터프레임 안의 카드 개수가 같다면
      card_l[i] <- card_l[i] + df$CD_USG_AMT[j] # 카드 사용 금액을 저장한다.
      c <- c+1 # 카드 사용 금액을 저장했다면 c를 하나 증가
    }
  }
  card_l[i] <- card_l[i] / c # 같은 카드 개수의 사용 금액을 모두 저장한 뒤 평균을 구하기 위해 누적된 카드 사용금액을 c로 나눠준다.
  c <- 0 # 다음 카드의 개수를 위해 c를 초기화 한다.
}

card_l
barplot(card_l, card_cnt,
        names = card_cnt,
        col = 'pink',
        main = '카드 개수 별 카드 사용금액의 평균',
        xlab = '카드 개수',
        ylab = '카드 사용금액의 평균')

for(i in 1:length(df$CD_USG_AMT)) {
  if(mean(card_l) == df$CD_USG_AMT[i]) {
    print(i)
  }
}
  

library(ggplot2) # 정규분포 그래프를 그리기 위해 ggplot2 라이브러리를 불러온다.
library(extrafont) # ggplot2에 한글제목을 달기 위해 extrafont 라이브러리를 불러온다.
font_import()

mean_list <- c() # 이상치 제거 후 평균을 저장하기 위해 빈 벡터를 생성한다.
max_list <- c() # 이상치 제거 후 최대값을 저장하기 위해 빈 벡터를 생성한다.
min_list <- c() # 이상치 제거 후 최소값을 저장하기 위해 빈 벡터를 생성한다.

# 1. 연체 금액에 대한 데이터 확인
table(df$OverdueAMT) # 연체금액에 대한 누적 분포를 구하기 위해 table()함수를 사용한다.
df_overdue <- subset(df, df$OverdueAMT != 0) # 연체 금액이 0인 데이터는 의미가 없으므로 제거한다.

hist(df_overdue$OverdueAMT,
     main = '이상치 제거 전 연체 금액의 히스토그램',
     xlab = '연체 금액',
     ylab = '빈도 수',
     col = 'green') # 데이터를 확인하기 위해 히스토그램을 그려본다.
boxplot(df_overdue$OverdueAMT,
        main = '이상치 제거 전 연체 금액의 박스플롯',
        xlab = '연체 금액',
        ylab = '') # 이상치를 확인하기 위해 박스플롯을 그려본다.
ggplot(data = df_overdue, aes(OverdueAMT)) +
  ggtitle('이상치 제거 전 정규 분포') +
  xlab('연체 금액') +
  theme(text = element_text(size = 12, family = 'AppleGothic')) +
  stat_function(fun = dnorm, 
                args = list(mean = mean(df_overdue$OverdueAMT), 
                            sd = sd(df_overdue$OverdueAMT))) # 정규 분포 함수를 그려본다. 연체금액이 정규분포를 따르기는 힘드나 매우 왼쪽으로 치우친 그래프를 확인할 수 있다.


# 데이터 정규화하기 위해 이상치 제거
stat <- boxplot.stats(df_overdue$OverdueAMT) # 앞선 박스플롯의 데이터 stat에 저장
low <- min(stat$out) # 이상치 중 최소값을 low에 저장
low # 이상치 최솟값 확인

df_overdue_revised <- subset(df_overdue, df_overdue$OverdueAMT < 475000) # 이상치의 최솟값보다 작은 연체금액을 subset함수를 통해 df_overdue_revised에 저장
boxplot(df_overdue_revised$OverdueAMT,
        main = '이상치 제거 후 연체 금액의 박스플롯') # 다시 박스 플롯을 확인 
# 아직 이상치가 존재하나 이상치 제거 전 박스플롯보다 훨씬 정규 분포에 가까운 박스플롯이 생성됨을 알 수 있다.
hist(df_overdue_revised$OverdueAMT,
     main = '이상치 제거 후 연체 금액의 히스토그램',
     xlab = '연체 금액',
     ylab = '빈도 수',
     col = 'orange') # 이상치 제거 후 히스토그램을 통해 데이터의 분포를 확인
# 아까보다 훨씬 데이터가 고르게 분포되어있음을 확인할 수 있다.
ggplot(data = df_overdue_revised, aes(OverdueAMT)) +
  ggtitle('이상치 제거 후 정규 분포') +
  xlab('연체 금액') +
  theme(text = element_text(size = 12, family = 'AppleGothic')) +
  stat_function(fun = dnorm, 
                args = list(mean = mean(df_overdue_revised$OverdueAMT), 
                            sd = sd(df_overdue_revised$OverdueAMT))) # 이상치 제거 후 정규분포 그래프를 그려본다.
# 이상치 제거 전 정규분포 그래프보다 훨씬 합리적인 그래프가 그려짐을 알 수 있다.

# 등급화를 위해 최소, 최대, 평균 값을 아까 만든 빈 벡터에 저장
mean_list[1]<-mean(df_overdue_revised$OverdueAMT)
max_list[1] <- max(df_overdue_revised$OverdueAMT)
min_list[1] <- min(df_overdue_revised$OverdueAMT)


# 2. 연체 횟수에 대한 누적분포를 구한다.
table(df$OverdueCNT)
df_overduecnt <- subset(df, df$OverdueCNT != 0)
boxplot(df_overduecnt$OverdueCNT)
hist(df_overduecnt$OverdueCNT)
table(df_overduecnt$OverdueCNT)
ggplot(data = df_overduecnt) + geom_density(mapping = aes(OverdueCNT))


# 앞선 그래프를 출력한 뒤 이상치 제거후 정규분포화한 뒤 재출력
stat <- boxplot.stats(df_overduecnt$OverdueCNT)
low <- min(stat$out)
low

df_overduecnt_revised <- subset(df_overduecnt, 
                             df_overduecnt$OverdueCNT < 91)
boxplot(df_overduecnt_revised$OverdueCNT)
hist(df_overduecnt_revised$OverdueCNT, breaks = 10)
ggplot(data = df_overduecnt_revised, aes(OverdueCNT)) +
  stat_function(fun = dnorm, args = list(mean = mean(df_overduecnt_revised$OverdueCNT), sd = sd(df_overduecnt_revised$OverdueCNT)))
mean_list[2] <- mean(df_overduecnt_revised$OverdueCNT)
max_list[2] <- max(df_overduecnt_revised$OverdueCNT)
min_list[2] <- min(df_overduecnt_revised$OverdueCNT)

# 3. 신용카드 대출 금액에 대한 누적 분포를 구한다.
table(df$CreditLoan_AMT)
df_creditloan <- subset(df, df$CreditLoan_AMT != 0)
boxplot(df_creditloan$CreditLoan_AMT)
hist(df_creditloan$CreditLoan_AMT)
table(df_creditloan$CreditLoan_AMT)
ggplot(data = df_creditloan) + geom_density(mapping = aes(CreditLoan_AMT))

# 앞선 그래프를 출력한 뒤 이상치 제거후 정규분포화한 뒤 재출력
stat <- boxplot.stats(df_creditloan$CreditLoan_AMT)
low <- min(stat$out)
low

df_creditloan_revised <- subset(df_creditloan, 
                                df_creditloan$CreditLoan_AMT < 3517500)
boxplot(df_creditloan_revised$CreditLoan_AMT)
hist(df_creditloan_revised$CreditLoan_AMT, breaks = 10)
ggplot(data = df_creditloan_revised, aes(CreditLoan_AMT)) +
  stat_function(fun = dnorm, args = list(mean = mean(df_creditloan_revised$CreditLoan_AMT), sd = sd(df_creditloan_revised$CreditLoan_AMT)))
mean_list[3] <- mean(df_creditloan_revised$CreditLoan_AMT)
max_list[3] <- max(df_creditloan_revised$CreditLoan_AMT)
min_list[3] <- min(df_creditloan_revised$CreditLoan_AMT)

# 4. 신용카드 대출 횟수에 대한 누적 분포를 구한다.
table(df$CreditLoan_CNT)
df_creditloancnt <- subset(df, df$CreditLoan_CNT != 0)
boxplot(df_creditloancnt$CreditLoan_CNT)
hist(df_creditloancnt$CreditLoan_CNT)
table(df_creditloancnt$CreditLoan_CNT)
ggplot(data = df_creditloancnt) + geom_density(mapping = aes(CreditLoan_CNT))

# 앞선 그래프를 출력한 뒤 이상치 제거후 정규분포화한 뒤 재출력
stat <- boxplot.stats(df_creditloancnt$CreditLoan_CNT)
low <- min(stat$out)
low

df_creditloancnt_revised <- subset(df_creditloancnt, 
                                df_creditloancnt$CreditLoan_CNT < 104)
boxplot(df_creditloancnt_revised$CreditLoan_CNT)
hist(df_creditloancnt_revised$CreditLoan_CNT, breaks = 10)
ggplot(data = df_creditloancnt_revised, aes(CreditLoan_CNT)) +
  stat_function(fun = dnorm, args = list(mean = mean(df_creditloancnt_revised$CreditLoan_CNT), sd = sd(df_creditloancnt_revised$CreditLoan_CNT)))
mean_list[4] <- mean(df_creditloancnt_revised$CreditLoan_CNT)
max_list[4] <- max(df_creditloancnt_revised$CreditLoan_CNT)
min_list[4] <- min(df_creditloancnt_revised$CreditLoan_CNT)

# 5. 대부 대출 금액에 대한 누적 분포를 구한다.
table(df$Loan_AMT)
df_loan <- subset(df, df$Loan_AMT != 0)
boxplot(df_loan$Loan_AMT)
hist(df_loan$Loan_AMT)
table(df_loan$Loan_AMT)
ggplot(data = df_loan) + geom_density(mapping = aes(Loan_AMT))

# 앞선 그래프를 출력한 뒤 이상치 제거후 정규분포화한 뒤 재출력
stat <- boxplot.stats(df_loan$Loan_AMT)
low <- min(stat$out)
low

df_loan_revised <- subset(df_loan, 
                                   df_loan$Loan_AMT < 325000) 
boxplot(df_loan_revised$Loan_AMT)
hist(df_loan_revised$Loan_AMT, breaks = 10)
ggplot(data = df_loan_revised, aes(Loan_AMT)) +
  stat_function(fun = dnorm, args = list(mean = mean(df_loan_revised$Loan_AMT), sd = sd(df_loan_revised$Loan_AMT)))
mean_list[5] <- mean(df_loan_revised$Loan_AMT)
max_list[5] <- max(df_loan_revised$Loan_AMT)
min_list[5] <- min(df_loan_revised$Loan_AMT)

# 6. 대부 대출 횟수에 대한 누적 분포를 구한다.
table(df$Loan_CNT)
df_loancnt <- subset(df, df$Loan_CNT != 0)
boxplot(df_loancnt$Loan_CNT)
hist(df_loancnt$Loan_CNT)
table(df_loancnt$Loan_CNT)
ggplot(data = df_loancnt) + geom_density(mapping = aes(Loan_CNT))

stat <- boxplot.stats(df_loancnt$Loan_CNT)
low <- min(stat$out)
low

# 앞선 그래프를 출력한 뒤 이상치 제거후 정규분포화한 뒤 재출력
stat <- boxplot.stats(df_loan$Loan_CNT)
low <- min(stat$out)
low

df_loancnt_revised <- subset(df_loan, 
                          df_loan$Loan_CNT < 48)
boxplot(df_loancnt_revised$Loan_CNT)
hist(df_loancnt_revised$Loan_CNT, breaks = 10)
ggplot(data = df_loancnt_revised, aes(Loan_CNT)) +
  stat_function(fun = dnorm, args = list(mean = mean(df_loancnt_revised$Loan_CNT), sd = sd(df_loancnt_revised$Loan_CNT)))
mean_list[6] <- mean(df_loancnt_revised$Loan_CNT)
max_list[6] <- max(df_loancnt_revised$Loan_CNT)
min_list[6] <- min(df_loancnt_revised$Loan_CNT)

# 7. 신용카드 개수에 대한 누적 분포를 구한다.
table(df$Card_CNT)
df_cardcnt <- subset(df, df$Card_CNT != 0)
boxplot(df_cardcnt$Card_CNT)
hist(df_cardcnt$Card_CNT)
table(df_cardcnt$Card_CNT)
ggplot(data = df_cardcnt) + geom_density(mapping = aes(Card_CNT))

# 앞선 그래프를 출력한 뒤 이상치 제거후 정규분포화한 뒤 재출력
stat <- boxplot.stats(df_cardcnt$Card_CNT)
low <- min(stat$out)
low

df_loancnt_revised <- subset(df_cardcnt, 
                             df_cardcnt$Card_CNT < 148) 
boxplot(df_loancnt_revised$Card_CNT)
hist(df_loancnt_revised$Card_CNT, breaks = 10)
ggplot(data = df_loancnt_revised, aes(Card_CNT)) +
  stat_function(fun = dnorm, args = list(mean = mean(df_loancnt_revised$Card_CNT), sd = sd(df_loancnt_revised$Card_CNT)))
mean_list[7] <- mean(df_loancnt_revised$Card_CNT)
max_list[7] <- max(df_loancnt_revised$Card_CNT)
min_list[7] <- min(df_loancnt_revised$Card_CNT)


# 8. 신용카드 한도에 대한 누적 분포를 구한다.
table(df$CD_USG_LMT)
df_usglmt <- subset(df, df$CD_USG_LMT != 0)
boxplot(df_usglmt$CD_USG_LMT)
hist(df_usglmt$CD_USG_LMT)
table(df_usglmt$CD_USG_LMT)
ggplot(data = df_usglmt) + geom_density(mapping = aes(CD_USG_LMT))

# 앞선 그래프를 출력한 뒤 이상치 제거후 정규분포화한 뒤 재출력
stat <- boxplot.stats(df_usglmt$CD_USG_LMT)
low <- min(stat$out)
low

df_usglmt_revised <- subset(df_usglmt, 
                             df_usglmt$CD_USG_LMT < 3323000)
boxplot(df_usglmt_revised$CD_USG_LMT)
hist(df_usglmt_revised$CD_USG_LMT, breaks = 10)
ggplot(data = df_usglmt_revised, aes(CD_USG_LMT)) +
  stat_function(fun = dnorm, args = list(mean = mean(df_usglmt_revised$CD_USG_LMT), sd = sd(df_usglmt_revised$CD_USG_LMT)))
mean_list[8] <- mean(df_usglmt_revised$CD_USG_LMT)
max_list[8] <- max(df_usglmt_revised$CD_USG_LMT)
min_list[8] <- min(df_usglmt_revised$CD_USG_LMT)

# 9. 현금서비스 한도에 대한 누적 분포를 구한다.
table(df$CD_CA_LMT)
df_calmt <- subset(df, df$CD_CA_LMT != 0)
boxplot(df_calmt$CD_CA_LMT)
hist(df_calmt$CD_CA_LMT)
table(df_calmt$CD_CA_LMT)
ggplot(data = df_calmt) + geom_density(mapping = aes(CD_CA_LMT))

# 앞선 그래프를 출력한 뒤 이상치 제거후 정규분포화한 뒤 재출력
stat <- boxplot.stats(df_calmt$CD_CA_LMT)
low <- min(stat$out)
low

df_calmt_revised <- subset(df_calmt, 
                            df_calmt$CD_CA_LMT < 1069000)
boxplot(df_calmt_revised$CD_CA_LMT)
hist(df_calmt_revised$CD_CA_LMT, breaks = 10)
ggplot(data = df_calmt_revised, aes(CD_CA_LMT)) +
  stat_function(fun = dnorm, args = list(mean = mean(df_calmt_revised$CD_CA_LMT), sd = sd(df_calmt_revised$CD_CA_LMT)))
mean_list[9] <- mean(df_calmt_revised$CD_CA_LMT)
max_list[9] <- max(df_calmt_revised$CD_CA_LMT)
min_list[9] <- min(df_calmt_revised$CD_CA_LMT)

# 10. 신용카드 이용금액 대한 누적 분포를 구한다.
table(df$CD_USG_AMT)
df_usgamt <- subset(df, df$CD_USG_AMT != 0)
boxplot(df_usgamt$CD_USG_AMT)
hist(df_usgamt$CD_USG_AMT)
table(df_usgamt$CD_USG_AMT)
ggplot(data = df_usgamt) + geom_density(mapping = aes(CD_USG_AMT))

# 앞선 그래프를 출력한 뒤 이상치 제거후 정규분포화한 뒤 재출력
stat <- boxplot.stats(df_usgamt$CD_USG_AMT)
low <- min(stat$out)
low

df_usgamt_revised <- subset(df_usgamt, 
                           df_usgamt$CD_USG_AMT < 93200)
boxplot(df_usgamt_revised$CD_USG_AMT)
hist(df_usgamt_revised$CD_USG_AMT, breaks = 10)
ggplot(data = df_usgamt_revised, aes(CD_USG_AMT)) +
  stat_function(fun = dnorm, args = list(mean = mean(df_usgamt_revised$CD_USG_AMT), sd = sd(df_usgamt_revised$CD_USG_AMT)))
mean_list[10] <- mean(df_usgamt_revised$CD_USG_AMT)
max_list[10] <- max(df_usgamt_revised$CD_USG_AMT)
min_list[10] <- min(df_usgamt_revised$CD_USG_AMT)

# 11. 현금서비스 이용금액에 대한 누적 분포를 구한다.
table(df$CD_CA_AMT)
df_caamt <- subset(df, df$CD_CA_AMT != 0)
boxplot(df_caamt$CD_CA_AMT)
hist(df_caamt$CD_CA_AMT)
table(df_caamt$CD_CA_AMT)
ggplot(data = df_caamt) + geom_density(mapping = aes(CD_CA_AMT))


# 앞선 그래프를 출력한 뒤 이상치 제거후 정규분포화한 뒤 재출력
stat <- boxplot.stats(df_caamt$CD_CA_AMT)
low <- min(stat$out)
low
df_caamt_revised <- subset(df_caamt, 
                            df_caamt$CD_CA_AMT < 24110)
boxplot(df_caamt_revised$CD_CA_AMT)
hist(df_caamt_revised$CD_CA_AMT, breaks = 10)
ggplot(data = df_caamt_revised, aes(CD_CA_AMT)) +
  stat_function(fun = dnorm, args = list(mean = mean(df_caamt_revised$CD_CA_AMT), sd = sd(df_caamt_revised$CD_CA_AMT)))
mean_list[11] <- mean(df_caamt_revised$CD_CA_AMT)
max_list[11] <- max(df_caamt_revised$CD_CA_AMT)
min_list[11] <- min(df_caamt_revised$CD_CA_AMT)

mean_list
max_list
min_list









