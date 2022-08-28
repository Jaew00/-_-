library(readr)

SDB1_A_ID <- read_csv("~/Code/201915036_R/finance_prj/SDB1/SDB1_A_ID.csv")
head(SDB1_A_ID)

'SDB1_A_DLQ <- read_csv("~/Code/201915036_R/finance_prj/SDB1/SDB1_A_DLQ.csv")
head(SDB1_A_DLQ)'

SDB1_P_LN <- read_csv("~/Code/201915036_R/finance_prj/SDB1/SDB1_P_LN.csv")
head(SDB1_P_LN)

'SDB1_P_LN_SP <- read_csv("~/Code/201915036_R/finance_prj/SDB1/SDB1_P_LN_SP.csv")
head(SDB1_P_LN_SP)

SDB1_P_CD_OPN <- read_csv("~/Code/201915036_R/finance_prj/SDB1/SDB1_P_CD_OPN.csv")
head(SDB1_P_CD_OPN)

SDB1_P_CD_USG <- read_csv("~/Code/201915036_R/finance_prj/SDB1/SDB1_P_CD_USG.csv")
head(SDB1_P_CD_USG)'

df <- SDB1_A_ID[,c(1,3,4)]
head(df)

df['LN_AMT'] = 0
for (i in 1:nrow(SDB1_P_LN)) {
  for (j in 1:nrow(df)) {
    if(SDB1_P_LN$JOIN_SN[i] == df$JOIN_SN[j]) {
      df$LN_AMT[j] <- df$LN_AMT[j] + SDB1_P_LN$JOIN_SN[i]
    }
  }
}


library(dplyr)
write.csv(df, 'df.csv')
new_df <- SDB1_P_LN[,c(2,11)]
new_df
result <- group_by(new_df, sum(new_df$JOIN_SN))
result


purchase_merge_data_8_['평균기온'] = 0
purchase_merge_data_8_['평균상대습도'] = 0
purchase_merge_data_8_['일강수량'] = 0

for (i in 1:nrow(purchase_merge_data_8_)) {
  for (j in 1:nrow(temp_weaherDB)) {
    if((purchase_merge_data_8_$구매일자[i] == temp_weaherDB$구매일자[j]) && (purchase_merge_data_8_$점포위치코드[i] == temp_weaherDB$점포위치코드[j])) {
      purchase_merge_data_8_$평균기온[i] <- temp_weaherDB$`평균기온(°C)`[j]
      purchase_merge_data_8_$평균상대습도[i] <- temp_weaherDB$`평균 상대습도(%)`[j]
      purchase_merge_data_8_$일강수량[i] <- temp_weaherDB$`일강수량(mm)`[j]
    }
  }
}


write.csv(purchase_merge_data, 'purchase+weather1.csv')






