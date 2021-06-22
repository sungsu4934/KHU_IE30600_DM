##### 1.	데이터 불러오기



# 국민건강영양조사 데이터 불러오기
install.packages("sas7bdat") #SAS파일을 R로 불러올 “sas7bdat” 패키지를 설치
library(sas7bdat) #설치한 “sas7bdat”패키지를 프로젝트 내부로 불러오기
data <- read.sas7bdat(("hn18_all.sas7bdat")) #read.sas7bdat함수로 국민건강영양조사 data를 불러오기
data2018 <- data #국민건강영양조사 data를 data2018에 복사하기



#### 2.	데이터 전처리



# 독신여성 필터링
install.packages("dplyr") #데이터를 필터링 하기위해 “dplyr”패키지 설치
library(dplyr) #”dplyr”패키지를 프로젝트 내부로 불러오기
data2018 <- data2018 %>%  filter(cfam == 1 & sex == 2) #가구원수가 1명이고 여성인 사람을 filter함수로 필터링


# PHQ-9 10점 이상인 사람을 우울증으로 분류 0:우울증 없음 //  1:우울증 있음
data2018$mh_PHQ_S [data2018$mh_PHQ_S < 10] <- 0 #PHQ-9점수가 10점미만인 사람을 0으로 분류
data2018$mh_PHQ_S [data2018$mh_PHQ_S >= 10] <- 1 #PHQ-9점수가 10점이상인 사람을 0으로 분류
data2018$mh_PHQ_S <- factor(data2018$mh_PHQ_S, levels = c(0,1)) #분류한 Value의 타입을 Factor로 변환
table(data2018$mh_PHQ_S) #정상적으로 분류되었는지 table함수를 통해 확인


# 우울증 의사진단 여부 0: 없음 // 1. 있음
data2018$DF2_dg[data2018$DF2_dg == 9] <- NA #모름/무응답으로 응답한 사람을 NA로 처리
data2018$DF2_dg <- factor(data2018$DF2_dg, levels = c(0,1)) #분류한 Value의 타입을 Factor로 변환
table(data2018$DF2_dg) #정상적으로 분류되었는지 table함수를 통해 확인


# 우울증 현재 유병 여부 0:없음, 1:있음
data2018$DF2_pr[data2018$DF2_pr %in% c(8,9)] <- NA #비해당 및 모름/무응답으로 응답한 사람을 NA로 처리
data2018$DF2_pr <- factor(data2018$DF2_pr, levels = c(0,1)) #분류한 Value의 타입을 Factor로 변환
table(data2018$DF2_pr) #정상적으로 분류되었는지 table함수를 통해 확인


# 만성질환 의사진단 가족력 여부 0:아니오 // 1:예
data2018$HE_fh[data2018$HE_fh == 9] <- NA #모름으로 설문한 사람을 NA로 처리 
data2018$HE_fh <- factor(data2018$HE_fh, levels = c(0,1)) #분류한 Value의 타입을 Factor로 변환
table(data2018$HE_fh) #정상적으로 분류되었는지 table함수를 통해 확인


# 고혈압 의사진단 여부(부) 0:아니오 // 1:예
data2018$HE_HPfh1[data2018$HE_HPfh1 == 9] <- NA #모름/무응답으로 응답한 사람을 NA로 처리
data2018$HE_HPfh1 <- factor(data2018$HE_HPfh1, levels = c(0,1)) #분류한 Value의 타입을 Factor로 변환
table(data2018$HE_HPfh1) #정상적으로 분류되었는지 table함수를 통해 확인


# 고혈압 의사진단 여부(모) 0:아니오 // 1:예
data2018$HE_HPfh2[data2018$HE_HPfh2 == 9] <- NA #모름/무응답으로 응답한 사람을 NA로 처리
data2018$HE_HPfh2 <- factor(data2018$HE_HPfh2, levels = c(0,1)) #분류한 Value의 타입을 Factor로 변환
table(data2018$HE_HPfh2) #정상적으로 분류되었는지 table함수를 통해 확인


# 고혈압 의사진단 여부(형제자매) 0:아니오 // 1:예
data2018$HE_HPfh3[data2018$HE_HPfh3 %in% c(8,9)] <- NA #비해당 및 모름/무응답으로 응답한 사람을 NA로 처리
data2018$HE_HPfh3 <- factor(data2018$HE_HPfh3, levels = c(0,1)) #분류한 Value의 타입을 Factor로 변환
table(data2018$HE_HPfh3) #정상적으로 분류되었는지 table함수를 통해 확인


# 고지혈증 의사진단 여부(부) 0:아니오 // 1:예
data2018$HE_HLfh1[data2018$HE_HLfh1 == 9] <- NA #모름/무응답으로 응답한 사람을 NA로 처리
data2018$HE_HLfh1 <- factor(data2018$HE_HLfh1, levels = c(0,1)) #분류한 Value의 타입을 Factor로 변환
table(data2018$HE_HLfh1) #정상적으로 분류되었는지 table함수를 통해 확인


# 고지혈증 의사진단 여부(모) 0:아니오 // 1:예
data2018$HE_HLfh2[data2018$HE_HLfh2 == 9] <- NA #모름/무응답으로 응답한 사람을 NA로 처리
data2018$HE_HLfh2 <- factor(data2018$HE_HLfh2, levels = c(0,1)) #분류한 Value의 타입을 Factor로 변환
table(data2018$HE_HLfh2) #정상적으로 분류되었는지 table함수를 통해 확인


# 고지혈증 의사진단 여부(형제자매) 0:아니오 // 1:예
data2018$HE_HLfh3[data2018$HE_HLfh3 %in% c(8,9)] <- NA #비해당 및 모름/무응답으로 응답한 사람을 NA로 처리
data2018$HE_HLfh3 <- factor(data2018$HE_HLfh3, levels = c(0,1)) #분류한 Value의 타입을 Factor로 변환
table(data2018$HE_HLfh3) #정상적으로 분류되었는지 table함수를 통해 확인


# 허혈성심장질환 의사진단 여부(부) 0:아니오 // 1:예
data2018$HE_IHDfh1[data2018$HE_IHDfh1 == 9] <- NA #모름/무응답으로 응답한 사람을 NA로 처리
data2018$HE_IHDfh1 <- factor(data2018$HE_IHDfh1, levels = c(0,1)) #분류한 Value의 타입을 Factor로 변환
table(data2018$HE_IHDfh1) #정상적으로 분류되었는지 table함수를 통해 확인


# 허혈성심장질환 의사진단 여부(모) 0:아니오 // 1:예
data2018$HE_IHDfh2[data2018$HE_IHDfh2 == 9] <- NA #모름/무응답으로 응답한 사람을 NA로 처리
data2018$HE_IHDfh2 <- factor(data2018$HE_IHDfh2, levels = c(0,1)) #분류한 Value의 타입을 Factor로 변환
table(data2018$HE_IHDfh2) #정상적으로 분류되었는지 table함수를 통해 확인


# 허혈성심장질환 의사진단 여부(형제자매) 0:아니오 // 1:예 
data2018$HE_IHDfh3[data2018$HE_IHDfh3 %in% c(8,9)] <- NA #비해당 및 모름/무응답으로 응답한 사람을 NA로 처리
data2018$HE_IHDfh3 <- factor(data2018$HE_IHDfh3, levels = c(0,1)) #분류한 Value의 타입을 Factor로 변환
table(data2018$HE_IHDfh3) #정상적으로 분류되었는지 table함수를 통해 확인


# 뇌졸중 의사진단 여부(부) 0:아니오 // 1:예
data2018$HE_STRfh1[data2018$HE_STRfh1 == 9] <- NA #모름/무응답으로 응답한 사람을 NA로 처리
data2018$HE_STRfh1 <- factor(data2018$HE_STRfh1, levels = c(0,1)) #분류한 Value의 타입을 Factor로 변환
table(data2018$HE_STRfh1) #정상적으로 분류되었는지 table함수를 통해 확인


# 뇌졸중 의사진단 여부(모) 0:아니오 // 1:예
data2018$HE_STRfh2[data2018$HE_STRfh2 == 9] <- NA #모름/무응답으로 응답한 사람을 NA로 처리
data2018$HE_STRfh2 <- factor(data2018$HE_STRfh2, levels = c(0,1)) #분류한 Value의 타입을 Factor로 변환
table(data2018$HE_STRfh2) #정상적으로 분류되었는지 table함수를 통해 확인


# 뇌졸중 의사진단 여부(형제자매) 0:아니오 // 1:예
data2018$HE_STRfh3[data2018$HE_STRfh3 %in% c(8,9)] <- NA #비해당 및 모름/무응답으로 응답한 사람을 NA로 처리
data2018$HE_STRfh3 <- factor(data2018$HE_STRfh3, levels = c(0,1)) #분류한 Value의 타입을 Factor로 변환
table(data2018$HE_STRfh3) #정상적으로 분류되었는지 table함수를 통해 확인


# 당뇨병 의사진단 여부(부) 0:아니오 // 1:예
data2018$HE_DMfh1[data2018$HE_DMfh1 == 9] <- NA #모름/무응답으로 응답한 사람을 NA로 처리
data2018$HE_DMfh1 <- factor(data2018$HE_DMfh1, levels = c(0,1)) #분류한 Value의 타입을 Factor로 변환
table(data2018$HE_DMfh1) #정상적으로 분류되었는지 table함수를 통해 확인


# 당뇨병 의사진단 여부(모) 0:아니오 // 1:예
data2018$HE_DMfh2[data2018$HE_DMfh2 == 9] <- NA #모름/무응답으로 응답한 사람을 NA로 처리
data2018$HE_DMfh2 <- factor(data2018$HE_DMfh2, levels = c(0,1)) #분류한 Value의 타입을 Factor로 변환
table(data2018$HE_DMfh2) #정상적으로 분류되었는지 table함수를 통해 확인


# 당뇨병 의사진단 여부(형제자매) 0:아니오 // 1:예
data2018$HE_DMfh3[data2018$HE_DMfh3 %in% c(8,9)] #비해당 및 모름/무응답으로 응답한 사람을 NA로 처리
data2018$HE_DMfh3 <- factor(data2018$HE_DMfh3, levels = c(0,1)) #분류한 Value의 타입을 Factor로 변환
table(data2018$HE_DMfh3) #정상적으로 분류되었는지 table함수를 통해 확인


# 갑상선질환 의사진단 여부(부) 0:아니오 // 1:예
data2018$HE_THfh1[data2018$HE_THfh1 == 9] <- NA #모름/무응답으로 응답한 사람을 NA로 처리
data2018$HE_THfh1 <- factor(data2018$HE_THfh1, levels = c(0,1)) #분류한 Value의 타입을 Factor로 변환
table(data2018$HE_THfh1) #정상적으로 분류되었는지 table함수를 통해 확인


# 갑상선질환 의사진단 여부(모) 0:아니오 // 1:예
data2018$HE_THfh2[data2018$HE_THfh2 == 9] <- NA #모름/무응답으로 응답한 사람을 NA로 처리
data2018$HE_THfh2 <- factor(data2018$HE_THfh2, levels = c(0,1)) #분류한 Value의 타입을 Factor로 변환
table(data2018$HE_THfh2) #정상적으로 분류되었는지 table함수를 통해 확인


# 갑상선질환 의사진단 여부(형제자매) 0:아니오 // 1:예
data2018$HE_THfh3[data2018$HE_THfh3 %in% c(8,9)] <- NA #비해당 및 모름/무응답으로 응답한 사람을 NA로 처리
data2018$HE_THfh3 <- factor(data2018$HE_THfh3, levels = c(0,1)) #분류한 Value의 타입을 Factor로 변환
table(data2018$HE_THfh3) #정상적으로 분류되었는지 table함수를 통해 확인


# B형간염 의사진단 여부(부) 0:아니오 // 1:예
data2018$HE_HBfh1[data2018$HE_HBfh1 == 9] <- NA #모름/무응답으로 응답한 사람을 NA로 처리
data2018$HE_HBfh1 <- factor(data2018$HE_HBfh1, levels = c(0,1)) #분류한 Value의 타입을 Factor로 변환
table(data2018$HE_HBfh1) #정상적으로 분류되었는지 table함수를 통해 확인


# B형간염 의사진단 여부(모) 0:아니오 // 1:예
data2018$HE_HBfh2[data2018$HE_HBfh2 == 9] <- NA #모름/무응답으로 응답한 사람을 NA로 처리
data2018$HE_HBfh2 <- factor(data2018$HE_HBfh2, levels = c(0,1)) #분류한 Value의 타입을 Factor로 변환
table(data2018$HE_HBfh2) #정상적으로 분류되었는지 table함수를 통해 확인


# B형간염 의사진단 여부(형제자매) 0:아니오 // 1:예
data2018$HE_HBfh3[data2018$HE_HBfh3 %in% c(8,9)] <- NA #비해당 및 모름/무응답으로 응답한 사람을 NA로 처리
data2018$HE_HBfh3 <- factor(data2018$HE_HBfh3, levels = c(0,1)) #분류한 Value의 타입을 Factor로 변환
table(data2018$HE_HBfh3) #정상적으로 분류되었는지 table함수를 통해 확인


# 기초 생활 수급 여부 0:미수급 // 1:수급
data2018$allownc[data2018$allownc == 99] <- NA #모름/무응답으로 설문한 사람들은 NA로 처리
data2018$allownc[data2018$allownc == 10] <- 1 #기초생활수급을 한 경험이 있는 사람은 1로 분류
data2018$allownc[data2018$allownc == 20] <- 0 #기초생활수급을 한 경험이 없는 사람은 0으로 분류
data2018$allownc <- factor(data2018$allownc, levels = c(0,1)) #분류한 Value의 타입을 Factor로 변환
table(data2018$allownc) #정상적으로 분류되었는지 table함수를 통해 확인


# 소득 0: 월 소득 300만원 이하 // 1: 월 소득 300만원 초과
data2018$ainc[data2018$ainc <= 300 | data2018$ainc == 17] <- 0 #월소득이 300만원 이하인 사람을 0으로 분류
data2018$ainc[data2018$ainc > 300] <- 1 #월소득이 300만원 초과인 사람을 1로 분류
data2018$ainc <- factor(data2018$ainc, levels = c(0,1)) #분류한 Value의 타입을 Factor로 변환
table(data2018$ainc) #정상적으로 분류되었는지 table함수를 통해 확인


# 식생활 형편이 좋은 사람:0 // 안 좋은 사람:1
data2018$LF_SAFE[data2018$LF_SAFE %in% c(1,2)] <- 0 #식생활형편이 좋은 사람을 0으로 분류
data2018$LF_SAFE[data2018$LF_SAFE %in% c(3,4)] <- 1 #식생활형편이 좋지 않은 사람을 0으로 분류
data2018$LF_SAFE[data2018$LF_SAFE == 9] <- NA #모름/무응답으로 응답한 사람을 NA로 처리
data2018$LF_SAFE <- factor(data2018$LF_SAFE, levels = c(0,1)) #분류한 Value의 타입을 Factor로 변환
table(data2018$LF_SAFE) #정상적으로 분류되었는지 table함수를 통해 확인


# 경제활동상태 여부 0: 아니오 // 1: 예 
data2018$EC1_1[data2018$EC1_1 %in% c(8,9)] <- NA #모름/무응답으로 응답한 사람을 NA로 처리
data2018$EC1_1[data2018$EC1_1 == 2] <- 0 #경제활동을 하지 않는 사람을 0으로 분류
data2018$EC1_1 <- factor(data2018$EC1_1, levels = c(0,1)) #분류한 Value의 타입을 Factor로 변환
table(data2018$EC1_1) #정상적으로 분류되었는지 table함수를 통해 확인


# 정상체중여부 0:정상 // 1:체중변화있다  
data2018$HE_obe[data2018$HE_obe %in% c(1,3,4,5,6)] <- 1 #저체중과 비만인 사람을 1로 분류
data2018$HE_obe[data2018$HE_obe == 2] <- 0 #정상체중인 사람은 0으로 분류
data2018$HE_obe <- factor(data2018$HE_obe, levels = c(0,1)) #분류한 Value의 타입을 Factor로 변환
table(data2018$HE_obe) #정상적으로 분류되었는지 table함수를 통해 확인


# BMI 지수 0:저체중 // 1: 정상 // 2: 비만
data2018$HE_BMI[data2018$HE_BMI < 18.5] <- 0 #BMI가 18.5미만으로 저체중인 사람을 0으로 분류
data2018$HE_BMI[data2018$HE_BMI >= 18.5 & data2018$HE_BMI <23] <- 1 #BMI가 18.5이상 23미만으로 정상체중인 사람을 1로 분류
data2018$HE_BMI[data2018$HE_BMI >= 23] <- 2 #BMI가 23이상으로 비만인사람을 2로 분류
data2018$HE_BMI <- factor(data2018$HE_BMI, levels = c(0,1,2), ordered = TRUE) #분류한 Value의 타입을 Factor로 변환
table(data2018$HE_BMI) #정상적으로 분류되었는지 table함수를 통해 확인


# 필요 의료서비스 미충족 여부 0:충족 // 1:미충족 
data2018$M_2_yr[data2018$M_2_yr %in% c(3,9)] <- NA #의료서비스가 필요 없었던 사람과 모름/무응답으로 설문한 사람을 NA로 처리
data2018$M_2_yr[data2018$M_2_yr == 2] <- 0 #의료서비스를 충족한 사람을 0으로 분류
data2018$M_2_yr <- factor(data2018$M_2_yr, levels = c(0,1)) #분류한 Value의 타입을 Factor로 변환
table(data2018$M_2_yr) #정상적으로 분류되었는지 table함수를 통해 확인


# 민간의료보험가입여부 0:아니오 // 1:예
data2018$npins[data2018$npins == 9] <- NA #모름/무응답으로 응답한 사람을 NA로 처리
data2018$npins[data2018$npins == 2] <- 0 #민간의료보험에 가입하지 않은 사람을 0으로 분류
data2018$npins <- factor(data2018$npins, levels = c(0,1)) #분류한 Value의 타입을 Factor로 변환
table(data2018$npins) #정상적으로 분류되었는지 table함수를 통해 확인


# 변형근로시간 1:주간근무 // 2:저녁근무 // 3:밤 근무 // 4:주야간 규칙적 교대근무 // 5:24시간 교대근무 // 6:분할근무 // 7:불규칙 교대근무 // 8:기타 
data2018$EC_wht_5[data2018$EC_wht_5 %in% c(88,99)] <- NA #비해당 및 모름/무응답으로 응답한 사람을 NA로 처리
data2018$EC_wht_5 <- factor(data2018$EC_wht_5, levels = c(1,2,3,4,5,6,7,8)) #분류한 Value의 타입을 Factor로 변환
table(data2018$EC_wht_5) #정상적으로 분류되었는지 table함수를 통해 확인


# 주당 평균 근로시간 0:52시간 이하 // 1:52시간 초과
data2018$EC_wht_23[data2018$EC_wht_23 %in% c(888,999)] <- NA #비해당 및 모름/무응답으로 응답한 사람을 NA로 처리
data2018$EC_wht_23[data2018$EC_wht_23 <= 52] <- 0 #주당 평균 근로시간이 52시간이하인 사람을 0으로 분류
data2018$EC_wht_23[data2018$EC_wht_23 > 52] <- 1 #주당 평균 근로시간이 52시간초과인 사람을 1로 분류
data2018$EC_wht_23 <- factor(data2018$EC_wht_23, levels = c(0,1)) #분류한 Value의 타입을 Factor로 변환
table(data2018$EC_wht_23) #정상적으로 분류되었는지 table함수를 통해 확인


# 장소이동 신체활동 여부 0:아니오 // 1:예
data2018$BE3_91[data2018$BE3_91 %in% c(8,9)] <- NA #비해당 및 모름/무응답으로 응답한 사람을 NA로 처리
data2018$BE3_91[data2018$BE3_91 == 2] <- 0 #장소이동 신체활동 여부가 없는 사람을 0으로 분류
data2018$BE3_91 <- factor(data2018$BE3_91, levels = c(0,1)) #분류한 Value의 타입을 Factor로 변환
table(data2018$BE3_91) #정상적으로 분류되었는지 table함수를 통해 확인


# 여가 고강도 신체활동 여부 0:아니오 // 1:예
data2018$BE3_75[data2018$BE3_75 %in% c(8,9)] <- NA #비해당 및 모름/무응답으로 응답한 사람을 NA로 처리
data2018$BE3_75[data2018$BE3_75 == 2] <- 0 #여가 고강도 신체활동 여부가 없는 사람을 0으로 분류
data2018$BE3_75 <- factor(data2018$BE3_75, levels = c(0,1)) #분류한 Value의 타입을 Factor로 변환
table(data2018$BE3_75) #정상적으로 분류되었는지 table함수를 통해 확인


# 여가 중강도 신체활동 여부 0:아니오 // 1:예
data2018$BE3_85[data2018$BE3_85 %in% c(8,9)] <- NA #비해당 및 모름/무응답으로 응답한 사람을 NA로 처리
data2018$BE3_85[data2018$BE3_85 == 2] <- 0 #여가 중강도 신체활동 여부가 없는 사람을 0으로 분류
data2018$BE3_85 <- factor(data2018$BE3_85, levels = c(0,1)) #분류한 Value의 타입을 Factor로 변환
table(data2018$BE3_85) #정상적으로 분류되었는지 table함수를 통해 확인


# 하루 60분 이상 신체활동 실천 일수 0:안함 // 1:1~2회 // 2:3~4회 // 3:5~6일 // 4:매일
data2018$BE9[data2018$BE9 %in% c(88,99)] <- NA #비해당 및 모름/무응답으로 응답한 사람을 NA로 처리
data2018$BE9[data2018$BE9 == 1] <- 0 #하루 60분이상 신체활동 실천일수가 0일인 사람을 0으로 분류
data2018$BE9[data2018$BE9 %in% c(2,3)] <- 1 #하루 60분이상 신체활동 실천일수가 1~2일인 사람을 1로 분류
data2018$BE9[data2018$BE9 %in% c(4,5)] <- 2 #하루 60분이상 신체활동 실천일수가 3~4일인 사람을 2로 분류
data2018$BE9[data2018$BE9 %in% c(6,7)] <- 3 #하루 60분이상 신체활동 실천일수가 5~6일인 사람을 3으로 분류
data2018$BE9[data2018$BE9 == 8] <- 4 #하루 60분이상 신체활동 실천일수가 매일인 사람을 4로 분류
data2018$BE9 <- factor(data2018$BE9, levels = c(0,1,2,3,4)) #분류한 Value의 타입을 Factor로 변환
table(data2018$BE9) #정상적으로 분류되었는지 table함수를 통해 확인


# 일 중강도 신체활동 여부 0: 아니오 // 1: 예 
data2018$BE3_81[data2018$BE3_81 %in% c(8,9)] <- NA #비해당 및 모름/무응답으로 응답한 사람을 NA로 처리
data2018$BE3_81[data2018$BE3_81 == 2] <- 0 #하루에 중강도의 신체활동을 안 하는 사람을 0으로 분류
data2018$BE3_81 <- factor(data2018$BE3_81, levels = c(0,1)) #분류한 Value의 타입을 Factor로 변환
table(data2018$BE3_81) #정상적으로 분류되었는지 table함수를 통해 확인


# 일 고강도 신체활동 여부 0: 아니오 // 1: 예 
data2018$BE3_71[data2018$BE3_71 %in% c(8,9)] <- NA #비해당 및 모름/무응답으로 응답한 사람을 NA로 처리
data2018$BE3_71[data2018$BE3_71 == 2] <- 0 #하루에 고강도의 신체활동을 안 하는 사람을 0으로 분류
data2018$BE3_71 <- factor(data2018$BE3_71, levels = c(0,1)) #분류한 Value의 타입을 Factor로 변환
table(data2018$BE3_71) #정상적으로 분류되었는지 table함수를 통해 확인


# 1년간 입원이용 여부 0:없음 // 1:있음
data2018$MH1_yr[data2018$MH1_yr == 9] <- NA #모름/무응답으로 응답한 사람을 NA로 처리
data2018$MH1_yr[data2018$MH1_yr == 2] <- 0 #1년간 입원을 하지 않은 사람을 0으로 분류
data2018$MH1_yr <- factor(data2018$MH1_yr, levels = c(0,1)) #분류한 Value의 타입을 Factor로 변환
table(data2018$MH1_yr) #정상적으로 분류되었는지 table함수를 통해 확인


# 입원이용 횟수 0:5회미만 // 1:5회이상
data2018$MH1_1[data2018$MH1_1 %in% c(88,99)] <- NA #비해당 및 모름/무응답으로 응답한 사람을 NA로 처리
data2018$MH1_1[data2018$MH1_1 < 5] <- 0 #입원이용횟수가 5회미만인 사람을 0으로 분류
data2018$MH1_1[data2018$MH1_1 >= 5] <- 1 #입원이용 횟수가 5회이상인 사람을 1로 분류
data2018$MH1_1 <- factor(data2018$MH1_1, levels = c(0,1)) #분류한 Value의 타입을 Factor로 변환
table(data2018$MH1_1) #정상적으로 분류되었는지 table함수를 통해 확인


# 외래이용 횟수 0:5회미만 // 1:5회이상
data2018$MO1_1[data2018$MO1_1 %in% c(88,99)] <- NA #비해당 및 모름/무응답으로 응답한 사람을 NA로 처리
data2018$MO1_1[data2018$MO1_1 < 5] <- 0 #외래이용횟수가 5회미만인 사람을 0으로 분류
data2018$MO1_1[data2018$MO1_1 >= 5] <- 1 #외래이용횟수가 5회이상인 사람을 1로 분류
data2018$MO1_1 <- factor(data2018$MO1_1, levels = c(0,1)) #분류한 Value의 타입을 Factor로 변환
table(data2018$MO1_1) #정상적으로 분류되었는지 table함수를 통해 확인


# 한 번에 마시는 음주량 0:4잔 이하 // 1:5잔 이상
data2018$BD2_1[data2018$BD2_1 %in% c(8,9)] <- NA #비해당 및 모름/무응답으로 응답한 사람을 NA로 처리
data2018$BD2_1[data2018$BD2_1 %in% c(1,2)] <- 0 #한 번에 마시는 음주량이 4잔이하인 사람을 0으로 분류
data2018$BD2_1[data2018$BD2_1 %in% c(3,4,5)] <- 1 #한 번에 마시는 음주량이 5잔이상인 사람을 1로 분류
data2018$BD2_1 <- factor(data2018$BD2_1, levels = c(0,1)) #분류한 Value의 타입을 Factor로 변환
table(data2018$BD2_1) #정상적으로 분류되었는지 table함수를 통해 확인


# 폭음 빈도 0:월 1회미만 // 1:월 1회 // 2:주 1회 // 3:거의매일
data2018$BD2_31[data2018$BD2_31 %in% c(8,9)] <- NA #비해당 및 모름/무응답으로 응답한 사람을 NA로 처리
data2018$BD2_31[data2018$BD2_31 %in% c(1,2)] <- 0 #폭음빈도가 월 1회미만인 사람을 0으로 분류
data2018$BD2_31[data2018$BD2_31 == 3] <- 1 #폭음빈도가 월 1회인 사람을 1로 분류
data2018$BD2_31[data2018$BD2_31 == 4] <- 2 #폭음빈도가 주 1회인 사람을 2로 분류
data2018$BD2_31[data2018$BD2_31 == 5] <- 3 #폭음빈도가 거의 매일인 사람을 3으로 분류
data2018$BD2_31 <- factor(data2018$BD2_31, levels = c(0,1,2,3), ordered = T) #분류한 Value의 타입을 Factor로 변환
table(data2018$BD2_31) #정상적으로 분류되었는지 table함수를 통해 확인


# 교육수준 0:대학교 졸업 // 1:고등학교졸업 이하 
data2018$edu[data2018$edu %in% c(1,2,3)] <- 1 #학력이 고등학교 졸업 이하인 사람을 0으로 분류
data2018$edu[data2018$edu == 4] <- 0 #학력이 대학 졸업이상인 사람을 0으로 분류
data2018$edu <- factor(data2018$edu, levels = c(0,1)) #분류한 Value의 타입을 Factor로 변환
table(data2018$edu) #정상적으로 분류되었는지 table함수를 통해 확인


# 활동제한여부 0:아니오 // 1:예 
data2018$LQ4_00[data2018$LQ4_00 == 9] <- NA #모름/무응답으로 설문한 사람을 NA로 처리
data2018$LQ4_00[data2018$LQ4_00 == 2] <- 0 #활동제한을 받지 않은 사람을 0으로 분류
data2018$LQ4_00 <- factor(data2018$LQ4_00, levels = c(0,1)) #분류한 Value의 타입을 Factor로 변환
table(data2018$LQ4_00) #정상적으로 분류되었는지 table함수를 통해 확인


# 나이대 별 분류
data2018$age<- ifelse(data2018$age >= 60, "60s", ifelse(data2018$age>=50, "50s", ifelse(data2018$age>=40, "40s", ifelse(data2018$age>=30, "30s", ifelse(data2018$age>=20, "20s", ifelse(data2018$age>=10, "10s", "0s")))))) #나이를 구간별로 구분하여 10세미만은 “0s”, 그리고 10대는 “10s”, 20대는 “20s” 등으로 분류하여 60살 이상은 “60s”로 묶어서 분류하였다.
data2018$age <- factor(data2018$age, levels = c("0s","10s", "20s", "30s", "40s", "50s", "60s"), ordered = TRUE) #분류한 Value의 타입을 Factor로 변환
table(data2018$age) #정상적으로 분류되었는지 table함수를 통해 확인


# 변수명 변경
data2018 <- rename(data2018, phq_score = mh_PHQ_S, norm_weight = HE_obe, chr_dis_fh = HE_fh, medi_ser_unsat = M_2_yr, act_rest = LQ4_00, pri_medi_ins = npins) #”dplyr”라이브러리의 rename함수로 전처리한 변수들을 알기 쉽도록 변수명 변경


# 변수명 바뀐 것을 확인하기 위해 table() 사용하여 6개의 바뀐 변수명 확인
table(c("phq_score", "norm_weight", "chr_dis_fh", "medi_ser_unsat", "act_rest", "pri_medi_ins") %in% colnames(data2018)) #변경된 변수명이 data2018내에 정상적으로 반영되었는지 table함수를 통해 파악



#### 3.	의사결정나무 그리기



# 의사결정나무 그리기
DT <- data2018 %>% 
  select(phq_score, norm_weight, allownc, chr_dis_fh, medi_ser_unsat, act_rest, pri_medi_ins) %>% 
  filter(phq_score %in% c(0,1) & norm_weight %in% c(0,1) & allownc %in% c(0,1) & chr_dis_fh %in% c(0,1) & medi_ser_unsat %in% c(0,1) & act_rest %in% c(0,1) & pri_medi_ins %in% c(0,1)) #최종적으로 가장 적절하다고 판단된 변수들을 모아 변수들을 select함수로 추려 NA값을 제외하고 분류한 Value를 filter함수로 필터링하여 새로운 데이터프레임 형성
table(DT$phq_score) #새로 형성한 데이터프레임내에 phq_score변수의 비율 확인


install.packages("caret") #층화추출법을 이용하기위해 “caret” 패키지를 설치
library(caret) #설치한 “caret”패키지를 프로젝트 내부로 불러오기
DT_row_idx <- createDataPartition(DT$phq_score, p=0.75, list=FALSE) #”caret”패키지의 createDataPartiotion함수로 phq_score비율에 맞게 전체의 75%뽑기
DT_train_data <- DT[DT_row_idx, ] #위에서 뽑은 전체의 75%비율의 데이터를 Train_set으로 분류
DT_test_data <- DT[-DT_row_idx, ] #그 외 25%비율의 데이터를 Test_set으로 분류


install.packages("rpart") #의사결정나무를 만들 데이터 구축을 위해 ”rpart”패키지 설치
library(rpart) #설치한 ”rpart” 패키지를 프로젝트 내부로 불러오기
DT_rpart_result <- rpart(phq_score~., data=DT_train_data, control=rpart.control(minsplit =2)) #”rpart”패키지의 rpart함수로 위에서 만든 Train_set을 이용하여 phq_score를 Target attribute로 하고 최소한 2가지로 분류되도록 파라미터를 설정하여 의사결정나무에 대한 데이터 구축


install.packages("rpart.plot") #구축한 의사결정나무 데이터를 시각화하기 위해 “rpart.plot” 패키지 설치
library(rpart.plot) #설치한 “rpart.plot”패키지를 프로젝트 내부로 불러오기
rpart.plot(DT_rpart_result) #”rpart.plot”패키지의 rpart.plot함수로 의사결정나무 데이터를 시각화


# 의사결정나무 평가
actual <- DT_test_data$phq_score #위에서 분류한 Test_set의 phq_score 변수를 actual에 할당 
expect <- predict(DT_rpart_result, DT_test_data, type="class") #위에서 분류한 Test_set을 predict함수를 이용하여 의사결정나무에 넣어 나온 결과를 expect에 할당
confusionMatrix(expect, actual, mode="everything") #”caret” 패키지의 confusionMatrix함수로 Test_set의 원래 값과 의사결정나무를 통해 분류된 값을 비교



#### 4.	의사결정나무 가지치기



# 의사결정나무 가지치기
DT_rpart_result$cptable #가지치기를 위해 위에서 그린 의사결정나무를 토대로 가지치기 후 가지수에 따른 CP(Complexity Parameter)와 이에 따른 오차정도를 파악
DT_prune_tree <- prune(DT_rpart_result, cp=0.01388889) #”rpart”패키지의 prune함수로 위에서 찾은 최적의 CP를 토대로 위에서 그린 의사결정나무를 가지치기 실시
rpart.plot(DT_prune_tree) #”rpart.plot”패키지의 rpart.plot함수로 가지치기한 의사결정나무 최적화


# 의사결정나무 가지 친 모델 평가
actual_prune <- DT_test_data$phq_score #위에서 분류한 Test_set의 phq_score변수를 actual_prune에 할당
expect_prune <- predict(DT_prune_tree, DT_test_data, type="class") #위에서 분류한 Test_set을 predict함수를 이용하여 의사결정나무에 넣어 나온 결과를 expect_actual에 할당
confusionMatrix(expect_prune, actual_prune, mode="everything") #”caret” 패키지의 confusionMatrix함수로 	Test_set의 원래 값과 의사결정나무를 통해 분류된 값을 비교