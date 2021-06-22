#### 1.	데이터 불러오기



# 필요 패키지 설치
install.packages("sas7bdat") #SAS파일을 R로 불러올 “sas7bdat” 패키지를 설치
library(sas7bdat)	#설치한 “sas7bdat”패키지를 프로젝트 내부로 불러오기

install.packages("dplyr") #데이터를 필터링 및 변수이름을 재설정할 “dplyr” 패키지를 설치
library(dplyr) #설치한 “dplyr”패키지를 프로젝트 내부로 불러오기

install.packages("mice") #결측치를 대체할 “mice” 패키지를 설치
library(mice) #설치한 “mice”패키지를 프로젝트 내부로 불러오기

install.packages("caret") #훈련데이터와 테스트데이터를 나눌 “caret” 패키지를 설치
library(caret) #설치한 “caret”패키지를 프로젝트 내부로 불러오기

install.packages("rpart") #의사결정나무를 만드는 “rpart” 패키지를 설치
library(rpart) #설치한 “rpart”패키지를 프로젝트 내부로 불러오기

install.packages("rpart.plot") #의사결정나무를 시각화할 “rpart.plot” 패키지를 설치
library(rpart.plot) #설치한 “rpart.plot”패키지를 프로젝트 내부로 불러오기

# 2018년 국민건강영양조사 원시자료 가져오기 및 PHQ-9에 설문한 인원들만 불러오기 
data2018 <- read.sas7bdat(("hn18_all.sas7bdat")) #국민건강영양조사 데이터를 R내부로 불러오기
df <- data2018 #가져온 국민건강영양조사 데이터를 복사
df <- df %>% filter(mh_PHQ_S %in% c(0:27)) #국민건강영양조사에서 PHQ-9을 실시한 사람만 불러오기



#### 2.	 클러스터링



# 주당 평균 근로 시간 전처리
df$EC_wht_23 <- ifelse(df$EC_wht_23 == 888, 0, ifelse(df$EC_wht_23 == 999, NA, df$EC_wht_23))
#비해당(최근 1년 동안 일을 하지 않음)(888)을 0으로, 모름/무응답(999)를 NA로 대체

# 주중 하루 평균 수면 시간(12세 이상) 전처리
df$Total_slp_wk[df$Total_slp_wk %in% c(8888,9999)] <- NA #비해당(소아)(8888), 모름/무응답(9999)를 NA로 대체

# 주말 하루 평균 수면 시간(12세 이상) 전처리
df$Total_slp_wd[df$Total_slp_wd %in% c(8888,9999)] <- NA #비해당(소아)(8888), 모름/무응답(9999)를 NA로 대체

# 하루 평균 수면 시간 (Total_slp)이라는 새로운 Attribute 생성
df$Total_slp <- (df$Total_slp_wd*5 + df$Total_slp_wk*2) / 7 #(주중 하루 수면 시간*5 + 주말 하루 평균 수면 시간*2)/7로 변수 생성

# 군집분석을 실시할 수면시간, 근로시간, 우울증을 가져와 결측치를 다중대체법으로 대체
cluster_df <- df %>% select(EC_wht_23, Total_slp, mh_PHQ_S) #근로시간, 수면시간, PHQ-9 점수만 가져와 데이터프레임 생성
cluster_df <- mice(cluster_df, m=5, printFlag = F, seed=1234) #mice패키지로 대체 가능한 결측치 집합 5개 생성
cluster_df <- complete(cluster_df, 2) #2번 집합으로 결측치를 대체

# 우울증을 정상(PHQ-9 10점 미만), 중등도 우울증(PHQ-9 10점이상 15점미만), 우울증 위험군(PHQ-9 15점이상)으로 분류
cluster_df$mh_PHQ_S[cluster_df$mh_PHQ_S < 10] <- 0 #PHQ-9점수가 10점 미만인 사람을 0으로 할당
cluster_df$mh_PHQ_S[(cluster_df$mh_PHQ_S >= 10) & (cluster_df$mh_PHQ_S < 15)] <- 1 #PHQ-9점수가 10점 이상 15점 미만인 사람을 1로 할당
cluster_df$mh_PHQ_S[cluster_df$mh_PHQ_S >= 15] <- 2 #PHQ-9점수가 15점 이상인 사람을 2로 할당

# 근로시간과 수면시간 정규화
x <- cbind(cluster_df$EC_wht_23,cluster_df$Total_slp) #근로시간과 수면시간을 정규화 하기위해 근로시간과 수면시간만 불러오기
for (i in 1:2){
  x[,i] <- (x[,i]-mean(x[,i]))/sd(x[,i])
} #for문을 이용하여 두 변수를 정규화 실시(평균을 빼고 표준편차로 나눈다)

# SSE를 통해 최적의 군집개수 도출
wssplot <- function(data, nc = 15, seed = 1000) {
  wss <- (nrow(data) - 1) * sum(apply(data, 2, var))
  for (i in 2:nc) {
    set.seed(seed)
    wss[i] <- sum(kmeans(data, centers=i)$withinss)}
  plot(1:nc, wss, type="b", xlab = "Number of Clusters",
       ylab = "Within groups sum of squares")} #군집 개수별로 SSE를 계산
wssplot(x) #군집 개수별로 SSE값을 그래프로 시각화

# 최적의 군집개수(4개)로 클러스터링 실시
clu_best <- kmeans(x,4) #4개의 군집이 최적으로 해석되어 KMEANS알고리즘을 이용하여 군집분석
table(clu_best$cluster) #클러스터별 할당된 인원수 파악
clu_best$centers #클러스터들의 중심점 파악

# Cluster시각화 및 개수확인
qplot(x[,1],x[,2], color = clu_best$cluster , data = cluster_df) #나눠진 군집을 시각화
table(cluster_df$mh_PHQ_S, clu_best$cluster) #군집내 PHQ-9점수에 따라 인원 분류

# Cluster가 나뉘어진 군집별 행번호 분류
index1 <- clu_best$cluster == 1 #1번 군집에 해당하는 사람들의 행번호를 지정
index2 <- clu_best$cluster == 2 #2번 군집에 해당하는 사람들의 행번호를 지정
index3 <- clu_best$cluster == 3 #3번 군집에 해당하는 사람들의 행번호를 지정
index4 <- clu_best$cluster == 4 #4번 군집에 해당하는 사람들의 행번호를 지정

# 해당하는 행번호의 data를 가져오기
df_c1 <- cluster_df[index1,] #1번 군집에 해당하는 인원들의 수면시간과 근로시간을 저장
df_c2 <- cluster_df[index2,] #2번 군집에 해당하는 인원들의 수면시간과 근로시간을 저장
df_c3 <- cluster_df[index3,] #3번 군집에 해당하는 인원들의 수면시간과 근로시간을 저장
df_c4 <- cluster_df[index4,] #4번 군집에 해당하는 인원들의 수면시간과 근로시간을 저장

# boxplot으로 군집별 수면시간 비교 및 시각화
par(mfrow=(c(1,4))) #4가지 그래프를 한 번에 보기위해 PLOT 칸을 4분할
boxplot(df_c1$Total_slp, main = "고근로 수면부족군", sub = "Total_slp", ylim = c(0,700)) + abline(h=447.5047,col="red",lty=3) #1번 군집에 대해 수면시간이 전체 평균을 기준으로 하여 명확히 분류되었는지 확인
boxplot(df_c2$Total_slp, main = "고근로 수면과잉군", sub = "Total_slp", ylim = c(0,700)) + abline(h=447.5047,col="red",lty=3) #2번 군집에 대해 수면시간이 전체 평균을 기준으로 하여 명확히 분류되었는지 확인
boxplot(df_c3$Total_slp, main = "저근로 수면과잉군", sub = "Total_slp", ylim = c(0,700)) + abline(h=447.5047,col="red",lty=3) #3번 군집에 대해 수면시간이 전체 평균을 기준으로 하여 명확히 분류되었는지 확인
boxplot(df_c4$Total_slp, main = "저근로 수면부족군", sub = "Total_slp", ylim = c(0,700)) + abline(h=447.5047,col="red",lty=3) #4번 군집에 대해 수면시간이 전체 평균을 기준으로 하여 명확히 분류되었는지 확인

# boxplot으로 군집별 근로시간 비교 및 시각화
boxplot(df_c1$EC_wht_23, main = "고근로 수면부족군", sub = "EC_wht_23", ylim = c(0,120)) + abline(h=27.68537,col="red",lty=3) #1번 군집에 대해 근로시간이 전체 평균을 기준으로 하여 명확히 분류되었는지 확인
boxplot(df_c2$EC_wht_23, main = "고근로 수면과잉군", sub = "EC_wht_23", ylim = c(0,120)) + abline(h=27.68537,col="red",lty=3) #2번 군집에 대해 근로시간이 전체 평균을 기준으로 하여 명확히 분류되었는지 확인
boxplot(df_c3$EC_wht_23, main = "저근로 수면과잉군", sub = "EC_wht_23", ylim = c(0,120)) + abline(h=27.68537,col="red",lty=3) #3번 군집에 대해 근로시간이 전체 평균을 기준으로 하여 명확히 분류되었는지 확인
boxplot(df_c4$EC_wht_23, main = "저근로 수면부족군", sub = "EC_wht_23", ylim = c(0,120)) + abline(h=27.68537,col="red",lty=3) #4번 군집에 대해 근로시간이 전체 평균을 기준으로 하여 명확히 분류되었는지 확인

# 각 군집의 근로시간과 수면시간의 평균 계산
a1 <- mean(df_c1$EC_wht_23) #1번 군집의 근로시간 평균값을 저장
b1 <-mean(df_c1$Total_slp) #1번 군집의 수면시간 평균값을 저장
a2 <-mean(df_c2$EC_wht_23) #2번 군집의 근로시간 평균값을 저장
b2<- mean(df_c2$Total_slp) #2번 군집의 수면시간 평균값을 저장
a3<-mean(df_c3$EC_wht_23) #3번 군집의 근로시간 평균값을 저장
b3<-mean(df_c3$Total_slp) #3번 군집의 수면시간 평균값을 저장
a4<-mean(df_c4$EC_wht_23) #4번 군집의 근로시간 평균값을 저장
b4<-mean(df_c4$Total_slp) #4번 군집의 수면시간 평균값을 저장

# 각 군집별 근로시간 및 노동시간의 평균 수치화
mean_work_time <-c(round(c(a1, a2,a3,a4),2)) #각 군집의 근로시간 평균을 둘째자리까지 나타내어 값을 저장
mean_work_time #값이 잘 들어갔는지 확인
mean_slp_time <-c(round(c(b1, b2, b3, b4),2)) #각 군집의 수면시간 평균을 둘째자리까지 나타내어 값을 저장
mean_slp_time #값이 잘 들어갔는지 확인
mean_df <- data.frame(mean_work_time, mean_slp_time) #위에서 추출한 벡터값을 합쳐 데이터프레임 형성
mean_df #데이터프레임이 잘 만들어졌는지 확인

# 각 군집별 근로시간 및 노동시간의 평균 시각화
par(mfrow=c(1,1)) #위에서 4분할 되었던 PLOT을 다시 합치기
bar.text <- barplot(as.matrix(t(mean_df)), main = "군집별 근로시간과 수면시간", legend= c("근로시간 (시간)","수면시간 (분)"), beside=T,names = c("고근로 수면부족군", "고근로 수면과잉군", "저근로 수면과잉군", "저근로 수면부족군"),col=rainbow(2), args.legend=list(x='topright'), ylim = c(0,600))   #수면시간과 근로시간의 평균을 그래프로 한 번에 시각화
text(bar.text, as.matrix(t(mean_df))+ 38, labels = as.matrix(t(mean_df)), pos= 1.5) #시각화된 그래프에 평균값 라벨 작성

# 각 군집별 수면시간의 표준편차 계산
sd(df_c1$Total_slp) #1번 군집의 수면시간에 대한 표준편차 계산
sd(df_c2$Total_slp) #2번 군집의 수면시간에 대한 표준편차 계산
sd(df_c3$Total_slp) #3번 군집의 수면시간에 대한 표준편차 계산
sd(df_c4$Total_slp) #4번 군집의 수면시간에 대한 표준편차 계산

# 각 군집별 근로시간의 표준편차 계산
sd(df_c1$EC_wht_23) #1번 군집의 근로시간에 대한 표준편차 계산
sd(df_c2$EC_wht_23) #2번 군집의 근로시간에 대한 표준편차 계산
sd(df_c3$EC_wht_23) #3번 군집의 근로시간에 대한 표준편차 계산
sd(df_c4$EC_wht_23) #4번 군집의 근로시간에 대한 표준편차 계산



#### 3.	 데이터 전처리



# 관심군인 근로시간이 적은 군집 추출
indexcluster <- clu_best$cluster %in% c(3,4) #관심있는 군집 3, 4번(저근로군)을 의사결정나무를 통해 분류분석을 하기 위해 불러오기
df_Cluster <- df[indexcluster,] #Attribute 중 ID가 의사결정나무의 품질을 떨어뜨리기에 제외
dim(df_Cluster) #불러온 데이터가 잘 불러져 왔는지 (행X열)을 통해 확인
table(df_Cluster$mh_PHQ_S) #불러온 데이터 내 PHQ점수의 빈도 확인

# 관심있는 변수들에 대한 무응답을 NA처리
df_Cluster$HE_fh[df_Cluster$HE_fh == 9] <- NA #모름에 해당하는 값을 NA로 대체
df_Cluster$BE3_81[df_Cluster$BE3_81 %in% c(8,9)] <- NA #모름 및 비해당에 해당하는 값을 NA로 대체
df_Cluster$EC_wht_0[df_Cluster$EC_wht_0 %in% c(8,9)] <- NA #모름 및 비해당에 해당하는 값을 NA로 대체
df_Cluster$BD2_1[df_Cluster$BD2_1 %in% c(8,9)] <- NA #모름 및 비해당에 해당하는 값을 NA로 대체
df_Cluster$LQ4_00[df_Cluster$LQ4_00 == 9] <- NA #모름, 무응답에 해당하는 값을 NA로 대체
df_Cluster$BO1_1[df_Cluster$BO1_1 %in% c(8,9)] <- NA #모름 및 비해당에 해당하는 값을 NA로 대체
df_Cluster$BE8_1[df_Cluster$BE8_1 %in% c(88,99)] <- NA #모름 및 비해당에 해당하는 값을 NA로 대체
df_Cluster$DF2_dg[df_Cluster$DF2_dg == 9] <- NA #모름/무응답으로 응답한 사람을 NA로 처리
df_Cluster$DF2_pr[df_Cluster$DF2_pr %in% c(8,9)] <- NA #비해당 및 모름/무응답으로 응답한 사람을 NA로 처리
df_Cluster$HE_HPfh1[df_Cluster$HE_HPfh1 == 9] <- NA #모름/무응답으로 응답한 사람을 NA로 처리
df_Cluster$HE_HPfh2[df_Cluster$HE_HPfh2 == 9] <- NA #모름/무응답으로 응답한 사람을 NA로 처리
df_Cluster$HE_HPfh3[df_Cluster$HE_HPfh3 %in% c(8,9)] <- NA #비해당 및 모름/무응답으로 응답한 사람을 NA로 처리
df_Cluster$HE_HLfh1[df_Cluster$HE_HLfh1 == 9] <- NA #모름/무응답으로 응답한 사람을 NA로 처리
df_Cluster$HE_HLfh2[df_Cluster$HE_HLfh2 == 9] <- NA #모름/무응답으로 응답한 사람을 NA로 처리
df_Cluster$HE_HLfh3[df_Cluster$HE_HLfh3 %in% c(8,9)] <- NA #비해당 및 모름/무응답으로 응답한 사람을 NA로 처리
df_Cluster$HE_IHDfh1[df_Cluster$HE_IHDfh1 == 9] <- NA #모름/무응답으로 응답한 사람을 NA로 처리
df_Cluster$HE_IHDfh2[df_Cluster$HE_IHDfh2 == 9] <- NA #모름/무응답으로 응답한 사람을 NA로 처리
df_Cluster$HE_IHDfh3[df_Cluster$HE_IHDfh3 %in% c(8,9)] <- NA #비해당 및 모름/무응답으로 응답한 사람을 NA로 처리
df_Cluster$HE_STRfh1[df_Cluster$HE_STRfh1 == 9] <- NA #모름/무응답으로 응답한 사람을 NA로 처리
df_Cluster$HE_STRfh2[df_Cluster$HE_STRfh2 == 9] <- NA #모름/무응답으로 응답한 사람을 NA로 처리
df_Cluster$HE_STRfh3[df_Cluster$HE_STRfh3 %in% c(8,9)] <- NA #비해당 및 모름/무응답으로 응답한 사람을 NA로 처리
df_Cluster$HE_DMfh1[df_Cluster$HE_DMfh1 == 9] <- NA #모름/무응답으로 응답한 사람을 NA로 처리
df_Cluster$HE_DMfh2[df_Cluster$HE_DMfh2 == 9] <- NA #모름/무응답으로 응답한 사람을 NA로 처리
df_Cluster$HE_DMfh3[df_Cluster$HE_DMfh3 %in% c(8,9)] #비해당 및 모름/무응답으로 응답한 사람을 NA로 처리
df_Cluster$HE_THfh1[df_Cluster$HE_THfh1 == 9] <- NA #모름/무응답으로 응답한 사람을 NA로 처리
df_Cluster$HE_THfh2[df_Cluster$HE_THfh2 == 9] <- NA #모름/무응답으로 응답한 사람을 NA로 처리
df_Cluster$HE_THfh3[df_Cluster$HE_THfh3 %in% c(8,9)] <- NA #비해당 및 모름/무응답으로 응답한 사람을 NA로 처리
df_Cluster$HE_HBfh1[df_Cluster$HE_HBfh1 == 9] <- NA #모름/무응답으로 응답한 사람을 NA로 처리
df_Cluster$HE_HBfh2[df_Cluster$HE_HBfh2 == 9] <- NA #모름/무응답으로 응답한 사람을 NA로 처리
df_Cluster$HE_HBfh3[df_Cluster$HE_HBfh3 %in% c(8,9)] <- NA #비해당 및 모름/무응답으로 응답한 사람을 NA로 처리
df_Cluster$allownc[df_Cluster$allownc == 99] <- NA #모름/무응답으로 설문한 사람들은 NA로 처리
df_Cluster$LF_SAFE[df_Cluster$LF_SAFE == 9] <- NA #모름/무응답으로 응답한 사람을 NA로 처리
df_Cluster$EC1_1[df_Cluster$EC1_1 %in% c(8,9)] <- NA #모름/무응답으로 응답한 사람을 NA로 처리
df_Cluster$M_2_yr[df_Cluster$M_2_yr %in% c(3,9)] <- NA #의료서비스가 필요 없었던 사람과 모름/무응답으로 설문한 사람을 NA로 처리
df_Cluster$npins[df_Cluster$npins == 9] <- NA #모름/무응답으로 응답한 사람을 NA로 처리
df_Cluster$EC_wht_5[df_Cluster$EC_wht_5 %in% c(88,99)] <- NA #비해당 및 모름/무응답으로 응답한 사람을 NA로 처리
df_Cluster$BE3_91[df_Cluster$BE3_91 %in% c(8,9)] <- NA #비해당 및 모름/무응답으로 응답한 사람을 NA로 처리
df_Cluster$BE3_75[df_Cluster$BE3_75 %in% c(8,9)] <- NA #비해당 및 모름/무응답으로 응답한 사람을 NA로 처리
df_Cluster$BE3_85[df_Cluster$BE3_85 %in% c(8,9)] <- NA #비해당 및 모름/무응답으로 응답한 사람을 NA로 처리
df_Cluster$BE3_71[df_Cluster$BE3_71 %in% c(8,9)] <- NA #비해당 및 모름/무응답으로 응답한 사람을 NA로 처리
df_Cluster$MH1_yr[df_Cluster$MH1_yr == 9] <- NA #모름/무응답으로 응답한 사람을 NA로 처리
df_Cluster$MH1_1[df_Cluster$MH1_1 %in% c(88,99)] <- NA #비해당 및 모름/무응답으로 응답한 사람을 NA로 처리
df_Cluster$MO1_1[df_Cluster$MO1_1 %in% c(88,99)] <- NA #비해당 및 모름/무응답으로 응답한 사람을 NA로 처리
df_Cluster$BD2_31[df_Cluster$BD2_31 %in% c(8,9)] <- NA #비해당 및 모름/무응답으로 응답한 사람을 NA로 처리

# NA 대체를 위해 MICE 사용
df_Cluster <- df_Cluster %>% select(DF2_dg, DF2_pr, HE_HPfh1, HE_HPfh2, HE_HPfh3, HE_HLfh1, HE_HLfh2, HE_HLfh3, HE_IHDfh1, HE_IHDfh2, HE_IHDfh3, HE_STRfh1, HE_STRfh2, HE_STRfh3, HE_DMfh1, HE_DMfh2, HE_DMfh3, HE_THfh1, HE_THfh2, HE_THfh3, HE_HBfh1, HE_HBfh2, HE_HBfh3, allownc, LF_SAFE, EC1_1, HE_obe, HE_BMI, M_2_yr, npins, EC_wht_5, BE3_91, BE3_75, BE3_85, BE3_71, MH1_yr, MH1_1, MO1_1, BD2_31, edu, age, HE_fh, BE3_81, EC_wht_0, BD2_1, LQ4_00, BO1_1, BE8_1, ainc, mh_PHQ_S) #유의미한 변수 추출
df_Cluster <- mice(df_Cluster, m=5, printFlag = F, seed=1000) #mice함수를 이용하여 뽑은 변수들에 대해 결측치를 다중대체법으로 대체한다. 결측치 집합을 5개를 만들고 이를 저장하기 위해 시드값을 표기하였다.
df_Cluster <- complete(df_Cluster, 2) #5개의 집합 중 2번 집합을 가져와 결측치를 대체한다.
table(is.na(df_Cluster)) #결측치가 잘 대체되었는지 확인

# MICE 후 범주화 등의 전처리 및 factor화
# 우울증 의사진단 여부 0: 없음 // 1. 있음
df_Cluster$DF2_dg <- factor(df_Cluster$DF2_dg, levels = c(0,1)) #분류한 Value의 타입을 Factor로 변환
table(df_Cluster$DF2_dg) #정상적으로 분류되었는지 table함수를 통해 확인
table(is.na(df_Cluster$DF2_dg)) #해당 변수에 결측치가 다 제거되었는지 확인

# 우울증 현재 유병 여부 0:없음, 1:있음
df_Cluster$DF2_pr <- factor(df_Cluster$DF2_pr, levels = c(0,1)) #분류한 Value의 타입을 Factor로 변환
table(df_Cluster$DF2_pr) #정상적으로 분류되었는지 table함수를 통해 확인
table(is.na(df_Cluster$DF2_pr)) #해당 변수에 결측치가 다 제거되었는지 확인

# 고혈압 의사진단 여부(부) 0:아니오 // 1:예
df_Cluster$HE_HPfh1 <- factor(df_Cluster$HE_HPfh1, levels = c(0,1)) #분류한 Value의 타입을 Factor로 변환
table(df_Cluster$HE_HPfh1) #정상적으로 분류되었는지 table함수를 통해 확인
table(is.na(df_Cluster$HE_HPfh1)) #해당 변수에 결측치가 다 제거되었는지 확인

# 고혈압 의사진단 여부(모) 0:아니오 // 1:예
df_Cluster$HE_HPfh2 <- factor(df_Cluster$HE_HPfh2, levels = c(0,1)) #분류한 Value의 타입을 Factor로 변환
table(df_Cluster$HE_HPfh2) #정상적으로 분류되었는지 table함수를 통해 확인
table(is.na(df_Cluster$HE_HPfh2)) #해당 변수에 결측치가 다 제거되었는지 확인

# 고혈압 의사진단 여부(형제자매) 0:아니오 // 1:예
df_Cluster$HE_HPfh3 <- factor(df_Cluster$HE_HPfh3, levels = c(0,1)) #분류한 Value의 타입을 Factor로 변환
table(df_Cluster$HE_HPfh3) #정상적으로 분류되었는지 table함수를 통해 확인
table(is.na(df_Cluster$HE_HPfh3)) #해당 변수에 결측치가 다 제거되었는지 확인

# 고지혈증 의사진단 여부(부) 0:아니오 // 1:예
df_Cluster$HE_HLfh1 <- factor(df_Cluster$HE_HLfh1, levels = c(0,1)) #분류한 Value의 타입을 Factor로 변환
table(df_Cluster$HE_HLfh1) #정상적으로 분류되었는지 table함수를 통해 확인
table(is.na(df_Cluster$HE_HLfh1)) #해당 변수에 결측치가 다 제거되었는지 확인

# 고지혈증 의사진단 여부(모) 0:아니오 // 1:예
df_Cluster$HE_HLfh2 <- factor(df_Cluster$HE_HLfh2, levels = c(0,1)) #분류한 Value의 타입을 Factor로 변환
table(df_Cluster$HE_HLfh2) #정상적으로 분류되었는지 table함수를 통해 확인
table(is.na(df_Cluster$HE_HLfh2)) #해당 변수에 결측치가 다 제거되었는지 확인

# 고지혈증 의사진단 여부(형제자매) 0:아니오 // 1:예
df_Cluster$HE_HLfh3 <- factor(df_Cluster$HE_HLfh3, levels = c(0,1)) #분류한 Value의 타입을 Factor로 변환
table(df_Cluster$HE_HLfh3) #정상적으로 분류되었는지 table함수를 통해 확인
table(is.na(df_Cluster$HE_HLfh3)) #해당 변수에 결측치가 다 제거되었는지 확인

# 허혈성심장질환 의사진단 여부(부) 0:아니오 // 1:예
df_Cluster$HE_IHDfh1 <- factor(df_Cluster$HE_IHDfh1, levels = c(0,1)) #분류한 Value의 타입을 Factor로 변환
table(df_Cluster$HE_IHDfh1) #정상적으로 분류되었는지 table함수를 통해 확인
table(is.na(df_Cluster$HE_IHDfh1)) #해당 변수에 결측치가 다 제거되었는지 확인

# 허혈성심장질환 의사진단 여부(모) 0:아니오 // 1:예
df_Cluster$HE_IHDfh2 <- factor(df_Cluster$HE_IHDfh2, levels = c(0,1)) #분류한 Value의 타입을 Factor로 변환
table(df_Cluster$HE_IHDfh2) #정상적으로 분류되었는지 table함수를 통해 확인
table(is.na(df_Cluster$HE_IHDfh2)) #해당 변수에 결측치가 다 제거되었는지 확인

# 허혈성심장질환 의사진단 여부(형제자매) 0:아니오 // 1:예 
df_Cluster$HE_IHDfh3 <- factor(df_Cluster$HE_IHDfh3, levels = c(0,1)) #분류한 Value의 타입을 Factor로 변환
table(df_Cluster$HE_IHDfh3) #정상적으로 분류되었는지 table함수를 통해 확인
table(is.na(df_Cluster$HE_IHDfh3)) #해당 변수에 결측치가 다 제거되었는지 확인

# 뇌졸중 의사진단 여부(부) 0:아니오 // 1:예
df_Cluster$HE_STRfh1 <- factor(df_Cluster$HE_STRfh1, levels = c(0,1)) #분류한 Value의 타입을 Factor로 변환
table(df_Cluster$HE_STRfh1) #정상적으로 분류되었는지 table함수를 통해 확인
table(is.na(df_Cluster$HE_STRfh1)) #해당 변수에 결측치가 다 제거되었는지 확인

# 뇌졸중 의사진단 여부(모) 0:아니오 // 1:예
df_Cluster$HE_STRfh2 <- factor(df_Cluster$HE_STRfh2, levels = c(0,1)) #분류한 Value의 타입을 Factor로 변환
table(df_Cluster$HE_STRfh2) #정상적으로 분류되었는지 table함수를 통해 확인
table(is.na(df_Cluster$HE_STRfh2)) #해당 변수에 결측치가 다 제거되었는지 확인

# 뇌졸중 의사진단 여부(형제자매) 0:아니오 // 1:예
df_Cluster$HE_STRfh3 <- factor(df_Cluster$HE_STRfh3, levels = c(0,1)) #분류한 Value의 타입을 Factor로 변환
table(df_Cluster$HE_STRfh3) #정상적으로 분류되었는지 table함수를 통해 확인
table(is.na(df_Cluster$HE_STRfh3)) #해당 변수에 결측치가 다 제거되었는지 확인

# 당뇨병 의사진단 여부(부) 0:아니오 // 1:예
df_Cluster$HE_DMfh1 <- factor(df_Cluster$HE_DMfh1, levels = c(0,1)) #분류한 Value의 타입을 Factor로 변환
table(df_Cluster$HE_DMfh1) #정상적으로 분류되었는지 table함수를 통해 확인
table(is.na(df_Cluster$HE_DMfh1)) #해당 변수에 결측치가 다 제거되었는지 확인

# 당뇨병 의사진단 여부(모) 0:아니오 // 1:예
df_Cluster$HE_DMfh2 <- factor(df_Cluster$HE_DMfh2, levels = c(0,1)) #분류한 Value의 타입을 Factor로 변환
table(df_Cluster$HE_DMfh2) #정상적으로 분류되었는지 table함수를 통해 확인
table(is.na(df_Cluster$HE_DMfh2)) #해당 변수에 결측치가 다 제거되었는지 확인

# 당뇨병 의사진단 여부(형제자매) 0:아니오 // 1:예
df_Cluster$HE_DMfh3 <- factor(df_Cluster$HE_DMfh3, levels = c(0,1)) #분류한 Value의 타입을 Factor로 변환
table(df_Cluster$HE_DMfh3) #정상적으로 분류되었는지 table함수를 통해 확인
table(is.na(df_Cluster$HE_DMfh3)) #해당 변수에 결측치가 다 제거되었는지 확인

# 갑상선질환 의사진단 여부(부) 0:아니오 // 1:예
df_Cluster$HE_THfh1 <- factor(df_Cluster$HE_THfh1, levels = c(0,1)) #분류한 Value의 타입을 Factor로 변환
table(df_Cluster$HE_THfh1) #정상적으로 분류되었는지 table함수를 통해 확인
table(is.na(df_Cluster$HE_THfh1)) #해당 변수에 결측치가 다 제거되었는지 확인

# 갑상선질환 의사진단 여부(모) 0:아니오 // 1:예
df_Cluster$HE_THfh2 <- factor(df_Cluster$HE_THfh2, levels = c(0,1)) #분류한 Value의 타입을 Factor로 변환
table(df_Cluster$HE_THfh2) #정상적으로 분류되었는지 table함수를 통해 확인
table(is.na(df_Cluster$HE_THfh2)) #해당 변수에 결측치가 다 제거되었는지 확인

# 갑상선질환 의사진단 여부(형제자매) 0:아니오 // 1:예
df_Cluster$HE_THfh3 <- factor(df_Cluster$HE_THfh3, levels = c(0,1)) #분류한 Value의 타입을 Factor로 변환
table(df_Cluster$HE_THfh3) #정상적으로 분류되었는지 table함수를 통해 확인
table(is.na(df_Cluster$HE_THfh3)) #해당 변수에 결측치가 다 제거되었는지 확인

# B형간염 의사진단 여부(부) 0:아니오 // 1:예
df_Cluster$HE_HBfh1 <- factor(df_Cluster$HE_HBfh1, levels = c(0,1)) #분류한 Value의 타입을 Factor로 변환
table(df_Cluster$HE_HBfh1) #정상적으로 분류되었는지 table함수를 통해 확인
table(is.na(df_Cluster$HE_HBfh1)) #해당 변수에 결측치가 다 제거되었는지 확인

# B형간염 의사진단 여부(모) 0:아니오 // 1:예
df_Cluster$HE_HBfh2 <- factor(df_Cluster$HE_HBfh2, levels = c(0,1)) #분류한 Value의 타입을 Factor로 변환
table(df_Cluster$HE_HBfh2) #정상적으로 분류되었는지 table함수를 통해 확인
table(is.na(df_Cluster$HE_HBfh2)) #해당 변수에 결측치가 다 제거되었는지 확인

# B형간염 의사진단 여부(형제자매) 0:아니오 // 1:예
df_Cluster$HE_HBfh3 <- factor(df_Cluster$HE_HBfh3, levels = c(0,1)) #분류한 Value의 타입을 Factor로 변환
table(df_Cluster$HE_HBfh3) #정상적으로 분류되었는지 table함수를 통해 확인
table(is.na(df_Cluster$HE_HBfh3)) #해당 변수에 결측치가 다 제거되었는지 확인

# 기초 생활 수급 여부 0:미수급 // 1:수급
df_Cluster$allownc[df_Cluster$allownc == 10] <- 1 #기초생활수급을 한 경험이 있는 사람은 1로 분류
df_Cluster$allownc[df_Cluster$allownc == 20] <- 0 #기초생활수급을 한 경험이 없는 사람은 0으로 분류
df_Cluster$allownc <- factor(df_Cluster$allownc, levels = c(0,1)) #분류한 Value의 타입을 Factor로 변환
table(df_Cluster$allownc) #정상적으로 분류되었는지 table함수를 통해 확인
table(is.na(df_Cluster$allownc)) #해당 변수에 결측치가 다 제거되었는지 확인

# 식생활 형편이 좋은 사람:0 // 안 좋은 사람:1
df_Cluster$LF_SAFE[df_Cluster$LF_SAFE %in% c(1,2)] <- 0 #식생활형편이 좋은 사람을 0으로 분류
df_Cluster$LF_SAFE[df_Cluster$LF_SAFE %in% c(3,4)] <- 1 #식생활형편이 좋지 않은 사람을 0으로 분류
df_Cluster$LF_SAFE <- factor(df_Cluster$LF_SAFE, levels = c(0,1)) #분류한 Value의 타입을 Factor로 변환
table(df_Cluster$LF_SAFE) #정상적으로 분류되었는지 table함수를 통해 확인
table(is.na(df_Cluster$LF_SAFE)) #해당 변수에 결측치가 다 제거되었는지 확인

# 경제활동상태 여부 0: 아니오 // 1: 예 
df_Cluster$EC1_1[df_Cluster$EC1_1 == 2] <- 0 #경제활동을 하지 않는 사람을 0으로 분류
df_Cluster$EC1_1 <- factor(df_Cluster$EC1_1, levels = c(0,1)) #분류한 Value의 타입을 Factor로 변환
table(df_Cluster$EC1_1) #정상적으로 분류되었는지 table함수를 통해 확인
table(is.na(df_Cluster$EC1_1)) #해당 변수에 결측치가 다 제거되었는지 확인

# 정상체중여부 0:정상 // 1:체중변화있다  
df_Cluster$HE_obe[df_Cluster$HE_obe %in% c(1,3,4,5,6)] <- 1 #저체중과 비만인 사람을 1로 분류
df_Cluster$HE_obe[df_Cluster$HE_obe == 2] <- 0 #정상체중인 사람은 0으로 분류
df_Cluster$HE_obe <- factor(df_Cluster$HE_obe, levels = c(0,1)) #분류한 Value의 타입을 Factor로 변환
table(df_Cluster$HE_obe) #정상적으로 분류되었는지 table함수를 통해 확인
table(is.na(df_Cluster$HE_obe)) #해당 변수에 결측치가 다 제거되었는지 확인

# BMI 지수 0:저체중 // 1: 정상 // 2: 비만
df_Cluster$HE_BMI[df_Cluster$HE_BMI < 18.5] <- 0 #BMI가 18.5미만으로 저체중인 사람을 0으로 분류
df_Cluster$HE_BMI[df_Cluster$HE_BMI >= 18.5 & df_Cluster$HE_BMI <23] <- 1 #BMI가 18.5이상 23미만으로 정상체중인 사람을 1로 분류
df_Cluster$HE_BMI[df_Cluster$HE_BMI >= 23] <- 2 #BMI가 23이상으로 비만인사람을 2로 분류
df_Cluster$HE_BMI <- factor(df_Cluster$HE_BMI, levels = c(0,1,2), ordered = TRUE) #분류한 Value의 타입을 Factor로 변환
table(df_Cluster$HE_BMI) #정상적으로 분류되었는지 table함수를 통해 확인
table(is.na(df_Cluster$HE_BMI)) #해당 변수에 결측치가 다 제거되었는지 확인

# 필요 의료서비스 미충족 여부 0:충족 // 1:미충족 
df_Cluster$M_2_yr[df_Cluster$M_2_yr == 2] <- 0 #의료서비스를 충족한 사람을 0으로 분류
df_Cluster$M_2_yr <- factor(df_Cluster$M_2_yr, levels = c(0,1)) #분류한 Value의 타입을 Factor로 변환
table(df_Cluster$M_2_yr) #정상적으로 분류되었는지 table함수를 통해 확인
table(is.na(df_Cluster$M_2_yr)) #해당 변수에 결측치가 다 제거되었는지 확인

# 민간의료보험가입여부 0:아니오 // 1:예
df_Cluster$npins[df_Cluster$npins == 2] <- 0 #민간의료보험에 가입하지 않은 사람을 0으로 분류
df_Cluster$npins <- factor(df_Cluster$npins, levels = c(0,1)) #분류한 Value의 타입을 Factor로 변환
table(df_Cluster$npins) #정상적으로 분류되었는지 table함수를 통해 확인
table(is.na(df_Cluster$npins)) #해당 변수에 결측치가 다 제거되었는지 확인

# 변형근로시간 1:주간근무 // 2:저녁근무 // 3:밤 근무 // 4:주야간 규칙적 교대근무 // 5:24시간 교대근무 // 6:분할근무 // 7:불규칙 교대근무 // 8:기타 
df_Cluster$EC_wht_5 <- factor(df_Cluster$EC_wht_5, levels = c(1,2,3,4,5,6,7,8)) #분류한 Value의 타입을 Factor로 변환
table(df_Cluster$EC_wht_5) #정상적으로 분류되었는지 table함수를 통해 확인
table(is.na(df_Cluster$EC_wht_5)) #해당 변수에 결측치가 다 제거되었는지 확인

# 장소이동 신체활동 여부 0:아니오 // 1:예
df_Cluster$BE3_91[df_Cluster$BE3_91 == 2] <- 0 #장소이동 신체활동 여부가 없는 사람을 0으로 분류
df_Cluster$BE3_91 <- factor(df_Cluster$BE3_91, levels = c(0,1)) #분류한 Value의 타입을 Factor로 변환
table(df_Cluster$BE3_91) #정상적으로 분류되었는지 table함수를 통해 확인
table(is.na(df_Cluster$BE3_91)) #해당 변수에 결측치가 다 제거되었는지 확인

# 여가 고강도 신체활동 여부 0:아니오 // 1:예
df_Cluster$BE3_75[df_Cluster$BE3_75 == 2] <- 0 #여가 고강도 신체활동 여부가 없는 사람을 0으로 분류
df_Cluster$BE3_75 <- factor(df_Cluster$BE3_75, levels = c(0,1)) #분류한 Value의 타입을 Factor로 변환
table(df_Cluster$BE3_75) #정상적으로 분류되었는지 table함수를 통해 확인
table(is.na(df_Cluster$BE3_75)) #해당 변수에 결측치가 다 제거되었는지 확인

# 여가 중강도 신체활동 여부 0:아니오 // 1:예
df_Cluster$BE3_85[df_Cluster$BE3_85 == 2] <- 0 #여가 중강도 신체활동 여부가 없는 사람을 0으로 분류
df_Cluster$BE3_85 <- factor(df_Cluster$BE3_85, levels = c(0,1)) #분류한 Value의 타입을 Factor로 변환
table(df_Cluster$BE3_85) #정상적으로 분류되었는지 table함수를 통해 확인
table(is.na(df_Cluster$BE3_85)) #해당 변수에 결측치가 다 제거되었는지 확인

# 일 고강도 신체활동 여부 0: 아니오 // 1: 예 
df_Cluster$BE3_71[df_Cluster$BE3_71 == 2] <- 0 #하루에 고강도의 신체활동을 안 하는 사람을 0으로 분류
df_Cluster$BE3_71 <- factor(df_Cluster$BE3_71, levels = c(0,1)) #분류한 Value의 타입을 Factor로 변환
table(df_Cluster$BE3_71) #정상적으로 분류되었는지 table함수를 통해 확인
table(is.na(df_Cluster$BE3_71)) #해당 변수에 결측치가 다 제거되었는지 확인

# 1년간 입원이용 여부 0:없음 // 1:있음
df_Cluster$MH1_yr[df_Cluster$MH1_yr == 2] <- 0 #1년간 입원을 하지 않은 사람을 0으로 분류
df_Cluster$MH1_yr <- factor(df_Cluster$MH1_yr, levels = c(0,1)) #분류한 Value의 타입을 Factor로 변환
table(df_Cluster$MH1_yr) #정상적으로 분류되었는지 table함수를 통해 확인
table(is.na(df_Cluster$MH1_yr)) #해당 변수에 결측치가 다 제거되었는지 확인

# 입원이용 횟수 0:5회미만 // 1:5회이상
df_Cluster$MH1_1[df_Cluster$MH1_1 < 5] <- 0 #입원이용횟수가 5회미만인 사람을 0으로 분류
df_Cluster$MH1_1[df_Cluster$MH1_1 >= 5] <- 1 #입원이용 횟수가 5회이상인 사람을 1로 분류
df_Cluster$MH1_1 <- factor(df_Cluster$MH1_1, levels = c(0,1)) #분류한 Value의 타입을 Factor로 변환
table(df_Cluster$MH1_1) #정상적으로 분류되었는지 table함수를 통해 확인
table(is.na(df_Cluster$MH1_1)) #해당 변수에 결측치가 다 제거되었는지 확인

# 외래이용 횟수 0:5회미만 // 1:5회이상
df_Cluster$MO1_1[df_Cluster$MO1_1 < 5] <- 0 #외래이용횟수가 5회미만인 사람을 0으로 분류
df_Cluster$MO1_1[df_Cluster$MO1_1 >= 5] <- 1 #외래이용횟수가 5회이상인 사람을 1로 분류
df_Cluster$MO1_1 <- factor(df_Cluster$MO1_1, levels = c(0,1)) #분류한 Value의 타입을 Factor로 변환
table(df_Cluster$MO1_1) #정상적으로 분류되었는지 table함수를 통해 확인
table(is.na(df_Cluster$MO1_1)) #해당 변수에 결측치가 다 제거되었는지 확인

# 폭음 빈도 0:월 1회미만 // 1:월 1회 // 2:주 1회 // 3:거의매일
df_Cluster$BD2_31[df_Cluster$BD2_31 %in% c(1,2)] <- 0 #폭음빈도가 월 1회미만인 사람을 0으로 분류
df_Cluster$BD2_31[df_Cluster$BD2_31 == 3] <- 1 #폭음빈도가 월 1회인 사람을 1로 분류
df_Cluster$BD2_31[df_Cluster$BD2_31 == 4] <- 2 #폭음빈도가 주 1회인 사람을 2로 분류
df_Cluster$BD2_31[df_Cluster$BD2_31 == 5] <- 3 #폭음빈도가 거의 매일인 사람을 3으로 분류
df_Cluster$BD2_31 <- factor(df_Cluster$BD2_31, levels = c(0,1,2,3), ordered = T) #분류한 Value의 타입을 Factor로 변환
table(df_Cluster$BD2_31) #정상적으로 분류되었는지 table함수를 통해 확인
table(is.na(df_Cluster$BD2_31)) #해당 변수에 결측치가 다 제거되었는지 확인

# 교육수준 0:대학교 졸업 // 1:고등학교졸업 이하 
df_Cluster$edu[df_Cluster$edu %in% c(1,2,3)] <- 1 #학력이 고등학교 졸업 이하인 사람을 0으로 분류
df_Cluster$edu[df_Cluster$edu == 4] <- 0 #학력이 대학 졸업이상인 사람을 0으로 분류
df_Cluster$edu <- factor(df_Cluster$edu, levels = c(0,1)) #분류한 Value의 타입을 Factor로 변환
table(df_Cluster$edu) #정상적으로 분류되었는지 table함수를 통해 확인
table(is.na(df_Cluster$edu)) #해당 변수에 결측치가 다 제거되었는지 확인

# 나이대 별 분류
df_Cluster$age<- ifelse(df_Cluster$age >= 60, "60s", ifelse(df_Cluster$age>=50, "50s", ifelse(df_Cluster$age>=40, "40s", ifelse(df_Cluster$age>=30, "30s", ifelse(df_Cluster$age>=20, "20s", ifelse(df_Cluster$age>=10, "10s", "0s")))))) #나이를 구간별로 구분하여 10세미만은 “0s”, 그리고 10대는 “10s”, 20대는 “20s” 등으로 분류하여 60살 이상은 “60s”로 묶어서 분류하였다.
df_Cluster$age <- factor(df_Cluster$age, levels = c("0s","10s", "20s", "30s", "40s", "50s", "60s"), ordered = TRUE) #분류한 Value의 타입을 Factor로 변환
table(df_Cluster$age) #정상적으로 분류되었는지 table함수를 통해 확인
table(is.na(df_Cluster$age)) #해당 변수에 결측치가 다 제거되었는지 확인

# 만성질환 의사진단 가족력 여부 0:아니오 // 1:예
table(df_Cluster$HE_fh) #해당 변수에 들어있는 값들에 대한 빈도수 확인
table(is.na(df_Cluster$HE_fh)) #해당 변수에 결측치가 다 제거되었는지 확인
df_Cluster$HE_fh <- as.factor(df_Cluster$HE_fh) #분류한 Value의 타입을 Factor로 변환

# 일 중강도 신체활동 여부 0:아니오 // 1:예 
table(df_Cluster$BE3_81) #해당 변수에 들어있는 값들에 대한 빈도수 확인
table(is.na(df_Cluster$BE3_81)) #해당 변수에 결측치가 다 제거되었는지 확인
df_Cluster$BE3_81[df_Cluster$BE3_81 == 2] <- 0 #Attribute의 value가 2인 값을 0으로 할당
table(df_Cluster$BE3_81) #정상적으로 분류되었는지 table함수를 통해 확인
df_Cluster$BE3_81 <- as.factor(df_Cluster$BE3_81) #분류한 Value의 타입을 Factor로 변환

# 비정규직 여부 0:정규직 // 1:비정규직
table(df_Cluster$EC_wht_0) #해당 변수에 들어있는 값들에 대한 빈도수 확인
table(is.na(df_Cluster$EC_wht_0)) #해당 변수에 결측치가 다 제거되어있는지 확인
df_Cluster$EC_wht_0[df_Cluster$EC_wht_0 == 1] <- 0 #Attribute value가 1인 값들을 0으로 할당
df_Cluster$EC_wht_0[df_Cluster$EC_wht_0 == 2] <- 1 #Attribue value가 2인 값들을 1로 할당
table(df_Cluster$EC_wht_0) #정상적으로 분류되었는지 table함수를 통해 확인
df_Cluster$EC_wht_0 <- as.factor(df_Cluster$EC_wht_0) #분류한 Value의 타입을 Factor로 변환

# 한 번에 마시는 음주량 0:4잔 이하 // 1:5잔 이상
table(df_Cluster$BD2_1) #해당 변수에 들어있는 값들에 대한 빈도수 확인
table(is.na(df_Cluster$BD2_1)) #해당 변수에 결측치가 다 제거되어있는지 확인
df_Cluster$BD2_1[df_Cluster$BD2_1 %in% c(1,2)] <- 0 #Attribute value가 1, 2인 값들을 0으로 할당
df_Cluster$BD2_1[df_Cluster$BD2_1 %in% c(3,4,5)] <- 1 #Attribue value가 3, 4, 5인 값들을 1로 할당
table(df_Cluster$BD2_1) #정상적으로 분류되었는지 table함수를 통해 확인
df_Cluster$BD2_1 <- as.factor(df_Cluster$BD2_1) #분류한 Value의 타입을 Factor로 변환

# 활동 제한 여부 0:아니오 // 1:예
table(df_Cluster$LQ4_00) #해당 변수에 들어있는 값들에 대한 빈도수 확인
table(is.na(df_Cluster$LQ4_00)) #해당 변수에 결측치가 다 제거되어있는지 확인
df_Cluster$LQ4_00[df_Cluster$LQ4_00 == 2] <- 0 #Attribue value가 2인 값들을 0으로 할당
table(df_Cluster$LQ4_00) #정상적으로 분류되었는지 table함수를 통해 확인
df_Cluster$LQ4_00 <- as.factor(df_Cluster$LQ4_00) #분류한 Value의 타입을 Factor로 변환

# 체중변화 0:변화없다 // 1:변화있다.
table(df_Cluster$BO1_1) #해당 변수에 들어있는 값들에 대한 빈도수 확인
table(is.na(df_Cluster$BO1_1)) #해당 변수에 결측치가 다 제거되어있는지 확인
df_Cluster$BO1_1[df_Cluster$BO1_1 == 1] <- 0 #Attribute value가 1인 값들을 0으로 할당
df_Cluster$BO1_1[df_Cluster$BO1_1 %in% c(2,3)] <- 1 #Attribute value가 2, 3인 값들을 1로 할당
table(df_Cluster$BO1_1) #정상적으로 분류되었는지 table함수를 통해 확인
df_Cluster$BO1_1 <- as.factor(df_Cluster$BO1_1) #분류한 Value의 타입을 Factor로 변환

# 하루에 앉아서 보내는 시간 
table(df_Cluster$BE8_1) #정상적으로 분류되었는지 table함수를 통해 확인
table(is.na(df_Cluster$BE8_1)) #해당 변수에 결측치가 다 제거되어있는지 확인

# 소득 
table(df_Cluster$ainc) #정상적으로 분류되었는지 table함수를 통해 확인
table(is.na(df_Cluster$ainc)) #해당 변수에 결측치가 다 제거되어있는지 확인

# 우울증을 정상(PHQ-9 10점 미만) // 우울증 위험군(PHQ-9 10점이상)으로 분류
table(df_Cluster$mh_PHQ_S) #해당 변수에 들어있는 값들에 대한 빈도수 확인
table(is.na(df_Cluster$mh_PHQ_S)) #해당 변수에 결측치가 다 제거되어있는지 확인
df_Cluster$mh_PHQ_S[df_Cluster$mh_PHQ_S < 10] <- 0 #Attribute value가 10보다 작은 값들을 0으로 할당
df_Cluster$mh_PHQ_S[df_Cluster$mh_PHQ_S >= 10] <- 1 #Attribute value가 10이상인 값들을 1로 할당
df_Cluster$mh_PHQ_S <- as.factor(df_Cluster$mh_PHQ_S) #데이터의 타입을 팩터타입으로 변환

# 보기 편하게 변수명 바꾸기
df_Cluster <- rename(df_Cluster, c(chr_dis_fh = "HE_fh",
                                   day_physic_act = "BE3_81",
                                   perma_posit = "EC_wht_0",
                                   liquor_amount = "BD2_1",   
                                   act_rest = "LQ4_00",        
                                   weight_change = "BO1_1",
                                   sit_time = "BE8_1",
                                   phq_score = "mh_PHQ_S")) #rename함수를 이용하여 변수명을 알기 쉽게 변경

# 변수명 바뀐 것을 확인하기 위해 table() 사용하여 6개의 Data Object
table(c("chr_dis_fh", "day_physic_act", "perma_posit", "liquor_amount", "act_rest", "weight_change","sit_time", "phq_score") %in% colnames(df_Cluster)) #변수명이 잘 변경되었는지 확인



#### 4.	 의사결정나무



# 의사결정나무 만들기
df_DT <- df_Cluster %>% select(chr_dis_fh, day_physic_act, perma_posit, liquor_amount, act_rest, weight_change, sit_time, ainc, phq_score) #의사결정나무에 넣을 변수들 추출

DT_Cluster_row_idx <- createDataPartition(df_DT$phq_score, p=0.75, list=FALSE) #phq_score Attribute를 기준으로 층화추출법을 이용하여  훈련 및 테스트 데이터를 3:1로 분류
DT_Cluster_train_data <- df_DT[DT_Cluster_row_idx, ] #훈련 데이터를 저장
DT_Cluster_test_data <- df_DT[-DT_Cluster_row_idx, ] #테스트 데이터를 저장

DT_Cluster_rpart_result <- rpart(phq_score~., data=DT_Cluster_train_data, control = rpart.control(minsplit=2)) #phq_score를 종속변수로 하여 훈련 데이터를 활용하여 의사결정나무 그리기
rpart.plot(DT_Cluster_rpart_result) #의사결정나무를 시각화

# 의사결정나무 평가
actual <- DT_Cluster_test_data$phq_score #테스트 데이터에 원래 저장되어 있던 phq_score attribute value 저장
expect <- predict(DT_Cluster_rpart_result, DT_Cluster_test_data, type="class") #테스트 데이터를 사용하여 위에서 모델링한 의사결정나무에서 예측 실시 후 예측한 phq_score값을 저장
confusionMatrix(expect, actual, mode="everything") #실제값과 예측값을 confusionMatrix로 만들어 비교

# 의사결정나무 가지치기
printcp(DT_Cluster_rpart_result) #위에서 만든 의사결정나무의 complexity추출 및 가지치기 경우의수 선별
DT_Cluster_rpart_result_prune_tree <- prune(DT_Cluster_rpart_result, cp=0.011792) #가장 최적의 complexity를 이용하여 가치지기 실시
rpart.plot(DT_Cluster_rpart_result_prune_tree) #가지치기한 의사결정나무를 시각화

# 의사결정나무 가지친 모델 평가
DT_Cluster_actual_prune <- DT_Cluster_test_data$phq_score #테스트 데이터에 원래 저장되어 있던 phq_score attribute value 저장
DT_Cluster_expect_prune <- predict(DT_Cluster_rpart_result_prune_tree, DT_Cluster_test_data, type="class") #테스트 데이터를 사용하여 가지치기한 의사결정나무에서 예측 실시 후 예측한 phq_score값을 저장
confusionMatrix(DT_Cluster_expect_prune, DT_Cluster_actual_prune, mode="everything") #실제값과 예측값을 confusionMatrix로 만들어 비교