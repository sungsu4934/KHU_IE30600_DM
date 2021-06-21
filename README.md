## 2020년 가을학기 고급데이터마이닝 프로젝트

## 주제: 청소년 비만 예측 및 패턴도출
 - ParentsLikeMe에 대한 Business Problem을 정의하고, 이를 Data Science Problem으로 전환하여 해결하고자함
 
### Introduction
 - 세계 비만 인구는 2008년 기준 총 14억 명, 매년 약 280만 명이 비만 관련 질병으로 사망
 - 청소년기 비만은 성인으로 이어질 확률이 높으며 따라서 Random Forest, Logistic Regression 등 분류 기법을 이용한 청소년 비만 사전 예측 및 청소년 비만 유발 요인 파악이 중요
 - 대상 기업 : PatientsLikeMe – “환자들의 페이스북”
 
### Methods
- 국민건강영양조사 및 청소년 건강행태 조사에 참여한 청소년 대상으로 분석 수행
- 기존에 발표된 청소년 비만 관련 논문을 바탕으로 Feature Selection
- EDA (탐색적 분석) : 다중 공선성, 데이터 시각화를 통한 Feature의 Value 분포, 통계량 등 확인
- Intermediate Report 연구 결과를 바탕으로 각 데이터별로 모델의 최고 성능을 보여준 결측치 처리 방법, 스케일링 여부, K = 5/10인 K-fold 교차 검증 수행
- 4가지 분류기법(Random Forest, Logistic Regression, SVM, MLP)사용
- 추가적으로 오버샘플링, PCA, K-Means 기법을 이용하여 모델의 Performance 변화를 확인
  > 모델 별 그리드 서치 수행하며 청소년 통합/남자/여자 데이터 각각에 대해 모델 수립.
 
### Results 및 Discussion
 
 
 
 - 성능척도에 AUC와 F2 Score를 추가하였고 성능 비교 간 F2 Score을 활용하여 기존 F1 Score에 본 연구의 주요 척도인 recall에 대한 가중치를 부여하여 비교
 - “기법 별 효과”는 Random Forest는 PCA의 효과를, Logistic Regression은 K-mean전처리의 효과를, SVM은 비선형 Kernel SVM의 성능향상에 대한 효과를 기재
 - △는 세 데이터 중 하나라도 성능이 하락한 경우 기재, -는 해당사항이 없는 항목에 대해 기재
 - 비선형 커널 트릭으로 데이터의 비선형성까지 포착하여 model의 예측력 견인
 

   >  청소년 남자, 여자 비만에 영향을 주는 인자가 거의 비슷하지만 미세하게 차이를 보임
  
### Limitation 
– 컴퓨팅 리소스 및 가중치 적용의 한계, 결측치 처리법의 한계, PCA 해석의 한계
