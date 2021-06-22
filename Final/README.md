## 성인 저근로군의 우울증 예측 모델

### Introduction
 - Business Problem(BP): 현대 사회 우울증의 심각성
 - Data Science Problem(DSP): Cluster Analysis를 이용하여 우울증이 빈발하는 군집을 찾아 Classification Analysis 기법 중 Decision Tree를 수립해 Pattern을 파악하여 우울증 예측 및 예방

 - 연구의의
  > 1) 현대 사회 우울증 심각성
  > 2) 정량적이고 객관적인 지표
  > 3) 참신성
  > 4) 사회적 기여도

### Method
 - Data Understanding
    1) Strength
     > 1) 우수한 접근성과 활용성
     > 2) 필요 데이터 추출 용이
     > 3) 공공데이터로 높은 신뢰도

    2) Limitation
     > 1) 연도별 데이터 가중치 처리
     > 2) 연도별 다른 일부 설문 항목
     > 3) 희소한 Numeric Attribute
   
    3) Benefit
     > 1) 무료로 데이터 사용 가능
     > 2) 동일 기수는 같은 가중치 사용으로 노력과 시간 절감
   
    4) Cost
     > 1) 많은 결측치 처리의 어려움

 - Feature Subset Selection
  > 1) Irrelevant Features Elimination (논문 및 Domain Knowledge 활용)
  > 2) Redundant Features Elimination (중복된 정보 제거)

 - Data Preprocessing
  > 1) 이해를 돕기 위해 변수명 변경
  > 2) 모름, 무응답 등 NA값 처리
  > 3) 관심 있는 대상을 1로, 관심 없는 대상을 0으로 이진화
  > 4) MICE: NA값 대체

 - Modeling
  > 1) Cluster Analysis: R 내장함수 kmeans를 활용 (군집 분류) 
  > 2) Staratified Sampling(층화 추출법): caret pacakage를 활용하여 train/test를 3:1분할
  > 3) Classification Analysis: rpart package를 활용하여 Decision Tree 수립

### Result
 - Elbow Point

![image](https://user-images.githubusercontent.com/28617435/122912372-3a7f3300-d393-11eb-86c2-376b8b74973e.png)

 - 군집화 결과

![image](https://user-images.githubusercontent.com/28617435/122912334-2fc49e00-d393-11eb-93e3-4b2a7d2e7b32.png)

 - 근로정도 및 수면정도에 따른 군집화 결과 (우울증 정도)

![image](https://user-images.githubusercontent.com/28617435/122912527-63072d00-d393-11eb-988f-8e32a0c6b254.png)
![image](https://user-images.githubusercontent.com/28617435/122912539-65698700-d393-11eb-8425-e862fcc23d3f.png)



 
 - Confusion Matrix
  > 1) Accuracy: 0.9502
  > 2) F1-score: 0.2564

### Discussion
 1) 저근로 비정규직 대상 향상된 의료 서비스 제공
 2) 의료보험의 가입을 권장 및 장려하는 정부 차원 지원의 필요성
 3) 지역사회에서 저근로 비정규직 종사자의 우울증 예방을 위한 정책과 프로그램 개발에 실질적 지표 제시

### Limitation
 1) Clustering에 2가지 변수만 활용
 2) Clustering에 연속형 변수만 사용
 3) 가중치 적용의 한계
