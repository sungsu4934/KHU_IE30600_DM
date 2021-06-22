## 성인 독신 여성의 우울증 예측 모델

### Introduction
 - Business Problem(BP): 성인 독신 여성에 대한 우울증 심각성
 - Data Science Problem(DSP): Classification Analysis 기법 중 Decision Tree를 이용해 성인 독신 여성의 Pattern을 파악하여 지역사회에서 독신 여성들의 우울증 예측 및 예방
 - 연구의의
  > 1) 높은 독신여성 우울증 유병률
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
   
    3) Benefit
     > 1) 무료로 데이터 사용 가능
     > 2) 동일 기수는 같은 가중치 사용으로 노력과 시간 절감
   
    4) Cost
     > 1) 많은 결측치 처리의 어려움

 - Feature Subset Selection
  > 1) Irrelevant Features Elimination
  > 2) Redundant Features Elimination

 - Data Preprocessing
  > 1) 이해를 돕기 위해 변수명 변경
  > 2) 모름, 무응답 등 NA값 처리
  > 3) 관심 있는 대상을 1로, 관심 없는 대상을 0으로 이진화

 - Modeling
  > 1) Staratified Sampling(층화 추출법): caret pacakage를 활용하여 train/test를 3:1분할
  > 2) Classification Analysis: rpart package를 활용하여 Decision Tree 수립

### Result
 - Accuracy: 0.9231
 - F1-score: 0.9579

### Discussion
 1) 소득이 낮은 성인 1인 가구 여성 대상 향상된 의료서비스 제공
 2) 의료보험의 가입을 권장 및 장려하는 정부 차원 지원의 필요성
 3) 지역사회에서 성인 독신 여성의 우울증 예방을 위한 정책과 프로그램 개발에 실질적 지표 제시


### Limitation
 1) 결측치 대체의 한계
 2) 가중치 적용의 한계
   


 

