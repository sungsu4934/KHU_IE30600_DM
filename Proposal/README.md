## 흡연 가능성이 높은 청소년에 대한 효율적 흡연 예방 교육의 필요성

### Business Understanding
 - Business Problem(BP): 흡연 가능성이 높은 청소년에 대한 효율적 흡연 예방교육의 필요성 

 - Data Science Probelm(DSP): Classification Analysis 기법 중 Decision Tree를 이용해 청소년 흡연자의 Pattern을 파악하여, 향후 흡연을 할 것으로 예측되는 청소년을 선별해내어 선별적인 흡연 예방 교육을 통해 보다 효율적인 청소년 흡연 예방 효과를 얻는다.

 - 연구의의
  > 1) 금연의 어려움
  > 2) 폐암 발병률
  > 3) 참신성
  > 4) 사회적 기여도

### Data Understanding
 - Strength
  > 1) 우수한 접근성 & 활용성
  > 2) 필요 데이터 추출 용이
  > 3) 공공 데이터 → 높은 신뢰도

 - Limitation
  > 1) 외부적 요인에 영향
  > 2) 연도별 다른 일부 설문항목

 - Benefit
  > 1) 무료로 데이터 사용 가능
  > 2) 동일 기의 데이터는 같은 가중치를 사용(노력 및 시간 절감)

 - Cost
  > 1) 많은 결측치 처리의 어려움

### (Expected) Data Preparation
 - 새로운 변수 생성 및 할당
  > 1) 스트레스 여부: 평상시 스트레스 인지 정도 1~5값을 조금이라도 인지하면 1로, 그렇지 않으면 0으로 이진화
  > 2) 간접흡연 여부: 공공기관실내 간접흡연 노출여부 및 가정실내 간접흡연 노출여부 중 하나라도 "예"라면 1을, 이외의 경우는 0을 할당

 - 모델링에 사용할 변수 선정
  > 1) 평소 스트레스 인지 정도(BP1)
  > 2) 주중 하루 평균 수면시간 [Total_slp_wk]
  > 3) 가정실내 & 공공장소 간접흡연 노출 여부 [BS9_2] & [BS13]

### (Expected) Modeling & Evaluation
 - Modeling: 의사결정나무 기반의 모델링
 - Evaluation: Classification 결과를 Confusion Matrix 기반 척도인 (Accuracy, F1-score, Precision 등을 통해 파악)

