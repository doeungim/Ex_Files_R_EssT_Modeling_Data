

# 단순 임의 추출 

  idx = sample(1:nrow(iris), nrow(iris) * 0.7, 
               replace = FALSE, prob = NULL)

  train = iris[idx,]
  test = iris[-idx,]

  
# 계통 표존 추출 
  
  #install.packages("doBy")
  library(dplyr)
  library(doBy)

  
  
  table(iris$Species)

  
  # 계층 추출을 위해서는 sampleBy 활용 
  
  sampleBy( ~Species,
            
            frac = 0.3,
            data = iris,
            replace = FALSE,
            systematic = TRUE) -> sample_sys

    
  
  ## 층화 - 각 그룹별 한개씩 추출 
  # iris Species 3개 중 각 10개를 층화 추출 
  #install.packages("sampling")
  
  library(sampling)

  strata(iris,
         c("Species"),
         size = c(10,10,10),
         method = "srswor") -> strata_sample
  
  ## srswor = 비복원 단순임의 추출
  ## srswr = 복원 단순임의 추출 
  ## possion
  ### systmetic = 계통추출
    
  
  getdata(iris, strata_sample) -> iris_sample

  
  
  ## 불균형 데이터 처리 
  
  ## 가. 업샘플링 (Upsampling )
  ## 데이터가 적은 표본을 더 많이 추출하는 방법 
  ## 음성이 많은경우, 적은 양성인 환자들을 더 많이 추출해서 불균형을 해소 
  
  ## 1. 데이터 손실 방지 
  ## 2. 언더 샘플링에 비해 높은 분류 정확도 확보 
  
  library(caret)
  library(lattice)
  
  read.csv("data/diabetes.csv") -> dia_df
  table(dia_df$Outcome)  
  
  
  # 1) Random Upsampling 
  ## 1-1) diabet data set 
  
  dia_df %>% 
    mutate(Outcome = as.factor(Outcome)) -> dia_df

  upSample(x = subset(dia_df, select = -Outcome),
           y = dia_df$Outcome) -> upsample

  table(upsample$Class)  

  ## 1-2) stagect data set 
  #install.packages("rpart")
  library(rpart)
  library(dplyr)
  
  table(stagec$ploidy)
  
  stagec %>% str()
  
  upSample(x = subset(stagec, select = -ploidy),
           y = stagec$ploidy) ->staget_upsample
  
  
  table(staget_upsample$Class)
  
  # 2) SMOTE (Syntehic Minority Over-sampling Techinique)
  
  # 비율이 낮은 분류의 데이터를 표본으로 더 많이 생성하는 방법 
  # 먼저 분류개수가 적은 쪽의 데이터의 샘플 취한 뒤, K 이웃을 찾는다
  # 현재 샘플과 이들 K개 이웃간의 차이를 구하고, 이 차이에서 0-1사이의
  #  임의의 값을 곱하여 원래 샘플에 더하는 방식 
  #  - 과적합 방지 
  #  - 분류겹치거나, 노이스 발생 
  
  #install.packages("smotefamily")
  library(smotefamily)
  
  SMOTE(dia_df[,-9],
        dia_df[, 9],
        dup_size = 1) -> smote_sample

  
  table(smote_sample$data$class)  
  
  
  # 2) IRIS DATA 
  
    iris %>% 
      mutate(factor = ifelse(Species=="setosa", "rare", "common")) %>% 
      select(-Species) -> iris_unbal
    
  
    table(iris_unbal$factor)
    iris_unbal %>% head()
    
  
    SMOTE(iris_unbal[,c(1,2)],
          iris_unbal[, 5],
          K = 3,
          dup_size = 1) -> iris_smote_sample     ## rate 갯수 = 100개 
  
    table(iris_smote_sample$data$class)
    
    # 3) ADASYN (Adaptive Synthetic Sampling)
  
  ADAS(dia_df[,-9],
       dia_df[, 9],
       K = 5) -> adas_sample

  
  table(adas_sample$K)
      
  
  
  ## 나. 다운 샘플링 
  ## 데이터가 많은 쪽을 적게 추출하는 방법 
  ## 장점) 1. 계산 시간이 감소 
  #        2. 과적합이 방지 
  
  #  담점) 1. 데이터 제거하기 때문에 정보 손실이 발생 
  #        2. 샘플이 편향되어 모집단을 대표하기 어려워짐 
  
  
  # 1) Random Under sampling 
  
    
  ## 1-1) diabet data set 
        downSample(x = subset(dia_df, select = -Outcome),
                   y = dia_df$Outcome) -> down_sampling

        
        table(down_sampling$Class)  
        
  ## 1-2)  stagect data set     
        
        downSample(x = subset(stagec, select = -ploidy),
                   y = stagec$ploidy) -> s_down_sample
        
        
        table(s_down_sample$Class)
        
        
  # 2) Tomke Links 
        
        library(unbalanced)

        ubTomek(dia_df[,-9],
                dia_df[, 9],
                verbose = TRUE) -> tomek_sample

        
        table(tomek_sample$Y)                
        
  
  # 3) CNN (Condensed Nearest Neighbor)
        
        ubCNN(dia_df[, -9],
              dia_df[, 9],
              verbose = TRUE) -> cnn_sample

        
        table(cnn_sample$Y)  
  
  
  
  ## ADP 23 기출 
        
        
        read.csv("data/problem1.csv") -> rdata

          
        # NA remove
        
        na.omit(rdata) -> rdata
        sum(is.na(rdata))  
  
        
        # scaling 
        
        minmax_scale = function(x) {
          
          y = (x - min(x)) / (max(x) - min(x))
          return(y)
        }

        rdata %>% 
          select(2:6) %>% 
          minmax_scale() %>% 
          
          bind_cols(rdata %>% select(date,Occupancy)) %>% 
          select(date, everything()) %>% 
          mutate(Occupancy = as.factor(Occupancy)) -> scaled_rdata

        
        
        ## 데이터 불균형 확인 
        
        table(scaled_rdata$Occupancy)
  
        ## Oversampling d: 데이터가 적은 쪽을 더 많이 추출하는 방법 
        # 객실이 비어있는 경우가 많음으로 작은 수의 데이터를 많이 추출 
        # 1. 데이터 손실 방지 2. 언더 샘플링에 비해 높은 분류 정확도 
        # 1. 과적합 발생 2. 계산 시간의 증가 
        
        
        ## Random Upsampling, 
       
        upSample(x = subset(scaled_rdata, select = -Occupancy),
                 y = scaled_rdata$Occupancy) -> over_sample
        
        table(over_sample$Class)
        
        over_sample
        
        over_sample$Occupancy <- over_sample$Class
        
        over_sample[,-7] %>% head()
        
        prop.table(table(over_sample$Occupancy))       
        
        
        
        # SMOTE 
        
        set.seed(1223)

        
        scaled_rdata %>% 
          mutate(Occupancy = as.numeric(Occupancy)) -> rdata2

        
        SMOTE(rdata2[, -c(1,7)], rdata2[,7], dup_size = 6) -> smote_rdata        

        table(smote_rdata$data$class)        
        
        
        # -------- Modeliing : General 
        
        
        rdata %>% 
          mutate(Occupancy = ifelse(Occupancy ==1, 0, 1)) %>% 
          mutate(Occupancy = as.numeric(Occupancy)) %>% 
          select(-1) -> r_df

        
        set.seed(1234)        

        idx = sample(1:nrow(r_df),
                     nrow(r_df) * 0.7,
                     replace = F)

        rtrain = r_df[idx,]
        rtest = r_df[-idx,]

        
        glm(Occupancy ~ ., 
            data = rtrain, family = "binomial") -> rtrain_glm        

        summary(rtrain_glm)

        
        # Predict 
        
        predict(rtrain_glm, 
                newdata = rtest,
                type = "response") -> pred_glm

        
        pred_glm_class = ifelse(pred_glm > 0.5, 1, 0)        

        ## confunsion Matrix 
        
        confusionMatrix(as.factor(pred_glm_class),
                        as.factor(rtest$Occupancy))
        
                
        
                
        ## Random Upsampling Data 
        
        rdata %>% 
          mutate(Occupancy = ifelse(Occupancy ==1, 0, 1)) %>% 
          mutate(Occupancy = as.factor(Occupancy)) %>% 
          select(-1) -> r_df

        
        upSample(r_df[,-6],
                 r_df$Occupancy) -> r_upsample

        
        r_upsample %>% head()
        
        # table(r_upsample$Class)         Accuracy : 0.9887  
        
        set.seed(1223)
        
        idx2 = sample(1:nrow(r_upsample),
                     nrow(r_upsample) * 0.7,
                     replace = F)

        
        over_train = r_upsample[idx2,]
        over_test = r_upsample[-idx2,]

        
        
        glm(Class ~ ., data = r_upsample, family= "binomial") -> up_glm        

        
        predict(up_glm, 
                newdata = over_test,
                type = "response") -> pred_up

        
        pred_up_class = ifelse(pred_up > 0.5 , 1, 0)                

        pred_up_class        
        
        confusionMatrix(as.factor(pred_up_class),
                        as.factor(over_test$Class))
        
        
        
        ## SMOTE  - numeric data로 해서 나누고   Accuracy : 0.989
        
        rdata %>% 
          mutate(Occupancy = ifelse(Occupancy ==1, 0, 1)) %>% 
          mutate(Occupancy = as.numeric(Occupancy)) %>% 
          select(-1) -> r_df

        r_df %>% head()
        
        
        
        SMOTE(r_df[ ,-6], 
              r_df[ , 6],
              dup_size = 1) -> smote_sample

        
        smote_df= smote_sample$data
        
        smote_df %>% 
          mutate(class = ifelse(class == 1, 0 ,1 )) %>% 
          mutate(class = as.factor(class)) -> smoted_df

        
        smoted
        
        # train/test 
        
        idx3 = sample(1:nrow(smoted_df),
                      nrow(smoted_df) * 0.7,
                      replace = F)

        
        smote_train = smoted_df[idx3, ]
        smote_test = smoted_df[-idx3, ]

        
        
        glm(class ~ ., 
            data = smote_train,
            family = "binomial") ->smote_glm        

        
        
        predict(smote_glm,
                newdata = smote_test,
                type = "response") -> smote_pred

        
        smote_pred = ifelse(smote_pred > 0.5, 1, 0)        

        
        confusionMatrix(as.factor(smote_pred),
                        as.factor(smote_test$class))        
        
        
        
        
        
        
        
        
        