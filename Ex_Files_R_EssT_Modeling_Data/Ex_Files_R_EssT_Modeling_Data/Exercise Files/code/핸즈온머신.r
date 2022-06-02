	## LINEAR REGRESSION - HANDS ON MACHINE LEARNING 
  
    # Helper packages
    library(dplyr)      # for data wrangling
    library(ggplot2)    # for awesome graphics
    library(rsample)    # for creating validation splits
    library(recipes)    # for feature engineering
    library(tidyquant)
    
    # Modeling packages
    library(caret)       # for fitting KNN models
    #install.packages("dslabs")
    library(dslabs)
    
    ames <- AmesHousing::make_ames()

    
    set.seed(1234)    
    split = initial_split(ames, 
                          prop = 0.7,
                          strata = "Sale_Price")

    
    
    ames_train =  training(split)   
    ames_test = testing(split)

    
    model1 = lm(Sale_Price ~ Gr_Liv_Area, data = ames_train)         

    
    ## Original Data of sale price & area liner line 
    model1 %>% 
      broom::augment() %>% 
      ggplot(aes(x = Gr_Liv_Area,
                 y = Sale_Price))+
      
      geom_point()+
      geom_smooth(se= FALSE, method = "lm")+
      scale_y_continuous(labels = scales::dollar)+
      theme_tq()
    
    
    model1 %>% 
      broom::augment() %>% 
      
      ggplot(aes(Gr_Liv_Area, Sale_Price))+
      geom_segment(aes(x = Gr_Liv_Area,
                       y = Sale_Price,
                       xend = Gr_Liv_Area,
                       yend = .fitted),
                   alpha = 0.3)+
      
      geom_smooth(se=FALSE, method = "lm")+
      scale_y_continuous(labels = scales::dollar)+
      theme_tq()
    
    
    # RMSE 
    sigma(model1)

    # MSE 
    sigma(model1)^2
    
    # confidence 
    confint(model1, level = 0.95)
    
    
    ## Multiple Linear 
    
    fit1 <- lm(Sale_Price ~ Gr_Liv_Area + Year_Built, data = ames_train)
    fit2 <- lm(Sale_Price ~ Gr_Liv_Area * Year_Built, data = ames_train)

    
    
    plot_grid = expand.grid(
      
      Gr_Liv_Area =   seq(from = min(ames_train$Gr_Liv_Area),
                           to = max(ames_train$Gr_Liv_Area),
                           length = 100),
    
    
      Year_Built =   seq(from = min(ames_train$Year_Built),
                            to = max(ames_train$Year_Built),
                        length = 100)
    ) 

    
    
    
    
    plot_grid$y1 = predict(fit1, newdata = plot_grid)    
    plot_grid$y2 = predict(fit2, newdata = plot_grid)

    
    ## check
    plot_grid
    
            
    plot_grid %>% 
      ggplot(aes(x = Gr_Liv_Area,
                 y = Year_Built,
                 z = y1,
                 fill = y1))+
      
      geom_tile()+
      geom_contour(color = "white")+
      viridis::scale_fill_viridis(name = "Predicted\nvalue", option = "inferno")
    
    
    
    
    ggplot(plot_grid, aes(x = Gr_Liv_Area, y = Year_Built, 
                          z = y2, fill = y1)) +
      geom_tile() +
      geom_contour(color = "white") +
      viridis::scale_fill_viridis(name = "Predicted\nvalue", option = "inferno") +
      theme_tq()
    
    
    
    model3 = lm(Sale_Price ~ ., data = ames_train)

    tidy(model3)
    
    
    
    # Assessing model accuracy
    
    # 10-fold cross validation 
    
    set.seed(12234)

    cv_model1 = train(
      Sale_Price ~ Gr_Liv_Area, 
      ames_train,
      method = "lm",
      trControl = trainControl(method = "cv", number =10)
    )    

    cv_model1    

    
    cv_model2 = train(
      Sale_Price ~ Gr_Liv_Area + Year_Built, 
      ames_train, 
      method = "lm",
      trContorl = trainControl(method ="cv", number = 10)
    )    

    
    cv_model2    

        
    cv_model3 = train(
      Sale_Price ~ ., 
      ames_train, 
      method ="lm",
      trControl = trainControl(method = "cv", number = 10)
    )
    
    cv_model3
    
    # Extract out of sample performance measures 
    
    summary(resamples(list(
      model1 = cv_model1,
      model2 = cv_model2,
      model3 = cv_model3
    )))
    
    
    ames_train %>% 
      ggplot(aes(Year_Built,
                 Sale_Price))+
      
      geom_point(size =1, alpha = .4)+
      geom_smooth(se= FALSE)+
      scale_y_continuous("Sale Price", labels = scales::dollar)+ 
      theme_tq()+
      ggtitle(paste("Non-transformed variable a\n 
                    with a non-liner relationship"))
    
    
    
    df1= broom::augment(cv_model1$finalModel, ames_train)

    df1$.std.resid
    
    df1 %>% 
      ggplot(aes(x = .fitted,
                 y = .std.resid))+
      geom_point(size = 1)+
      theme_tq()

    
    
    df2 = broom::augment(cv_model3$finalModel, ames_train)    

    
      
    df2 %>% 
      na.omit() %>% 
      ggplot(aes(x = .fitted,
                 y = .std.resid))+
      
      geom_point(size = 1)
    
    
    
    # --------------------
    
    df1 = df1 %>% mutate(df1, id = row_number())
    df2 = df2 %>% mutate(df2, id = row_number())

    
    df1 %>% 
      ggplot(aes(id, .std.resid))+
      geom_point(size =1)+
      xlab("Row ID")+
      theme_tq()
    
    
    summary(cv_model3) %>% 
      tidy() %>% 
      
      filter(term %in% c("Garage_Area","Garage_Cars"))

    # Model without Garage_Area 
    
    set.seed(1234)
    
    train(Sale_Price ~ ., 
          select(ames_train, -Garage_Cars),
          method = "lm",
          trControl = trainControl(method = "cv", number = 10)
          ) -> model_no_garage_cars

    
    
    summary(model_no_garage_cars) %>% 
      tidy() %>% 
      filter(term == "Garage_Area")
        
        
    # perform 10-fold cross validation on a PCR model tuning the 
    # number of principal components to use as predictors from 1-20
    
    
    set.seed(1234)

  cv_model_pcr= train(
        Sale_Price ~ ., 
        ames_train, 
        method = "pcr",
        trControl = trainControl(method = "cv", number = 10),
        preProcess = c("zv","center","scale"),
        tuneLength = 20
      )

  cv_model_pcr$bestTune    
    
  ggplot(cv_model_pcr)  

  
  
  # -------------------
  #install.packages("AppliedPredictiveModeling")
  library(AppliedPredictiveModeling)
  library(recipes)
  library(tidyr)

      
    data(solubility)

    s_df = cbind(solTrainX, solTrainY)    

    
    
    
    ## PCA 
    
    pca_df = recipe(solTrainY ~ .,s_df) %>% 
      
      step_center(all_predictors()) %>% 
      step_scale(all_predictors()) %>% 
      step_pca(all_predictors()) %>% 
      prep(traning = s_df, retain = TRUE) %>% 
      
      juice() %>% 
      select(PC1, PC2, solTrainY) %>% 
      rename(`PCR Component 1` = "PC1", 
             `PCR Component 2` = "PC2") %>% 
      
      gather(component, value, -solTrainY)
        
    
    pca_df 
    
    pca_df %>% 
      ggplot(aes(value, solTrainY))

    
    
    
    
    
    # ----------------------------- KNN 
    pacman::p_load(caret, magrittr, pacman, rio, tidyverse)


import("data/b5_df.rds") -> df


df %>% 
  pull(Open)


      df %>% 
        mutate(open_t = case_when(
          
          Open >= 4 ~ "Yes",
          TRUE ~ "No"
        
        )) -> r_df

      
      
      train = r_df %>% sample_frac(0.7)
      test = anti_join(r_df, train)      

      r_df
      
      ## KNN 
      
      ctrl = trainControl(
        method = "repeatedcv",
        number = 10,
        repeats = 3
      )
      
      
      train(
        open_t ~ age + gender + Extrav + Neurot + Agree +Consc, 
        data = train,
        method = "knn",
        trControl = ctrl,
        tuneLength = 20,
        na.action = "na.omit"
      ) -> fit

      
            
      fit
      summary(fit)
    
      
      
      predict(
        fit,
        newdata = test
      ) -> pred_open

      
      ## table convert 0.5747
      
      table(
        actual_open = test$open_t,
        prediction_open = pred_open
      ) %>% 
        confusionMatrix()
    
      
      
      
      # Helper packages
      library(dplyr)      # for data wrangling
      library(ggplot2)    # for awesome graphics
      library(rsample)    # for creating validation splits
      library(recipes)    # for feature engineering
      
      # Modeling packages
      library(caret)       # for fitting KNN models
      #install.packages("dslabs")
      library(dslabs)
      
      ames <- AmesHousing::make_ames()
      
      data(attrition) 
      str(attrition)
      
      
      attrit = attrition %>% 
        mutate_if(is.ordered, factor, ordered = FALSE)

      
      # ---------------------------------------------
      set.seed(12334)      

      
      churn_split = initial_split(attrit, 
                                  prop = 0.7,
                                  strata = "Attrition")      

      
      chrun_train = training(churn_split)      

      mnist <- dslabs::read_mnist()
      names(mnist)
      # ----------------------------------------------
      
      ames_split = initial_split(ames,
                                 prop = 0.7,
                                 strata = "Sale_Price")

      
      
      ames_train = training(ames_split)
      ames_test = testing(ames_split)
      
      
      library(ggmap)
      library(recipes)
      library(FNN)
      #install.packages("ggmap")
      
      df= recipe(Sale_Price ~ ., 
             data = ames_train) %>% 
        
        step_nzv(all_nominal()) %>% 
        step_integer(matches("Qual|Cond|QC|Qu")) %>% 
        step_center(all_numeric(), -all_outcomes()) %>% 
        step_scale(all_numeric(), -all_outcomes()) %>% 
        step_dummy(all_nominal(), - all_outcomes(), one_hot = TRUE) %>% 
        prep(traning = ames_train, retain = TRUE) %>% 
        juice() %>% 
        select(-Sale_Price)
      

      home <-30
      k = 10 
      index <- as.vector(FNN::knnx.index(df[-home, ], df[home, ], k = k))
      knn_homes <- ames_train[c(home,index),]
      
      knn_homes %>% 
        select(Longitude, Latitude) %>% 
        mutate(desc = factor(c('House of Interest',
                               rep('Closest neighbors', k)), 
                levels = c('House of interest', 'Closest neighbors'))) %>%
        
        
       qmplot(Longitude, Latitude,
              data = .,
              maptype = "toner-background",
              darken = .7,
              color = desc,
              size = I(2.5))
      
      
      # ----------------------AMES 
      
      two_houses = ames_train[1:2, c("Gr_Liv_Area","Year_Built")]
      
      two_houses
     
      dist(two_houses, method = "euclidean")
      dist(two_houses, method = "manhattan")

      
      ggplot(two_houses,
             aes(Gr_Liv_Area,
                 Year_Built))+
        geom_point()+
        geom_line()
      
      
      # home1 : 4 bedroom & year 2008
      
      home1 <- ames %>% 
        mutate(id = row_number()) %>% 
        select(Bedroom_AbvGr, Year_Built, id) %>% 
        filter(Bedroom_AbvGr ==4 & Year_Built == 2008) %>% 
        slice(1) %>% 
        mutate(home = "home1") %>% 
        select(home, everything())

      home1
      
      ## home2 : 2 bedroom & year 2008
      home2 = ames %>% 
        mutate(id = row_number()) %>% 
        select(Bedroom_AbvGr, Year_Built) %>% 
        filter(Bedroom_AbvGr ==2 & Year_Built == 2008) %>% 
        slice(1) %>% 
        mutate(home2 = "home2") %>% 
        select(home2, everything())

      
      home2

      
      ## home3 :  3 bedroom built a decade earlier.            
      
      home3 = ames %>% 
                mutate(id = row_number()) %>% 
                select(Bedroom_AbvGr, Year_Built) %>% 
                filter(Bedroom_AbvGr == 2 & Year_Built == 1998) %>% 
                slice(1) %>% 
                mutate(home3 = "home3") %>% 
                select(home3, everything())
      
      home3

      features <- c("Bedroom_AbvGr", "Year_Built")
            
      
      dist(rbind(home1[,features], home2[,features]))     

      dist(rbind(home1[,features], home3[,features]))      
      
      
      
      ## ------------------------- KMEAN / H-cluster 
	  
	  
    # Helper packages
    library(dplyr)       # for data manipulation
    library(ggplot2)     # for data visualization
    
    # Modeling packages
    library(cluster)     # for general clustering algorithms
    library(factoextra)  # for visualizing cluster results
    
    
    
    # 1. Hands on Machine Learning 
    
    
     ## Step 1_ Scaling     
    ames_scale = AmesHousing::make_ames() %>% 
      select_if(is.numeric) %>% 
      select(-Sale_Price) %>% 
      mutate_all(as.double) %>% 
      scale() 

    
    ## Step 3_ dist, hclust 
    
    set.seed(1234)

    #### dissimilarity values with dist()
    d = dist(ames_scale, method = "euclidean")    
    
    #### feed these values into hclust() 
    hc_c = hclust(d, method = "complete")
    hc_a = hclust(d, method = "average")
    hc_s = hclust(d, method = "single")
    hc_d = hclust(d, method = "ward.D")

    
    ####  clustering with agnes
    hc_agnes = agnes(ames_scale, method = "complete")
    
    #### Agglomerative coefficient 
    hc_agnes$ac

    
    m = c("average", "single","complete","ward")
    names(m)=  c( "average", "single", "complete", "ward")
       
    ac = function(x){ 
      agnes(ames_scale, method = x)$ac}     

    purrr::map_dbl(m, ac)    
    
    
    ### diana
    hc_diana = diana(ames_scale)
    hc_diana$dc

    
    ### STEP_3  Determining optimal clusters
    p1 = fviz_nbclust(ames_scale, FUN = hcut, method = "wss", k.max = 10)
    p2 = fviz_nbclust(ames_scale, FUN = hcut, method = "silhouette", k.max = 10)
    p3 = fviz_nbclust(ames_scale, FUN = hcut, method = "gap_stat", k.max = 10)
    
    gridExtra::grid.arrange(p1, p2, p3, nrow = 1)
    
    d = dist(ames_scale, method = "euclidean")
    hc_ward = hclust(d, method = "ward.D2")
    dend_plot <- fviz_dend(hc_ward)
    
    dend_data <- attr(dend_plot, "dendrogram")
    dend_cut <- cut(dend_data, h = 8)

    fviz_dend(dend_cut$lower[[2]])        
        
    ####  cut_tree 
    
    sub_grp = cutree(hc_ward, k = 8)
    table(sub_grp)    
    
    
    
    fviz_dend(hc_ward, 
              k = 8, 
              horiz = TRUE,
              rect = TRUE,
              rect_fill = TRUE,
              k_color = "jco",
              cex = 0.1)

    
    
    
    fviz_dend(dend_cut$lower[[1]])
    
    
    
    pacman::p_load(cluster, factoextra, magrittr, pacman, rio, 
                   tidyverse)
    
    
    import("data/StateData.xlsx") %>% as_tibble() %>% 
      select(state_code, instagram:modernDance) -> df

    
    hc <- df %>%  # Get data
      dist %>%    # Compute distance/dissimilarity matrix
      hclust      # Compute hierarchical clusters

    hc %>% plot(label = df$state_code,
                hung = -1,
                cex = 0.6)    
     
    # ----------------------------------------AMES
    ames_hc <- ames_scale %>% 
      dist %>% 
      hclust
    
    ames_hc %>% plot(hung = -1, cex = 0.6)
    
    ames_scale %>% 
      fviz_nbclust(FUN=hcut, method = "wss")+
      geom_vline(xintercept = 5,
                 color = "red",
                 linetype = "dotted")
    
    
    ames_scale %>% 
      fviz_nbclust(FUN= hcut, 
                   method = "silhouette")
    
    
    ames_scale %>% 
      fviz_nbclust(FUN = hcut, method = "gap_stat", k.max = 10)
    
    
    ames_scale %>% 
      clusGap(
        FUN = hcut, 
        nstart = 50,
        K.max = 10, 
        B= 100
      ) %>% 
      fviz_gap_stat()
    
    
    
    par (mar = c (1, 1, 1, 1))
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
        
    
    
    
        
      
      
      
      
      
      
      
      
      
      

      
      
      
      
            
            
    
    
    
        
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    