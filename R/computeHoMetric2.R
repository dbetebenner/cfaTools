## Fair Trend 
computeHoMetric2 <- function(covid.data){
  #write.csv(covid.data, file="Simulated_Panel_Data.csv")
  
  #Fair Trend is operationalized as a comparison between expected and observed scores
  #in 2021 for a cohort of students, where the expected scores are predicted using 
  #2017 data in a regression with coefficients estimated pre-pandemic 
  
  ## A. Estimate Predicted 2021 Values 
  reg.list     <- vector("list", 4) #these don't need to be lists
  predict.list <- vector("list", 4)
  predict.stack <- NULL
  
  for(g in 5:8){
    #A1. Regressions for 2017 on 2019, by Subject
    historical.cohort <- rbind(covid.data[covid.data$GRADE == g   & covid.data$YEAR == "2019",],
                               covid.data[covid.data$GRADE == g-2 & covid.data$YEAR == "2017",])
    historical.cohort <- reshape(historical.cohort, idvar = c("CONTENT_AREA", "ID"),
                                 direction = "wide", timevar = "YEAR")
    names(historical.cohort) <- gsub("2017", "prior",   names(historical.cohort))
    names(historical.cohort) <- gsub("2019", "current", names(historical.cohort))
    
    #Note, this doesn't have to be a list as it inside the loop
    reg.list[[g-4]] <- vector("list", 2); names(reg.list[[g-4]]) <- c("ELA", "MATHEMATICS")
    reg.list[[g-4]]$ELA <- lm(SCALE_SCORE.current~SCALE_SCORE.prior, 
                              data=historical.cohort[historical.cohort$CONTENT_AREA == "ELA",])
    reg.list[[g-4]]$MATHEMATICS <- lm(SCALE_SCORE.current~SCALE_SCORE.prior, 
                                      data=historical.cohort[historical.cohort$CONTENT_AREA == "MATHEMATICS",])
    
    #A2. Use Resulting Parameters to Create Predicted Values 
    current.cohort <- rbind(covid.data[covid.data$GRADE == g   & covid.data$YEAR == "2021",],
                            covid.data[covid.data$GRADE == g-2 & covid.data$YEAR == "2019",])
    current.cohort <- reshape(current.cohort, idvar = c("CONTENT_AREA", "ID"),
                              direction = "wide", timevar = "YEAR")
    names(current.cohort) <- gsub("2019", "prior",   names(current.cohort))
    names(current.cohort) <- gsub("2021", "current", names(current.cohort))
    
    predict.list[[g-4]] <- vector("list", 2); names(predict.list[[g-4]]) <- c("ELA", "MATHEMATICS") 
    predict.list[[g-4]]$ELA <- data.frame(CONTENT_AREA  = "ELA",
                                          ID            = current.cohort$ID[current.cohort$CONTENT_AREA == "ELA"],
                                          YEAR          = 2021,
                                          GRADE         = g, 
                                          SCALE_SCORE_predict =  predict(object  = reg.list[[g-4]]$ELA, 
                                                                         newdata = current.cohort[current.cohort$CONTENT_AREA == "ELA",]))
    
    predict.list[[g-4]]$MATHEMATICS <- data.frame(CONTENT_AREA  = "MATHEMATICS",
                                                  ID            = current.cohort$ID[current.cohort$CONTENT_AREA == "MATHEMATICS"],
                                                  YEAR          = 2021,
                                                  GRADE         = g, 
                                                  SCALE_SCORE_predict =  predict(object  = reg.list[[g-4]]$MATHEMATICS, 
                                                                                 newdata = current.cohort[current.cohort$CONTENT_AREA == "MATHEMATICS",]))
    
    predict.list[[g-4]] <- do.call(rbind, predict.list[[g-4]])
    
    #A3. Stack up across grades
    predict.stack <- rbind(predict.stack,  predict.list[[g-4]])
    
  }
  
  return(predict.stack)
  #need to merge while maintaining order
}
