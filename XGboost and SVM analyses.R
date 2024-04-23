########## xgboost and SVM analyses #########

install.packages("e1071")      
install.packages("pROC")       
install.packages("sampling")
install.packages("scutr")             
install.packages("randomForest")      
install.packages("dplyr")             
install.packages("tidyverse")         
install.packages("fastshap")          
install.packages("gtools")
install.packages("caret")         
install.packages("xgboost")         
install.packages("Matrix")        
install.packages("Ckmeans.1d.dp") 
install.packages("ggplot2")
install.packages("ggnewscale")
install.packages("RColorBrewer")
install.packages("customLayout")
install.packages("patchwork")     
install.packages("Cairo")
install.packages("rJava")
install.packages("xlsx")
install.packages("readxl")

library(e1071)      
library(pROC)       
library(sampling)
library(scutr)             
library(randomForest)      
library(dplyr)             
library(tidyverse)         
library(fastshap)          
library(gtools)
library(caret)         
library(xgboost)         
library(Matrix)        
library(Ckmeans.1d.dp) 
library(ggplot2)
library(ggnewscale)
library(RColorBrewer)
library(customLayout)
library(patchwork)     
library(Cairo)
library(rJava)
library(xlsx)
library(readxl)


rm(list=ls())         






data1 <- c("original data")             
data2 <- c("crust")               

SamplingMethod <- c("Strata")      # Strata or Overall or Balance
set_groups_colname <- c("Subtype")

set_directory_name <-                     
  c("RF SVM and XGB output")
set_directory_location <-                  
  c("C:\\Users\\Administrator\\Desktop\\R adakite")

output_file_name <- paste(data1, data2, SamplingMethod)

if (data1 == c("original data")) {   
  if(data2 == c("regions"))
  {excel_name = c("regions for R original data")}
  else{excel_name = c("crust for R original data")}
  sheet_name = c("original data by clr")
  set_datafile_location = 
    paste0(set_directory_location,"\\Imputation and clr output\\",excel_name,".xlsx")
}
if (data1 == c("imputed data")) {   
  if(data2 == c("regions"))
  {excel_name = c("regions for R imputed data")}
  else{excel_name = c("crust for R imputed data")}
  sheet_name = c("imputed data by MICE and clr")
  set_datafile_location = 
    paste0(set_directory_location,"\\Imputation and clr output\\",excel_name,".xlsx")
}
if (data1 == c("PCA data")) {      
  if(data2 == c("regions"))
  {excel_name = c("regions for R PCA result")}
  else{excel_name = c("crust for R PCA result")}
  sheet_name = c("samples coordinate")
  set_datafile_location = 
    paste0(set_directory_location,"\\PCAs output\\",excel_name,".xlsx")
}

setwd(set_directory_location)                        
dir.create(set_directory_name)                       
setwd(set_directory_name)                             





if (length(grep("regions", output_file_name)) == 1) {    
   set_class_level   <- 
    c(
      "Central_S_America",
      "Cu-Au",
      "Cu-Mo",
      "North_N_America",
      "N_China",
       "SW_Pacific",
      "Tethys_collision",
       "W_CAOB"
    )
  
   set_palette =  c(
    "#BEBADA",
    "#FFFF33",
    "#4DAF4A",
#    "#BC80BD",
    "#FDB462",
    "#8DD3C7",
    "#FCCDE5",
    "#80B1D3",
    "#B3DE69"
  )
  set_shape = c(25, 21, 21, 24, 25, 24, 25, 24)
  set_gamma = seq(0.01,0.2,0.05) 
  set_cost = seq(10,50,5)
  }
if (length(grep("crust", output_file_name)) == 1) {    
   set_class_level   <- 
    c(
      "Cu-Au",
      "Cu-Mo",
      "juvenile_crust",
      "mature_crust")
   set_palette =  c(
    "#FFFF00",
    "#4DAF4A",
    "#FCCDE5",
    "#80B1D3")
  set_shape = c(21, 21, 24, 25)
  set_gamma = seq(0.03,0.3,0.05)
  set_cost = seq(10,40,5)
  }


element_gather <-
  c(
    "SiO2",
    "TiO2",
    "Al2O3",
    "Fe2O3",
    "FeO",
    "CaO",
    "MgO",
    "MnO",
    "K2O",
    "Na2O",
    "P2O5",
    "Ag",
    "As",
    "Au",
    "Ba",
    "Bi",
    "Ce",
    "Cr",
    "Co",
    "Cu",
    "Cs",
    "Dy",
    "Eu",
    "Er",
    "Ga",
    "Ge",
    "Gd",
    "Hf",
    "Hg",
    "Ho",
    "In",
    "La",
    "Lu",
    "Mo",
    "Nb",
    "Nd",
    "Ni",
    "Pb",
    "Pr",
    "Rb",
    "Sc",
    "Se",
    "Sn",
    "Sb",
    "Sr",
    "Sm",
    "Te",
    "Ta",
    "Tl",
    "Th",
    "Tb",
    "Tm",
    "U",
    "V",
    "W",
    "Zn",
    "Y",
    "Yb",
    "Zr",
    "Tdm",
    "Dim.1",
    "Dim.2"
  )

if (data2 == c("crust")) {
  set_width = c(9)
  set_height = c(12)
} else if (data2 == c("regions")){
  set_width = c(12)
  set_height = c(18)
}




Rock.Component <- read_excel(set_datafile_location,sheet=sheet_name)

selected.groups  <-  data.frame()  
col_NO <-
  which(colnames(Rock.Component) == set_groups_colname) 
for (i in (1:length( set_class_level))) 
{
  temp_Rock.Component          <-
    Rock.Component[which(Rock.Component[,col_NO] ==  set_class_level[i]),]  
  selected.groups         <-
    rbind(selected.groups, temp_Rock.Component)                                
}
Rock.Component <- selected.groups


Rock.Component.colname  <- colnames(Rock.Component)
Rock.Component.depth    <- dim(Rock.Component)[1]      
Rock.Data <- as.data.frame(1:Rock.Component.depth)      


print("================================================")
Check.Num <- 1
for (i in Rock.Component.colname) {
  j <- paste0("^", i, "$")
  Rock.Component.match <- length(grep(j, element_gather)) 
  if (Rock.Component.match == 1) {
    ele.temp   <-
      as.data.frame(Rock.Component[, which(colnames(Rock.Component) == i)])
    colnames(ele.temp) = i
    Rock.Data  <- cbind(Rock.Data, ele.temp)
  } else{
    print(
      paste0("[",Check.Num,"]",i,"-This element are not defined in elements_gather, please check."
      )
    )
    Check.Num  <- Check.Num + 1
  }
}
print("================================================")
print("Check Finished")

Rock.Data <- Rock.Data[, -1] 

Rock.Component  <- cbind(Rock.Component[,col_NO],Rock.Data) 

class_level <- unique(Rock.Component[,1])                    
Rock.Component[,1] = factor(Rock.Component[,1], levels = class_level)





# sampling the data by strata, overall or Balance
if (SamplingMethod == "Strata") {           
  tempNum <- NA
  NumClass <- length(class_level)            
  for (i in (1:NumClass)) {                 
    TempNumClass <- length(which(Rock.Component$Subtype == class_level[i])) 
    tempNumSam <-
      round(0.7 * TempNumClass)              
    tempNum <- append(tempNum, tempNumSam)   
  }

  NumSampling <- tempNum[-1]                   
    set.seed(1) 
    trainrn <- strata(                      
    Rock.Component,
    stratanames = ("Subtype"),
    size = NumSampling,                      
    method = "srswor"
  )
  
  traindb <- Rock.Component[as.numeric(rownames(trainrn)),]    
  testsdb <- Rock.Component[-as.numeric(rownames(trainrn)),]   

  
} else if (SamplingMethod == "Overall") {
  trainrn <- sample(nrow(Rock.Component), round(3 / 4 * nrow(Rock.Component)))
  traindb <- Rock.Component[trainrn,]
  testsdb <- Rock.Component[-trainrn,]
} else if (SamplingMethod == "Balance") {
  Rock.Component.balance <- SCUT(        
    Rock.Component,
    "Subtype",
    oversample = oversample_smote,       
    undersample = undersample_kmeans,    
    osamp_opts = list(),
    usamp_opts = list()
  )
  trainrn <- sample(nrow(Rock.Component.balance), round(3 / 4 * nrow(Rock.Component.balance)))
  traindb <- Rock.Component.balance[trainrn,]
  testsdb <- Rock.Component.balance[-trainrn,]
} else {
  print("Not Defined Sampling Method, Check SamplingMethod please")
}


traindb <- mutate(traindb, Subtype = as.factor(Subtype)) 
testsdb <- mutate(testsdb, Subtype = as.factor(Subtype))

traindb <- arrange(traindb, Subtype)  
testsdb <- arrange(testsdb, Subtype)  
summary(traindb)
summary(testsdb)






#### xgboost #####

#model_xgb <- xgb.load(paste0("xgboost model for ", output_file_name, ".model"))

# training dataset
traindb_x_XGB <- data.matrix(traindb[, -1]) 
traindb_x_XGB <- Matrix(traindb_x_XGB, sparse = T) 
traindb_y_XGB <- as.numeric(traindb[, 1]) - 1 
traindb_XGB <-
  list(data = traindb_x_XGB, label = traindb_y_XGB) 

traindb_Matrix <-
  xgb.DMatrix(data = traindb_XGB$data, label = traindb_XGB$label)


testsdb_x_XGB <- data.matrix(testsdb[, -1]) 
testsdb_x_XGB <- Matrix(testsdb_x_XGB, sparse = T)
testsdb_y_XGB <- as.numeric(testsdb[, 1]) - 1
testsdb_XGB <-
  list(data = testsdb_x_XGB, label = testsdb_y_XGB)
testsdb_Matrix <-
  xgb.DMatrix(data = testsdb_XGB$data, label = testsdb_XGB$label)



fitControl = trainControl(method = "cv",   
                          number = 10,     
                          search = "grid" 
)

tune_xgb = train(traindb_y_XGB~.,     
                  data = cbind(traindb_y_XGB,traindb[, -1]),
                  method="xgbTree", 
                  trControl=fitControl,
                  nthread = 4  
)


model_xgb <- xgboost(
  data = traindb_Matrix,
  objective = 'multi:softprob',       
  num_class = length(class_level),           
  nrounds = tune_xgb$bestTune$nrounds,
  max_depth = tune_xgb$bestTune$max_depth,
  eta = tune_xgb$bestTune$eta,
  gamma = tune_xgb$bestTune$gamma,
  colsample_bytree = tune_xgb$bestTune$colsample_bytree,
  min_child_weight = tune_xgb$bestTune$min_child_weight,
  subsample = tune_xgb$bestTune$subsample
)

xgb.save(model_xgb, paste0("xgboost model for ", output_file_name, ".model"))





pred_xgb_prob <- predict(model_xgb, newdata = testsdb_Matrix, type = "prob") 
pred_xgb_prob <- matrix(pred_xgb_prob, ncol = length(class_level), byrow = TRUE)       
xgb.roc <- multiclass.roc(testsdb_y_XGB, pred_xgb_prob[, 1]) 
pdf(paste0("xgboost ROC for ", output_file_name, " ", Reduce(paste, colnames(Rock.Data)), ".pdf"),
    paper = "a4",   
    width = 6,      
    height = 6)     
opar <- par(no.readonly = TRUE) 
rocplot <-
  plot(
    xgb.roc$rocs[[1]],             
    col =  set_palette[1],
    xlim = c(1, 0),
    ylim = c(0, 1)
  )  

j<-1
for (i in (2:length(class_level))) {
  j <- j + 1
  rocplot <-
    plot.roc(xgb.roc$rocs[[i]], add = TRUE, col =  set_palette[j]) 
}
legend(x = "bottomright",
       inset = c(0, 0), 
       title = "XGBoost",
       bty = "n",       
       c(class_level), 
       lty = c(1),
       col = c( set_palette),
       lwd = 2,
       cex = 0.8, 
       xpd = TRUE) 

on.exit(par(opar)) 
dev.off()       




#### output xgboost data in xlsx #### 
pred_xgb_clas <- max.col(pred_xgb_prob) - 1 
xgb_ConfusionMatrix <-
  confusionMatrix(as.factor(pred_xgb_clas), as.factor(testsdb_y_XGB))
xgb_ConfusionMatrix

Accuracy <- xgb_ConfusionMatrix
paramater_value <- c(as.numeric(auc(xgb.roc))) 
paramater_name <- c("mean_AUC")
paramater<-cbind(paramater_name,paramater_value)


temp_auc=auc(xgb.roc$rocs[[1]])
for (k in (1:(length(class_level)-1))){
  temp_auc=rbind(temp_auc, auc(xgb.roc$rocs[[k+1]]))
}
names(temp_auc) = c("AUC_value")
names(class_level) = c("Regions")
multiclass_auc<-cbind(class_level,temp_auc)

table_Name <- paste0(output_file_name," xgboost for ",Reduce(paste, colnames(Rock.Data)),".xlsx")
write.xlsx2(paramater, table_Name, "#1 Basic Paramater")
write.xlsx2(Accuracy$table, table_Name, "#2 Confusion Matrix", append = TRUE)
write.xlsx2(Accuracy$overall, table_Name, "#3 XGBoost Overall", append = TRUE)
write.xlsx2(t(Accuracy[[3]]), table_Name, "#4 Paramater", append = TRUE)
write.xlsx2(t(Accuracy[[4]]), table_Name, "#5 Detail Type", append = TRUE)
write.xlsx2(multiclass_auc, table_Name, "#6 Multiclass_AUC", append = TRUE)







#### evaluate xgboost by shap en-bedded in xgboost package ####
# overall importance histogram 
xgb_importance <-
  xgb.importance(model = model_xgb, feature_names = traindb_x_XGB@Dimnames[[2]])
head(xgb_importance)
pdf(paste0("xgboost elemental importance for ", output_file_name, ".pdf"), width = 6, height = 8)  
xgb.ggplot.importance(xgb_importance) 
dev.off()


for (i in (0:(length(class_level) - 1))) {    
  pdf(paste0("xgboost shap summary and dependence for ", output_file_name, " ", sort(class_level)[i + 1], ".pdf"),
    width = 15, height = 18)            
  p <-                                           
    xgb.ggplot.shap.summary(      
      testsdb_x_XGB,
      model = model_xgb,
      target_class = i,                         
      top_n = 30,
      subsample = 0.1,                           
    ) + ggtitle(sort(class_level)[i + 1])       
  print(p)
  
  p <-                                          
    xgb.plot.shap(
      testsdb_x_XGB,
      model = model_xgb,
      target_class = i,
      top_n = 24,
      n_col = 4,
      pch = 21,        
      col = "#3484BE", 
      bg  = "#CFE1EF"  
    )
  print(p)
  dev.off()                                     
}


pdf(paste0("xgboost shap summary for ", output_file_name, ".pdf"), width = 4, height = 6)
for (i in (0:(length( set_class_level) - 1))) { 
  p <-                                          
    xgb.ggplot.shap.summary(      
      testsdb_x_XGB,
      model = model_xgb,
      target_class = i, 
      top_n = 36,
      subsample = 0.1,                          
    ) + 
    labs(y = paste("SHAP value for ",   set_class_level[i + 1]),
         x = element_blank(),
         color = "element value") +
    theme(legend.position = c(.82, .16),
          legend.background = element_rect(fill = "white"))
  fig_name <- paste0("xgb.shap.sum.",i)           
  assign(fig_name,p)                    
  print(p)
}
dev.off()


pdf(
  paste0("xgboost shap_summary in one page for ", output_file_name, ".pdf"),
  width = set_width,
  height = set_height
)
if (data2 == c("crust")) {
  (xgb.shap.sum.0 + xgb.shap.sum.1) /
    (xgb.shap.sum.2 + xgb.shap.sum.3) + 
  plot_annotation(tag_levels = 'a') &
    theme(plot.tag = element_text(size = 12))
} else if (data2 == c("regions")) {
    (xgb.shap.sum.1 + xgb.shap.sum.2 + xgb.shap.sum.3) /
    (xgb.shap.sum.5 + xgb.shap.sum.7 + xgb.shap.sum.0) /
    (xgb.shap.sum.6 + xgb.shap.sum.4) +
  plot_annotation(tag_levels = 'a') &
    theme(plot.tag = element_text(size = 12))

} 

dev.off()


CairoSVG(paste0("xgboost shap_summary in one page for ", output_file_name),
         width = set_width,
         height = set_height)
if (data2 == c("crust")) {
  (xgb.shap.sum.0 + xgb.shap.sum.1) /
    (xgb.shap.sum.2 + xgb.shap.sum.3) + 
    plot_annotation(tag_levels = 'a') &
    theme(plot.tag = element_text(size = 6))
} else if (data2 == c("regions")) {
  (xgb.shap.sum.1 + xgb.shap.sum.2 + xgb.shap.sum.3) /
    (xgb.shap.sum.5 + xgb.shap.sum.7 + xgb.shap.sum.0) /
    (xgb.shap.sum.6 + xgb.shap.sum.4) +
    plot_annotation(tag_levels = 'a') &
    theme(plot.tag = element_text(size = 12))
} 

dev.off()


#### SVM ####

# load saved SVM models
#model.svm.tuned <- readRDS(paste0("SVM model for ", output_file_name, ".rds"))


model.svm <- svm(Subtype ~ ., data = traindb, probability=T) 


model.svm.pred <- predict(model.svm, testsdb[,-1])         
svm.table <- table(model.svm.pred, testsdb$Subtype) 
View(svm.table)
classAgreement(svm.table)
confusionMatrix(svm.table)


tuned <- tune.svm(         
  Subtype ~ .,
  data = traindb,
  gamma = set_gamma, 
  cost = set_cost
)


plotdata <- tuned$performances
Tuneplot<-ggplot(plotdata,aes(x = cost, y = gamma))+
  geom_tile(aes(fill = error))+
  scale_fill_gradientn(colours=brewer.pal(9,"OrRd"))+
  ggtitle("Performance of SVM")
pdf(
  paste0("tune svm heatmap for ", output_file_name, " ", Reduce(paste, colnames(Rock.Data)),".pdf"),
  paper = "a4",
  width = 8,
  height = 4
)
Tuneplot
dev.off()                                                        

model.svm.tuned <- svm(                     
  Subtype ~ .,
  data = traindb,
  gamma = tuned$best.parameters$gamma,
  cost = tuned$best.parameters$cost,
  probability = TRUE
)
saveRDS(model.svm.tuned, file = paste0("SVM model for ", output_file_name, ".rds"))



model.svmt.pred <- predict(model.svm.tuned, testsdb[,-1], type = "prob", probability = TRUE) 
testsdb <- testsdb[names(model.svmt.pred), ]
svmt.table <- table(model.svmt.pred, testsdb$Subtype) 
svmt.table
classAgreement(svmt.table)
confusionMatrix(svmt.table)





model.svmt.pred.roc <-
  attr(model.svmt.pred, "probabilities") 
model.svmt.roc <- multiclass.roc(testsdb$Subtype, model.svmt.pred.roc[, 1]) 

pdf(paste0("SVM ROC for ", output_file_name, " ", Reduce(paste, colnames(Rock.Data)), ".pdf"),
    width = 6,     
    height = 6)     

opar <- par(no.readonly = TRUE)


rocplot <-
  plot(
    model.svmt.roc$rocs[[1]],            
    col =  set_palette[1],
    xlim = c(1, 0),
    ylim = c(0, 1)
  )  


j<-1
for (i in (2:length(class_level))) {
  j <- j + 1
  rocplot <-
    plot.roc(model.svmt.roc$rocs[[i]], add = TRUE, col =  set_palette[j]) 
}
legend(x = "bottomright",
       inset = c(0, 0), 
       title = "SVM",
       bty="n",       
       c(class_level), 
       lty = c(1),
       col = c( set_palette),
       lwd = 2,
       cex = 0.8,    
       xpd = TRUE) 


on.exit(par(opar))


dev.off()                                                         




#### output svm data in xlsx #### 
Accuracy <- confusionMatrix(svmt.table)
paramater_value <- c(model.svm.tuned$cost,
                     model.svm.tuned$gamma,
                     model.svm.tuned$kernel,
                     as.numeric(auc(model.svmt.roc)) 
)
paramater_name <- c("cost","gamma","Kernel","mean_AUC")
paramater<-cbind(paramater_name,paramater_value)

temp_auc=auc(model.svmt.roc$rocs[[1]])
for (k in (1:(length(class_level)-1))){
  temp_auc=rbind(temp_auc,auc(model.svmt.roc$rocs[[k+1]]))
}
names(temp_auc) = c("AUC_value")
names(class_level) = c("Regions")
multiclass_auc<-cbind(class_level,temp_auc)

Sheet_Name <- paste0("SVM data for ", output_file_name, " ", Reduce(paste, colnames(Rock.Data)),".xlsx")
write.xlsx2(paramater, Sheet_Name, "#1 Basic Paramater")
write.xlsx2(Accuracy$table, Sheet_Name, "#2 Confusion Matrix", append = TRUE)
write.xlsx2(Accuracy$overall, Sheet_Name, "#3 SVM Overall", append = TRUE)
write.xlsx2(t(Accuracy[[3]]), Sheet_Name, "#4 Paramater", append = TRUE)
write.xlsx2(t(Accuracy[[4]]), Sheet_Name, "#5 Detail Type", append = TRUE)
write.xlsx2(multiclass_auc, Sheet_Name, "#6 Multiclass_AUC", append = TRUE)




#### explain svm by shap using fastshap #### 
set.seed(101)  
for (i in (1:length(class_level))) {             
  group_name <- sort(class_level)[i]
  simu_No <- c("30")                   
  predict_svm <- function(object, newdata) {  
    pred <- predict(object = object,
                    newdata = newdata,
                    probability = T)
     return(attr(pred, "probabilities")[, i]) 
  }
  shap_svm <- fastshap::explain(
    model.svm.tuned,
    feature_names = names(traindb[, -1]),
    nsim = simu_No,           
    X = traindb[, -1],
    pred_wrapper = predict_svm
  )

  
  pdf(paste0("svm_shap_with_simulation ", simu_No, " ", group_name, " ", output_file_name, ".pdf"),
    width = 6,
    height = 4)

  plot <- 
    autoplot(shap_svm, type = "importance",fill="#84BADB") + 
    ggtitle(group_name)
  print(plot)


  No_important_elements <- 27                         
  shap_svm_importance <- data.frame(
    elements = names(shap_svm),
    importance = apply(shap_svm, MARGIN = 2, FUN = function(x) sum(abs(x)))
  )
  shap_svm_importance <- arrange(shap_svm_importance,desc(importance))    
  shap_svm_importance <- shap_svm_importance[c(1:No_important_elements),] 


    data_shap <- shap_svm %>%                 
    as.data.frame() %>%
    mutate(id = 1:n())%>%
    pivot_longer(cols = -ncol(traindb), values_to = "shap")

  data_element <- traindb[,-1] %>%
    mutate(id = 1:n()) %>%
    pivot_longer(cols = -ncol(traindb))  

  data_shap_element <- data_shap %>%
    left_join(data_element) %>%
    rename("feature"="name") %>%
    group_by(feature) %>%
    mutate(value = (value - min(value)) / (max(value)- min(value))) 

  data4  <- data.frame() 
  for (k in (1:No_important_elements)) {
    svm_shap_important_features          <-
      data_shap_element[which(data_shap_element[,2] == shap_svm_importance$elements[k]),]  
    data4 <- rbind(data4, svm_shap_important_features)                                     
  }
    plot <- ggplot(data4[sample(nrow(data4),0.1*nrow(data4)),]) + 
    aes(x = shap, y = feature, color = value) +
    scale_y_discrete(limits=rev(shap_svm_importance$elements)) + 
      geom_jitter(size = 2, height = 0.1, width = 0, alpha = 0.5) +
    scale_color_gradient(
      low = "#FFCC33", high = "#6600cc", breaks = c(0, 1),
      labels = c("Low", "High"),
      guide = guide_colorbar(barwidth = 1, barheight = 10)
    ) +
    labs(x = paste("SHAP value for", group_name), y = element_blank(), color = "Element content") + 

    theme(legend.position = c(.8, .25),
            legend.background = element_rect(fill = "white"))
    fig_name <- paste0("svm.shap.sum.",i-1)           
    assign(fig_name,plot)                     
  print(plot) 

  for (j in (1:No_important_elements)) {
    plot <-
      autoplot(
        shap_svm,
        type = "dependence",
        feature = shap_svm_importance$elements[j],
        X = traindb,
        smooth = TRUE,
        color_by = "Subtype"
      ) + ggtitle(group_name)
    fig_name <- paste0("svm.shap.depd.",j)           
    assign(fig_name,plot)
    print(plot)                                                   
  }
  dev.off()
}


pdf(
  paste0("svm shap_summary in one page for ", output_file_name, ".pdf"),
  width = set_width,
  height = set_height
)
if (data2 == c("crust")) {
  (svm.shap.sum.0 + svm.shap.sum.1) /
    (svm.shap.sum.2 + svm.shap.sum.3) + 
    plot_annotation(tag_levels = 'a') &
    theme(plot.tag = element_text(size = 10))
} else if (data2 == c("regions")) {
  (svm.shap.sum.1 + svm.shap.sum.2 + svm.shap.sum.3) /
    (svm.shap.sum.5 + svm.shap.sum.7 + svm.shap.sum.0) /
    (svm.shap.sum.6 + svm.shap.sum.4) +
    plot_annotation(tag_levels = 'a') &
    theme(plot.tag = element_text(size = 12))
} else {
  print("check data2")
}
dev.off()

CairoSVG(paste0("svm shap_summary in one page for ", output_file_name),
         width = set_width,
         height = set_height)
if (data2 == c("crust")) {
  (svm.shap.sum.0 + svm.shap.sum.1) /
    (svm.shap.sum.2 + svm.shap.sum.3) 
} else if (data2 == c("regions")) {
  (svm.shap.sum.1 + svm.shap.sum.2 + svm.shap.sum.3) /
    (svm.shap.sum.5 + svm.shap.sum.7 + svm.shap.sum.0) /
    (svm.shap.sum.6 + svm.shap.sum.4) 
} else {
  print("check data2")
}
dev.off()






#### save or load all R data and models ####
save.image(file=paste0(output_file_name, " xgboost and svm shap with ", simu_No," simulation",  ".RData"))
#load(file="svm_shap_with_simulation 30 imputed data regions Balance.RData")




#### use PCA data create discrimination plot  ####
  if (data1 == c("PCA data")) {print("the following operations could be performed")
  } else {print("the following operation is only applicable to PCA data")}
# load saved svm models trained by PCA data
# model.svm.tuned <- readRDS(paste0("SVM model for ", output_file_name, ".rds"))
  predict_model <- c("SVM")   
  SVMmodelGrid <-  
    expand.grid(seq(-10, 10, length.out = 500), 
                seq(-10, 5, length.out = 500))
  names(SVMmodelGrid) <- names(traindb)[2:3]
  
  if (predict_model == c("SVM")) {
    Predicted_range <- predict(model.svm.tuned, SVMmodelGrid)
  } 

  
  SVMaes <- data.frame(SVMmodelGrid, Predicted_range)  
  SVMaes_ratio <- (max(SVMaes$Dim.1) - min(SVMaes$Dim.1)) /
    (max(SVMaes$Dim.2) - min(SVMaes$Dim.2))
  
# create discrimination plot
  SVM_plot <- ggplot() +
    geom_tile(aes(x = Dim.1, y = Dim.2, fill = Predicted_range), data = SVMaes) + 
    scale_x_continuous(expand = c(0, 0),
                       limits = c(min(SVMaes[, 1]), max(SVMaes[, 1]))) +
    scale_y_continuous(expand = c(0, 0),
                       limits = c(min(SVMaes[, 2]), max(SVMaes[, 2]))) +
    coord_fixed(ratio = SVMaes_ratio) +
    scale_color_discrete(name = "Predicted region") +
    labs (x = "PC 1", y = "PC 2") +
    scale_fill_manual(values =  set_palette) +
    theme(axis.title = element_text(size = 12),
          axis.text = element_text(size = 10)) +
    theme(
      legend.key.size = unit(0.1, "inches"),
      legend.text = element_text(size = 10)) + 
    annotate(geom = "rect",xmin = -9.5, xmax = -7, ymin = 4.63, ymax = 2.75, fill="white", alpha = 0.6)
  saveRDS(SVM_plot, file = paste0(output_file_name, " ", predict_model, " plot", ".rds"))
  plot(SVM_plot)
  SVM_plot_dots <- SVM_plot + # add scatters
    new_scale("fill") +
    geom_point(
      data = Rock.Component,
      aes(
        x = Dim.1,
        y = Dim.2,
        fill = Subtype,
        shape = Subtype
      ),
      size = 3
    ) +
    scale_shape_manual(values = set_shape) +
    scale_fill_manual(values =  set_palette) +
    coord_fixed(ratio = SVMaes_ratio) +
    theme(axis.title = element_text(size = 12),
          axis.text = element_text(size = 10)) +
    theme(
      legend.position = c(1.05, 0.5),
      legend.justification = c(0, 0),
      legend.key.size = unit(0.05, "inches"),
      legend.box.just = "right",
      legend.box = "horizontal",
      legend.box.background = element_rect(color = "grey20", size = 0.5),
      legend.margin = margin(3, 3, 3, 3),
      legend.title = element_text(size = 10),
      legend.text = element_text(size = 10)
    ) + 
    annotate(geom = "rect",xmin = -9.5, xmax = -7, ymin = 4.63, ymax = 2.75, fill="white", alpha = 0.6)
  plot(SVM_plot_dots)
  saveRDS(SVM_plot_dots,
          file = paste0(output_file_name, " ", predict_model, " plotdots", ".rds"))
#### output figures in pdf & svg from loaded data ####  
  pdf(
    paste0(
      predict_model,
      " model and scatters for ",
      output_file_name,
      " ",
      Reduce(paste, colnames(Rock.Data)),
      ".pdf"
    ),
    width = 16,
    height = 8
  )
  SVM_plot | SVM_plot_dots # patchwork package
  dev.off()
  

  P1 <-
    readRDS(paste0("PCA data", " regions", " Balance", " SVM plot", ".rds")) + 
    annotate(geom = "text",x = -8.25, y = 3.69, label = "a", size=12)
  P2 <-
    readRDS(paste0("PCA data", " regions", " Balance", " SVM plotdots", ".rds")) + 
    annotate(geom = "text",x = -8.25, y = 3.69, label = "b", size=12)
  P3 <-
    readRDS(paste0("PCA data", " crust", " Balance", " SVM plot", ".rds")) + 
    annotate(geom = "text",x = -8.25, y = 3.69, label = "c", size=12)
  P4 <-
    readRDS(paste0("PCA data", " crust", " Balance", " SVM plotdots", ".rds")) + 
    annotate(geom = "text",x = -8.25, y = 3.69, label = "d", size=12)
  pdf(
    paste0(
      predict_model,
      " model and scatters for ",
      data1,
      " ",
      SamplingMethod,
      " regions and crust ",
      Reduce(paste, colnames(Rock.Data)),
      ".pdf"
    ),
    #  paper = "a4",
    width = 14,
    height = 12
  )
  P1 + P2 + P3 + P4 + plot_layout(guides = "collect")
  dev.off()
  
  CairoSVG(paste0(
    predict_model,
    " model and scatters for ",
    data1,
    " ",
    SamplingMethod,
    " regions and crust ",
    Reduce(paste, colnames(Rock.Data))
  ),
           #    paper = "a4",
           width = set_width,
           height = set_height)
  P1 + P2 + P3 + P4 + plot_layout(guides = "collect")
  dev.off()
  print("complete")
  
  