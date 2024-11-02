

library(VIM)
library(xlsx)
library(DMwR2)
library(ggplot2)
library(mice)
library(rJava)
library(readxl)
library(compositions)


rm(list = ls())



excel_name <- c("adakite for R")
table_name <- c("regions for R")

directory_name   <-   ("Imputation and clr output")
directory_location <-
  c("C:\\Users\\Administrator\\Desktop\\R adakite")
datafile_location <-
  paste0(directory_location, "\\", excel_name, ".xlsx")
Num.major.element <- 11
Num.trace.element <- 34
setwd(directory_location)
dir.create(directory_name)
setwd(directory_name)



data <- read_excel(datafile_location, sheet = table_name)
grp  <-
  data$Subtype
Unique_ID   <-
  data$Unique_ID
data = as.data.frame(apply(data, 2, as.numeric))
k    <-
  3
data <-
  cbind(Unique_ID = Unique_ID, Subtype = grp, data[, k:length(data)])



pMiss <-
  function(x) {
    sum(is.na(x)) / length(x) * 100
  }

Type <-
  unique(data$Subtype)
NumSubtype <- length(Type)

mice.comb   <- data.frame()
original.comb   <- data.frame()
miss.data.comb  <-
  matrix(NA, nrow = (length(data) - k + 1), ncol = 1)
for (i in (1:NumSubtype))
{
  temp_data    <-
    data[which(data$Subtype == Type[i]), ]
  temp_dataSubtype      <-
    temp_data$Subtype
  temp_dataID      <-
    temp_data$Unique_ID
  temp_missdata      <-
    apply(temp_data[, k:length(data)], 2, pMiss)
  
  miss.data.comb     <-
    cbind(miss.data.comb, temp_missdata)
  miceimputed       <-
    mice(temp_data[, k:length(data)], m = 5, method = "cart")
  imputed_data <-
    complete(miceimputed)
  name_imputed_data    <-
    cbind(Unique_ID = temp_dataID, Subtype = temp_dataSubtype, imputed_data)
  mice.comb <-
    rbind(mice.comb, name_imputed_data)
  
  name_original_data    <-
    cbind(Unique_ID = temp_dataID, Subtype = temp_dataSubtype, temp_data[, k:length(data)])
  original.comb <-
    rbind(original.comb, name_original_data)
}

miss.data.comb   <-
  rbind(Type, miss.data.comb)

rm(mice.comb.clr)
rm(original.comb.clr)


temp.mice.comb <- mice.comb
temp.original.comb <- original.comb

temp.mice.comb[, c(k:(k + Num.major.element - 1))] <-
  mice.comb[, c(k:(k + Num.major.element - 1))] * 10000 #wt% to ppm
temp.original.comb[, c(k:(k + Num.major.element - 1))] <-
  original.comb[, c(k:(k + Num.major.element - 1))] * 10000 #wt% to ppm

mice.comb.clr <-
  compositions::clr(temp.mice.comb[, c(k:(k + Num.major.element + Num.trace.element - 1))])      #clr
original.comb.clr <-
  compositions::clr(temp.original.comb[, c(k:(k + Num.major.element + Num.trace.element - 1))])      #clr

mice.comb.clr <- cbind(mice.comb[, c(1, 2)], mice.comb.clr)
original.comb.clr <- cbind(original.comb[, c(1, 2)], original.comb.clr)

original.comb.clr <- replace(original.comb.clr, original.comb.clr == 0, "")




write.xlsx2(
  miss.data.comb[, -1],
  paste0(table_name, " imputed data.xlsx"),
  "precentage of missing data"
)
write.xlsx2(
  mice.comb.clr,
  paste0(table_name, " imputed data.xlsx"),
  "imputed data by MICE and clr",
  append = TRUE
)
write.xlsx2(mice.comb,
            paste0(table_name, " imputed data.xlsx"),
            "imputed data by MICE",
            append = TRUE)

write.xlsx2(original.comb.clr,
            paste0(table_name, " original data.xlsx"),
            "original data by clr")
print("===complete===")
