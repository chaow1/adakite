

library(Rtsne)
library(Rmisc)
library(ggplot2)
library(customLayout)

library(caret)
library(corrplot)
library(randomForest)
library(Boruta)
library(ranger)

library(Cairo)
library(patchwork)


library(rJava)
library(xlsx)
library(readxl)

rm(list = ls())



data1 <- c("crust for R")  # without Japan
#data1 <- c("regions for R") # with Japan


# change data2 and tSNE_method then go to t-SNE
# data2 <- c("all elements")
# data2 <- c("elements filtered by rfe")
# data2 <- c("elements filtered by boruta")
data2 <- c("elements filtered by rfe and boruta")

#tSNE_method <- c("single calculation")
tSNE_method <- c("multiple calculation")
if (tSNE_method == "multiple calculation") {
  n_runs <- 5
}


sheet_name <- c("imputed data by MICE and clr")
set_directory_name   <-
  c("tSNE output")
set_directory_location <-
  c("C:\\Users\\Administrator\\Desktop\\R adakite")
set_datafile_location <-
  paste0(set_directory_location,
         "\\Imputation and clr output\\",
         data1,
         ".xlsx")
set_Lengend_Label <- c("Subtype")

setwd(set_directory_location)
dir.create(set_directory_name)
setwd(set_directory_name)


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
    "Ree",
    "Tdm",
    "F.Lc.Continuous",
    "Grt.Lc.Continuous",
    "F.Emorb.Continuous",
    "Grt.Emorb.Continuous",
    "Dim.1",
    "Dim.2"
  )

set_groups_colname <-
  c("Subtype")

set_color_vector       <-
  c(
    "#7A1B80",
    "#424296",
    "#467EBE",
    "#56A1AF",
    "#73B284",
    "#89B96B",
    "#C6B544",
    "#E68D34",
    "#E35526",
    "#D52616"
  )

if (length(grep("regions", data1)) == 1) {
  set_Level_Label   <-
    c(
      "North_N_America",
      "SW_Pacific",
      "W_CAOB",
      "Japan",
      "Central_S_America",
      "Tethys_collision",
      "N_China",
      "Cu-Au",
      "Cu-Mo"
    )
  set_color =  c(
    "#BEBADA",
    "#FFFF33",
    "#4DAF4A",
    "#BC80BD",
    "#FDB462",
    "#8DD3C7",
    "#FCCDE5",
    "#80B1D3",
    "#B3DE69"
  )
  set_shape = c(25, 21, 21, 25, 25, 24, 24, 25, 24)
}
if (length(grep("crust", data1)) == 1) {
  set_Level_Label   <-
    c("mature_crust", "juvenile_crust", "Cu-Au", "Cu-Mo")
  set_color =  c("#FFFF33", "#4DAF4A", "#FCCDE5", "#80B1D3")
  set_shape = c(21, 21, 24, 25)
}

set_color_gradient <- c(
  "#3D2B93",
  "#423FE2",
  "#4961F4",
  "#2C83F7",
  "#1AA4E0",
  "#2DC1B5",
  "#52C972",
  "#B8C225",
  "#FEC038",
  "#F6F30D"
)




Rock.Component <- read_excel(set_datafile_location, sheet = sheet_name)




selected.groups  <- data.frame()
type_col_numb <-
  which(colnames(Rock.Component) == set_groups_colname)

Rock.Component.colname  <- colnames(Rock.Component)

for (i in set_Level_Label) {
  temp_Rock.Component          <-
    Rock.Component[which(Rock.Component[, type_col_numb] == i), ]
  selected.groups         <-
    rbind(selected.groups, temp_Rock.Component)
}
Rock.Component <- selected.groups





Rock.Component.depth    <-
  dim(Rock.Component)[1]
Rock.Data <-
  as.data.frame(1:Rock.Component.depth)


print("================================================")

Check.Num  <- 1
for (i in Rock.Component.colname) {
  j <- paste0("^", i, "$")
  if (length(grep(j, element_gather)) == 1) {
    ele.temp   <-
      as.data.frame(Rock.Component[, which(colnames(Rock.Component) == i)])
    colnames(ele.temp) = i
    Rock.Data  <- cbind(Rock.Data, ele.temp)
  } else{
    print(
      paste0(
        "[",
        Check.Num,
        "]",
        i,
        "-This element are not defined in elements gather, please check."
      )
    )
    Check.Num  <- Check.Num + 1
  }
}
print("================================================")
print("Check Finished.")
Sys.sleep(0.5)
Rock.Data <- Rock.Data[, -1]




FinalTab        <- Rock.Data
Rock.Data.class <- Rock.Component[, type_col_numb]
Rock <- cbind(Rock.Data.class, FinalTab)
Rock[, 1] = factor(Rock[, 1], levels = unique(Rock[, 1]))

#### feature filter ####
# 1.RF-rfe
# 2.boruta
# 3.SVM-rfe
# 4.importance plots pdf
# 5.extract data by RF-rfe
# 6.extract data by boruta
# 7.correlation pdf and xlsx


#### 1.RF-RFE by caret  ####
rfeControl = rfeControl(
  functions = rfFuncs,
  saveDetails = T,
  method = "cv",
  number = 10
)

set.seed(1)
RFProfile <- rfe(Rock[, -1],
                 Rock[, 1],
                 sizes = c(2:ncol(Rock[, -1])),
                 rfeControl = rfeControl)
optimal_features_rfe <- predictors(RFProfile)
print(optimal_features_rfe)


accuracy_plot_rfe <- plot(RFProfile, type = c("g", "o"))


varimp_data <- data.frame(feature = row.names(varImp(RFProfile))[1:ncol(Rock[, -1])],
                          importance = varImp(RFProfile)[1:ncol(Rock[, -1]), 1])

variable_importance_plot_rfe <-
  ggplot(data = varimp_data, aes(
    x = reorder(feature, -importance),
    y = importance,
    fill = feature
  )) +
  geom_bar(stat = "identity") +
  labs(x = "Features", y = "Variable Importance") +
  geom_text(
    aes(label = round(importance, 2)),
    vjust = 1.6,
    color = "white",
    size = 4
  ) +
  theme_bw() +
  theme(legend.position = "none",
        axis.text.x = element_text(angle = 45, hjust = 1))



#### 2.feature elimination by Boruta ####
set.seed(123)
boruta_Profile <- Boruta(
  Rock[, -1],
  Rock[, 1],
  ntree = 200,
  doTrace = 2,
  holdHistory = TRUE,
  getImp = getImpRfZ
)
print(boruta_Profile)

boruta.importance <- attStats(boruta_Profile)
sorted.importance <- boruta.importance[order(-boruta.importance$meanImp), ]

var_imp_plot_Boruta <- plot(boruta_Profile, las = 2, main = "Boruta Feature Selection")
plot(var_imp_plot_Boruta)




var_imp_Boruta_Zscore <- getImpRfZ(Rock[, -1], Rock[, 1], ntree = 200)
var_imp_data_Boruta_Zscore <- data.frame(
  feature = names(var_imp_Boruta_Zscore)[1:ncol(FinalTab)],
  importance = unname(var_imp_Boruta_Zscore)[1:ncol(FinalTab)]
)
var_imp_plot_Boruta_Zscore <-
  ggplot(data = var_imp_data_Boruta_Zscore, aes(
    x = reorder(feature, -importance),
    y = importance,
    fill = feature
  )) +
  geom_bar(stat = "identity") +
  labs(x = "Features", y = "Variable Importance Z-score") +
  theme_bw() +
  theme(legend.position = "none",
        axis.text.x = element_text(angle = 45, hjust = 1))
plot(var_imp_plot_Boruta_Zscore)


#### 3.svm-rfe ####
library("mt")
imp_feature_svm_ref <- fs.rfe(Rock[, -1], Rock[, 1], fs.len = "full", kernel =
                                "Radial")
imp_feature_svm_ref$fs.order



#### 4.output importance plots in pdf ####

pdf(paste0(data1, " rfe_accuracy_importance.pdf"),
    width = 12,
    height = 8)
print(accuracy_plot_rfe)
print(variable_importance_plot_rfe)
dev.off()


pdf(paste0(data1, " Boruta_importance.pdf"),
    width = 12,
    height = 8)
plot(var_imp_plot_Boruta)
print(var_imp_plot_Boruta_Zscore)
dev.off()




#### 5.extract the data by RF-rfe ####
rock.element.rfe <- as.data.frame(1:dim(Rock.Component)[1])

for (i in optimal_features_rfe) {
  ele.temp   <-
    as.data.frame(Rock.Component[, which(colnames(Rock.Component) == i)])
  colnames(ele.temp) = i
  rock.element.rfe  <- cbind(rock.element.rfe, ele.temp)
}
rock.element.rfe <- rock.element.rfe[, -1]
rock.rfe <- cbind(Rock.Data.class, rock.element.rfe)


#### 6.extract the data by boruta ####

var_imp_data_Boruta_Zscore <- var_imp_data_Boruta_Zscore[order(var_imp_data_Boruta_Zscore$importance), ]

optimal_features_boruta <- var_imp_data_Boruta_Zscore[-c(1:5), ]
print(optimal_features_boruta)
optimal_features_boruta_decrease <- optimal_features_boruta[order(optimal_features_boruta$importance, decreasing = TRUE), ]

rock.element.boruta <- as.data.frame(1:dim(Rock.Component)[1])

for (i in optimal_features_boruta_decrease[, 1]) {
  ele.temp   <-
    as.data.frame(Rock.Component[, which(colnames(Rock.Component) == i)])
  colnames(ele.temp) = i
  rock.element.boruta  <- cbind(rock.element.boruta, ele.temp)
}
rock.element.boruta <- rock.element.boruta[, -1]
rock.boruta <- cbind(Rock.Data.class, rock.element.boruta)




#### 7.output correlation plots and xlsx ####
pdf(paste0(data1, " correlation_comparison.pdf"),
    width = 8,
    height = 10)
corrplot(cor(FinalTab), title = "all elements")
corrplot(cor(rock.element.rfe), title = "elements filtered by rfe")
corrplot(cor(rock.element.boruta), title = "elements filtered by boruta")
dev.off()


xlsx_name <- paste0(data1, " filtered elements result", ".xlsx")
write.xlsx2(excluded_by_ref, xlsx_name, "elements exluded")
write.xlsx2(optimal_features_rfe, xlsx_name, "elements included", append = TRUE)




if (data1 == c("crust for R")) {
  excluded_elements = c("Pr", "Dy", "Fe2O3", "FeO", "P2O5", "Eu", "Gd", "Tb", "Hf")
}
if (data1 == c("regions for R")) {
  excluded_elements = c("Fe2O3", "FeO", "Y", "Pr", "Nd", "Tb", "Er", "Tm")
}
filtered_elements_rfe_boruta <- setdiff(colnames(FinalTab), excluded_elements)
print(paste(
  "elements included by rfe and boruta:",
  filtered_elements_rfe_boruta
))

rock.element.rfe.boruta <- as.data.frame(1:dim(Rock.Component)[1])    #dim[1]=row number，dim[2]=column numberʾ
for (i in filtered_elements_rfe_boruta) {
  ele.temp   <-
    as.data.frame(Rock.Component[, which(colnames(Rock.Component) == i)])
  colnames(ele.temp) = i
  rock.element.rfe.boruta  <- cbind(rock.element.rfe.boruta, ele.temp)
}
rock.element.rfe.boruta <- rock.element.rfe.boruta[, -1] #delete the initially defined null dataframe
rock.rfe.boruta <- cbind(Rock.Data.class, rock.element.rfe.boruta)


#### t-SNE analysis ####
# 1.define data for calculation
# 2.single calculation
# 3.multiple calculation
# 4.output the result in xlsx

# 1.define data for calculation
if (data2 == c("all elements")) {
  elements_for_tsne = FinalTab
}
if (data2 == c("elements filtered by rfe")) {
  elements_for_tsne = rock.element.rfe
}
if (data2 == c("elements filtered by boruta")) {
  elements_for_tsne = rock.element.boruta
}
if (data2 == c("elements filtered by rfe and boruta")) {
  elements_for_tsne = rock.element.rfe.boruta
}

# 2.single calculation
if (tSNE_method == "single calculation") {
  set.seed(42)
  tsne_result <- Rtsne(
    elements_for_tsne,
    dims = 2,
    perplexity = 40,
    max_iter = 5000
  )
}

# 3.multiple calculation (Horrocks et al., 2019)
if (tSNE_method == "multiple calculation") {
  set.seed(42)
  tsnes <- vector("list", n_runs)
  for (i in 1:n_runs) {
    print(paste0('Run #', i))
    tsne_out <- Rtsne(
      elements_for_tsne,
      perplexity = 40,
      verbose = FALSE,
      check_duplicates = FALSE,
      max_iter = 5000,
      PCA = FALSE
    )
    tsnes[[i]] <- tsne_out
    print(paste0('Embedding error #', tail(tsne_out$itercosts, n = 1)))
  }
  
  
  errors <- vector(mode = "numeric", length = n_runs)
  i <- 1
  for (tsne in tsnes) {
    errors[[i]] <- tail(tsne$itercosts, n = 1)
    i <- i + 1
  }
  best_i <- which.min(errors)
  tsne_result <- tsnes[[best_i]]
}

# 4.output the result in xlsx

Rock.tsne.data <- data.frame(Rock.Data.class,
                             Dim1 = tsne_result$Y[, 1],
                             Dim2 = tsne_result$Y[, 2],
                             elements_for_tsne)
write.xlsx(Rock.tsne.data,
           paste("t-SNE data", data1, data2, tSNE_method, ".xlsx"),
           "tsne data")



Dim1.arr <- abs(max(Rock.tsne.data$Dim1) - min(Rock.tsne.data$Dim1))
Dim2.arr <- abs(max(Rock.tsne.data$Dim2) - min(Rock.tsne.data$Dim2))
if (Dim1.arr < Dim2.arr) {
  if (max(Rock.tsne.data$Dim2) > 0) {
    x.max <- ((max(Rock.tsne.data$Dim2)) %/% 10 + 1) * 10
    y.max <- x.max
  } else if (max(Rock.tsne.data$Dim2) < 0) {
    x.max <- ((max(Rock.tsne.data$Dim2)) %/% 10) * 10
    y.max <- x.max
  }
  
  if (min(Rock.tsne.data$Dim2) > 0) {
    x.min <- ((min(Rock.tsne.data$Dim2)) %/% 10 + 1) * 10
    y.min <- x.min
  } else if (min(Rock.tsne.data$Dim2) < 0) {
    x.min <- ((min(Rock.tsne.data$Dim2)) %/% 10) * 10
    y.min <- x.min
  }
  
} else if (Dim1.arr > Dim2.arr) {
  if (max(Rock.tsne.data$Dim1) > 0) {
    x.max <- ((max(Rock.tsne.data$Dim1)) %/% 10 + 1) * 10
    y.max <- x.max
  } else if (max(Rock.tsne.data$Dim1) < 0) {
    x.max <- ((max(Rock.tsne.data$Dim1)) %/% 10) * 10
    y.max <- x.max
  }
  
  if (min(Rock.tsne.data$Dim1) > 0) {
    x.min <- ((min(Rock.tsne.data$Dim1)) %/% 10 + 1) * 10
    y.min <- x.min
  } else if (min(Rock.tsne.data$Dim1) < 0) {
    x.min <- ((min(Rock.tsne.data$Dim1)) %/% 10) * 10
    y.min <- x.min
  }
}



tsne.plot <- ggplot(Rock.tsne.data) +
  geom_point(aes(
    x = Dim1,
    y = Dim2,
    fill = Subtype,
    shape = Subtype
  ), size = 2) +
  scale_shape_manual(values = set_shape) +
  scale_fill_manual(values = set_color) +
  xlim(x.min, x.max) + ylim(y.min, y.max) +
  theme_bw() +
  coord_fixed(ratio = 1) +
  xlab("t-SNE Dim 1") +
  ylab("t-SNE Dim 2") +
  theme(
    # legend.position = c(0.01, 0.2),
    legend.position = c(0.75, 0.15),
    legend.justification = c(0.01, 0.8),
    legend.key.size = unit(0.2, "cm"),
    legend.box.just = "right",
    legend.box = "horizontal",
    legend.box.background = element_rect(color = "grey20", size = 0.5),
    legend.title = element_blank(),
    #    legend.title = element_text(size = 8),
    legend.text = element_text(size = 8)
  ) +
  #  annotate(geom = "rect",xmin = -60, xmax = -50, ymin = 40, ymax = 50, fill="lightblue")
  annotate(
    geom = "rect",
    xmin = -40,
    xmax = -30,
    ymin = 50,
    ymax = 60,
    fill = "lightblue"
  )
plot(tsne.plot)
saveRDS(tsne.plot ,
        file = paste("t-SNE scatter plot", data1, data2, tSNE_method, ".rds"))

pdf(paste("t-SNE scatter plot", data1, data2, tSNE_method, ".pdf"),
    paper = "a4")
plot(tsne.plot)
dev.off()



variables <- names(Rock.tsne.data)[4:ncol(Rock.tsne.data)]

rm(p)
p <- lapply(variables, function(variable) {
  ggplot(Rock.tsne.data) +
    geom_point(aes(
      x = Dim1,
      y = Dim2,
      fill = eval(parse(text = variable)),
      col = eval(parse(text = variable)),
      shape = Subtype
    ),
    size = 0.7) +
    scale_color_gradientn(colors = set_color_gradient) +
    scale_fill_gradientn(colors = set_color_gradient) +
    scale_shape_manual(values = set_shape) +
    xlim(x.min, x.max) + ylim(y.min, y.max) +
    theme_bw() +
    
    geom_text(
      x = -25,
      y = 55,
      label = variable,
      size = 4
    ) +
    coord_fixed(ratio = 1) +
    theme(
      legend.position = "none",
      plot.title = element_text(hjust = 0.5, vjust = 1),
      axis.ticks = element_blank(),
      axis.title = element_blank(),
      axis.text = element_blank(),
      plot.margin = unit(c(0.1, 0, 0, 0), "cm"),
      legend.title = element_blank(),
      legend.key.size = unit(0.3, "cm"),
      legend.text = element_text(size = unit(0.5, "cm"))
    )
})


lay <- lay_new(t(matrix(1:40, nc = 8)))
if (data2 == "all elements") {
  lay <- lay_new(t(matrix(1:45, nc = 9)))
}

pdf(
  paste(
    "tSNE plot coloured by elemental concentrations",
    data1,
    data2,
    tSNE_method,
    ".pdf"
  ),
  paper = "a4",
  width = 0,
  height = 0
)
lay_grid(p[1:length(variables)], lay)
dev.off()

CairoSVG(
  paste(
    "tSNE plot coloured by elemental concentrations",
    data1,
    data2,
    tSNE_method
  ),
  width = 9,
  height = 12
)
lay_grid(p[1:length(variables)], lay)
dev.off()


P.region <-
  readRDS(
    "t-SNE scatter plot regions for R elements filtered by rfe and boruta multiple calculation .rds"
  ) +
  annotate(
    geom = "text",
    x = -55,
    y = 46,
    label = "a",
    size = 9
  )
P.crust <-
  readRDS(
    "t-SNE scatter plot crust for R elements filtered by rfe and boruta multiple calculation .rds"
  ) +
  annotate(
    geom = "text",
    x = -35,
    y = 55,
    label = "b",
    size = 9
  )


pdf("t-SNE scatter plot regions and crust.pdf",
    width = 12,
    height = 8)
P.region | P.crust
dev.off()

CairoSVG("t-SNE scatter plot regions and crust",
         width = 12,
         height = 8)
P.region | P.crust
dev.off()


print("t-SNE complete")


save.image(file = paste("all data of", data1, data2, tSNE_method, ".Rdata"))
load(file = paste("all data of", data1, data2, tSNE_method, ".Rdata"))
