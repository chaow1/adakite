#### Treeheatr anylysis ####



#### Package ####

install.packages("treeheatr")
install.packages("sampling")
install.packages("ggplot2")
install.packages("rJava")
install.packages("xlsx")
install.packages("readxl")

library(treeheatr)
library(sampling)
library(ggplot2)
library(rJava)
library(xlsx)
library(readxl)

rm(list = ls())



#data1 <- c("original data")
data1 <- c("imputed data")

# classification <- c("only regions")
classification <- c("only Cu-Mo-Au")
# classification <- c("only crust")
#classification <- c("Cu-Mo-Au and crust")


set_directory_name <-
  c("Treeheatr output")
set_directory_location <-
  c("C:\\Users\\Administrator\\Desktop\\R adakite")

if (data1 == c("original data")) {
  excel_name = c("adakite 2021-5-1 for R")
  set_datafile_location =
    paste0(set_directory_location, "\\", excel_name, ".xlsx")
  sheet_name = c("crust for R")
  if (classification == ("only regions")) {
    sheet_name = c("regions for R")
  }
  if (classification == ("only Cu-Mo-Au")) {
    sheet_name = c("regions for R")
  }
}
if (data1 == c("imputed data")) {
  sheet_name = c("imputed data by MICE")
  excel_name = c("crust for R")
  set_datafile_location =
    paste0(set_directory_location,
           "\\Imputation output\\",
           excel_name,
           ".xlsx")
  if (classification == ("only regions")) {
    excel_name = c("regions for R")
  }
  if (classification == ("only Cu-Mo-Au")) {
    excel_name = c("regions for R")
  }
}

# set plot parameters
if (classification == c("only Cu-Mo-Au")) {
  set_width = c(20)
  set_height = c(10)
  set_lev_fac = c(0.5)
} else if (classification == c("only crust")) {
  set_width = c(20)
  set_height = c(10)
  set_lev_fac = c(1)
} else if (classification == c("Cu-Mo-Au and crust")) {
  set_width = c(30)
  set_height = c(15)
  set_lev_fac = c(1.3)
} else if (classification == c("only regions")) {
  set_width = c(30)
  set_height = c(15)
  set_lev_fac = c(0.5)
}


setwd(set_directory_location)
dir.create(set_directory_name)
setwd(set_directory_name)




if (length(grep("only regions", classification)) == 1) {
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
  color =  c(
    "#BEBADA",
    "#FFFF33",
    "#4DAF4A",
    #             "#BC80BD",
    "#FDB462",
    "#8DD3C7",
    "#FCCDE5",
    "#80B1D3",
    "#B3DE69"
  )
}
if (length(grep("Cu-Mo-Au and crust", classification)) == 1) {
  set_class_level   <-
    c("mature_crust",
      "juvenile_crust",
      "Cu-Au",
      "Cu-Mo")
  color =  c("#80B1D3",
             "#FCCDE5",
             "#FFFF33",
             "#4DAF4A")
}
if (length(grep("only crust", classification)) == 1) {
  set_class_level   <-
    c("mature_crust",
      "juvenile_crust")
  color =  c("#80B1D3",
             "#FCCDE5")
}
if (length(grep("only Cu-Mo-Au", classification)) == 1) {
  set_class_level   <-
    c("Cu-Au",
      "Cu-Mo")
  color =  c("#FFFF33",
             "#4DAF4A")
}

# set the elements needed to be analyse
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
    "Tdm",
    "Dim.1",
    "Dim.2"
  )
SamplingMethod <- c("Strata")                # Strata or Overall


Rock.Component <- read_excel(set_datafile_location, sheet = sheet_name)
Rock.Component.colname  <- colnames(Rock.Component)
col_NO <- grep("Subtype", Rock.Component.colname)

selected.groups  <- data.frame()
for (i in (1:length(set_class_level)))
{
  temp_Rock.Component          <-
    Rock.Component[which(Rock.Component[, col_NO] == set_class_level[i]), ]
  selected.groups         <-
    rbind(selected.groups, temp_Rock.Component)
}
Rock.Component <- selected.groups



Rock.Component.depth    <-
  dim(Rock.Component)[1]
Rock.Data <-
  as.data.frame(1:Rock.Component.depth)
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
      paste0(
        "[",
        Check.Num,
        "]",
        i,
        "-This element are not defined in elements_gather, please check."
      )
    )
    Check.Num  <- Check.Num + 1
  }
}
print("================================================")
print("Check Finished.")
Sys.sleep(0.5)
View(Rock.Data)
Rock.Data <- Rock.Data[,-1]

Rock.Component  <- cbind(Rock.Component[, col_NO], Rock.Data)
View(Rock.Component)


class_level <- unique(Rock.Component$Subtype)
Rock.Component$Subtype = factor(Rock.Component$Subtype, levels = class_level)


tempNum <- NA

#### Sampling the data ####
set.seed(1)

if (SamplingMethod == "Strata") {
  NumType <- length(unique(Rock.Component$Subtype))
  for (i in (1:NumType)) {
    tempGpLeng <-
      length(which(Rock.Component$Subtype == class_level[i]))
    tempNumSam <-
      round(0.7 * tempGpLeng)
    tempNum <- append(tempNum, tempNumSam)
  }
  
  NumSampling <- tempNum[-1]
  trainrn <- strata(
    Rock.Component,
    stratanames = ("Subtype"),
    size = NumSampling,
    method = "srswor"
  )
  
  traindb <- Rock.Component[as.numeric(rownames(trainrn)), ]
  testsdb <- Rock.Component[-as.numeric(rownames(trainrn)), ]
  
  
} else if (SamplingMethod == "Overall") {
  trainrn <-
    sample(nrow(Rock.Component), 3 / 4 * nrow(Rock.Component))
  traindb <- Rock.Component[trainrn, ]
  testsdb <- Rock.Component[-trainrn, ]
} else{
  print("Not Defined Sampling Method, Check SamplingMethod please")
}


#### heat_tree_plot ####

heat_tree_plot <-
  heat_tree(
    traindb,
    data_test = testsdb,
    target_lab = "Subtype",
    target_cols = color,
    par_node_vars = list(
      label.size = 0.1,
      label.col = "white",
      label.padding = ggplot2::unit(0.1, 'lines'),
      line_list = list(ggplot2::aes(label = splitvar),
                       ggplot2::aes(label = paste(
                         'p =', formatC(p.value, format = 'e', digits = 1)
                       ))),
      line_gpar = list(list(size = 10),
                       list(size = 10)),
      id = 'inner'
    ),
    
    
    show = "heat-tree",
    target_legend = TRUE,
    cont_legend = TRUE,
    cate_legend = TRUE,
    panel_space = 0.001,
    lev_fac = set_lev_fac,
    print_eval = TRUE
  )

pdf(
  paste0(classification, " ", data1, " ", Reduce(paste, colnames(Rock.Component)), ".pdf"),
  width = set_width,
  height = set_height
)
plot(heat_tree_plot)
dev.off()




if (classification == "only Cu-Mo-Au") {
  heat_tree_plot <-
    heat_tree(
      traindb,
      data_test = testsdb,
      target_lab = "Subtype",
      target_cols = color,
      par_node_vars = list(
        label.size = 0.1,
        label.col = "white",
        label.padding = ggplot2::unit(0.1, 'lines'),
        line_list = list(ggplot2::aes(label = splitvar),
                         ggplot2::aes(label = paste(
                           'p =', formatC(p.value, format = 'e', digits = 1)
                         ))),
        line_gpar = list(list(size = 10),
                         list(size = 10)),
        id = 'inner'
      ),
      show = "heat-tree",
      target_legend = TRUE,
      cont_legend = TRUE,
      cate_legend = TRUE,
      panel_space = 0.001,
      print_eval = TRUE
    )
}
if (classification == "only crust") {
  heat_tree_plot <-
    heat_tree(
      traindb,
      data_test = testsdb,
      target_lab = "Subtype",
      target_cols = color,
      par_node_vars = list(
        label.size = 0.1,
        label.col = "white",
        label.padding = ggplot2::unit(0.1, 'lines'),
        line_list = list(ggplot2::aes(label = splitvar),
                         ggplot2::aes(label = paste(
                           'p =', formatC(p.value, format = 'e', digits = 1)
                         ))),
        line_gpar = list(list(size = 10),
                         list(size = 10)),
        id = 'inner'
      ),
      
      show = "heat-tree",
      target_legend = TRUE,
      cont_legend = TRUE,
      cate_legend = TRUE,
      panel_space = 0.001,
      lev_fac = 1,
      print_eval = TRUE
    )
}
if (classification == "Cu-Mo-Au and crust") {
  heat_tree_plot <-
    heat_tree(
      traindb,
      data_test = testsdb,
      target_lab = "Subtype",
      target_cols = color,
      par_node_vars = list(
        label.size = 0.1,
        label.col = "white",
        label.padding = ggplot2::unit(0.1, 'lines'),
        line_list = list(ggplot2::aes(label = splitvar),
                         ggplot2::aes(label = paste(
                           'p =', formatC(p.value, format = 'e', digits = 1)
                         ))),
        line_gpar = list(list(size = 10),
                         list(size = 10)),
        id = 'inner'
      ),
      
      show = "heat-tree",
      target_legend = TRUE,
      cont_legend = TRUE,
      cate_legend = TRUE,
      panel_space = 0.001,
      lev_fac = 1.3,
      print_eval = TRUE
    )
  pdf(
    paste0(
      classification,
      " ",
      data1,
      " ",
      Reduce(paste, colnames(Rock.Component)),
      ".pdf"
    ),
    width = 30,
    height = 15
  )
  plot(heat_tree_plot)
  dev.off()
}
if (classification == "only regions") {
  heat_tree_plot <-
    heat_tree(
      traindb,
      data_test = testsdb,
      target_lab = "Subtype",
      target_cols = color,
      par_node_vars = list(
        label.size = 0.1,
        label.col = "white",
        label.padding = ggplot2::unit(0.1, 'lines'),
        line_list = list(ggplot2::aes(label = splitvar),
                         ggplot2::aes(label = paste(
                           'p =', formatC(p.value, format = 'e', digits = 1)
                         ))),
        line_gpar = list(list(size = 10),
                         list(size = 10)),
        id = 'inner'
      ),
      show = "heat-tree",
      target_legend = TRUE,
      cont_legend = TRUE,
      cate_legend = TRUE,
      panel_space = 0.001,
      lev_fac = 0.5,
      print_eval = TRUE
    )
  
  heat_tree_plot
  dev.off() 