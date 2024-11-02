rm(list = ls())
start_time <- proc.time()


library("FactoMineR")
library("factoextra")

library("devtools")
library("plyr")


library("ggplot2")
library("ggExtra")
library("corrplot")


library("rJava")
library("xlsx")
library("readxl")

library("ggrepel")
library("customLayout")
library("patchwork")
library("gtools")
library("gridExtra")
library(Cairo)








excel_name <- c("crust for R")  # without Japan
#excel_name <- c("regions for R") # with Japan

sheet_name <- c("imputed data by MICE and clr")
set_directory_name   <-
  c("PCAs output")
set_directory_location <-
  c("C:\\Users\\Administrator\\Desktop\\R adakite")
set_datafile_location <-
  paste0(set_directory_location,
         "\\Imputation and clr output\\",
         excel_name,
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
    #    "Mo",
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

if (length(grep("regions", excel_name)) == 1) {
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
if (length(grep("crust", excel_name)) == 1) {
  set_Level_Label   <-
    c("mature_crust", "juvenile_crust", "Cu-Au", "Cu-Mo")
  set_color =  c("#FFFF33", "#4DAF4A", "#FCCDE5", "#80B1D3")
  set_shape = c(21, 21, 24, 25)
}



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


FinalTab.pca <-
  PCA(FinalTab,
      scale.unit = TRUE,
      ncp = 7,
      graph = FALSE)

individual_coordinate = as.data.frame(FinalTab.pca$ind$coord)
individual_coordinate <- cbind(Rock.Data.class, individual_coordinate[, 1:3])

names(individual_coordinate) <- c("Subtype", "Dim.1", "Dim.2", "Dim.3")

varr = as.data.frame(FinalTab.pca$var$coord)
group = as.data.frame(Rock.Data.class)
colnames(group) = set_groups_colname
eigenvalue = as.data.frame(FinalTab.pca$eig)



TotalElement    <- colnames(Rock.Data)
TotalElement    <- Reduce(paste, TotalElement)

xlsx_name <- paste0(excel_name, " PCA result", ".xlsx")
write.xlsx2(individual_coordinate, xlsx_name, "samples coordinate")
write.xlsx2(eigenvalue,
            xlsx_name,
            "eigenvalue(explanation) for principle components",
            append = TRUE)
write.xlsx2(varr, xlsx_name, "Variances_New Corrod Sys", append = TRUE)
print("complete")




group$Subtype = factor(group$Subtype, levels = set_Level_Label)



PC1_Num <-
  eigenvalue[1, 2]
PC2_Num <-
  eigenvalue[2, 2]
PC3_Num <-
  eigenvalue[3, 2]



eigenvalue_rank <-
  fviz_eig(FinalTab.pca, addlabels = TRUE, ylim = c(0, 50)) +
  ggtitle("a     Scree plot")





Contributions_to_PC1 <-
  fviz_contrib(
    FinalTab.pca,
    choice = "var",
    axes = 1,
    top = length(Rock.Component)
  ) +
  ggtitle("b     Contributions of variables to PC1")
sorted_by_PC1 <- names(sort(FinalTab.pca$var$contrib[, 1], decreasing = TRUE))

Contributions_to_PC2 <-
  fviz_contrib(
    FinalTab.pca,
    choice = "var",
    axes = 2,
    top = length(Rock.Component)
  ) +
  scale_x_discrete(limits = sorted_by_PC1) +
  ggtitle("c     Contributions of variables to PC2")

Contributions_to_PC3 <-
  fviz_contrib(
    FinalTab.pca,
    choice = "var",
    axes = 3,
    top = length(Rock.Component)
  ) +
  scale_x_discrete(limits = sorted_by_PC1) +
  ggtitle("d     Contributions of variables to PC3")

Contributions_to_PC4 <-
  fviz_contrib(
    FinalTab.pca,
    choice = "var",
    axes = 4,
    top = length(Rock.Component)
  ) +
  scale_x_discrete(limits = sorted_by_PC1) +
  ggtitle("e     Contributions of variables to PC4")

lay1 <- lay_new(matrix(1:5, nc = 1))

pdf(
  paste0(
    excel_name,
    " eigenvalue_rank and contributions of variables to PC 1 2 3 4.pdf"
  ),
  width = 8,
  height = 16
)
lay_grid(
  list(
    eigenvalue_rank,
    Contributions_to_PC1,
    Contributions_to_PC2,
    Contributions_to_PC3,
    Contributions_to_PC4
  ),
  lay1
)
dev.off()

CairoSVG(
  paste0(
    excel_name,
    " eigenvalue_rank and contributions of variables to PC 1 2 3 4.svg"
  ),
  width = 8,
  height = 16
)
lay_grid(
  list(
    eigenvalue_rank,
    Contributions_to_PC1,
    Contributions_to_PC2,
    Contributions_to_PC3,
    Contributions_to_PC4
  ),
  lay1
)
dev.off()




angle       <- seq(-pi, pi, length = 100)
Circle_data <-
  data.frame(x = sin(angle), y = cos(angle))

vect1_2.length <- sqrt((varr$Dim.1) ^ 2 + (varr$Dim.2) ^ 2)
vect1_2 <- ggplot() +
  geom_path(aes(x, y), data = Circle_data, colour = "grey70") +
  geom_segment(
    varr,
    mapping = aes(
      x = 0,
      y = 0,
      xend = Dim.1,
      yend = Dim.2,
      color = vect1_2.length
    ),
    arrow = arrow(length = unit(0.25, 'picas')),
    size = 0.25
  ) +
  geom_text_repel(
    data = varr,
    aes(
      x = Dim.1,
      y = Dim.2,
      label = rownames(varr),
      color = vect1_2.length
    ),
    force = 0.1,
    size = 3
  ) +
  scale_color_gradientn(colors = set_color_vector, name = c("cos2")) +
  xlab(paste0("PC1 / ", format(PC1_Num, digits = 4), "%", sep = "")) +
  ylab(paste0("PC2 / ", format(PC2_Num, digits = 4), "%", sep = "")) +
  theme_bw() +
  theme(
    legend.position = c(0.01, 0.2),
    legend.justification = c(0.01, 0.8),
    legend.key.size = unit(0.15, "inches"),
    legend.box.just = "right",
    legend.box.background = element_rect(color = "grey20", size = 0.5),
    legend.margin = margin(3, 3, 3, 3),
    legend.title = element_text(size = 10),
    legend.text = element_text(size = 8)
  ) +
  
  annotate(
    geom = "rect",
    xmin = -1.1,
    xmax = -0.9,
    ymin = 0.9,
    ymax = 1.1,
    fill = "lightblue"
  ) +
  annotate(
    geom = "text",
    x = -Inf,
    y = Inf,
    label = "a",
    hjust = -0.6,
    vjust = 1.2,
    size = 8
  ) +
  geom_vline(xintercept = 0, size = 0.2) +
  geom_hline(yintercept = 0, size = 0.2) +
  coord_fixed(
    ratio = 1,
    xlim = c(-1.02, 1.02),
    ylim = c(-1.02, 1.02)
  )



vect1_3.length <- sqrt((varr$Dim.1) ^ 2 + (varr$Dim.3) ^ 2)
vect1_3 <- ggplot() +
  geom_path(aes(x, y), data = Circle_data, colour = "grey70") +
  geom_segment(
    varr,
    mapping = aes(
      x = 0,
      y = 0,
      xend = Dim.1,
      yend = Dim.3,
      color = vect1_3.length
    ),
    arrow = arrow(length = unit(0.25, 'picas')),
    size = 0.25
  ) +
  geom_text_repel(
    data = varr,
    aes(
      x = Dim.1,
      y = Dim.3,
      label = rownames(varr),
      color = vect1_3.length
    ),
    force = 0.1,
    size = 3
  ) +
  scale_color_gradientn(colors = set_color_vector, name = c("cos2")) +
  xlab(paste0("PC1 / ", format(PC1_Num, digits = 4), "%", sep = "")) +
  ylab(paste0("PC3 / ", format(PC3_Num, digits = 4), "%", sep = "")) +
  theme_bw() +
  theme(
    legend.position = c(0.01, 0.2),
    legend.justification = c(0.01, 0.8),
    legend.key.size = unit(0.15, "inches"),
    legend.box.just = "right",
    legend.box.background = element_rect(color = "grey20", size = 0.5),
    legend.margin = margin(3, 3, 3, 3),
    legend.title = element_text(size = 10),
    legend.text = element_text(size = 8)
  ) +
  annotate(
    geom = "rect",
    xmin = -1.1,
    xmax = -0.9,
    ymin = 0.9,
    ymax = 1.1,
    fill = "lightblue"
  ) +
  annotate(
    geom = "text",
    x = -Inf,
    y = Inf,
    label = "b",
    hjust = -0.55,
    vjust = 1.2,
    size = 8
  ) +
  geom_vline(xintercept = 0, size = 0.2) +
  geom_hline(yintercept = 0, size = 0.2) +
  coord_fixed(
    ratio = 1,
    xlim = c(-1.02, 1.02),
    ylim = c(-1.02, 1.02)
  )



pdf(paste0(excel_name, " PCA vector.pdf"),
    
    width = 10,
    height = 5)
lay2 <- lay_new(matrix(1:2, nc = 2))
lay_grid(list(vect1_2, vect1_3), lay2)
dev.off()



pc1_2_all.scatter <-
  ggplot(data = individual_coordinate, aes(
    x = Dim.1,
    y = Dim.2,
    fill = Subtype,
    shape = Subtype
  )) + #
  geom_point(aes(fill = Subtype, shape = Subtype), size = 2) +
  scale_fill_manual(values = set_color) +
  scale_shape_manual(values = set_shape) +
  labs(
    x = paste("PC1 / ", format(PC1_Num, digits = 4), "%", sep = ""),
    y = paste("PC2 / ", format(PC2_Num, digits = 4), "%", sep = "")
  ) +
  geom_vline(xintercept = 0, size = 0.2) +
  geom_hline(yintercept = 0, size = 0.2) +
  theme_bw() +
  theme(
    legend.position = "none",
    legend.justification = c(0.01, 0.8),
    legend.key.size = unit(0.25, "cm"),
    legend.box.just = "right",
    legend.box = "horizontal",
    legend.box.background = element_rect(color = "grey20", size = 0.5),
    legend.margin = margin(3, 3, 3, 3),
    legend.title = element_blank(),
    legend.text = element_text(size = 8)
  ) +
  annotate(
    geom = "rect",
    xmin = -14,
    xmax = -11.5,
    ymin = 8.8,
    ymax = 10.8,
    fill = "lightblue"
  ) +
  annotate(
    geom = "text",
    x = -Inf,
    y = Inf,
    label = "c",
    hjust = -0.55,
    vjust = 1.2,
    size = 8
  ) +
  coord_fixed(ratio = 1.25,
              xlim = c(-13, 13),
              ylim = c(-10, 10))
plot(pc1_2_all.scatter)



pc1_3_all.scatter <-
  ggplot(data = individual_coordinate, aes(
    x = Dim.1,
    y = Dim.3,
    fill = Subtype,
    shape = Subtype
  )) +
  geom_point(aes(fill = Subtype, shape = Subtype), size = 2) +
  scale_fill_manual(values = set_color) +
  scale_shape_manual(values = set_shape) +
  labs(
    x = paste("PC1 / ", format(PC1_Num, digits = 4), "%", sep = ""),
    y = paste("PC3 / ", format(PC3_Num, digits = 4), "%", sep = "")
  ) +
  geom_vline(xintercept = 0, size = 0.2) +
  geom_hline(yintercept = 0, size = 0.2) +
  theme_bw() +
  theme(
    legend.position = c(0.01, 0.285),
    legend.justification = c(0.01, 0.8),
    legend.key.size = unit(0.2, "cm"),
    legend.box.just = "right",
    legend.box = "horizontal",
    legend.box.background = element_rect(color = "grey20", size = 0.5),
    legend.margin = margin(3, 3, 3, 3),
    legend.title = element_blank(),
    legend.text = element_text(size = 8)
  ) +
  annotate(
    geom = "rect",
    xmin = -14,
    xmax = -11.5,
    ymin = 8.8,
    ymax = 10.8,
    fill = "lightblue"
  ) +
  annotate(
    geom = "text",
    x = -Inf,
    y = Inf,
    label = "d",
    hjust = -0.55,
    vjust = 1.2,
    size = 8
  ) +
  coord_fixed(ratio = 1.25,
              xlim = c(-13, 13),
              ylim = c(-10, 10))
plot(pc1_3_all.scatter)



set_Level_Label_deposit   <-
  c("Cu-Au", "Cu-Mo")
set_Level_Label_juvenile   <-
  c("North_N_America", "SW_Pacific", "W_CAOB")
set_Level_Label_mature   <-
  c("Central_S_America", "Tethys_collision", "N_China")
set_color_deposit =  c("#FFFF33", "#4DAF4A")
set_color_juvenile =  c("#FDB462", "#FCCDE5", "#B3DE69")
set_color_juvenile =  c("#FDB462", "#FCCDE5", "#B3DE69")
set_color_mature =  c("#BC80BD", "#8DD3C7", "#80B1D3")
set_shape_deposit = c(21, 21)
set_shape_juvenile = c(24, 24, 24)
set_shape_mature = c(25, 25, 25)



type_col_numb_coordinate <-
  which(colnames(individual_coordinate) == set_groups_colname)

selected.groups_deposit  <- data.frame()
for (i in (1:length(set_Level_Label_deposit)))
{
  temp_individual_coordinate_deposit          <-
    individual_coordinate[which(individual_coordinate[, type_col_numb_coordinate] == set_Level_Label_deposit[i]), ]
  selected.groups_deposit         <-
    rbind(selected.groups_deposit,
          temp_individual_coordinate_deposit)
}
individual_coordinate_deposit <- selected.groups_deposit


selected.groups_juvenile  <- data.frame()
for (i in (1:length(set_Level_Label_juvenile)))
{
  temp_individual_coordinate_juvenile          <-
    individual_coordinate[which(individual_coordinate[, type_col_numb_coordinate] == set_Level_Label_juvenile[i]), ]
  selected.groups_juvenile         <-
    rbind(selected.groups_juvenile,
          temp_individual_coordinate_juvenile)
}
individual_coordinate_juvenile <- selected.groups_juvenile


selected.groups_mature  <- data.frame()
for (i in (1:length(set_Level_Label_mature)))
{
  temp_individual_coordinate_mature          <-
    individual_coordinate[which(individual_coordinate[, type_col_numb_coordinate] == set_Level_Label_mature[i]), ]
  selected.groups_mature         <-
    rbind(selected.groups_mature, temp_individual_coordinate_mature)
}
individual_coordinate_mature <- selected.groups_mature



pc1_2_juvenile.scatter <-
  ggplot() +
  geom_point(
    data = individual_coordinate_juvenile,
    mapping = aes(
      x = Dim.1,
      y = Dim.2,
      fill = Subtype,
      shape = Subtype
    ),
    size = 2.5
  ) +
  scale_fill_manual(values = set_color_juvenile) +
  scale_shape_manual(values = set_shape_juvenile) +
  labs(
    x = paste("PC1 / ", format(PC1_Num, digits = 4), "%", sep = ""),
    y = paste("PC2 / ", format(PC2_Num, digits = 4), "%", sep = "")
  ) +
  geom_vline(xintercept = 0, size = 0.2) +
  geom_hline(yintercept = 0, size = 0.2) +
  theme_bw() +
  theme(
    legend.position = "none",
    legend.justification = c(0.01, 0.8),
    legend.key.size = unit(0.4, "cm"),
    legend.box.just = "right",
    legend.box = "horizontal",
    legend.box.background = element_rect(color = "grey20", size = 0.5),
    legend.margin = margin(3, 3, 3, 3),
    legend.title = element_blank(),
    legend.text = element_text(size = 8)
  ) +
  annotate(
    geom = "rect",
    xmin = -14,
    xmax = -11.5,
    ymin = 8.8,
    ymax = 10.8,
    fill = "lightblue"
  ) +
  annotate(
    geom = "text",
    x = -Inf,
    y = Inf,
    label = "e",
    hjust = -0.55,
    vjust = 1.2,
    size = 8
  ) +
  coord_fixed(ratio = 1.25,
              xlim = c(-13, 13),
              ylim = c(-10, 10))

pc1_2_mature.scatter <-
  ggplot() +
  geom_point(
    data = individual_coordinate_mature,
    mapping = aes(
      x = Dim.1,
      y = Dim.2,
      fill = Subtype,
      shape = Subtype
    ),
    size = 2.5
  ) +
  scale_fill_manual(values = set_color_mature) +
  scale_shape_manual(values = set_shape_mature) + #״
  labs(
    x = paste("PC1 / ", format(PC1_Num, digits = 4), "%", sep = ""),
    y = paste("PC2 / ", format(PC2_Num, digits = 4), "%", sep = "")
  ) +
  geom_vline(xintercept = 0, size = 0.2) +
  geom_hline(yintercept = 0, size = 0.2) +
  theme_bw() +
  theme(
    legend.position = "none",
    legend.justification = c(0.01, 0.8),
    legend.key.size = unit(0.4, "cm"),
    legend.box.just = "right",
    legend.box = "horizontal",
    legend.box.background = element_rect(color = "grey20", size = 0.5),
    legend.margin = margin(3, 3, 3, 3),
    legend.title = element_blank(),
    legend.text = element_text(size = 8)
  ) +
  annotate(
    geom = "rect",
    xmin = -14,
    xmax = -11.5,
    ymin = 8.8,
    ymax = 10.8,
    fill = "lightblue"
  ) +
  annotate(
    geom = "text",
    x = -Inf,
    y = Inf,
    label = "g",
    hjust = -0.55,
    vjust = 1.2,
    size = 8
  ) +
  coord_fixed(ratio = 1.25,
              xlim = c(-13, 13),
              ylim = c(-10, 10))


pc1_3_juvenile.scatter <-
  ggplot() +
  geom_point(
    data = individual_coordinate_juvenile,
    mapping = aes(
      x = Dim.1,
      y = Dim.3,
      fill = Subtype,
      shape = Subtype
    ),
    size = 2.5
  ) +
  scale_fill_manual(values = set_color_juvenile) +
  scale_shape_manual(values = set_shape_juvenile) +
  labs(
    x = paste("PC1 / ", format(PC1_Num, digits = 4), "%", sep = ""),
    y = paste("PC3 / ", format(PC3_Num, digits = 4), "%", sep = "")
  ) +
  geom_vline(xintercept = 0, size = 0.2) +
  geom_hline(yintercept = 0, size = 0.2) +
  theme_bw() +
  theme(
    legend.position = c(0.01, 0.2),
    legend.justification = c(0.01, 0.8),
    legend.key.size = unit(0.1, "cm"),
    legend.box.just = "right",
    legend.box = "horizontal",
    legend.box.background = element_rect(color = "grey20", size = 0.5),
    legend.margin = margin(3, 3, 3, 3),
    legend.title = element_blank(),
    legend.text = element_text(size = 7)
  ) +
  annotate(
    geom = "rect",
    xmin = -14,
    xmax = -11.5,
    ymin = 8.8,
    ymax = 10.8,
    fill = "lightblue"
  ) +
  annotate(
    geom = "text",
    x = -Inf,
    y = Inf,
    label = "f",
    hjust = -0.55,
    vjust = 1.17,
    size = 8
  ) +
  coord_fixed(ratio = 1.25,
              xlim = c(-13, 13),
              ylim = c(-10, 10))


pc1_3_mature.scatter <-
  ggplot() +
  geom_point(
    data = individual_coordinate_mature,
    mapping = aes(
      x = Dim.1,
      y = Dim.3,
      fill = Subtype,
      shape = Subtype
    ),
    size = 2.5
  ) +
  scale_fill_manual(values = set_color_mature) +
  scale_shape_manual(values = set_shape_mature) + #״
  labs(
    x = paste("PC1 / ", format(PC1_Num, digits = 4), "%", sep = ""),
    y = paste("PC3 / ", format(PC3_Num, digits = 4), "%", sep = "")
  ) +
  geom_vline(xintercept = 0, size = 0.2) +
  geom_hline(yintercept = 0, size = 0.2) +
  theme_bw() +
  theme(
    legend.position = c(0.01, 0.2),
    legend.justification = c(0.01, 0.8),
    legend.key.size = unit(0.1, "cm"),
    legend.box.just = "right",
    legend.box = "horizontal",
    legend.box.background = element_rect(color = "grey20", size = 0.5),
    legend.margin = margin(3, 3, 3, 3),
    legend.title = element_blank(),
    legend.text = element_text(size = 7)
  ) +
  annotate(
    geom = "rect",
    xmin = -14,
    xmax = -11.5,
    ymin = 8.8,
    ymax = 10.8,
    fill = "lightblue"
  ) +
  annotate(
    geom = "text",
    x = -Inf,
    y = Inf,
    label = "h",
    hjust = -0.55,
    vjust = 1.17,
    size = 8
  ) +
  coord_fixed(ratio = 1.25,
              xlim = c(-13, 13),
              ylim = c(-10, 10))






pc1_all.scatter_deposit_top <- ggplot() +
  geom_density(data = individual_coordinate_deposit,
               aes(Dim.1, fill = Subtype, colour = Subtype),
               alpha = 0.7) +
  scale_fill_manual(values = set_color_deposit) +
  scale_color_manual(values = set_color_deposit) +
  xlim(-13, 13) +
  theme(legend.position = "none") +
  theme(
    axis.title = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank()
  )
pc2_all.scatter_deposit_right <- ggplot() +
  geom_density(data = individual_coordinate_deposit,
               aes(Dim.2, fill = Subtype, colour = Subtype),
               alpha = 0.7) +
  scale_fill_manual(values = set_color_deposit) +
  scale_color_manual(values = set_color_deposit) +
  xlim(-10, 10) +
  coord_flip() +
  theme(legend.position = "none") +
  theme(
    axis.title  = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank()
  )
empty <- ggplot() +
  theme(
    panel.background = element_blank(),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks = element_blank()
  )

pc1_2_all.scatter.deposit.density <-
  grid.arrange(
    pc1_all.scatter_deposit_top,
    empty,
    pc1_2_all.scatter,
    pc2_all.scatter_deposit_right,
    ncol = 2,
    nrow = 2,
    widths = c(4, 1),
    heights = c(1, 4)
  )
plot(pc1_2_all.scatter.deposit.density)

pc3_all.scatter_deposit_right <- ggplot() +
  geom_density(data = individual_coordinate_deposit,
               aes(Dim.3, fill = Subtype, colour = Subtype),
               alpha = 0.7) +
  scale_fill_manual(values = set_color_deposit) +
  scale_color_manual(values = set_color_deposit) +
  xlim(-10, 10) +
  coord_flip() +
  theme(legend.position = "none") +
  theme(
    axis.title  = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank()
  )

pc1_3_all.scatter.deposit.density <-
  grid.arrange(
    pc1_all.scatter_deposit_top,
    empty,
    pc1_3_all.scatter,
    pc3_all.scatter_deposit_right,
    ncol = 2,
    nrow = 2,
    widths = c(4, 1),
    heights = c(1, 4)
  )
plot(pc1_3_all.scatter.deposit.density)

pc1.scatter_juvenile_top <- ggplot() +
  geom_density(data = individual_coordinate_juvenile,
               aes(Dim.1, fill = Subtype, colour = Subtype),
               alpha = 0.7) +
  scale_fill_manual(values = set_color_juvenile) +
  scale_color_manual(values = set_color_juvenile) +
  xlim(-13, 13) +
  theme(legend.position = "none") +
  theme(
    axis.title = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank()
  )
pc2.scatter_juvenile_right <- ggplot() +
  geom_density(data = individual_coordinate_juvenile,
               aes(Dim.2, fill = Subtype, colour = Subtype),
               alpha = 0.7) +
  scale_fill_manual(values = set_color_juvenile) +
  scale_color_manual(values = set_color_juvenile) +
  xlim(-10, 10) +
  coord_flip() +
  theme(legend.position = "none") +
  theme(
    axis.title  = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank()
  )
pc1_2.scatter.juvenile.density <-
  grid.arrange(
    pc1.scatter_juvenile_top,
    empty,
    pc1_2_juvenile.scatter,
    pc2.scatter_juvenile_right,
    ncol = 2,
    nrow = 2,
    widths = c(4, 1),
    heights = c(1, 4)
  )
plot(pc1_2.scatter.juvenile.density)


pc3.scatter_juvenile_right <- ggplot() +
  geom_density(data = individual_coordinate_juvenile,
               aes(Dim.3, fill = Subtype, colour = Subtype),
               alpha = 0.7) +
  scale_fill_manual(values = set_color_juvenile) +
  scale_color_manual(values = set_color_juvenile) +
  xlim(-10, 10) +
  coord_flip() +
  theme(legend.position = "none") +
  theme(
    axis.title  = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank()
  )
pc1_3.scatter.juvenile.density <-
  grid.arrange(
    pc1.scatter_juvenile_top,
    empty,
    pc1_3_juvenile.scatter,
    pc3.scatter_juvenile_right,
    ncol = 2,
    nrow = 2,
    widths = c(4, 1),
    heights = c(1, 4)
  )
plot(pc1_3.scatter.juvenile.density)


pc1.scatter_mature_top <- ggplot() +
  geom_density(data = individual_coordinate_mature,
               aes(Dim.1, fill = Subtype, colour = Subtype),
               alpha = 0.7) +
  scale_fill_manual(values = set_color_mature) +
  scale_color_manual(values = set_color_mature) +
  xlim(-13, 13) +
  theme(legend.position = "none") +
  theme(
    axis.title = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank()
  )
pc2.scatter_mature_right <- ggplot() +
  geom_density(data = individual_coordinate_mature,
               aes(Dim.2, fill = Subtype, colour = Subtype),
               alpha = 0.7) +
  scale_fill_manual(values = set_color_mature) +
  scale_color_manual(values = set_color_mature) +
  xlim(-10, 10) +
  coord_flip() +
  theme(legend.position = "none") +
  theme(
    axis.title  = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank()
  )
pc1_2.scatter.mature.density <-
  grid.arrange(
    pc1.scatter_mature_top,
    empty,
    pc1_2_mature.scatter,
    pc2.scatter_mature_right,
    ncol = 2,
    nrow = 2,
    widths = c(4, 1),
    heights = c(1, 4)
  )
plot(pc1_2.scatter.mature.density)


pc3.scatter_mature_right <- ggplot() +
  geom_density(data = individual_coordinate_mature,
               aes(Dim.3, fill = Subtype, colour = Subtype),
               alpha = 0.7) +
  scale_fill_manual(values = set_color_mature) +
  scale_color_manual(values = set_color_mature) +
  xlim(-10, 10) +
  coord_flip() +
  theme(legend.position = "none") +
  theme(
    axis.title  = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank()
  )
pc1_3.scatter.mature.density <-
  grid.arrange(
    pc1.scatter_mature_top,
    empty,
    pc1_3_mature.scatter,
    pc3.scatter_mature_right,
    ncol = 2,
    nrow = 2,
    widths = c(4, 1),
    heights = c(1, 4)
  )
plot(pc1_3.scatter.mature.density)

pdf(paste0(excel_name, " PCA_layout.pdf"),
    width = 10,
    height = 20)
vect1_2 + vect1_3 + pc1_2_all.scatter.deposit.density + pc1_3_all.scatter.deposit.density + pc1_2.scatter.juvenile.density + pc1_3.scatter.juvenile.density + pc1_2.scatter.mature.density + pc1_3.scatter.mature.density +  plot_layout(ncol =
                                                                                                                                                                                                                                           2)
dev.off()

CairoSVG(paste0(excel_name, " PCA_layout.svg"),
         width = 10,
         height = 20)
vect1_2 + vect1_3 + pc1_2_all.scatter.deposit.density + pc1_3_all.scatter.deposit.density + pc1_2.scatter.juvenile.density + pc1_3.scatter.juvenile.density + pc1_2.scatter.mature.density + pc1_3.scatter.mature.density +  plot_layout(ncol =
                                                                                                                                                                                                                                           2)
dev.off()


end_time <- proc.time()
print(paste("PCA complete after", (end_time - start_time)[3], "seconds"))
