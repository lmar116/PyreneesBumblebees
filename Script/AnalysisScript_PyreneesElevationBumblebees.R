

########################################################################################'
##                                                                                    ##
##                          Full Analysis Script                                      ##
##                          Pyrenees Bumblebees 115 Years                             ##
##                          Author: Leon Marshall                                     ##
##                          Article DOI: 10.1098/rspb.2020.2201                       ##                    
##                                                                                    ##
########################################################################################'


#Contents
#1. Packages
#2. Functions
#3. Session Info
#4. Bumblebee Analysis
#5. Plant Analysis
#6. Individual Plant Graphs
#8. Plant-pollinator elevation shifts
#9. Climate Range Shifts
#10. Climate Changes
#11. Land Use Changes
#12. Rarefaction - Suppl. Material

#_______________________________________________________________________________________

#    --------------------------------- 1. Packages ---------------------------------

#_______________________________________________________________________________________
library(plyr) #Version 1.8.6
library(reshape2) #Version 1.4.4
library(ggplot2) #Version 3.3.2
library(scales) #Version 1.1.1
library(dplyr) #Version 1.0.0
library(forcats) #Version 0.5.0
library(ggrepel) #Version 0.8.2
library(ggpmisc) #Version 0.3.5
library(ggpubr) #Version 0.4.0
library(ggsci) #Version 2.9
library(showtext) #Version 0.8-1
library(rstatix) #Version 0.6.0
library(MASS)  #Version 7.3-51.5
library(iNEXT) #Version 2.0.20
#========================================================================================'


#_______________________________________________________________________________________

#    --------------------------------- 2. Functions ---------------------------------

#_______________________________________________________________________________________

#function to calculate standard error
st.err <- function(x) {sd(x) / sqrt(length(x))}
#========================================================================================'


#_______________________________________________________________________________________

#    -------------------------------  3. Session Info  --------------------------

#_______________________________________________________________________________________

#========================================================================================'

# R version 4.0.0 (2020-04-24)
# Platform: x86_64-w64-mingw32/x64 (64-bit)
# Running under: Windows 10 x64 (build 19041)
#
# Matrix products: default
#
# locale:
#   [1] LC_COLLATE=English_United Kingdom.1252  LC_CTYPE=English_United Kingdom.1252
# [3] LC_MONETARY=English_United Kingdom.1252 LC_NUMERIC=C
# [5] LC_TIME=English_United Kingdom.1252
#
# attached base packages:
#   [1] stats     graphics  grDevices utils     datasets  methods   base
#
# other attached packages:
#   [1] data.table_1.12.8 iNEXT_2.0.20      rstatix_0.6.0     showtext_0.8-1    showtextdb_3.0
# [6] sysfonts_0.8.1    ggsci_2.9         ggpubr_0.4.0      ggpmisc_0.3.5     ggrepel_0.8.2
# [11] forcats_0.5.0     dplyr_1.0.0       scales_1.1.1      ggplot2_3.3.2     reshape2_1.4.4
# [16] plyr_1.8.6        MASS_7.3-51.5
#
# loaded via a namespace (and not attached):
#   [1] tinytex_0.25     tidyselect_1.1.0 xfun_0.15        purrr_0.3.4      splines_4.0.0
# [6] haven_2.3.1      lattice_0.20-41  carData_3.0-4    colorspace_1.4-1 vctrs_0.3.1
# [11] generics_0.0.2   mgcv_1.8-31      rlang_0.4.6      pillar_1.4.4     foreign_0.8-78
# [16] glue_1.4.1       withr_2.2.0      sp_1.4-2         readxl_1.3.1     lifecycle_0.2.0
# [21] stringr_1.4.0    munsell_0.5.0    ggsignif_0.6.0   gtable_0.3.0     cellranger_1.1.0
# [26] raster_3.1-5     zip_2.0.4        codetools_0.2-16 labeling_0.3     rio_0.5.16
# [31] curl_4.3         broom_0.5.6      Rcpp_1.0.4.6     polynom_1.4-0    backports_1.1.7
# [36] abind_1.4-5      farver_2.0.3     digest_0.6.25    hms_0.5.3        stringi_1.4.6
# [41] openxlsx_4.1.5   grid_4.0.0       tools_4.0.0      magrittr_1.5     tibble_3.0.1
# [46] crayon_1.3.4     tidyr_1.1.0      car_3.0-8        pkgconfig_2.0.3  Matrix_1.2-18
# [51] ellipsis_0.3.1   rstudioapi_0.11  R6_2.4.1         nlme_3.1-147     compiler_4.0.0

#_______________________________________________________________________________________

#    -------------------------------  4. Bumblebee Analysis  --------------------------

#_______________________________________________________________________________________
#load bumblebee data
bbees_file <-
  "http://raw.github.com/lmar116/PyreneesBumblebees/master/Data/Bumblebee_data_Marshall_et_al_2020.csv"
bbees <- read.csv(bbees_file, header = T)


#Random Re-sampling of Elevation Shifts

randsamptab = data.frame()#create empty data.frame to fill during loop
diff_TAB = data.frame()#create empty data.frame to fill during loop


for (i in 1:1000) {
  #Subset surveys
  bbees1889 <- subset(bbees, Data == "Survey1889")
  bbees0506 <- subset(bbees, Data == "Survey2005-06")
  
  #Subset each elevation zone
  E1000_1200_89 <- subset(bbees1889, EleZone == "1000-1200")
  E1000_1200_05 <- subset(bbees0506, EleZone == "1000-1200")
  E1200_1400_89 <- subset(bbees1889, EleZone == "1200-1400")
  E1200_1400_05 <- subset(bbees0506, EleZone == "1200-1400")
  E1400_1600_89 <- subset(bbees1889, EleZone == "1400-1600")
  E1400_1600_05 <- subset(bbees0506, EleZone == "1400-1600")
  E1600_1800_89 <- subset(bbees1889, EleZone == "1600-1800")
  E1600_1800_05 <- subset(bbees0506, EleZone == "1600-1800")
  E1800_2000_89 <- subset(bbees1889, EleZone == "1800-2000")
  E1800_2000_05 <- subset(bbees0506, EleZone == "1800-2000")
  E2000_2200_89 <- subset(bbees1889, EleZone == "2000-2200")
  E2000_2200_05 <- subset(bbees0506, EleZone == "2000-2200")
  
  #Compare sampling and return minimum
  E1000_1200_s <- min(nrow(E1000_1200_89), nrow(E1000_1200_05))
  E1200_1400_s <- min(nrow(E1200_1400_89), nrow(E1200_1400_05))
  E1400_1600_s <- min(nrow(E1400_1600_89), nrow(E1400_1600_05))
  E1600_1800_s <- min(nrow(E1600_1800_89), nrow(E1600_1800_05))
  E1800_2000_s <- min(nrow(E1800_2000_89), nrow(E1800_2000_05))
  E2000_2200_s <- min(nrow(E2000_2200_89), nrow(E2000_2200_05))
  
  #Resample each elvation for each period with lower sampling value
  E1000_1200_89 <-
    E1000_1200_89[sample(nrow(E1000_1200_89), E1000_1200_s), ]
  E1000_1200_05 <-
    E1000_1200_05[sample(nrow(E1000_1200_05), E1000_1200_s), ]
  E1200_1400_89 <-
    E1200_1400_89[sample(nrow(E1200_1400_89), E1200_1400_s), ]
  E1200_1400_05 <-
    E1200_1400_05[sample(nrow(E1200_1400_05), E1200_1400_s), ]
  E1400_1600_89 <-
    E1400_1600_89[sample(nrow(E1400_1600_89), E1400_1600_s), ]
  E1400_1600_05 <-
    E1400_1600_05[sample(nrow(E1400_1600_05), E1400_1600_s), ]
  E1600_1800_89 <-
    E1600_1800_89[sample(nrow(E1600_1800_89), E1600_1800_s), ]
  E1600_1800_05 <-
    E1600_1800_05[sample(nrow(E1600_1800_05), E1600_1800_s), ]
  E1800_2000_89 <-
    E1800_2000_89[sample(nrow(E1800_2000_89), E1800_2000_s), ]
  E1800_2000_05 <-
    E1800_2000_05[sample(nrow(E1800_2000_05), E1800_2000_s), ]
  E2000_2200_89 <-
    E2000_2200_89[sample(nrow(E2000_2200_89), E2000_2200_s), ]
  E2000_2200_05 <-
    E2000_2200_05[sample(nrow(E2000_2200_05), E2000_2200_s), ]
  
  #Join new tables
  pyr_bbees <- rbind(
    E1000_1200_05,
    E1200_1400_05,
    E1400_1600_05,
    E1600_1800_05,
    E1800_2000_05,
    E2000_2200_05,
    E1000_1200_89,
    E1200_1400_89,
    E1400_1600_89,
    E1600_1800_89,
    E1800_2000_89,
    E2000_2200_89
  )
  
  #Calculate summary values
  aggmeancol <- aggregate(
    pyr_bbees$MaxEle,
    by = list(pyr_bbees$Visitor, pyr_bbees$Data),
    FUN = mean
  )
  
  aggSEcol <- aggregate(
    pyr_bbees$MaxEle,
    by = list(pyr_bbees$Visitor, pyr_bbees$Data),
    FUN = st.err
  )
  
  #Join summary values
  ele_graph = cbind(aggmeancol, aggSEcol[, -c(1:2)])
  
  #Restructure and rename columns and levels
  names(ele_graph) <- c("Group.1", "Group.2", "Mean", "SE")
  ele_graph$Group.2 <-
    factor(ele_graph$Group.2,
           levels <- c("Survey1889", "Survey2005-06"))
  ele_graph$Group.2 <- revalue(ele_graph$Group.2,
                               c("Survey1889" = "1889-90", "Survey2005-06" = "2005-06"))
  d <- reshape2::melt(ele_graph, c("Group.1", "Group.2"))
  d$Run <- c(paste(i)) #add Run column
  randsamptab <- rbind(randsamptab, d) #join to loop table
  
  #calculate mean shift
  diff_tab <- ele_graph[, 1:3]
  diff_tab <-
    reshape2::dcast(diff_tab, Group.1 ~ Group.2, value.var = 'Mean')
  diff_tab$diff <-  diff_tab$`2005-06` - diff_tab$`1889-90`
  diff_tab$Run <- c(paste(i)) #add Run column
  diff_TAB <- rbind(diff_TAB, diff_tab) #join to loop table
  
  print(paste("Run ", i))
}

#_____________________________________________________________________________________________________

#Get means of all runs
randsamptabmean <- subset(randsamptab, variable == "Mean")
aggmean_sample <-
  aggregate(
    randsamptabmean$value,
    by = list(randsamptabmean$Group.1, randsamptabmean$Group.2),
    FUN = mean
  )
aggmean_sample$variable <- c("SampleMean")
names(aggmean_sample) <-
  c("Group.1", "Group.2", "value", "variable")
aggmean_sample <- aggmean_sample[, c(1, 2, 4, 3)]
#Get means of each run
randsamptabserror <- subset(randsamptab, variable == "SE")
aggserror_sample <-
  aggregate(
    randsamptabserror$value,
    by = list(randsamptabserror$Group.1, randsamptabserror$Group.2),
    FUN = mean
  )
aggserror_sample$variable <- c("Sampleserror")
names(aggserror_sample) <-
  c("Group.1", "Group.2", "value", "variable")
aggserror_sample <- aggserror_sample[, c(1, 2, 4, 3)]

#Get min and max elevations per bumblebee
aggmin <-
  aggregate(bbees$MinEle,
            by = list(bbees$Visitor, bbees$Data),
            FUN = min)
aggmax <-
  aggregate(bbees$MaxEle,
            by = list(bbees$Visitor, bbees$Data),
            FUN = max)
ele <- cbind(aggmin, aggmax[, 3])
names(ele) <- c("Group.1", "Group.2", "Min", "Max")
ele$Group.2 <-
  factor(ele$Group.2, levels = c("Survey1889", "Survey2005-06"))
ele$Group.2 <- revalue(ele$Group.2,
                       c("Survey1889" = "1889-90", "Survey2005-06" = "2005-06"))
d_min_max <- reshape2::melt(ele, c("Group.1", "Group.2"))

#Join to main table
d.1 = rbind(d_min_max, aggmean_sample, aggserror_sample)

#Select bumblebees in both periods
d.1$Group.1 <- factor(d.1$Group.1)
d.2 <- subset(d.1, variable == "Min")
bumbles <-
  levels(factor(d.2$Group.1[duplicated(d.2$Group.1)])) #species that occur twice

d2 <-
  subset(d.1, Group.1 %in% bumbles) #select species in both periods

#Visualise elevations
ggplot(d2, aes(x = reorder(Group.1, value, min), y = value)) +
  geom_point(
    data = d2[d2$variable == "SampleMean", ],
    aes(fill = Group.2),
    position = position_dodge(width = 0.6),
    pch = 2,
    size = 2
  ) +
  geom_line(
    data = d2[d2$variable != "Sampleserror", ],
    aes(linetype = Group.2, color = Group.1),
    position = position_dodge(width = 0.6),
    size = 0.5
  ) +
  xlab("Bumblebee Species") + ylab("Elevation Range (m)") +
  scale_y_continuous(breaks = pretty_breaks(n = 7), limits = c(1000, 2250)) +
  scale_linetype_manual(
    name = "Collection Years",
    values = c("dashed", "solid"),
    labels = c("1889-90", "2005-06"),
    guide = "none"
  ) +
  scale_color_hue(h = c(0, 180), guide = 'none') +
  scale_fill_manual(values = c("black", "black"), guide = "none") +
  theme(
    axis.text.x = element_text(angle = 55, hjust = 1),
    axis.text.y = element_text(hjust = -0.5),
    axis.line = element_line(colour = "black"),
    panel.background = element_blank(),
    panel.border = element_rect(
      colour = "black",
      fill = NA,
      size = 0.5
    ),
    axis.ticks.length = unit(-0.1, "cm")
  )

#Test elevation shift
samplemean_mc = subset(d2, variable == "SampleMean" &
                         Group.2 == "1889-90")
samplemean_kb = subset(d2, variable == "SampleMean" &
                         Group.2 == "2005-06")
mean(samplemean_mc$value)
mean(samplemean_kb$value)
t.test(samplemean_kb$value, samplemean_mc$value, paired = T)
shapiro.test(samplemean_kb$value)
shapiro.test(samplemean_mc$value)
res.ftest <- var.test(value ~ Group.2, data = d2)
res.ftest

#Get means of differences - needed later to compare
diff_TAB2 = na.omit(diff_TAB)
diffmean = aggregate(diff_TAB2$diff,
                     by = list(diff_TAB2$Group.1),
                     FUN = mean)
diffsd = aggregate(diff_TAB2$diff,
                   by = list(diff_TAB2$Group.1),
                   FUN = sd)

diffTAB <- merge(diffmean, diffsd, by = "Group.1")
names(diffTAB) <- c("Species", "Mean", "SD")
#========================================================================================'

#_______________________________________________________________________________________

#    -------------------------------  5. Plant Analysis  --------------------------

#_______________________________________________________________________________________
#load plant data
plants_file <-
  "http://raw.github.com/lmar116/PyreneesBumblebees/master/Data/Plant_data_Marshall_et_al_2020.csv"
plants <- read.csv(plants_file, header = T)

#Random Re-sampling of Elevation Shifts

randsamptab.all = data.frame()#create empty data.frame to fill during loop
diff_TAB.all = data.frame()#create empty data.frame to fill during loop


for (i in 1:1000) {
  #Subset surveys
  plants1889 <- subset(plants, Data == "Survey1889")
  plants0506 <- subset(plants, Data == "Survey2005-06")
  
  #Subset each elevation zone
  E1000_1200_89 <- subset(plants1889, EleZone == "1000-1200")
  E1000_1200_05 <- subset(plants0506, EleZone == "1000-1200")
  E1200_1400_89 <- subset(plants1889, EleZone == "1200-1400")
  E1200_1400_05 <- subset(plants0506, EleZone == "1200-1400")
  E1400_1600_89 <- subset(plants1889, EleZone == "1400-1600")
  E1400_1600_05 <- subset(plants0506, EleZone == "1400-1600")
  E1600_1800_89 <- subset(plants1889, EleZone == "1600-1800")
  E1600_1800_05 <- subset(plants0506, EleZone == "1600-1800")
  E1800_2000_89 <- subset(plants1889, EleZone == "1800-2000")
  E1800_2000_05 <- subset(plants0506, EleZone == "1800-2000")
  E2000_2200_89 <- subset(plants1889, EleZone == "2000-2200")
  E2000_2200_05 <- subset(plants0506, EleZone == "2000-2200")
  
  #Compare sampling and return minimum
  E1000_1200_s <- min(nrow(E1000_1200_89), nrow(E1000_1200_05))
  E1200_1400_s <- min(nrow(E1200_1400_89), nrow(E1200_1400_05))
  E1400_1600_s <- min(nrow(E1400_1600_89), nrow(E1400_1600_05))
  E1600_1800_s <- min(nrow(E1600_1800_89), nrow(E1600_1800_05))
  E1800_2000_s <- min(nrow(E1800_2000_89), nrow(E1800_2000_05))
  E2000_2200_s <- min(nrow(E2000_2200_89), nrow(E2000_2200_05))
  
  #Resample each elvation for each period with lower sampling value
  E1000_1200_89 <-
    E1000_1200_89[sample(nrow(E1000_1200_89), E1000_1200_s),]
  E1000_1200_05 <-
    E1000_1200_05[sample(nrow(E1000_1200_05), E1000_1200_s),]
  E1200_1400_89 <-
    E1200_1400_89[sample(nrow(E1200_1400_89), E1200_1400_s),]
  E1200_1400_05 <-
    E1200_1400_05[sample(nrow(E1200_1400_05), E1200_1400_s),]
  E1400_1600_89 <-
    E1400_1600_89[sample(nrow(E1400_1600_89), E1400_1600_s),]
  E1400_1600_05 <-
    E1400_1600_05[sample(nrow(E1400_1600_05), E1400_1600_s),]
  E1600_1800_89 <-
    E1600_1800_89[sample(nrow(E1600_1800_89), E1600_1800_s),]
  E1600_1800_05 <-
    E1600_1800_05[sample(nrow(E1600_1800_05), E1600_1800_s),]
  E1800_2000_89 <-
    E1800_2000_89[sample(nrow(E1800_2000_89), E1800_2000_s),]
  E1800_2000_05 <-
    E1800_2000_05[sample(nrow(E1800_2000_05), E1800_2000_s),]
  E2000_2200_89 <-
    E2000_2200_89[sample(nrow(E2000_2200_89), E2000_2200_s),]
  E2000_2200_05 <-
    E2000_2200_05[sample(nrow(E2000_2200_05), E2000_2200_s),]
  
  #Join new tables
  pyr_plants <- rbind(
    E1000_1200_05,
    E1200_1400_05,
    E1400_1600_05,
    E1600_1800_05,
    E1800_2000_05,
    E2000_2200_05,
    E1000_1200_89,
    E1200_1400_89,
    E1400_1600_89,
    E1600_1800_89,
    E1800_2000_89,
    E2000_2200_89
  )
  
  #Calculate summary values
  aggmeancol <- aggregate(
    pyr_plants$MaxEle,
    by = list(pyr_plants$Plant, pyr_plants$Data),
    FUN = mean
  )
  
  aggSEcol <- aggregate(
    pyr_plants$MaxEle,
    by = list(pyr_plants$Plant, pyr_plants$Data),
    FUN = st.err
  )
  
  #Join summary values
  ele_graph = cbind(aggmeancol, aggSEcol[,-c(1:2)])
  
  #Restructure and rename columns and levels
  names(ele_graph) <- c("Group.1", "Group.2", "Mean", "SE")
  ele_graph$Group.2 <-
    factor(ele_graph$Group.2,
           levels <- c("Survey1889", "Survey2005-06"))
  ele_graph$Group.2 <- revalue(ele_graph$Group.2,
                               c("Survey1889" = "1889-90", "Survey2005-06" = "2005-06"))
  d <- reshape2::melt(ele_graph, c("Group.1", "Group.2"))
  d$Run <- c(paste(i)) #add Run column
  randsamptab.all <- rbind(randsamptab.all, d) #join to loop table
  
  #calculate mean shift
  diff_TAB.all <- ele_graph[, 1:3]
  diff_TAB.all <-
    reshape2::dcast(diff_TAB.all, Group.1 ~ Group.2, value.var = 'Mean')
  diff_TAB.all$diff <-
    diff_TAB.all$`2005-06` - diff_TAB.all$`1889-90`
  diff_TAB.all$Run <- c(paste(i)) #add Run column
  diff_TAB.all <-
    rbind(diff_TAB.all, diff_TAB.all) #join to loop table
  
  print(paste("Run ", i))
}

#_____________________________________________________________________________________________________

#Get means of all runs
randsamptab.allmean <- subset(randsamptab.all, variable == "Mean")
aggmean_sample.all <-
  aggregate(
    randsamptab.allmean$value,
    by = list(randsamptab.allmean$Group.1, randsamptab.allmean$Group.2),
    FUN = mean
  )
aggmean_sample.all$variable <- c("SampleMean")
names(aggmean_sample.all) <-
  c("Group.1", "Group.2", "value", "variable")
aggmean_sample.all <- aggmean_sample.all[, c(1, 2, 4, 3)]

#Get standard error of all runs
randsamptab.allserror <- subset(randsamptab.all, variable == "SE")
aggserror_sample.all <-
  aggregate(
    randsamptab.allserror$value,
    by = list(
      randsamptab.allserror$Group.1,
      randsamptab.allserror$Group.2
    ),
    FUN = mean
  )
aggserror_sample.all$variable <- c("Sampleserror")
names(aggserror_sample.all) <-
  c("Group.1", "Group.2", "value", "variable")
aggserror_sample.all <- aggserror_sample.all[, c(1, 2, 4, 3)]

#Get min and max elevations per bumblebee
aggmin.all <-
  aggregate(plants$MinEle,
            by = list(plants$Plant, plants$Data),
            FUN = min)
aggmax.all <-
  aggregate(plants$MaxEle,
            by = list(plants$Plant, plants$Data),
            FUN = max)
ele.all <- cbind(aggmin.all, aggmax.all[, 3])
names(ele.all) <- c("Group.1", "Group.2", "Min", "Max")
ele.all$Group.2 <-
  factor(ele.all$Group.2, levels = c("Survey1889", "Survey2005-06"))
ele.all$Group.2 = revalue(ele.all$Group.2,
                          c("Survey1889" = "1889-90", "Survey2005-06" = "2005-06"))
d_min_max.all <- reshape2::melt(ele.all, c("Group.1", "Group.2"))

d.1.all = rbind(d_min_max.all, aggmean_sample.all, aggserror_sample.all)


#Get list of plants in both periods visited by bumblebees found at least twice
summ_pyr <-
  data.frame(plants %>% count(Plant, Data)) #count of each  visitor and collector combination
df_.all <- subset(summ_pyr, n > 1)# only found at least twice
df_.all <- df_.all[duplicated(df_.all[1]),]
df_.all[, 1] <- factor(df_.all[, 1])
plants.list <- levels(df_.all[, 1])
list.both.plants <-
  subset(plants, grepl("Bombus", plants[[1]]), drop = TRUE)
summ_pyr.all = data.frame(list.both.plants %>% count(Plant, Data))
df_.all2 = summ_pyr.all[duplicated(summ_pyr.all[1]),]
df_.all2[, 1] = factor(df_.all2[, 1])
list.plants.both = levels(df_.all2[, 1])
#now need to compare the two lists
plantlist <- intersect(plants.list, list.plants.both)
#get only with bumblebees from both periods

d2.all <- subset(d.1.all, Group.1 %in% plantlist)

#Visualise elevations
ggplot(d2.all, aes(x = reorder(Group.1, value, min), y = value)) +
  geom_point(
    data = d2.all[d2.all$variable == "SampleMean", ],
    aes(fill = Group.2),
    position = position_dodge(width = 0.6),
    pch = 2,
    size = 2
  ) +
  geom_line(
    data = d2.all[d2.all$variable != "Sampleserror", ],
    aes(linetype = Group.2, color = Group.1),
    position = position_dodge(width = 0.6),
    size = 0.5
  ) +
  xlab("Bumblebee Species") + ylab("Elevation Range (m)") +
  scale_y_continuous(breaks = pretty_breaks(n = 7), limits = c(1000, 2250)) +
  scale_linetype_manual(
    name = "Collection Years",
    values = c("dashed", "solid"),
    labels = c("1889-90", "2005-06"),
    guide = "none"
  ) +
  scale_color_hue(h = c(0, 180), guide = 'none') +
  scale_fill_manual(values = c("black", "black"), guide = "none") +
  theme(
    axis.text.x = element_text(angle = 55, hjust = 1),
    axis.text.y = element_text(hjust = -0.5),
    axis.line = element_line(colour = "black"),
    panel.background = element_blank(),
    panel.border = element_rect(
      colour = "black",
      fill = NA,
      size = 0.5
    ),
    axis.ticks.length = unit(-0.1, "cm")
  )

#Test elevation shift
samplemean_mc = subset(d2.all, variable == "SampleMean" &
                         Group.2 == "1889-90")
samplemean_kb = subset(d2.all, variable == "SampleMean" &
                         Group.2 == "2005-06")
mean(samplemean_mc$value)
mean(samplemean_kb$value)
t.test(samplemean_kb$value, samplemean_mc$value, paired = T)
shapiro.test(samplemean_kb$value)
shapiro.test(samplemean_mc$value)
res.ftest <- var.test(value ~ Group.2, data = d2)
res.ftest

#Get means of differences - needed later to compare
diff_TAB.all2 = na.omit(diff_TAB.all)
diffmean.all = aggregate(diff_TAB.all2$diff,
                         by = list(diff_TAB.all2$Group.1),
                         FUN = mean)
diffsd.all = aggregate(diff_TAB.all2$diff,
                       by = list(diff_TAB.all2$Group.1),
                       FUN = sd)

diffTAB.all <- merge(diffmean.all, diffsd.all, by = "Group.1")
names(diffTAB.all) <- c("Species", "Mean", "SD")
diffTAB.all2 <-
  subset(diffTAB.all, Species %in% plants)#select only plant found at least twice
#========================================================================================'


#_______________________________________________________________________________________

#    --------------------------  6. Individual Plant Graphs  -----------------------

#_______________________________________________________________________________________
plants$MinEle = as.numeric(as.character(plants$MinEle))
plants$RoundEle = round(plants$MinEle, digits = -2)

#aconitum graph
aconit = subset(plants, Plant == "Aconitum sp.")#select species of interest
aconit$Visitor = factor(aconit$Visitor)
levels(aconit$Visitor)#check empty values i.e no visitor
aconit$Family = factor(aconit$Family)
levels(aconit$Family)

aconit2 = aconit[!(aconit$Family == "" &
                     aconit$Visitor == " "), ]#remove non visitation sites

aconit3 = aconit[, c(6, 10)] #select only data and rounded elevation columns
aconit3$RoundEle = factor(aconit3$RoundEle) #define as factor
levels(aconit3$RoundEle) #check levels
aconit3$RoundEle <-
  factor(aconit3$RoundEle, levels = c(levels(aconit3$RoundEle), "1000", "1300")) #add missing levels

aconit4 = data.frame(prop.table(table(aconit3), 1)) #get proportions table
aconit4$RoundEle = as.numeric(as.character(aconit4$RoundEle)) #make elevation continuous

ggplot(aconit4, aes(
  x = RoundEle,
  y = Freq,
  colour = Data,
  group = Data
)) +
  geom_line() +
  coord_flip() +
  geom_ribbon(aes(
    ymin = 0,
    ymax = Freq,
    fill = Data,
    group = Data
  ), alpha = .8) +
  theme_bw() +
  scale_x_continuous(breaks = pretty_breaks(n = 14)) +
  scale_y_continuous(limits = c(0, 0.8))

#Carduus defloratus
card = subset(plants, Plant == "Carduus defloratus")#select species of interest
card$Visitor = factor(card$Visitor)
levels(card$Visitor)#check empty values i.e no visitor
card$Family = factor(card$Family)
levels(card$Family)

card2 = card[!(card$Family == "" &
                 card$Visitor == " "), ]#remove non visitation sites

card3 = card2[, c(6, 10)] #select only data and rounded elevation columns
card3$RoundEle = factor(card3$RoundEle) #define as factor
levels(card3$RoundEle) #check levels
card3$RoundEle <-
  factor(card3$RoundEle, levels = c(levels(card3$RoundEle), "1500", "2200")) #add missing levels

card4 = data.frame(prop.table(table(card3), 1)) #get proportions table
card4$RoundEle = as.numeric(as.character(card4$RoundEle)) #make elevation continuous

ggplot(card4, aes(
  x = RoundEle,
  y = Freq,
  colour = Data,
  group = Data
)) +
  geom_line() +
  coord_flip() +
  geom_ribbon(aes(
    ymin = 0,
    ymax = Freq,
    fill = Data,
    group = Data
  ), alpha = .8) +
  theme_bw() +
  scale_x_continuous(breaks = pretty_breaks(n = 14)) +
  scale_y_continuous(limits = c(0, 0.8))

#Cirsium eriophorum
cire = subset(plants, Plant == "Cirsium eriophorum")#select species of interest
cire$Visitor = factor(cire$Visitor)
levels(cire$Visitor)#check empty values i.e no visitor
cire$Family = factor(cire$Family)
levels(cire$Family)

cire2 = cire[!(cire$Family == "" &
                 cire$Visitor == " "), ]#remove non visitation sites

cire3 = cire2[, c(6, 10)] #select only data and rounded elevation columns
cire3$RoundEle = factor(cire3$RoundEle) #define as factor
levels(cire3$RoundEle) #check levels
cire3$RoundEle <-
  factor(cire3$RoundEle,
         levels = c(levels(cire3$RoundEle), "1000", "1100", "1300", "1400")) #add missing levels

cire4 = data.frame(prop.table(table(cire3), 1)) #get proportions table
cire4$RoundEle = as.numeric(as.character(cire4$RoundEle)) #make elevation continuous

ggplot(cire4, aes(
  x = RoundEle,
  y = Freq,
  colour = Data,
  group = Data
)) +
  geom_line() +
  coord_flip() +
  geom_ribbon(aes(
    ymin = 0,
    ymax = Freq,
    fill = Data,
    group = Data
  ), alpha = .8) +
  theme_bw() +
  scale_x_continuous(breaks = pretty_breaks(n = 14)) +
  scale_y_continuous(limits = c(0, 0.8))
#========================================================================================'


#_______________________________________________________________________________________

#    --------------------------  7. Plant Visitor Graphs  -----------------------

#_______________________________________________________________________________________

#aconitum graph
aconit.b <- subset(plants, Plant == "Aconitum sp.")

aconit2.b <-
  aconit.b[, c(1, 6, 10)]#select only visitor, data, and elevation
aconit2.b$RoundEle <- factor(aconit2.b$RoundEle) #define as factor
aconit2.b$Visitor <- factor(aconit2.b$Visitor) #define as factor

bumble <-
  levels(aconit2.b$Visitor)[c(3:8, 10)] #list of visitors - only bumblebees chosen
aconit3.b <-
  aconit2.b[aconit2.b$Visitor %in% bumble, ] #select only those visitors i am interested in
aconit3.b$Visitor = factor(aconit3.b$Visitor) #redefine factor levels

aconit3.b$RoundEle = factor(aconit3.b$RoundEle)
levels(aconit3.b$RoundEle) #check levels
aconit3.b$RoundEle <-
  factor(aconit3.b$RoundEle, levels = c(levels(aconit3.b$RoundEle),
                                        "1000", "1300", "2000"))#add missing levels

aconit3.bm <- subset(aconit3.b, Data == "Survey1889")[,-2]
aconit4.bm <-
  data.frame(prop.table(table(aconit3.bm), 2)) #get proportions table - no number gives total proportion, 1 gives per species, 2 gives per elevation
aconit3.bb <- subset(aconit3.b, Data == "Survey2005-06")[,-2]
aconit4.bb <- data.frame(prop.table(table(aconit3.bb), 2))
aconit4.bm$Data <- c("Survey1889")
aconit4.bb$Data <- c("Survey2005-06")
aconit4.b <- rbind(aconit4.bm, aconit4.bb)
aconit4.b$RoundEle <-
  as.numeric(as.character(aconit4.b$RoundEle)) #make elevation continuous

#Graph
ggplot(aconit4.b , aes(x = RoundEle, y = Freq, fill = Visitor)) +
  geom_line() +
  coord_flip() +
  geom_area() +
  theme_bw() +
  scale_x_continuous(breaks = pretty_breaks(n = 14)) + #start from 1000m
  scale_y_continuous(limits = c(0, 1)) +
  facet_grid(. ~ Data)

#Carduus defloratus graph
card.b = subset(plants, Plant == "Carduus defloratus")

card2.b = card.b[, c(1, 6, 10)]#select only visitor and elevation
card2.b$RoundEle = factor(card2.b$RoundEle) #define as factor
card2.b$Visitor = factor(card2.b$Visitor) #define as factor

bumble = levels(card2.b$Visitor)[c(7:17)] #list of visitors - subsetted bu those I am interested in
card3.b <-
  card2.b[card2.b$Visitor %in% bumble, ] #select only those visitors i am interested in
card3.b$Visitor = factor(card3.b$Visitor) #redefine factor levels

card3.b$RoundEle = factor(card3.b$RoundEle)
levels(card3.b$RoundEle) #check levels
card3.b$RoundEle <-
  factor(card3.b$RoundEle, levels = c(levels(card3.b$RoundEle),
                                      "1000", "2200"))#add missing levels

card3.bm = subset(card3.b, Data == "Survey1889")[,-2]
card4.bm = data.frame(prop.table(table(card3.bm), 2)) #get proportions table - no number gives total proportion, 1 gives per species, 2 gives per elevation
card3.bb = subset(card3.b, Data == "Survey2005-06")[,-2]
card4.bb = data.frame(prop.table(table(card3.bb), 2))
card4.bm$Data = c("Survey1889")
card4.bb$Data = c("Survey2005-06")
card4.b = rbind(card4.bm, card4.bb)
card4.b$RoundEle = as.numeric(as.character(card4.b$RoundEle)) #make elevation continuous

ggplot(card4.b , aes(x = RoundEle, y = Freq, fill = Visitor)) +
  geom_line() +
  coord_flip() +
  geom_area() +
  theme_bw() +
  scale_x_continuous(breaks = pretty_breaks(n = 14)) + #start from 1000m
  scale_y_continuous(limits = c(0, 1)) +
  facet_grid(. ~ Data)


#Cirsium eriophorum graph
cirs.b = subset(plants, Plant == "Cirsium eriophorum")

cirs2.b = cirs.b[, c(1, 6, 10)]#select only visitor and elevation
cirs2.b$RoundEle = factor(cirs2.b$RoundEle) #define as factor
cirs2.b$Visitor = factor(cirs2.b$Visitor) #define as factor

bumble = levels(cirs2.b$Visitor)[c(3:10, 13)] #list of visitors - subsetted bu those I am interested in
cirs3.b <-
  cirs2.b[cirs2.b$Visitor %in% bumble, ] #select only those visitors i am interested in
cirs3.b$Visitor = factor(cirs3.b$Visitor) #redefine factor levels

cirs3.b$RoundEle = factor(cirs3.b$RoundEle)
levels(cirs3.b$RoundEle) #check levels
cirs3.b$RoundEle <-
  factor(cirs3.b$RoundEle,
         levels = c(levels(cirs3.b$RoundEle),
                    "1000", "1100", "1300", "1400"))#add missing levels

cirs3.bm = subset(cirs3.b, Data == "Survey1889")[,-2]
cirs4.bm = data.frame(prop.table(table(cirs3.bm), 2)) #get proportions table - no number gives total proportion, 1 gives per species, 2 gives per elevation
cirs3.bb = subset(cirs3.b, Data == "Survey2005-06")[,-2]
cirs4.bb = data.frame(prop.table(table(cirs3.bb), 2))
cirs4.bm$Data = c("Survey1889")
cirs4.bb$Data = c("Survey2005-06")
cirs4.b = rbind(cirs4.bm, cirs4.bb)
cirs4.b$RoundEle = as.numeric(as.character(cirs4.b$RoundEle)) #make elevation continuous

ggplot(cirs4.b , aes(x = RoundEle, y = Freq, fill = Visitor)) +
  geom_line() +
  coord_flip() +
  geom_area() +
  theme_bw() +
  scale_x_continuous(breaks = pretty_breaks(n = 14)) + #start from 1000m
  scale_y_continuous(limits = c(0, 1)) +
  facet_grid(. ~ Data)
#========================================================================================'


#_______________________________________________________________________________________

#    --------------------  8. Plant-pollinator elevation shifts  -------------------

#_______________________________________________________________________________________

#get list of plants per bumblebee and then calculate mean shift and sd
diffTAB$Species = factor(diffTAB$Species)
bumblebees = levels(diffTAB$Species)

DF = data.frame()

for (b in 1:length(bumblebees)) {
  sb <- subset(plants, Visitor == bumblebees[b])
  
  #Get plant elevations shift per bumblebee weighted by recorded number of visits
  sb$Plant = factor(sb$Plant)
  pls <- levels(sb$Plant)
  
  plssb.1 = subset(diffTAB.all, Species %in% pls)
  freq = data.frame(table(sb$Plant))
  plssb = merge(plssb.1, freq, by.x = "Species", by.y = "Var1")
  
  pls_mn = weighted.mean(plssb$Mean, plssb$Freq)
  pls_sd = sd(plssb$Mean)
  pls_se = st.err(plssb$Mean)
  pls_sdmn = sd(plssb$SD)
  
  #now only plant they visited in both periods
  sb <- subset(plants, Visitor == bumblebees[b])
  sb_uni <- unique(sb[, c(4, 2)])
  plsdup <- sb_uni[duplicated(sb_uni$Plant),][, 2]
  
  plssb_dup.1 = subset(diffTAB.all, Species %in% plsdup)
  plssb_dup = merge(plssb_dup.1, freq, by.x = "Species", by.y = "Var1")
  
  pls_mndup = weighted.mean(plssb_dup$Mean, plssb_dup$Freq)
  pls_sddup = sd(plssb_dup$Mean)
  pls_sedup = st.err(plssb_dup$Mean)
  pls_sdmndup = sd(plssb_dup$SD)
  
  df = data.frame(
    MeanPL = pls_mn,
    SDPL = pls_sd,
    SEPL = pls_se,
    SDMNPL = pls_sdmn,
    MeanPLBoth = pls_mndup,
    SDPLBoth = pls_sddup,
    SEPLBoth = pls_sedup,
    SDMNPLBoth = pls_sdmndup
  )
  df$Species = bumblebees[b]
  df$N = nrow(plssb)
  df$NBoth = length(plsdup)
  DF = rbind(DF, df)
}

#full table with all differences between periods
FullDiff = merge(diffTAB, DF, by = "Species")

#Check fit
fit <- lm(MeanPL ~ Mean, data = FullDiff)
summary(fit)

#Get species names for graph
FullDiff$Name = gsub("ombus", ".", FullDiff$Species)

#Visualise 
ggplot(FullDiff, aes(x = Mean, y = MeanPL)) +
  geom_point(shape = 19, cex = 3) +
  stat_poly_eq(
    formula = y ~ x,
    aes(
      label = paste(..eq.label.., ..rr.label.., ..p.value.label.., sep = "~~~")
    ),
    parse = TRUE,
    p.digits = 1
  ) +
  labs(y = "Mean Elevation Shift of Plants Visited", x = "Mean Elevation Shift of Bumblebees") +
  geom_text_repel(
    aes(label = paste0(Name, " (", N, ")")),
    box.padding   = 0,
    colour = 'black',
    point.padding = 0.3,
    segment.color = 'grey50',
    fontface = "italic",
    size = 4
  ) +
  theme(
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 14, face = "bold"),
    plot.title = element_text(size = 22),
    legend.position = "none",
    panel.border = element_rect(
      colour = "black",
      fill = NA,
      size = 1
    ),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_blank(),
    panel.background = element_blank()
  ) +
  geom_abline(
    intercept = 0,
    slope = 1,
    color = "black",
    linetype = "dashed",
    size = 0.5
  ) +
  geom_errorbar(
    aes(ymin = MeanPL - SDMNPL, ymax = MeanPL + SDMNPL),
    width = 0,
    size = 0.5,
    linetype = 3
  ) +
  geom_errorbarh(
    aes(xmin = Mean - SD, xmax = Mean + SD),
    height = 1,
    size = 0.5,
    linetype = 3
  ) +
  scale_x_continuous(expand = c(0, 0), limits = c(-260, 520)) +
  scale_y_continuous(expand = c(0, 0), limits = c(-260, 520)) +
  coord_cartesian(xlim = c(-260, 520), ylim = c(0, 520))


#========================================================================================'


#_______________________________________________________________________________________

#    --------------------  9. Climate Range Shifts  -------------------

#_______________________________________________________________________________________
#load file with coordinates
clim_file <-
  "https://raw.githubusercontent.com/lmar116/PyreneesBumblebees/master/Data/Bumblebee_data_climate_Marshall_et_al_2020.csv"
clim <- read.csv(clim_file, header = T)

theme_set(theme_light(base_size = 24))

ggplot(clim, aes(
  x = reorder(Visitor, clim),
  y = clim,
  fill = Data,
  color = Data
)) +
  coord_flip() +
  scale_y_continuous(limits = c(9.5, 20), expand = c(0.005, 0.005)) +
  scale_color_uchicago() +
  labs(x = NULL, y = "Mean August Temperature (Degrees Celsius)") +
  theme(
    legend.position = "none",
    axis.title = element_text(size = 24),
    axis.text.x = element_text(size = 20),
    panel.grid = element_blank()
  ) +
  stat_compare_means(
    aes(group = Data),
    method = "kruskal.test",
    label = "p.signif",
    label.y = c(18),
    method.args = list(alternative = "greater")
  ) +
  stat_compare_means(
    aes(label = ..p.adj..),
    method = "kruskal.test",
    method.args = list(alternative = "greater"),
    label.y = c(10)
  ) +
  geom_boxplot(color = "gray60", outlier.alpha = 0) +
  #geom_point(size = 3, alpha = 0.15)+
  geom_jitter(
    position = position_jitter(seed = 2019, width = 0.2),
    size = 2,
    alpha = 0.25
  )


#========================================================================================'


#_______________________________________________________________________________________

#    --------------------  10. Climate Changes  -------------------

#_______________________________________________________________________________________


#========================================================================================'
#load file with august temps for the two periods for August
aug_clim_file <-
  "https://raw.githubusercontent.com/lmar116/PyreneesBumblebees/master/Data/Regional_August_Temps_1910_2010.csv"
aug_clim <- read.csv(aug_clim_file, header = T)

aug_clim$Year <- factor(aug_clim$Year)#convert to factor

#Visualize
ggplot(data = aug_clim , aes(x = Elevation, y = value)) +
  geom_point(
    data = subset(aug_clim, variable == "Tave08"),
    aes(group = Year, colour = Year),
    pch = 17
  ) +
  geom_point(
    data = subset(aug_clim, variable == "Tmin08"),
    aes(group = Year, colour = Year),
    pch = 18
  ) +
  geom_point(
    data = subset(aug_clim, variable == "Tmax08"),
    aes(group = Year, colour = Year),
    pch = 19
  ) +
  scale_colour_manual(values = c("grey50", "black"), guide = "none") +
  xlab("Elevation Range (m)") + ylab("Temperature �C") +
  theme(
    axis.text.y = element_text(hjust = -0.5),
    axis.line = element_line(colour = "black"),
    panel.background = element_blank(),
    panel.border = element_rect(
      colour = "black",
      fill = NA,
      size = 0.5
    ),
    axis.ticks.length = unit(-0.1, "cm")
  )

#changes in elevation per temperature
aug_clim_elediff <- aug_clim
aug_clim_elediff$value = factor(aug_clim_elediff$value)
elediff_clim = dcast(
  aggregate(
    aug_clim_elediff$Elevation,
    by = list(
      aug_clim_elediff$value,
      aug_clim_elediff$variable,
      aug_clim_elediff$Year
    ),
    FUN = mean
  ),
  Group.1 + Group.2 ~ Group.3
)

#calcualte elevation differences between same temps in the two different periods
elediff_clim <- na.omit(elediff_clim)
elediff_clim$elediff <- elediff_clim$`2005` - elediff_clim$`1905`
elediff_clim_summary.mean <- aggregate(elediff_clim$elediff,
                                       by = list(elediff_clim$Group.2),
                                       FUN = mean)
elediff_clim_summary.sd <- aggregate(elediff_clim$elediff,
                                     by = list(elediff_clim$Group.2),
                                     FUN = sd)
elediff_clim_summary <-
  cbind(elediff_clim_summary.mean, elediff_clim_summary.sd[, 2])
names(elediff_clim_summary) <- c("Temp", "Mean", "SD")
elediff_clim_summary

#load file with temporal trends for region
trend_clim_file <-
  "https://raw.githubusercontent.com/lmar116/PyreneesBumblebees/master/Data/Regional_Trend_Temps.csv"
trend_clim <- read.csv(trend_clim_file, header = T)

#get the per grid mean of the mean annual temp
trendmean <-
  aggregate(trend_clim$AnnualMeanTemp,
            by = list(trend_clim$Year),
            FUN = mean)

#model trend
m1 <- lm(AnnualMeanTemp ~ Year, data = trend_clim)
summary(m1)

#Visualise
ggplot(data = trendmean, aes(x = Group.1, y = x)) +
  geom_point() +
  geom_smooth(
    method = 'lm',
    formula = y ~ x,
    level = 0.90,
    alpha = 0.2
  ) +
  theme(
    axis.text.y = element_text(hjust = -0.5),
    axis.line = element_line(colour = "black"),
    panel.background = element_blank(),
    panel.border = element_rect(
      colour = "black",
      fill = NA,
      size = 0.5
    ),
    axis.ticks.length = unit(-0.1, "cm")
  )

#_______________________________________________________________________________________

#    --------------------  11. Land Use Changes -------------------

#_______________________________________________________________________________________
#load file with land use information
lu_file <-
  "https://raw.githubusercontent.com/lmar116/PyreneesBumblebees/master/Data/Regional_LandUse.csv"
lu <- read.csv(lu_file, header = T)

#Proportions of each land use in 1910 and 2010
# 333 = Forest, 444 = Grassland, 555 = Other (e.g. Snow and bare ground)
LU_1910 <- lu[, c("LU1910", "EleZone")]
LU_1910.prop <- data.frame(prop.table(table(LU_1910), 2))
LU_1910.prop
LU_2010 <- lu [, c("LU2010", "EleZone")]
LU_2010.prop = data.frame(prop.table(table(LU_2010), 2))
LU_2010.prop

#Split data per elevation zone - grids between 1000m and 2250m
LU_melt = melt(lu[, c("LU1910", "LU2010", "EleZone")])
LU_montane = subset(LU_melt, EleZone == "z1")
LU_subalpine = subset(LU_melt, EleZone == "z2")
LU_alpine = subset(LU_melt, EleZone == "z3")

#Statistically compare sites
tbl_m = table(LU_montane$variable, LU_montane$value)
tbl_m
chisq.test(tbl_m)
tbl_sa = table(LU_subalpine$variable, LU_subalpine$value)
tbl_sa
chisq.test(tbl_sa)
tbl_a = table(LU_alpine$variable, LU_alpine$value)
tbl_a
chisq.test(tbl_a)

#========================================================================================'


#_______________________________________________________________________________________

#    --------------------  12. Rarefaction - Suppl. Material  -------------------

#_______________________________________________________________________________________


#3 elevation zones
rare_bbee = dcast(bbees, EleZone + Data ~ Visitor)
rare_bbee2 = aggregate(. ~ Data + EleZone, rare_bbee, sum)
rare_bbee2$rownames = paste(rare_bbee2$EleZone, rare_bbee2$Data, sep = "_")
row.names(rare_bbee2) = rare_bbee2[, 24]
rare_b = rare_bbee2[,-c(1:2, 24)]
rare_b$Merge = c(1, 2, 1, 2, 3, 4, 3, 4, 5, 6, 5, 6)


rare_b2 = aggregate(. ~ Merge, rare_b, sum)
row.names(rare_b2) = c(
  "1000-1400_89",
  "1000-1400_05",
  "1400-1800_89",
  "1400-1800_05",
  "1800-2200_89",
  "1800-2200_05"
)
rare_b3 = rare_b2[,-1]
rare_b3$EleZone = c("1000-1400",
                    "1000-1400",
                    "1400-1800",
                    "1400-1800",
                    "1800-2200",
                    "1800-2200")


rare_b_1.2 = subset(rare_b3, EleZone == "1000-1400")[,-c(22)]
rare_b_2.2 = subset(rare_b3, EleZone == "1400-1800")[,-c(22)]
rare_b_3.2 = subset(rare_b3, EleZone == "1800-2200")[,-c(22)]

res1.2 <- lapply(split(rare_b_1.2, row.names(rare_b_1.2)), unlist)
res2.2 <- lapply(split(rare_b_2.2, row.names(rare_b_2.2)), unlist)
res3.2 <- lapply(split(rare_b_3.2, row.names(rare_b_3.2)), unlist)


# call iNEXT a vector
out1.2 <- iNEXT(res1.2, q = 0, datatype = "abundance")
ggiNEXT(out1.2, type = 1)
out2.2 <- iNEXT(res2.2, q = 0, datatype = "abundance")
ggiNEXT(out2.2, type = 1)
out3.2 <- iNEXT(res3.2, q = 0, datatype = "abundance")
ggiNEXT(out3.2, type = 1)
