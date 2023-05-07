library(DescTools)
library(RCPA3)

#############################################################################################################################
#
# ANALISI GINI
#
#############################################################################################################################

folder<-"C:/Dati/Dropbox/Progetti/progetto siveas/lavori COVID/spatial accessibility/GitHubHealthcare/ABM4PatientMobility/additional_files/"
folder<-"E:/Dropbox/Progetti/progetto siveas/lavori COVID/spatial accessibility/GitHubHealthcare/ABM4PatientMobility/additional_files/"
dataSimulation<-read.csv(paste(folder,"gini_analysis.csv",sep = ""), TRUE)
provinces <- unique(dataSimulation$regpaz)
df <- data.frame(0,0,0,0,0,0)
names(df) <- c("province", "media", "gini", "devst", "mobility_real", "mobility_regression")  
province<-provinces[1]
for (province in provinces) {
  print(province)
  x <- dataSimulation[dataSimulation$regpaz == province, 6]
  w <- dataSimulation[dataSimulation$regpaz == province, 5]
  gini <- Gini(x, w, TRUE)
  #
  media<-sum(x*w)/sum(w)
  devst<-sqrt(sum(((x - media)^2)*w)/(sum(w)-1))
  passive_real <- dataMobilityRegion[tolower(dataMobilityRegion$`Etichette di riga`)==tolower(province), c("active_real")]
  passive_regression <- dataMobilityRegion[tolower(dataMobilityRegion$`Etichette di riga`)==tolower(province), c("active_regression")]

  df.singleDoc <- data.frame(province, media, gini, devst, passive_real, passive_regression)
  names(df.singleDoc) <- c("province", "media", "gini","devst", "mobility_real", "mobility_regression")  
  df <- rbind(df, df.singleDoc)
}
df<-df[-1,]


write_xlsx(df, "E:/Dropbox/Progetti/progetto siveas/lavori COVID/spatial accessibility/GitHubHealthcare/ABM4PatientMobility/gini_regre_reg_2.xlsx")

regression <- lm(gini ~ var1, df)
summary(regression)$r.squared
plot(df$var1, df$gini)
text(df$var1, df$gini, labels=df$province)
abline(regression)


regression <- lm(gini ~ mobility_regression, df)
summary(regression)$r.squared
plot(df$mobility_regression, df$gini)
text(df$mobility_regression, df$gini, labels=df$province)
abline(regression)


#############################################################################################################################
#
# ANALISI ACCURATEZZA E PRECISIONE SU FILE MULTIPLI + ANALISI REGRESSIONI SU SPECIFICA FINESTRA
#
#############################################################################################################################

library(readxl)
library(writexl)
library(dplyr)
library(ggplot2)
library(irr)
#library(pivottabler)

folder<-"C:/Dati/Dropbox/Progetti/progetto siveas/lavori COVID/spatial accessibility/GitHubHealthcare/ABM4PatientMobility/output/"
folder<-"E:/Dropbox/Progetti/progetto siveas/lavori COVID/spatial accessibility/GitHubHealthcare/ABM4PatientMobility/output/"

# all-fixed-april-16
# all-random-april-16
# all-60-tests
# all-60-tests-fixed

files <- list.files(path=paste(folder, "all-60-tests", sep = ""), pattern="*.csv", full.names=TRUE, recursive=FALSE)
n_files<-length(files)+4-1
dataMobility <- read_excel(paste(folder,"mobility_data.xlsx", sep = ""), sheet = "Foglio1")
dataMobilityRegion <- read_excel(paste(folder,"mobility_data_region.xlsx", sep = ""), sheet = "Foglio1")

#rm(passive_btw_data)
#rm(active_btw_data)
#passive_btw_data <- data.frame(province = dataMobility$propaz, regression = dataMobility$passive_regression, real = dataMobility$passive_original)
#active_btw_data <- data.frame(region = dataMobilityRegion$`Etichette di riga`, regression = dataMobilityRegion$active_regression, real = dataMobilityRegion$active_real)

rm(passive_whole)
rm(active_whole)
rm(flow_whole)
passive_whole <- data.frame(province = dataMobility$propaz, regression = dataMobility$passive_regression, real = dataMobility$passive_original)
active_whole <- data.frame(region = dataMobilityRegion$`Etichette di riga`, regression = dataMobilityRegion$active_regression, real = dataMobilityRegion$active_real)
i<-51
index<-1
for (file in files) {
  print(index)
  index<-index+1
  dataSimulationAll<-read.csv(file, TRUE, "|")
  dataSimulation <- dataSimulationAll[dataSimulationAll$week_treated >= i & dataSimulationAll$week_treated <= (i + 51), ]
  
  passive_mobility <- dataSimulation %>% count(propaz,stay)
  passive_mobility <- passive_mobility[order(passive_mobility$propaz),]
  passive_stayed <- passive_mobility %>% filter(passive_mobility$stay=="true")
  dataMobility <- dataMobility[order(dataMobility$propaz),]
  passive_moved <- passive_mobility %>% filter(passive_mobility$stay=="false")
  passive_merged <- data.frame(province = passive_stayed$propaz, province2 = dataMobility$propaz, stayed = passive_stayed$n, moved = passive_moved$n, mobility = (passive_moved$n / (passive_moved$n + passive_stayed$n)), regression = dataMobility$passive_regression, real = dataMobility$passive_original)
  passive_whole <- data.frame(passive_whole, mobility = (passive_moved$n / (passive_moved$n + passive_stayed$n)))
  
  active_flow <- dataSimulation %>% count(reghos,regpaz)
  active_mobility <- dataSimulation %>% count(reghos,stay)
  active_stayed <- active_mobility %>% filter(active_mobility$stay=="true")
  active_moved <- active_mobility %>% filter(active_mobility$stay=="false")
  active_merged <- data.frame(region = active_stayed$reghos, region2 = dataMobilityRegion$`Etichette di riga`, local = active_stayed$n, active = active_moved$n, total = active_stayed$n + active_moved$n, mobility = (active_moved$n / (active_moved$n + active_stayed$n)), regression = dataMobilityRegion$active_regression, real = dataMobilityRegion$active_real, interventions = dataMobilityRegion$interventions, difference = dataMobilityRegion$interventions - (active_stayed$n + active_moved$n), percentage = (dataMobilityRegion$interventions - (active_stayed$n + active_moved$n)) / (active_stayed$n + active_moved$n))
  active_whole <- data.frame(active_whole, mobility = (active_moved$n / (active_moved$n + active_stayed$n)))
  
  total_flow <- dataSimulation %>% count(reghos)
  flow_flow <- full_join(active_flow, total_flow, by = c("reghos"))
  flow_flow$rate <- flow_flow$n.x/flow_flow$n.y
  if (index == 2) {
    flow_whole <- data.frame(reghos = flow_flow$reghos, regpaz = flow_flow$regpaz, mobility = flow_flow$rate)
  } else {
    flow_flow <- flow_flow[,c("reghos", "regpaz", "rate")]
    flow_whole <- full_join(flow_whole, flow_flow, by = c("reghos", "regpaz"))
  }
  
}


passive_whole <- data.frame(passive_whole, average = rowMeans(passive_whole[, c(4:ncol(passive_whole))], na.rm=TRUE))
active_whole <- data.frame(active_whole, average = rowMeans(active_whole[, c(4:ncol(active_whole))], na.rm=TRUE))
flow_whole <- data.frame(flow_whole, average = rowMeans(flow_whole[, c(4:ncol(flow_whole))], na.rm=TRUE))

df_res<-data.frame(matrix(nrow = 0, ncol = 5))
names(df_res) <- c("session", "r2_passive_regr", "r2_passive_real", "r2_active_regr", "r2_active_real")  

for (i in 4:n_files) {
  passive_regression <- lm(passive_whole[, i] ~ regression, passive_whole)
  passive_real <- lm(passive_whole[, i] ~ real, passive_whole)
  active_regression <- lm(active_whole[, i] ~ regression, active_whole)
  active_real <- lm(active_whole[, i] ~ real, active_whole)
  df_res.singleDoc <- data.frame(i-3, summary(passive_regression)$r.squared, summary(passive_real)$r.squared, summary(active_regression)$r.squared, summary(active_real)$r.squared)
  names(df_res.singleDoc) <- c("session", "r2_passive_regr", "r2_passive_real", "r2_active_regr", "r2_active_real")  
  df_res <- rbind(df_res, df_res.singleDoc)
}

#passive_btw_data <- data.frame(passive_btw_data, average = rowMeans(passive_whole[, c(4:ncol(passive_whole))], na.rm=TRUE))
#active_btw_data <- data.frame(active_btw_data, average = rowMeans(active_whole[, c(4:ncol(active_whole))], na.rm=TRUE))

# intraclass correlation coefficient 
passive_icc_data <- passive_whole[, c(4:n_files)] 
passive_icc_result_precision <- icc(passive_icc_data, model = "twoway", type = "agreement", unit = "single")
passive_icc_result_accuracy_regression <- icc(data.frame(average = passive_whole[,c(ncol(passive_icc_data))], regression = passive_whole[, c(2)]), model = "twoway", type = "agreement", unit = "average")
passive_icc_result_accuracy_real <- icc(data.frame(average = passive_whole[,c(ncol(passive_icc_data))], regression = passive_whole[, c(3)]), model = "twoway", type = "agreement", unit = "average")
active_icc_data <- active_whole[,c(4:n_files)] 
active_icc_result_precision <- icc(active_icc_data, model = "twoway", type = "agreement", unit = "single")
active_icc_result_accuracy_regression <- icc(data.frame(average = active_whole[,c(ncol(active_whole))], regression = active_whole[, c(2)]), model = "twoway", type = "agreement", unit = "average")
active_icc_result_accuracy_real <- icc(data.frame(average = active_whole[,c(ncol(active_whole))], regression = active_whole[, c(3)]), model = "twoway", type = "agreement", unit = "average")

#whole <- read_excel(paste(folder,"output-for-repeat.xlsx", sep = ""), sheet = "Foglio2")
flow_icc_data <- flow_whole[,c(4:n_files)] 
passive_icc_result_precision_all <- icc(flow_icc_data, model = "twoway", type = "agreement", unit = "single")
passive_icc_result_precision_no_intra <- icc(flow_icc_data[flow_icc_data$regpaz!=whole$flow_icc_data,], model = "twoway", type = "agreement", unit = "single")

icc_res<-data.frame(matrix(nrow = 0, ncol = 5))
names(icc_res) <- c("what", "icc_value", "lower_bound", "upper_bound", "p_value")  
icc_res.singleDoc <- data.frame("passive precision", passive_icc_result_precision$value, passive_icc_result_precision$lbound, passive_icc_result_precision$ubound, passive_icc_result_precision$p.value)
names(icc_res.singleDoc) <- c("what", "icc_value", "lower_bound", "upper_bound", "p_value")  
icc_res <- rbind(icc_res, icc_res.singleDoc)
icc_res.singleDoc <- data.frame("passive accuracy regression", passive_icc_result_accuracy_regression$value, passive_icc_result_accuracy_regression$lbound, passive_icc_result_accuracy_regression$ubound, passive_icc_result_accuracy_regression$p.value)
names(icc_res.singleDoc) <- c("what", "icc_value", "lower_bound", "upper_bound", "p_value")  
icc_res <- rbind(icc_res, icc_res.singleDoc)
icc_res.singleDoc <- data.frame("passive accuracy real", passive_icc_result_accuracy_real$value, passive_icc_result_accuracy_real$lbound, passive_icc_result_accuracy_real$ubound, passive_icc_result_accuracy_real$p.value)
names(icc_res.singleDoc) <- c("what", "icc_value", "lower_bound", "upper_bound", "p_value")  
icc_res <- rbind(icc_res, icc_res.singleDoc)
icc_res.singleDoc <- data.frame("active precision", active_icc_result_precision$value, active_icc_result_precision$lbound, active_icc_result_precision$ubound, active_icc_result_precision$p.value)
names(icc_res.singleDoc) <- c("what", "icc_value", "lower_bound", "upper_bound", "p_value")  
icc_res <- rbind(icc_res, icc_res.singleDoc)
icc_res.singleDoc <- data.frame("active accuracy regression", active_icc_result_accuracy_regression$value, active_icc_result_accuracy_regression$lbound, active_icc_result_accuracy_regression$ubound, active_icc_result_accuracy_regression$p.value)
names(icc_res.singleDoc) <- c("what", "icc_value", "lower_bound", "upper_bound", "p_value")  
icc_res <- rbind(icc_res, icc_res.singleDoc)
icc_res.singleDoc <- data.frame("active accuracy real", active_icc_result_accuracy_real$value, active_icc_result_accuracy_real$lbound, active_icc_result_accuracy_real$ubound, active_icc_result_accuracy_real$p.value)
names(icc_res.singleDoc) <- c("what", "icc_value", "lower_bound", "upper_bound", "p_value")  
icc_res <- rbind(icc_res, icc_res.singleDoc)
icc_res.singleDoc <- data.frame("patient flow precision all", passive_icc_result_precision_all$value, passive_icc_result_precision_all$lbound, passive_icc_result_precision_all$ubound, passive_icc_result_precision_all$p.value)
names(icc_res.singleDoc) <- c("what", "icc_value", "lower_bound", "upper_bound", "p_value")  
icc_res <- rbind(icc_res, icc_res.singleDoc)
icc_res.singleDoc <- data.frame("patient flow precision no intra", passive_icc_result_precision_no_intra$value, passive_icc_result_precision_no_intra$lbound, passive_icc_result_precision_no_intra$ubound, passive_icc_result_precision_no_intra$p.value)
names(icc_res.singleDoc) <- c("what", "icc_value", "lower_bound", "upper_bound", "p_value")  
icc_res <- rbind(icc_res, icc_res.singleDoc)

print(df_res)
print(icc_res)



#############################################################################################################################
#
# ANALISI NUMEROSITA' SESSIONI PER HOTSPOT
#
#############################################################################################################################

library(readxl)
library(writexl)
library(dplyr)
library(ggplot2)
library(irr)
#library(pivottabler)

folder<-"C:/Dati/Dropbox/Progetti/progetto siveas/lavori COVID/spatial accessibility/GitHubHealthcare/ABM4PatientMobility/output/"
folder<-"E:/Dropbox/Progetti/progetto siveas/lavori COVID/spatial accessibility/GitHubHealthcare/ABM4PatientMobility/output/"

# all-fixed-april-16
# all-random-april-16
# all-60-tests
# all-60-tests-fixed

files <- list.files(path=paste(folder, "all-60-tests-fixed", sep = ""), pattern="*.csv", full.names=TRUE, recursive=FALSE)
n_files<-length(files)+4-1
dataSimulationAgg <- data.frame(matrix(nrow = 0, ncol = 17))
i<-51
for (j in 1:length(files)) {
  print(j)
  dataSimulationAll<-read.csv(files[j], TRUE, "|")
  dataSimulation <- dataSimulationAll[dataSimulationAll$week_treated >= i & dataSimulationAll$week_treated <= (i + 51), ]
  dataSimulation$session <- j
  dataSimulationAgg <- rbind(dataSimulationAgg, dataSimulation)
}

df_res<-data.frame(matrix(nrow = 0, ncol = 3))
names(df_res) <- c("session", "r_squared", "SE")  

for (j in 2:(length(files)-1)) {
  print(j)
  data_pre <- dataSimulationAgg[dataSimulationAgg$session %in% c(1:j-1),] 
  data_post <- dataSimulationAgg[dataSimulationAgg$session %in% c(1:j),]
  
  # calcoli per mobilità pre
  passive_mobility <- data_pre %>% count(id_municipality,stay)
  passive_stayed <- passive_mobility %>% filter(passive_mobility$stay=="true") 
  passive_moved <- passive_mobility %>% filter(passive_mobility$stay=="false")
  passive_merged <- full_join(passive_stayed, passive_moved, by = c("id_municipality"))
  passive_merged <- passive_merged[, c(1,3,5)]
  passive_merged[is.na(passive_merged)] <- 0
  passive_merged$sum <- passive_merged$n.x + passive_merged$n.y
  passive_merged$mobility <- passive_merged$n.y / passive_merged$sum
  passive_merged_pre <- passive_merged[, c(1,5)]
 
  # calcoli per mobilità post
  passive_mobility <- data_post %>% count(id_municipality,stay)
  passive_stayed <- passive_mobility %>% filter(passive_mobility$stay=="true") 
  passive_moved <- passive_mobility %>% filter(passive_mobility$stay=="false")
  passive_merged <- full_join(passive_stayed, passive_moved, by = c("id_municipality"))
  passive_merged <- passive_merged[, c(1,3,5)]
  passive_merged[is.na(passive_merged)] <- 0
  passive_merged$sum <- passive_merged$n.x + passive_merged$n.y
  passive_merged$mobility <- passive_merged$n.y / passive_merged$sum
  passive_merged_post <- passive_merged[, c(1,5)]
  
  # merged
  passive_merged_cfn <- full_join(passive_merged_pre, passive_merged_post, by = c("id_municipality"))
  names(passive_merged_cfn) <- c("id_municipality", "mobility_pre", "mobility_post")
  passive_merged_cfn$mobility_rms<-((passive_merged_cfn$mobility_pre - passive_merged_cfn$mobility_post)^2)
  names(passive_merged_cfn) <- c("id_municipality", "mobility_pre", "mobility_post", "mobility_rms")
  
  # errore standard
  passive_r_squared <- lm(mobility_pre ~ mobility_post, passive_merged_cfn)

  df_res.singleDoc <- data.frame(j, summary(passive_r_squared)$r.squared, sum(passive_merged_cfn$mobility_rms, na.rm = TRUE)/(length(passive_merged_cfn$mobility_rms)-2))
  names(df_res.singleDoc) <- c("session", "r_squared", "SE")  
  df_res <- rbind(df_res, df_res.singleDoc)
  
}

write.csv(passive_merged, paste(folder,"output-analysis-hotspot.csv",sep = ""))


#############################################################################################################################
#
# ANALISI MOBILITA' ATTIVA E PASSIVA SU SINGOLO FILE
#
#############################################################################################################################

#analisi passiva
dataSimulation<-read.csv(paste(folder,"output.csv",sep = ""), TRUE, "|")
dataMobility <- read_excel(paste(folder,"mobility_data.xlsx", sep = ""), sheet = "Foglio1")
dataMobilityRegion <- read_excel(paste(folder,"mobility_data_region.xlsx", sep = ""), sheet = "Foglio1")
passive_mobility <- dataSimulation %>% count(propaz,stay)
passive_mobility_space <- dataSimulation %>% count(propaz,b) 
passive_mobility_nospace <- passive_mobility_space %>% filter(passive_mobility_space$b==0)
passive_mobility <- passive_mobility[order(passive_mobility$propaz),]
passive_stayed <- passive_mobility %>% filter(passive_mobility$stay=="true")
dataMobility <- dataMobility[order(dataMobility$propaz),]
passive_moved <- passive_mobility %>% filter(passive_mobility$stay=="false")
passive_merged <- data.frame(province = passive_stayed$propaz, province2 = dataMobility$propaz, stayed = passive_stayed$n, moved = passive_moved$n, mobility = (passive_moved$n / (passive_moved$n + passive_stayed$n)), regression = dataMobility$passive_regression, real = dataMobility$passive_original)
passive_regression <- lm(mobility ~ regression, passive_merged)
passive_real <- lm(mobility ~ real, passive_merged)
summary(passive_regression)$r.squared
summary(passive_real)$r.squared

# analisi attiva
active_flow <- dataSimulation %>% count(reghos,regpaz)
active_mobility <- dataSimulation %>% count(reghos,stay)
active_stayed <- active_mobility %>% filter(active_mobility$stay=="true")
active_moved <- active_mobility %>% filter(active_mobility$stay=="false")
active_merged <- data.frame(region = active_stayed$reghos, region2 = dataMobilityRegion$`Etichette di riga`, local = active_stayed$n, active = active_moved$n, total = active_stayed$n + active_moved$n, mobility = (active_moved$n / (active_moved$n + active_stayed$n)), regression = dataMobilityRegion$active_regression, real = dataMobilityRegion$active_real, interventions = dataMobilityRegion$interventions, difference = dataMobilityRegion$interventions - (active_stayed$n + active_moved$n), percentage = (dataMobilityRegion$interventions - (active_stayed$n + active_moved$n)) / (active_stayed$n + active_moved$n))
active_regression <- lm(mobility ~ regression, active_merged)
active_real <- lm(mobility ~ real, active_merged)
summary(active_regression)$r.squared
summary(active_real)$r.squared

# plot and data
plot(passive_merged$regression, passive_merged$mobility)
text(passive_merged$regression, passive_merged$mobility, labels=passive_merged$province)
abline(passive_regression)
View(passive_merged)

# plot and data
barplot(active_merged$difference, names.arg = active_merged$region, las = 2)
barplot(active_merged$difference/active_merged$total, names.arg = active_merged$region, las = 2)
plot(active_merged$regression, active_merged$mobility)
text(active_merged$regression, active_merged$mobility, labels=active_merged$region)
abline(active_regression)
View(active_merged)


#############################################################################################################################
#
# WINDOWING: ANALISI MOBILITA' ATTIVA E PASSIVA SU SINGOLO FILE
#
#############################################################################################################################

# analisi passiva
ll<-40
ul<-max(dataSimulationAll$week_treated)-51
#ul<-52
dataSimulationAll<-read.csv(paste(folder,"output.csv",sep = ""), TRUE, "|")
dataMobility <- read_excel(paste(folder,"mobility_data.xlsx", sep = ""), sheet = "Foglio1")
dataMobilityRegion <- read_excel(paste(folder,"mobility_data_region.xlsx", sep = ""), sheet = "Foglio1")
df_res<-data.frame(matrix(nrow = 0, ncol = 6))
names(df_res) <- c("week_start", "patients", "r2_passive_regr", "r2_passive_real", "r2_active_regr", "r2_active_real")  

dataSimulation <- dataSimulationAll
passive_mobility <- dataSimulation %>% count(propaz,stay)
passive_mobility_space <- dataSimulation %>% count(propaz,b) 
passive_mobility_nospace <- passive_mobility_space %>% filter(passive_mobility_space$b==0)
passive_mobility <- passive_mobility[order(passive_mobility$propaz),]
passive_stayed <- passive_mobility %>% filter(passive_mobility$stay=="true")
dataMobility <- dataMobility[order(dataMobility$propaz),]
passive_moved <- passive_mobility %>% filter(passive_mobility$stay=="false")
passive_merged <- data.frame(province = passive_stayed$propaz, province2 = dataMobility$propaz, stayed = passive_stayed$n, moved = passive_moved$n, mobility = (passive_moved$n / (passive_moved$n + passive_stayed$n)), regression = dataMobility$passive_regression, real = dataMobility$passive_original)
passive_regression <- lm(mobility ~ regression, passive_merged)
passive_real <- lm(mobility ~ real, passive_merged)
r2_passive_regr<-summary(passive_regression)$r.squared
r2_passive_real<-summary(passive_real)$r.squared

# analisi attiva
active_flow <- dataSimulation %>% count(reghos,regpaz)
active_mobility <- dataSimulation %>% count(reghos,stay)
active_stayed <- active_mobility %>% filter(active_mobility$stay=="true")
active_moved <- active_mobility %>% filter(active_mobility$stay=="false")
active_merged <- data.frame(region = active_stayed$reghos, region2 = dataMobilityRegion$`Etichette di riga`, local = active_stayed$n, active = active_moved$n, moved=active_moved$n, total = active_stayed$n + active_moved$n, patients=active_stayed$n+active_moved$n, mobility = (active_moved$n / (active_moved$n + active_stayed$n)), regression = dataMobilityRegion$active_regression, real = dataMobilityRegion$active_real, interventions = dataMobilityRegion$interventions, difference = dataMobilityRegion$interventions - (active_stayed$n + active_moved$n), percentage = (dataMobilityRegion$interventions - (active_stayed$n + active_moved$n)) / (active_stayed$n + active_moved$n))
active_regression <- lm(mobility ~ regression, active_merged)
active_real <- lm(mobility ~ real, active_merged)
r2_active_regr<-summary(active_regression)$r.squared
r2_active_real<-summary(active_real)$r.squared

df_res.singleDoc <- data.frame(0, length(dataSimulation[,1]), r2_passive_regr, r2_passive_real, r2_active_regr, r2_active_real)
names(df_res.singleDoc) <- c("week_start", "patients", "r2_passive_regr", "r2_passive_real", "r2_active_regr", "r2_active_real")  
df_res <- rbind(df_res, df_res.singleDoc)


for(i in ll:ul) { 
  dataSimulation <- dataSimulationAll[dataSimulationAll$week_treated >= i & dataSimulationAll$week_treated <= (i + 51), ]
  passive_mobility <- dataSimulation %>% count(propaz,stay)
  passive_mobility_space <- dataSimulation %>% count(propaz,b) 
  passive_mobility_nospace <- passive_mobility_space %>% filter(passive_mobility_space$b==0)
  passive_mobility <- passive_mobility[order(passive_mobility$propaz),]
  passive_stayed <- passive_mobility %>% filter(passive_mobility$stay=="true")
  dataMobility <- dataMobility[order(dataMobility$propaz),]
  passive_moved <- passive_mobility %>% filter(passive_mobility$stay=="false")
  passive_merged <- data.frame(province = passive_stayed$propaz, province2 = dataMobility$propaz, stayed = passive_stayed$n, moved = passive_moved$n, mobility = (passive_moved$n / (passive_moved$n + passive_stayed$n)), regression = dataMobility$passive_regression, real = dataMobility$passive_original)
  passive_regression <- lm(mobility ~ regression, passive_merged)
  passive_real <- lm(mobility ~ real, passive_merged)
  r2_passive_regr<-summary(passive_regression)$r.squared
  r2_passive_real<-summary(passive_real)$r.squared
  
  # analisi attiva
  active_flow <- dataSimulation %>% count(reghos,regpaz)
  active_mobility <- dataSimulation %>% count(reghos,stay)
  active_stayed <- active_mobility %>% filter(active_mobility$stay=="true")
  active_moved <- active_mobility %>% filter(active_mobility$stay=="false")
  active_merged <- data.frame(region = active_stayed$reghos, region2 = dataMobilityRegion$`Etichette di riga`, local = active_stayed$n, active = active_moved$n, moved=active_moved$n, total = active_stayed$n + active_moved$n, patients=active_stayed$n+active_moved$n, mobility = (active_moved$n / (active_moved$n + active_stayed$n)), regression = dataMobilityRegion$active_regression, real = dataMobilityRegion$active_real, interventions = dataMobilityRegion$interventions, difference = dataMobilityRegion$interventions - (active_stayed$n + active_moved$n), percentage = (dataMobilityRegion$interventions - (active_stayed$n + active_moved$n)) / (active_stayed$n + active_moved$n))
  active_regression <- lm(mobility ~ regression, active_merged)
  active_real <- lm(mobility ~ real, active_merged)
  r2_active_regr<-summary(active_regression)$r.squared
  r2_active_real<-summary(active_real)$r.squared
  
  df_res.singleDoc <- data.frame(i, length(dataSimulation[,1]), r2_passive_regr, r2_passive_real, r2_active_regr, r2_active_real)
  names(df_res.singleDoc) <- c("week_start", "patients", "r2_passive_regr", "r2_passive_real", "r2_active_regr", "r2_active_real")  
  df_res <- rbind(df_res, df_res.singleDoc)
  
}

bp<-barplot(df_res$r2_passive_regr, names.arg = df_res$week_start, las = 2, ylim=c(0,1))
abline(h=0)
text(bp, df_res$r2_passive_regr, labels = round(df_res$r2_passive_regr, digits = 3))

bp<-barplot(df_res$r2_passive_real, names.arg = df_res$week_start, las = 2, ylim=c(0,1))
abline(h=0)
text(bp, df_res$r2_passive_real, labels = round(df_res$r2_passive_real, digits = 3))

bp<-barplot(df_res$r2_active_regr, names.arg = df_res$week_start, las = 2, ylim=c(0,1))
abline(h=0)
text(bp, df_res$r2_active_regr, labels = round(df_res$r2_active_regr, digits = 3))

bp<-barplot(df_res$r2_active_real, names.arg = df_res$week_start, las = 2, ylim=c(0,1))
abline(h=0)
text(bp, df_res$r2_active_real, labels = round(df_res$r2_active_real, digits = 3))

plot(active_merged$regression, active_merged$mobility)
text(active_merged$regression, active_merged$mobility, labels=active_merged$region)
abline(active_regression)


plot(passive_merged$regression, passive_merged$mobility)
text(passive_merged$regression, passive_merged$mobility, labels=passive_merged$province)
abline(passive_regression)

df_comp<-data.frame(region=active_merged$region,real=round(active_merged$interventions/sum(active_merged$interventions)*sum(active_merged$total)), simul=active_merged$total)
barplot((df_comp$simul-df_comp$real)/df_comp$real, names.arg = df_comp$region, las = 2)

#############################################################################################################################
#
# ???????????????????????
#
#############################################################################################################################


# intraclass correlation coefficient (precision)
passive_icc_data <- passive_whole[, c(4:8)] 
passive_icc_result_precision <- icc(passive_icc_data, model = "twoway", type = "agreement", unit = "single")
passive_icc_result_accuracy <- icc(data.frame(average = passive_icc_data[,c(ncol(passive_icc_data))], regression = passive_whole[, c(3)]), model = "twoway", type = "agreement", unit = "average")
active_icc_data <- active_whole[,c(4:8)] 
active_icc_result_precision <- icc(active_icc_data, model = "twoway", type = "agreement", unit = "single")
active_icc_result_accuracy <- icc(data.frame(average = active_icc_data[,c(ncol(active_icc_data))], regression = active_whole[, c(3)]), model = "twoway", type = "agreement", unit = "average")

# correlation coefficient average vs. true values 
passive_regression <- lm(average ~ regression, passive_whole)
passive_real <- lm(average ~ real, passive_whole)
summary(passive_regression)$r.squared
summary(passive_real)$r.squared
active_regression <- lm(average ~ regression, active_whole)
active_real <- lm(average ~ real, active_whole)
summary(active_regression)$r.squared
summary(active_real)$r.squared


######################################


dataSimulation<-read.csv(paste(folder,"output-4.csv",sep = ""), TRUE, "|")
mobility_flow <- dataSimulation %>% count(reghos,regpaz)
mobility_flow <- mobility_flow %>% mutate(newcol = 4)
# new
mobility_flow_all <- mobility_flow
# add
mobility_flow_all <- rbind(mobility_flow_all, mobility_flow)

mobility_flow_all_data <- mobility_flow_all %>% group_by(reghos,regpaz,newcol) %>% summarise(sum_pat=sum(n), .groups = 'drop')
mobility_flow_all_data<-mobility_flow_all_data[!(mobility_flow_all_data$reghos==mobility_flow_all_data$regpaz),]
mobility_flow_all_data <- data.frame(reghos_pas = paste(mobility_flow_all_data$reghos, " - ", mobility_flow_all_data$regpaz), mobility_flow_all_data)
mobility_flow_wide <- reshape(mobility_flow_all_data[,-c(2,3)], idvar = "reghos_pas", timevar = "newcol", direction = "wide")
mobility_flow_wide[is.na(mobility_flow_wide)] <- 0
mobility_flow_wide <- mobility_flow_wide[,-c(1)]
colSum <- colSums(mobility_flow_wide)
mobility_flow_wide_norm <- mobility_flow_wide / colSum
icc(mobility_flow_wide, model = "twoway", type = "agreement", unit = "single")

#######################################

folderPopolazione<-"C:/Dati/Dropbox/Progetti/progetto siveas/lavori COVID/spatial accessibility/GitHubHealthcare/ABM4PatientMobility/additional_files/"
folderPopolazione<-"E:/Dropbox/Progetti/progetto siveas/lavori COVID/spatial accessibility/GitHubHealthcare/ABM4PatientMobility/additional_files/"

datiPopolazione <- read_excel(paste(folderPopolazione,"estrazione_elenco_popolazione_ABM4health_v2.xlsm",sep = ""), sheet = "dati_long")
idPopolazione<-datiPopolazione[,c(1)]
probPopolazione<-datiPopolazione[,c("probabilità")]
idSamplePopolazione<-sample(as.numeric(unlist(idPopolazione)), size = 52000, replace = TRUE, prob = as.numeric(unlist(probPopolazione)))
dataSamplePopolazione<-datiPopolazione[idSamplePopolazione,]

datiPopolazione[datiPopolazione$id==1,c("individui")] = datiPopolazione[datiPopolazione$id==1,c("individui")] + 1

write_xlsx(dataSamplePopolazione, paste(folderPopolazione,"popolazione_estratta.xlsx",sep = ""), sheet = "result")



datiPopolazioneAgg <- read_excel(paste(folderPopolazione,"estrazione_elenco_popolazione_ABM4health_v2.xlsm",sep = ""), sheet = "dati_long_agg")
idPopolazioneAgg<-datiPopolazioneAgg[,c(1)]
probPopolazioneAgg<-datiPopolazioneAgg[,c("probabilità")]
idSamplePopolazioneAgg<-sample(as.numeric(unlist(idPopolazioneAgg)), size = 52000, replace = TRUE, prob = as.numeric(unlist(probPopolazioneAgg)))
dataSamplePopolazioneAgg<-datiPopolazioneAgg[idSamplePopolazioneAgg,]

write_xlsx(dataSamplePopolazioneAgg, paste(folderPopolazione,"popolazione_estratta_agg.xlsx",sep = ""))





######################################

library(leaps)
library(readxl)
dati <- read_excel("C:/Dati/Dropbox/Progetti/progetto siveas/lavori COVID/spatial accessibility/GitHubHealthcare/ABM4PatientMobility/output/datiAncaABM_xls.xlsx", sheet = "Foglio1")
regsubsets.out <- regsubsets(mobility~., data=dati, nbest = 1, nvmax = NULL, force.in = NULL, force.out = NULL, method = "exhaustive")
summary.out <- summary(regsubsets.out)
summary.out$which[which.max(summary.out$adjr2),]
selected<-summary.out$which[which.max(summary.out$adjr2),]
removed<-which(selected==FALSE)
mod <- lm(paste('mobility~',paste(colnames(dati)[-c(removed-1,ncol(dati))], collapse = '+')), data=dati)
summary(mod)

mod2 <- lm(paste('mobility~',paste(colnames(dati)[-c(5,8,ncol(dati))], collapse = '+')), data=dati)
summary(mod2)

mod_all <- lm(paste('mobility~',paste(colnames(dati)[-c(ncol(dati))], collapse = '+')), data=dati)
summary(mod_all)

mod_last <- lm(paste('staying~',paste(colnames(dati)[-c(1,2,8,9,10,11)], collapse = '+')), data=dati)
summary(mod_last)



nonlin.eq <- function(waiting, satisfaction, int_intra, ret_intra, bed_intra, int_extra, ret_extra, bed_extra, c1, c2, c3, c4, c5, cost) {
  cost + c1 * waiting + c2 * satisfaction + c3 * (int_extra / (int_intra + int_extra)) + c4 * (ret_extra / (ret_intra + ret_extra)) + c5 * (bed_extra / (bed_intra + bed_extra))
}

nonlin.eq(106.2358439,46.2,163.7485851,0.018596491,17.90506127,263.0112559,0.003502233,4.261737681,1,2,3,4,5,6,7,8,100)

nl_model <- nls(mobility ~ nonlin.eq(waiting, satisfaction, int_intra, ret_intra, bed_intra, int_extra, ret_extra, bed_extra, c1, c2, c3, c4, c5, cost), 
                data = dati2, start = list(c1 = 1, c2 = 1, c3 = 1, c4 = 1, c5 = 1, cost = 1), trace = T)





nonlin.eq <- function(waiting, satisfaction, bed_intra, bed_extra, c, c1, c2, cost) {
  cost + c1 * waiting + c2 * satisfaction + c * (bed_extra / (bed_intra))
}

nl_model <- nls(mobility ~ nonlin.eq(waiting, satisfaction, bed_intra, bed_extra, c, c1, c2, cost), 
                data = dati2, start = list(c = 1, c1 = 1, c2 = 1, cost = 1), trace = T)

(modelr::rsquare(nl_model, dati2))







nl_model

' nls(formula, data, start, control, algorithm, trace, subset, weights, na.action, model, lower, upper, . . .)
