install.packages("meta")
install.packages("readxl")
install.packages("rmeta")
library(meta)
library(rmeta)
library(dplyr)
library(ggplot2)
library(readxl)


data <- read_excel("Forest_plot_data_2025-07-29.xlsx", sheet= "HGL")

###------------------------------------------------------------------------
library(meta)
data <- data %>%
  mutate(logTE = log(data$`Risk ratio`), logLower = log(data$`95%CI low` ), logUpper = log(data$`95%CI up`))
meta_all <- metagen(TE = logTE, lower = logLower, upper = logUpper,
                    studlab = data$`Author, year`, data = data,
                    sm = "RR", method.I2 = "Q" , subgroup = Group)
head(meta_all)


forest(meta_all,
       xlab = "Risk Ratio (IC 95%)",  
       leftcols = "studlab",          
       rightcols = c("effect", "ci"),
       col.diamond = "blue",         
       col.study = "black",           
       col.square = "black",            
       col.subgroup = "darkgreen",          
       print.tau2 = FALSE,             
       print.I2 = TRUE, 
       common = FALSE ,
       random = TRUE ,
       test.subgroup = FALSE,
       comb.random = TRUE,
       print.pval.Q = TRUE, 
       print.pval.subgroup = FALSE,
       backtransf = TRUE)             

#Edit the summary forest plot Cervical Cancer risk

ata <- read_excel("Forest_plot_data_2025-07-29.xlsx", sheet= "CC")
data <- data %>% mutate(`Author, year` = factor(`Author, year`, levels = rev(unique(`Author, year`))))
data <- data %>%
  mutate(logTE = log(data$`Risk ratio`), logLower = log(data$`95%CI low` ), logUpper = log(data$`95%CI up`))
meta_all <- metagen(TE = logTE, lower = logLower, upper = logUpper
                    , data = data,
                    sm = "RR", method.I2 = "Q" , subgroup = Group)



forest(meta_all,
       xlab = "Risk Ratio (IC 95%)",  
       leftcols = "studlab",          
       rightcols = c("effect", "ci"),
       col.diamond = "blue",         
       col.study = "black",           
       col.square = "black",            
       col.subgroup = "darkgreen",          
       print.tau2 = FALSE,             
       print.I2 = TRUE, 
       common = FALSE ,
       random = TRUE ,
       test.subgroup = FALSE,
       study.results = FALSE,
       comb.random = TRUE,
       print.pval.Q = TRUE, 
       print.pval.subgroup = FALSE,
       backtransf = TRUE)     

#Edit the summary forest plot HGL risk

data <- read_excel("Forest_plot_data_2025-07-29.xlsx", sheet= "HGL")
data <- data %>% mutate(`Author, year` = factor(`Author, year`, levels = rev(unique(`Author, year`))))
data <- data %>%
  mutate(logTE = log(data$`Risk ratio`), logLower = log(data$`95%CI low` ), logUpper = log(data$`95%CI up`))
meta_all <- metagen(TE = logTE, lower = logLower, upper = logUpper
                    , data = data,
                    sm = "RR", method.I2 = "Q" , subgroup = Group)



forest(meta_all,
       xlab = "Risk Ratio (IC 95%)",  
       leftcols = "studlab",          
       rightcols = c("effect", "ci"),
       col.diamond = "blue",         
       col.study = "black",           
       col.square = "black",            
       col.subgroup = "darkgreen",          
       print.tau2 = FALSE,             
       print.I2 = TRUE, 
       common = FALSE ,
       random = TRUE ,
       test.subgroup = FALSE,
       study.results = FALSE,
       comb.random = TRUE,
       print.pval.Q = TRUE, 
       print.pval.subgroup = FALSE,
       backtransf = TRUE)     


#Forest plot CC by country OECD
data <- read_excel("MA_data_country_2025-03-01.xlsx", sheet= "CC")
data <- data %>% mutate(`Author, year` = factor(`Author, year`, levels = rev(unique(`Author, year`))))

library(meta)
data <- data %>%
  mutate(logTE = log(data$`Risk ratio`), logLower = log(data$`95%CI low` ), logUpper = log(data$`95%CI up`))
meta_all <- metagen(TE = logTE, lower = logLower, upper = logUpper,
                    studlab = data$`Author, year`, data = data,
                    sm = "RR", method.I2 = "Q" , subgroup = data$OECD_classification)



forest(meta_all,
       xlab = "Risk Ratio (IC 95%)",  
       leftcols = "studlab",          
       rightcols = c("effect", "ci"),
       col.diamond = "blue",         
       col.study = "black",           
       col.square = "black",            
       col.subgroup = "darkgreen",          
       print.tau2 = FALSE,             
       print.I2 = TRUE, 
       common = FALSE ,
       random = TRUE ,
       test.subgroup = FALSE,
       comb.random = TRUE,
       print.pval.Q = TRUE, 
       subgroup = FALSE,
       print.pval.subgroup = FALSE,
       backtransf = TRUE) 


#---------------------------------------------------------------------------------------------------------------------------------
#Funnel plot CC 
data <- read_excel("Forest_plot_data_2025-03-06_v3.xlsx", sheet= "CC")
data <- data %>%
  mutate(logTE = log(data$`Risk ratio`), logLower = log(data$`95%CI low` ), logUpper = log(data$`95%CI up`))

head(data)
str(data)
names(data)
summary(data$...6)
all(is.na(data$`...6`))
data <- data[, !names(data) %in% "...6"]
head(data)
min(data$logTE)
max(data$logTE)
which(is.infinite(data$logTE) & data$logTE == -Inf)
print(data[37,])

#CC pooled
meta_cc<-  metagen(TE = logTE, lower = logLower, upper = logUpper,
                   studlab = data$`Author, year`, data = data,
                   sm = "RR", random= TRUE, method.I2 = "Q" , subgroup = Group)

funnel(meta_cc, random = TRUE, backtransf = TRUE)



# 1- WLses
meta_lses<-  metagen(TE = logTE, lower = logLower, upper = logUpper,
                     studlab = data$`Author, year`, data = data,
                     sm = "RR", random= TRUE, method.I2 = "Q" , subgroup = Group, subset = Group == "WLSES")

funnel(meta_lses, random = TRUE, backtransf = TRUE)

funnel(meta_lses)
#2-WLHIV :

meta_hiv<-  metagen(TE = logTE, lower = logLower, upper = logUpper,
                    studlab = data$`Author, year`, data = data,
                    sm = "RR", random= TRUE, method.I2 = "Q" , subgroup = Group, subset = Group == "WLWH")

funnel(meta_hiv, random = TRUE, backtransf = TRUE)

#3- Migrants 

meta_mig<-  metagen(TE = logTE, lower = logLower, upper = logUpper,
                    studlab = data$`Author, year`, data = data,
                    sm = "RR", random= TRUE, method.I2 = "Q" , subgroup = Group, subset = Group == "Migrants")

funnel(meta_mig, random = TRUE, backtransf = TRUE)

#4-WSUD
meta_sud<-  metagen(TE = logTE, lower = logLower, upper = logUpper,
                    studlab = data$`Author, year`, data = data,
                    sm = "RR", random= TRUE, method.I2 = "Q" , subgroup = Group, subset = Group == "WSUD")

funnel(meta_sud, random = TRUE, backtransf = TRUE)

#5- WMI

meta_mental<-  metagen(TE = logTE, lower = logLower, upper = logUpper,
                       studlab = data$`Author, year`, data = data,
                       sm = "RR", random= TRUE, method.I2 = "Q" , subgroup = Group, subset = Group == "WMI")

funnel(meta_mental, random = TRUE, backtransf = TRUE)

#6- Prisoners


meta_prison<-  metagen(TE = logTE, lower = logLower, upper = logUpper,
                       studlab = data$`Author, year`, data = data,
                       sm = "RR", random= TRUE, method.I2 = "Q" , subgroup = Group, subset = Group == "Prisoners")

funnel(meta_prison, random = TRUE, backtransf = TRUE)

# Funnel plot HGL
data2 <- read_excel("Forest_plot_data_2025-07-29.xlsx", sheet= "HGL")
data2 <- data2 %>%
  mutate(logTE = log(data2$`Risk ratio`), logLower = log(data2$`95%CI low` ), logUpper = log(data2$`95%CI up`))
head(data2)

#HGL pooled 
meta_hgl<-  metagen(TE = logTE, lower = logLower, upper = logUpper,
                    studlab = data2$`Author, year`, data = data2,
                    sm = "RR", random= TRUE, method.I2 = "Q" , subgroup = Group)

funnel(meta_hgl, random = TRUE, backtransf = TRUE)

# 1- WLses
meta_lses<-  metagen(TE = logTE, lower = logLower, upper = logUpper,
                     studlab = data$`Author, year`, data = data2,
                     sm = "RR", random= TRUE, method.I2 = "Q" , subgroup = Group, subset = Group == "WLSES")

funnel(meta_lses, random = TRUE, backtransf = FALSE)

#2-WLHIV 

meta_hiv<-  metagen(TE = logTE, lower = logLower, upper = logUpper,
                    studlab = data2$`Author, year`, data = data2,
                    sm = "RR", random= TRUE, method.I2 = "Q" , subgroup = Group, subset = Group == "WLWH")


funnel(meta_hiv, random = TRUE, backtransf = TRUE)



#3- Migrants 

meta_mig<-  metagen(TE = logTE, lower = logLower, upper = logUpper,
                    studlab = data$`Author, year`, data = data2,
                    sm = "RR", random= TRUE, method.I2 = "Q" , subgroup = Group, subset = Group == "Migrants")

funnel(meta_mig, random = TRUE, backtransf = TRUE)

#4-WSUD
meta_sud<-  metagen(TE = logTE, lower = logLower, upper = logUpper,
                    studlab = data$`Author, year`, data = data2,
                    sm = "RR", random= TRUE, method.I2 = "Q" , subgroup = Group, subset = Group == "WSUD")

funnel(meta_sud, random = TRUE, backtransf = TRUE)

#5- WMI

meta_mental<-  metagen(TE = logTE, lower = logLower, upper = logUpper,
                       studlab = data$`Author, year`, data = data2,
                       sm = "RR", random= TRUE, method.I2 = "Q" , subgroup = Group, subset = Group == "WMI")

funnel(meta_mental, random = TRUE, backtransf = TRUE)

#6- Prisoners


meta_prison<-  metagen(TE = logTE, lower = logLower, upper = logUpper,
                       studlab = data$`Author, year`, data = data2,
                       sm = "RR", random= TRUE, method.I2 = "Q" , subgroup = Group, subset = Group == "Prisoners")

funnel(meta_prison, random = TRUE, backtransf = TRUE)


#---------------------------------------------------------------------------------------------------------
#Forest plot effect of year, bias, income,design on the primary outcomes 
#1-CC
t <- read_excel("MA_data_country_2025-07-29.xlsx", sheet= "CC_unique_studies")
t <- t %>%
  mutate(year_c = case_when(
    year < 2005 ~ "<2005",
    year >= 2005 & year <= 2015 ~ "2005-2015",
    year > 2015 ~ ">2015"
  ))
t <- t%>%
  mutate(year_c = factor(year_c, levels = c("<2005", "2005-2015", ">2015")))
table(t$year_c)
table(t$design)
table(t$OECD_classification)

data <- read_excel("MA_data_country_2025-07-29.xlsx", sheet= "CC")
# Conversion of date to factor
data <- data %>%
  mutate(year_c = case_when(
    year < 2005 ~ "<2005",
    year >= 2005 & year <= 2015 ~ "2005-2015",
    year > 2015 ~ ">2015"
  ))
data <- data %>%
  mutate(year_c = factor(year_c, levels = c("<2005", "2005-2015", ">2015")))
data[,10:ncol(data)]
#Conversion of ROB to factor

data <- data %>%
  mutate(ROB_c_level = case_when(
    ROB_C == "H" ~ "High",
    ROB_C == "L"  ~ "Low",
    ROB_C== "M" ~ "Moderate"
  ))
data <- data %>%
  mutate(ROB_c_level = factor(ROB_c_level, levels = c("Low", "Moderate", "High")))

head(data[, c("year_c", "ROB_c_level", "design")], 10)

#conversion of design to factor
data$design<-as.factor(data$design)

#Forest plot by ROB 
data <- data %>%
  mutate(logTE = log(data$`Risk ratio`), logLower = log(data$`95%CI low` ), logUpper = log(data$`95%CI up`))
meta_all <- metagen(TE = logTE, lower = logLower, upper = logUpper
                    , data = data,
                    sm = "RR", method.I2 = "Q" , subgroup = ROB_c_level)


forest(meta_all,
       xlab = "Risk Ratio (IC 95%)",  
       leftcols = "studlab",          
       rightcols = c("effect", "ci"),
       col.diamond = "blue",         
       col.study = "black",           
       col.square = "black",            
       col.subgroup = "darkgreen",          
       print.tau2 = FALSE,             
       print.I2 = TRUE, 
       common = FALSE ,
       random = TRUE ,
       test.subgroup = FALSE,
       study.results = FALSE,
       comb.random = TRUE,
       print.pval.Q = TRUE, 
       print.pval.subgroup = FALSE,
       backtransf = TRUE)  

#Forest plot by year levels
data <- data %>%
  mutate(logTE = log(data$`Risk ratio`), logLower = log(data$`95%CI low` ), logUpper = log(data$`95%CI up`))
meta_all <- metagen(TE = logTE, lower = logLower, upper = logUpper
                    , data = data,
                    sm = "RR", method.I2 = "Q" , subgroup = year_c)
table(data$year_c)
table(data$design)

forest(meta_all,
       xlab = "Risk Ratio (IC 95%)",  
       leftcols = "studlab",          
       rightcols = c("effect", "ci"),
       col.diamond = "blue",         
       col.study = "black",           
       col.square = "black",            
       col.subgroup = "darkgreen",          
       print.tau2 = FALSE,             
       print.I2 = TRUE, 
       common = FALSE ,
       random = TRUE ,
       test.subgroup = FALSE,
       study.results = FALSE,
       comb.random = TRUE,
       print.pval.Q = TRUE, 
       print.pval.subgroup = FALSE,
       backtransf = TRUE)  

#Forest plot by study design 
data <- data %>%
  mutate(logTE = log(data$`Risk ratio`), logLower = log(data$`95%CI low` ), logUpper = log(data$`95%CI up`))
meta_all <- metagen(TE = logTE, lower = logLower, upper = logUpper
                    , data = data,
                    sm = "RR", method.I2 = "Q" , subgroup = design)


forest(meta_all,
       xlab = "Risk Ratio (IC 95%)",  
       leftcols = "studlab",          
       rightcols = c("effect", "ci"),
       col.diamond = "blue",         
       col.study = "black",           
       col.square = "black",            
       col.subgroup = "darkgreen",          
       print.tau2 = FALSE,             
       print.I2 = TRUE, 
       common = FALSE ,
       random = TRUE ,
       test.subgroup = FALSE,
       study.results = FALSE,
       comb.random = TRUE,
       print.pval.Q = TRUE, 
       print.pval.subgroup = FALSE,
       backtransf = TRUE)  
#Forest plot by level of income
data <- data %>%
  mutate(logTE = log(data$`Risk ratio`), logLower = log(data$`95%CI low` ), logUpper = log(data$`95%CI up`))
meta_all <- metagen(TE = logTE, lower = logLower, upper = logUpper
                    , data = data,
                    sm = "RR", method.I2 = "Q" , subgroup = OECD_classification)


forest(meta_all,
       xlab = "Risk Ratio (IC 95%)",  
       leftcols = "studlab",          
       rightcols = c("effect", "ci"),
       col.diamond = "blue",         
       col.study = "black",           
       col.square = "black",            
       col.subgroup = "darkgreen",          
       print.tau2 = FALSE,             
       print.I2 = TRUE, 
       common = FALSE ,
       random = TRUE ,
       test.subgroup = FALSE,
       study.results = FALSE,
       comb.random = TRUE,
       print.pval.Q = TRUE, 
       print.pval.subgroup = FALSE,
       backtransf = TRUE)  
#Forest plot summary of variables : ROB, year, OECD, design 

data <- read_excel("summary_plot_cov_v3.xlsx", sheet= "CC")
head(data)
data <- data %>%
  mutate(logTE = log(data$`Risk_ratio`), logLower = log(data$`95%CI low` ), logUpper = log(data$`95%CI up`))
meta_all <- metagen(TE = logTE, lower = logLower, upper = logUpper
                    , data = data,
                    sm = "RR", method.I2 = "Q" , studlab = strata, subgroup = category)

meta_all$n_studies<-data$n_studies
forest(meta_all,
       xlab = "Risk Ratio (IC 95%)",  
       leftcols = c("studlab", "n_studies"),
       leftlabs = c("Variable", "N studies"),
       rightcols = c("effect", "ci"),
       col.diamond = "blue",         
       col.study = "black",           
       col.square = "black",            
       col.subgroup = "darkgreen",          
       print.tau2 = FALSE,             
       print.I2 = TRUE, 
       common = FALSE ,
       random = TRUE ,
       test.subgroup = FALSE,
       study.results = TRUE,
       comb.random = TRUE,
       print.pval.Q = TRUE, 
       print.pval.subgroup = TRUE,
       backtransf = TRUE)  
#--------------------------------------------------------------
#2-HGL

#conversion date to factor
t<-read_excel("MA_data_country_2025-07-29.xlsx", sheet= "HGL_unique_studies")
t <- t %>%
  mutate(year_c = case_when(
    year < 2005 ~ "<2005",
    year >= 2005 & year <= 2015 ~ "2005-2015",
    year > 2015 ~ ">2015"
  ))
table(t$year_c)


#Conversion ROB to factor
t <- t %>%
  mutate(ROB_c_level = case_when(
    ROB_C == "H" ~ "High",
    ROB_C == "L"  ~ "Low",
    ROB_C== "M" ~ "Moderate"
  ))
t <- t %>%
  mutate(ROB_c_level = factor(ROB_c_level, levels = c("Low", "Moderate", "High")))

head(t[, c("year_c", "ROB_c_level", "design")], 10)

#conversion of design to factor
data$design<-as.factor(data$design)

table(t$year_c)
table(t$design)
table(t$ROB_c_level)
table(t$OECD_classification)

data <- read_excel("MA_data_country_2025-07-29.xlsx", sheet= "HGL")


#convertir design en factor
data$design<-as.factor(data$design)

#Forest plot by ROB 
data <- data %>%
  mutate(logTE = log(data$`Risk ratio`), logLower = log(data$`95%CI low` ), logUpper = log(data$`95%CI up`))
meta_all <- metagen(TE = logTE, lower = logLower, upper = logUpper
                    , data = data,
                    sm = "RR", method.I2 = "Q" , subgroup = ROB_c_level)


forest(meta_all,
       xlab = "Risk Ratio (IC 95%)",  
       leftcols = "studlab",          
       rightcols = c("effect", "ci"),
       col.diamond = "blue",         
       col.study = "black",           
       col.square = "black",            
       col.subgroup = "darkgreen",          
       print.tau2 = FALSE,             
       print.I2 = TRUE, 
       common = FALSE ,
       random = TRUE ,
       test.subgroup = FALSE,
       study.results = FALSE,
       comb.random = TRUE,
       print.pval.Q = TRUE, 
       print.pval.subgroup = FALSE,
       backtransf = TRUE)  

#Forest plot by year levels
data <- data %>%
  mutate(logTE = log(data$`Risk ratio`), logLower = log(data$`95%CI low` ), logUpper = log(data$`95%CI up`))
meta_all <- metagen(TE = logTE, lower = logLower, upper = logUpper
                    , data = data,
                    sm = "RR", method.I2 = "Q" , subgroup = year_c)


forest(meta_all,
       xlab = "Risk Ratio (IC 95%)",  
       leftcols = "studlab",          
       rightcols = c("effect", "ci"),
       col.diamond = "blue",         
       col.study = "black",           
       col.square = "black",            
       col.subgroup = "darkgreen",          
       print.tau2 = FALSE,             
       print.I2 = TRUE, 
       common = FALSE ,
       random = TRUE ,
       test.subgroup = FALSE,
       study.results = FALSE,
       comb.random = TRUE,
       print.pval.Q = TRUE, 
       print.pval.subgroup = FALSE,
       backtransf = TRUE)  

#Forest plot by study design 
data <- data %>%
  mutate(logTE = log(data$`Risk ratio`), logLower = log(data$`95%CI low` ), logUpper = log(data$`95%CI up`))
meta_all <- metagen(TE = logTE, lower = logLower, upper = logUpper
                    , data = data,
                    sm = "RR", method.I2 = "Q" , subgroup = design)


forest(meta_all,
       xlab = "Risk Ratio (IC 95%)",  
       leftcols = "studlab",          
       rightcols = c("effect", "ci"),
       col.diamond = "blue",         
       col.study = "black",           
       col.square = "black",            
       col.subgroup = "darkgreen",          
       print.tau2 = FALSE,             
       print.I2 = TRUE, 
       common = FALSE ,
       random = TRUE ,
       test.subgroup = FALSE,
       study.results = FALSE,
       comb.random = TRUE,
       print.pval.Q = TRUE, 
       print.pval.subgroup = FALSE,
       backtransf = TRUE)  

#Forest plot by OECD classification 
data <- data %>%
  mutate(logTE = log(data$`Risk ratio`), logLower = log(data$`95%CI low` ), logUpper = log(data$`95%CI up`))
meta_all <- metagen(TE = logTE, lower = logLower, upper = logUpper
                    , data = data,
                    sm = "RR", method.I2 = "Q" , subgroup = OECD_classification)


forest(meta_all,
       xlab = "Risk Ratio (IC 95%)",  
       leftcols = "studlab",          
       rightcols = c("effect", "ci"),
       col.diamond = "blue",         
       col.study = "black",           
       col.square = "black",            
       col.subgroup = "darkgreen",          
       print.tau2 = FALSE,             
       print.I2 = TRUE, 
       common = FALSE ,
       random = TRUE ,
       test.subgroup = FALSE,
       study.results = FALSE,
       comb.random = TRUE,
       print.pval.Q = TRUE, 
       print.pval.subgroup = FALSE,
       backtransf = TRUE)  
# Forest plot HGL of variables : level of income , ROB , year and design 

data <- read_excel("summary_plot_cov_v3.xlsx", sheet= "HGL")
head(data)
data$category <- as.factor(data$category)
data <- data %>% filter(!is.na(category))
head(data)

data <- data %>%
  mutate(logTE = log(data$`Risk_ratio`), logLower = log(data$`95%CI low` ), logUpper = log(data$`95%CI up`))
meta_all <- metagen(TE = logTE, lower = logLower, upper = logUpper
                    , data = data,
                    sm = "RR", method.I2 = "Q" , studlab = strata, subgroup = category)

meta_all$n_studies<-data$n_studies
forest(meta_all,
       xlab = "Risk Ratio (IC 95%)",  
       leftcols = c("studlab", "n_studies"),
       leftlabs = c("Variable", "N studies"),
       rightcols = c("effect", "ci"),
       col.diamond = "blue",         
       col.study = "black",           
       col.square = "black",            
       col.subgroup = "darkgreen",          
       print.tau2 = FALSE,             
       print.I2 = TRUE, 
       common = FALSE ,
       random = TRUE ,
       test.subgroup = FALSE,
       study.results = TRUE,
       comb.random = TRUE,
       print.pval.Q = TRUE, 
       print.pval.subgroup = TRUE,
       backtransf = TRUE)  

