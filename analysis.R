library(dplyr)
library(xts)
library(ggplot2)
library(reshape2)

data <- read.csv("data/states_all.csv")

#ESTUDOS UNIVARIADOS

data$REVENUE_PERCAPTA_FEDERALREVENUE <- data$FEDERAL_REVENUE / data$GRADES_ALL_G
data$REVENUE_PERCAPTA_STATEREVENUE <- data$STATE_REVENUE / data$GRADES_ALL_G
data$REVENUE_PERCAPTA_LOCALREVENUE <- data$LOCAL_REVENUE / data$GRADES_ALL_G
data$REVENUE_PERCAPTA_TOTALREVENUE <- data$TOTAL_REVENUE / data$GRADES_ALL_G
data$EXPENDITURE_PERCAPTA_INSTRUTION <- data$INSTRUCTION_EXPENDITURE / data$GRADES_ALL_G
data$EXPENDITURE_PERCAPTA_SUPPORTSERVICE <- data$SUPPORT_SERVICES_EXPENDITURE / data$GRADES_ALL_G
data$EXPENDITURE_PERCAPTA_CAPITALOUTLAY <- data$CAPITAL_OUTLAY_EXPENDITURE / data$GRADES_ALL_G

#an?lise das receitas federal, estaduais e locais
groupby_FEDERALREVENUE_states <- data %>% group_by(STATE = as.character(STATE)) %>% summarise(FEDERAL_REVENUE = sum(na.omit(FEDERAL_REVENUE)), REVENUE_PERCAPTA_FEDERALREVENUE = sum(na.omit(REVENUE_PERCAPTA_FEDERALREVENUE)))
groupby_FEDERALREVENUE_states <- groupby_FEDERALREVENUE_states[groupby_FEDERALREVENUE_states$FEDERAL_REVENUE>0,]
groupby_FEDERALREVENUE_years <- data %>% group_by(YEAR = as.character(YEAR)) %>% summarise(YEAR_REVENUE = sum(na.omit(FEDERAL_REVENUE)), REVENUE_PERCAPTA_FEDERALREVENUE = sum(na.omit(REVENUE_PERCAPTA_FEDERALREVENUE)))
groupby_FEDERALREVENUE_years <- groupby_FEDERALREVENUE_years[groupby_FEDERALREVENUE_years$YEAR_REVENUE>0,]

groupby_STATEREVENUE_states <- data %>% group_by(STATE = as.character(STATE)) %>% summarise(STATE_REVENUE = sum(na.omit(STATE_REVENUE)), REVENUE_PERCAPTA_STATEREVENUE = sum(na.omit(REVENUE_PERCAPTA_STATEREVENUE)))
groupby_STATEREVENUE_states <- groupby_STATEREVENUE_states[groupby_STATEREVENUE_states$STATE_REVENUE >0,]
groupby_STATEREVENUE_years <- data %>% group_by(YEAR = as.character(YEAR)) %>% summarise(YEAR_REVENUE = sum(na.omit(STATE_REVENUE)), REVENUE_PERCAPTA_LOCALREVENUE = sum(na.omit(REVENUE_PERCAPTA_STATEREVENUE)))
groupby_STATEREVENUE_years <- groupby_STATEREVENUE_years[groupby_STATEREVENUE_years$YEAR_REVENUE >0,]

groupby_LOCALREVENUE_states <- data %>% group_by(STATE = as.character(STATE)) %>% summarise(LOCAL_REVENUE = sum(na.omit(LOCAL_REVENUE)), REVENUE_PERCAPTA_LOCALREVENUE = sum(na.omit(REVENUE_PERCAPTA_LOCALREVENUE)))
groupby_LOCALREVENUE_states <- groupby_LOCALREVENUE_states[groupby_LOCALREVENUE_states$LOCAL_REVENUE >0,]
groupby_LOCALREVENUE_years <- data %>% group_by(YEAR = as.character(YEAR)) %>% summarise(YEAR_REVENUE = sum(na.omit(LOCAL_REVENUE)), REVENUE_PERCAPTA_LOCALREVENUE = sum(na.omit(REVENUE_PERCAPTA_LOCALREVENUE)))
groupby_LOCALREVENUE_years <- groupby_LOCALREVENUE_years[groupby_LOCALREVENUE_years$YEAR_REVENUE >0,]

#an?lise da despesas estaduais
groupby_INSTRUTION_states <- data %>% group_by(STATE = as.character(STATE)) %>% summarise(INSTRUCTION_EXPENDITURE = sum(na.omit(INSTRUCTION_EXPENDITURE)), EXPENDITURE_PERCAPTA_INSTRUTION = sum(na.omit(EXPENDITURE_PERCAPTA_INSTRUTION)))
groupby_INSTRUTION_states <- groupby_INSTRUTION_states[groupby_INSTRUTION_states$INSTRUCTION_EXPENDITURE >0,]
groupby_INSTRUTION_years <- data %>% group_by(YEAR = as.character(YEAR)) %>% summarise(INSTRUCTION_EXPENDITURE = sum(na.omit(INSTRUCTION_EXPENDITURE)), EXPENDITURE_PERCAPTA_INSTRUTION = sum(na.omit(EXPENDITURE_PERCAPTA_INSTRUTION)))
groupby_INSTRUTION_years <- groupby_INSTRUTION_years[groupby_INSTRUTION_years$INSTRUCTION_EXPENDITURE >0,]

groupby_SUPPORTSERVICES_states <- data %>% group_by(STATE = as.character(STATE)) %>% summarise(SUPPORT_SERVICES_EXPENDITURE = sum(na.omit(SUPPORT_SERVICES_EXPENDITURE)), EXPENDITURE_PERCAPTA_SUPPORTSERVICE = sum(na.omit(EXPENDITURE_PERCAPTA_SUPPORTSERVICE)))
groupby_SUPPORTSERVICES_states <- groupby_SUPPORTSERVICES_states[groupby_SUPPORTSERVICES_states$SUPPORT_SERVICES_EXPENDITURE >0,]
groupby_SUPPORTSERVICES_years <- data %>% group_by(YEAR = as.character(YEAR)) %>% summarise(SUPPORT_SERVICES_EXPENDITURE = sum(na.omit(SUPPORT_SERVICES_EXPENDITURE)), EXPENDITURE_PERCAPTA_SUPPORTSERVICE = sum(na.omit(EXPENDITURE_PERCAPTA_SUPPORTSERVICE)))
groupby_SUPPORTSERVICES_years <- groupby_SUPPORTSERVICES_years[groupby_SUPPORTSERVICES_years$SUPPORT_SERVICES_EXPENDITURE >0,]

groupby_CAPITALOUTLAY_states <- data %>% group_by(STATE = as.character(STATE)) %>% summarise(CAPITAL_OUTLAY_EXPENDITURE = sum(na.omit(CAPITAL_OUTLAY_EXPENDITURE)), EXPENDITURE_PERCAPTA_CAPITALOUTLAY = sum(na.omit(EXPENDITURE_PERCAPTA_CAPITALOUTLAY)))
groupby_CAPITALOUTLAY_states <- groupby_CAPITALOUTLAY_states[groupby_CAPITALOUTLAY_states$CAPITAL_OUTLAY_EXPENDITURE >0,]
groupby_CAPITALOUTLAY_years <- data %>% group_by(YEAR = as.character(YEAR)) %>% summarise(CAPITAL_OUTLAY_EXPENDITURE = sum(na.omit(CAPITAL_OUTLAY_EXPENDITURE)), EXPENDITURE_PERCAPTA_CAPITALOUTLAY = sum(na.omit(EXPENDITURE_PERCAPTA_CAPITALOUTLAY)))
groupby_CAPITALOUTLAY_years <- groupby_CAPITALOUTLAY_years[groupby_CAPITALOUTLAY_years$CAPITAL_OUTLAY_EXPENDITURE >0,]

#an?lise de quantidade de alunos
groupby_GRRADES912_states <- data %>% group_by(STATE = as.character(STATE)) %>% summarise(GRADES_9_12_G = sum(na.omit(GRADES_9_12_G)))
groupby_GRRADES912_states <- groupby_GRRADES912_states[groupby_GRRADES912_states$GRADES_9_12_G>0,]
groupby_GRRADES912_years <- data %>% group_by(YEAR = as.character(YEAR)) %>% summarise(GRADES_9_12_G = sum(na.omit(GRADES_9_12_G )))
groupby_GRRADES912_years <- groupby_GRRADES912_years[groupby_GRRADES912_years$GRADES_9_12_G>0,]

#an?lise dos desempenhos de alunos - matem?tica e portugu?s
groupby_MATH4SCORE_states <- data %>% group_by(STATE = as.character(STATE)) %>% summarise(AVG_MATH_4_SCORE = mean(na.omit(AVG_MATH_4_SCORE)), VAR_MATH_4_SCORE = var(AVG_MATH_4_SCORE, na.rm = TRUE))
groupby_MATH4SCORE_years <- data %>% group_by(YEARS = as.character(YEAR)) %>% summarise(AVG_MATH_4_SCORE = mean(na.omit(AVG_MATH_4_SCORE)), VAR_MATH_4_SCORE = var(AVG_MATH_4_SCORE, na.rm = TRUE))
groupby_MATH8SCORE_states <- data %>% group_by(STATE = as.character(STATE)) %>% summarise(AVG_MATH_8_SCORE = mean(na.omit(AVG_MATH_8_SCORE)), VAR_MATH_4_SCORE = var(AVG_MATH_8_SCORE, na.rm = TRUE))
groupby_MATH8SCORE_years <- data %>% group_by(YEARS = as.character(YEAR)) %>% summarise(AVG_MATH_8_SCORE = mean(na.omit(AVG_MATH_8_SCORE)), VAR_MATH_4_SCORE = var(AVG_MATH_8_SCORE, na.rm = TRUE))

#ESTUDOS MULTIVARIADOS


#CHARTS

#LINES CHARTS
ggplot(data = groupby_FEDERALREVENUE_years, aes(x = YEAR, y = REVENUE_PERCAPTA_FEDERALREVENUE, group=1)) + 
  geom_line(color = "#00AFBB", size = 1) +
  ggtitle("REVENUE PERCAPTA - FEDERAL") +
  xlab("YEARS") + ylab("TOTAL REVENUE - FEDERAL") 

ggplot(data = groupby_STATEREVENUE_years, aes(x = YEAR, y = REVENUE_PERCAPTA_STATEREVENUE, group=1)) + 
  geom_line(color = "#00AFBB", size = 1) + 
  ggtitle("REVENUE PERCAPTA - STATE") +
  xlab("YEARS") + ylab("TOTAL REVENUE - STATE") 

ggplot(data = groupby_LOCALREVENUE_years, aes(x = YEAR, y = REVENUE_PERCAPTA_LOCALREVENUE, group=1)) + 
  geom_line(color = "#00AFBB", size = 1) + 
  ggtitle("REVENUE PERCAPTA - LOCAL") +
  xlab("YEARS") + ylab("TOTAL REVENUE - LOCAL") 

ggplot(data = groupby_INSTRUTION_years, aes(x = YEAR, y = EXPENDITURE_PERCAPTA_INSTRUTION, group=1)) + 
  geom_line(color = "#00AFBB", size = 1) + 
  ggtitle("EXPENDITURE PERCAPTA - SUPPORT SERVICE") +
  xlab("YEARS") + ylab("TOTAL EXPENDITURE - INSTRUTION") 

ggplot(data = groupby_CAPITALOUTLAY_years, aes(x = YEAR, y = EXPENDITURE_PERCAPTA_CAPITALOUTLAY, group=1)) + 
  geom_line(color = "#00AFBB", size = 1) + 
  ggtitle("EXPENDITURE PERCAPTA - SUPPORT SERVICE") +
  xlab("YEARS") + ylab("TOTAL EXPENDITURE - CAPITALOUTLAY") 

ggplot(data = groupby_SUPPORTSERVICES_years, aes(x = YEAR, y = EXPENDITURE_PERCAPTA_SUPPORTSERVICE, group=1)) + 
  geom_line(color = "#00AFBB", size = 1) + 
  ggtitle("EXPENDITURE PERCAPTA - SUPPORT SERVICE") +
  xlab("YEARS") + ylab("TOTAL EXPENDITURE - SUPPORT AND SERVICES") 

ggplot(data = groupby_GRRADES912_years, aes(x = YEAR, y = GRADES_9_12_G, group=1)) + 
  geom_line(color = "#00AFBB", size = 1) + 
  ggtitle("TOTAL STUDENTS - GRADES_9_12_G") +
  xlab("YEARS") + ylab("TOTAL STUDENTS - GRADES_9_12_G")


#BAR CHARTS  

ggplot(data=groupby_STATEREVENUE_states, aes(x=STATE, y=REVENUE_PERCAPTA_STATEREVENUE)) +
  geom_bar(stat="identity") +
  ggtitle("REVENUE PERCAPTA - STATES") +
  xlab("STATES") + ylab("TOTAL REVENUE - STATES") +
  coord_flip()

ggplot(data=groupby_FEDERALREVENUE_states, aes(x=STATE, y=REVENUE_PERCAPTA_FEDERALREVENUE)) +
  geom_bar(stat="identity") +
  ggtitle("REVENUE PERCAPTA - FEDERAL") +
  xlab("STATES") + ylab("TOTAL REVENUE - FEDERAL") +
  coord_flip()

ggplot(data=groupby_LOCALREVENUE_states, aes(x=STATE, y=REVENUE_PERCAPTA_LOCALREVENUE)) +
  geom_bar(stat="identity") +
  ggtitle("REVENUE PERCAPTA - LOCAL") +
  xlab("STATES") + ylab("TOTAL REVENUE - LOCAL") +
  coord_flip()


ggplot(data=groupby_INSTRUTION_states, aes(x=STATE, y=EXPENDITURE_PERCAPTA_INSTRUTION)) +
  geom_bar(stat="identity") +
  ggtitle("EXPENDITURE PERCAPTA - SUPPORT SERVICE") +
  xlab("STATES") + ylab("TOTAL EXPENDITURE - INSTRUTION") +
  coord_flip()

ggplot(data=groupby_CAPITALOUTLAY_states, aes(x=STATE, y=EXPENDITURE_PERCAPTA_CAPITALOUTLAY)) +
  geom_bar(stat="identity") +
  ggtitle("EXPENDITURE PERCAPTA - SUPPORT SERVICE") +
  xlab("STATES") + ylab("TOTAL EXPENDITURE - CAPITAL OUTLAY") +
  coord_flip()

ggplot(data=groupby_SUPPORTSERVICES_states, aes(x=STATE, y=EXPENDITURE_PERCAPTA_SUPPORTSERVICE)) +
  geom_bar(stat="identity") +
  ggtitle("EXPENDITURE PERCAPTA - SUPPORT SERVICE") +
  xlab("STATES") + ylab("TOTAL EXPENDITURE - SUPPORT AND SERVICES") +
  coord_flip()

ggplot(data=groupby_GRRADES912_states, aes(x=STATE, y=GRADES_9_12_G)) +
  geom_bar(stat="identity") +
  ggtitle("TOTAL STUDENTS - GRADES_9_12_G") +
  xlab("STATES") + ylab("TOTAL STUDENTS - GRADES_9_12_G") +
  coord_flip()



















