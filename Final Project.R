# Final Project Group 4

# Package Library
library(readxl)
library(dplyr)
library(lubridate)
library(ggplot2)
library(fastDummies)

# This reads RawData.xlsx
RawData <- read_xlsx("RawData.xlsx", sheet = "Raw-Data")
RawData  # Reads Raw-Data sheet
Calendar <- read_xlsx("RawData.xlsx", sheet = "Calendar")
Calendar  # Reads Calendar sheet


# Column Adding
## Add Quarter column
Quarter <- RawData %>%
  group_by(`Receipt Date`) %>%
  mutate(Quarter = quarters.Date(`Receipt Date`))
## Add Year column 
Year <- RawData %>%
  group_by(`Receipt Date`) %>%
  mutate(Year = year(`Receipt Date`))


# Calculations
## Calculate In-Transit Lead Time
RawData$'In Transit Lead Time' <- as.numeric(difftime(as.POSIXct(RawData$`Receipt Date`), 
                                                        as.POSIXct(RawData$`Ship Date`), 
                                                        units = "days"))
## Calculate Manufacturing Lead Time
RawData$'Manufacturing Lead Time' <- as.numeric(difftime(as.POSIXct(RawData$`Ship Date`), 
                                                           as.POSIXct(Rawdata$`PO Download Date`), 
                                                           units = "days"))


# Cleaning
## Clean RawData columns; not date column
RawData [duplicated(RawData$'PO Download Date',RawData$'Ship Date',RawData$'Receipt Date') == TRUE,]
nrow(RawData)
## Function creation
clean <- function(data) {
  cleaned_data <- data[!duplicated(data[c("PO Download Date", "Ship Date", "Receipt Date")]),]
  return(cleaned_data)
}
## Creation of CleanData
CleanData <- clean(RawData)
### Counts CleanData rows
nrow(CleanData)
### Add In Transit Lead Time to CleanData
In_Transit_LT <- RawData$'In Transit Lead Time'
### Add Manufacturing Lead Time to CleanData
Manufacturing_LT <- RawData$'Manufacturing Lead Time'
## Adds Quarter and Year columns from RawData to CleanData
CleanData <- RawData %>%
  mutate(Quarter = quarters.Date(`Receipt Date`)) %>%
  mutate(Year = year(`Receipt Date`)) %>% 
  mutate(In_Transit_LT = `In Transit Lead Time`) %>% 
  mutate(Manufacturing_LT = `Manufacturing Lead Time`)
## Delete rows that contain NA
CleanData <- na.omit(CleanData)
### Counts CleanData rows
nrow(CleanData)




# Data Viz section 
## Relationships between In-Transit Lead Time and other columns
### Naming of column variables
LOB <- CleanData$LOB
Origin <- CleanData$Origin
Ship_Mode <- CleanData$`Ship Mode`
Ship_Date <- CleanData$`Ship Date`
Quarter2 <- CleanData$Quarter
ITLT <- CleanData$`In Transit Lead Time`
## In-Transit Lead Time and Origin
ggplot(CleanData, aes(x = Origin, y = In_Transit_LT, color = LOB)) +
  geom_col() +
  labs(
    title = "Relationship between In-Transit Lead Time & LOB",
    x = "Origin",
    y = "In-Transit Lead Time"
  ) 
## In-Transit Lead Time and Ship Mode
ggplot(CleanData, aes(x = Ship_Mode, y = In_Transit_LT, color = Origin)) +
  geom_col() +
  labs(
    title = "Relationship between In-Transit Lead Time & Ship Mode",
    x = "Ship Mode",
    y = "In-Transit Lead Time"
  ) 
## In-Transit Lead Time and Ship Date
ggplot(CleanData, aes(x = Ship_Date, y = In_Transit_LT, color = LOB)) +
  geom_jitter() +
  labs(
    title = "Relationship between In-Transit Lead Time & Ship Date",
    x = "Ship Date",
    y = "In-Transit Lead Time"
  ) 
## In-Transit Lead Time and Quarter
ggplot(CleanData, aes(x = Quarter2, y = ITLT, color = LOB)) +
  geom_col() +
  labs(
    title = "Relationship between In-Transit Lead Time & Quarter",
    x = "Receipt Quarter",
    y = "In-Transit Lead Time"
  )


# Correlations
## Use CleanData to create dummy columns
CleanData_Dummies <- dummy_cols(CleanData, select_columns = c('Origin', 'LOB'))
## Creation of Correlation Table
cor_table <- cor(CleanData_Dummies[, c("In Transit Lead Time", "Origin_Site A", 
                                       "Origin_Site B", "Origin_Site C", 
                                       "Origin_Site D", "LOB_Product A", 
                                       "LOB_Product B", "LOB_Product C")])
## Orders the Correlation Table in descending order
cor_table <- cor_table[order(-cor_table[,1]),]
## Removes the row variables of In Transit Lead Time from cor_table
cor_table <- cor_table[!(rownames(cor_table) == "In Transit Lead Time"),]
## Converts to data frame
cor_table <- as.data.frame(as.table(cor_table))
## Gives column names to cor_table
colnames(cor_table) <- c("Predictor 1", "Predictor 2", "Correlation")
## Orders table in descending order by Correlation
cor_table <- cor_table[order(-cor_table$Correlation),]
## Sets the lowest possible correlation variable greater or equal to 0
cor_table <- cor_table[abs(cor_table$Correlation) >= 0,]
## Will filter rows where Correlation is negative and arrange in descending order
cor_table %>% 
  filter(!if_any('Correlation') < 0) %>% 
  arrange(desc('Correlation'))
## Will filter Correlation not equal to 1 and greater or equal to 0
cor_table %>% 
  filter(if_any(Correlation) != 1) %>% 
  filter(if_any(Correlation) >= 0)
