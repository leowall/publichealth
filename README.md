# publichealth
Public Health Data using the fingertipsR package

library(tidyverse)
library(fingertipsR)
library(plotly)


# returns list of indicators within Fingertips
inds <- indicators()

# select life expectancy indicators from list
life_expectancy <- inds[grepl("life expectancy", tolower(inds$IndicatorName)),]

# select unique values only
life_expectancy <- unique(life_expectancy[duplicated(life_expectancy$IndicatorID)
                                      == FALSE, c("IndicatorID", "IndicatorName")])
# get area typs list from fingertips
areaTypes <- area_types()

# get IMD data at upper and lower tier local authorities
dep <- deprivation_decile(AreaTypeID = 102, Year = 2015)

# get fingertips data for life expectancy at birth and health life expectancy at birth
indicators <- c(90362, 90366)
LEdata <- fingertips_data(IndicatorID = indicators, AreaTypeID = 102)


data <- LEdata %>%
  filter(AreaType == "County & UA" & Timeperiod == "2012 - 14" & !is.na(Value)) %>%
  select(IndicatorID, AreaCode, AreaName, Sex, Timeperiod, Value)
  
data <- data %>%
  left_join(dep, by = "AreaCode")

write_excel_csv(LEdata, "C:/Users/Leo/Documents/R/FingertipsData.csv")
write_excel_csv(dep, "C:/Users/Leo/Documents/R/depData.csv")
write_excel_csv(inds, "C:/Users/Leo/Documents/R/indsData.csv")
write_excel_csv(areaTypes, "C:/Users/Leo/Documents/R/areaTypesData.csv")
write_excel_csv(HLF, "C:/Users/Leo/Documents/R/HLF.csv")

ggplot(data, aes(IMDscore, Value, Col = factor(IndicatorID))) +
  geom_point() +
  geom_smooth(se = FALSE, method = "loess") +
  facet_wrap(~ Sex) +
  scale_color_manual(name = "Indicator", 
                     breaks = c("90366", "90362"),
                     labels = c("life expectancy", "Healthy life expectancy"),
                     values = c("#128c4a", "#88c857")) +
  scale_x_reverse() +
  labs(x = "IMD deprivation",
       y = "Age",
       title = "Life expectancy and healthy life at birth \nfor Upper Tier Local Authorities (2012 - 2014)") +
  theme_bw()

HLF <- data %>%
  filter(IndicatorID == 90362)

plot_ly(data = HLF,  x = ~IMDscore, y = ~Value, color = ~Sex, 
        text = ~paste(AreaName)) %>%
