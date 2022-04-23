
library(tidyverse)
library(glamr)
# si_setup()
# only need to do si_setup() first time running code
library(gophr)


# Read in  dataset. Make sure to change your the file name to yours
df<-read_tsv("C:/Users/jstephens/Documents/ICPI/Calc over time/Data/preview_MER_Structured_Datasets_Site_IM_FY20-22_20220208_CalcAcrTime_Haiti.txt") %>%
  mutate(pre_rgnlztn_hq_mech_code = as.character(pre_rgnlztn_hq_mech_code)) %>%
  mutate(mech_code = as.character(mech_code))  %>%
  mutate(fiscal_year = as.character(fiscal_year))
glimpse(df)

flags<-read_tsv("C:/Users/jstephens/Documents/ICPI/Calc over time/Data/test_MER_Structured_Dataset_CalcAcrTime_flags_FY20-22_20220208_Haiti.txt") %>%
  mutate(mech_code = as.character(mech_code))  %>%
  mutate(fiscal_year = as.character(fiscal_year))
glimpse(flags)






##munge

df_lng2 <- df %>%
  filter(indicator %in% c("TX_CURR", "TX_CURR_Lag1", "TX_CURR_Lag2",
                          "TX_PVLS", "TX_NEW","TX_NET_NEW", "TX_NET_NEW_SHIFT"),
         standardizeddisaggregate %in% c("Total Numerator", "Total Denominator")) %>%
  clean_indicator() %>%
  select(orgunituid, sitename, psnu, mech_code, indicator, indicatortype,
         fiscal_year, dataelementuid, categoryoptioncombouid, starts_with("qtr")) %>%
   reshape_msd() %>% 
  select(-period_type)


glimpse(df_lng2)

#munge flags
flag_clean<-flags %>%
  mutate(period=case_when(
    period_code == "2019Q4"~ "FY20Q1",
    period_code == "2020Q1"  ~ "FY20Q2",
    period_code == "2020Q2"  ~ "FY20Q3",
    period_code == "2020Q3"~ "FY20Q4",
    period_code == "2020Q4"  ~ "FY21Q1",
    period_code == "2021Q1"  ~ "FY21Q2",
    period_code == "2021Q2"~ "FY21Q3",
    period_code == "2021Q3"  ~ "FY21Q4",
    period_code == "2021Q4"  ~ "FY22Q1" )) %>%
  filter(indicator %in% c("TX_CURR", "TX_CURR_Lag1", "TX_CURR_Lag2",
                        "TX_PVLS", "TX_NEW","TX_NET_NEW", "TX_NET_NEW_SHIFT"),
  standardizeddisaggregate %in% c("Total Numerator", "Total Denominator")) %>%
  clean_indicator() 

glimpse(flag_clean)




df_join3<-df_lng2 %>%
  left_join(flag_clean,  by = c("orgunituid"="orgunituid", "mech_code"="mech_code", "dataelementuid"="dataelementuid", 
                                "categoryoptioncombouid"="categoryoptioncombouid", "period"="period",
                                "sitename"="sitename", "indicator"="indicator", "indicatortype"="indicatortype"))

df_join3_wide<-df_join3 %>%
  pivot_wider(names_from = indicator,
              values_from = value)

 write_csv(df_join3_wide, "C:/Users/jstephens/Documents/ICPI/Calc over time/Dataout/FY20-22_HTI_NN_VLC_MSD_flags_20220412.csv", na = "")


