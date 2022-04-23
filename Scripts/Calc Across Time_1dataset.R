
library(tidyverse)
library(glamr)
library(gophr)


# Read in  dataset. Make sure to change your the file name to yours
df<-read_tsv("C:/Users/jstephens/Documents/ICPI/Calc over time/Data/test_MER_Structured_Datasets_Site_IM_with_CAT_flags_FY20-22_20220318_Eswatini.txt") %>%
   mutate(fiscal_year = as.character(fiscal_year))
glimpse(df)


##munge
names(df)
head(df, 10)

df_longd <- df %>%
  filter(indicator %in% c("TX_CURR", "TX_CURR_Lag1", "TX_CURR_Lag2",
                          "TX_PVLS", "TX_NEW","TX_NET_NEW", "TX_NET_NEW_SHIFT"),
         standardizeddisaggregate %in% c("Total Numerator", "Total Denominator")) %>%
  select(funding_agency, orgunituid, sitename, psnu, mech_code, numeratordenom, indicator, indicatortype,standardizeddisaggregate,
         fiscal_year, starts_with("qtr")) %>% 
  tidyr::pivot_longer(col=qtr1:qtr4,
                      names_to="qtr",
                      values_to = "val") %>%
  unite('period', c("fiscal_year","qtr"), remove=FALSE, sep="") %>% 
  unite('indicator_nd', c("indicator","numeratordenom"), sep="_")%>% 
  tidyr::pivot_wider( names_from="indicator_nd",
                      values_from = "val") %>% 
  tidyr::pivot_longer(col=qtr1_lag1_mech_change_type:qtr4_lag1_mech_change_type,
                      names_to="lag1_mech_change_type",
                      values_to = "val_lag1") %>% 
  tidyr::pivot_longer(col=qtr1_lag2_mech_change_type:qtr4_lag2_mech_change_type,
                      names_to="lag2_mech_change_type",
                      values_to = "val_lag2")



# 
# df_long <- df %>%
#   filter(indicator %in% c("TX_CURR", "TX_CURR_Lag1", "TX_CURR_Lag2",
#                           "TX_PVLS", "TX_NEW","TX_NET_NEW", "TX_NET_NEW_SHIFT"),
#          standardizeddisaggregate %in% c("Total Numerator", "Total Denominator")) %>%
#   clean_indicator() %>%
#   select(orgunituid, sitename, psnu, mech_code, numeratordenom, indicator, indicatortype,standardizeddisaggregate,
#          fiscal_year, starts_with("qtr"))%>%
#   tidyr::pivot_longer(col=qtr1:qtr4,
#                       names_to="qtr",
#                       values_to = "val") %>%
#   unite('period', c("fiscal_year","qtr"), remove=FALSE, sep="") %>% 
#   tidyr::pivot_wider( names_from="indicator",
#                       values_from = "val") %>% 
# tidyr::pivot_longer(col=qtr1_lag1_mech_change_type:qtr4_lag1_mech_change_type,
#                     names_to="lag1_mech_change_type",
#                     values_to = "val_lag1") %>% 
#   tidyr::pivot_longer(col=qtr1_lag2_mech_change_type:qtr4_lag2_mech_change_type,
#                       names_to="lag2_mech_change_type",
#                       values_to = "val_lag2") 

df_qtr1<-df_longd %>% 
  filter(qtr=="qtr1"& lag1_mech_change_type=="qtr1_lag1_mech_change_type" & lag2_mech_change_type=="qtr1_lag2_mech_change_type")
df_qtr2<-df_longd %>% 
  filter(qtr=="qtr2"& lag1_mech_change_type=="qtr2_lag1_mech_change_type" & lag2_mech_change_type=="qtr2_lag2_mech_change_type")
df_qtr3<-df_longd %>% 
  filter(qtr=="qtr3"& lag1_mech_change_type=="qtr3_lag1_mech_change_type" & lag2_mech_change_type=="qtr3_lag2_mech_change_type")
df_qtr4<-df_longd %>% 
  filter(qtr=="qtr4"& lag1_mech_change_type=="qtr4_lag1_mech_change_type" & lag2_mech_change_type=="qtr4_lag2_mech_change_type")

df_long2<-rbind(df_qtr1,df_qtr2, df_qtr3, df_qtr4) %>% 
  select(!c(lag1_mech_change_type,lag2_mech_change_type)) %>% 
  rename(lag1_mech_change_type=val_lag1, lag2_mech_change_type=val_lag2)

names(df_long2)

df_group<-df_long2 %>% 
   group_by(funding_agency,orgunituid, sitename, psnu,mech_code,indicatortype, period, fiscal_year, qtr, lag1_mech_change_type, lag2_mech_change_type) %>% 
  summarise(
    TX_CURR_N = sum(TX_CURR_N, na.rm = TRUE),
    TX_CURR_Lag1_N = sum(TX_CURR_Lag1_N, na.rm = TRUE),
    TX_CURR_Lag2_N = sum(TX_CURR_Lag2_N, na.rm = TRUE),
    TX_PVLS_D = sum(TX_PVLS_D, na.rm = TRUE),
    TX_PVLS_N = sum(TX_PVLS_N, na.rm = TRUE),
    TX_NET_NEW_SHIFT_N = sum(TX_NET_NEW_SHIFT_N, na.rm = TRUE),
    TX_NET_NEW_N = sum(TX_NET_NEW_N, na.rm = TRUE),
    TX_NEW_N = sum(TX_NEW_N, na.rm = TRUE))

df_flags<-df_group %>% 
  mutate(useVLC=case_when(TX_CURR_Lag2_N==0 | TX_PVLS_D==0 ~"drop", 
                          TRUE~"keep")) 
         


write_tsv(df_flags, "C:/Users/jstephens/Documents/ICPI/Calc over time/Dataout/FY20-22_ESW_NN_VLC_MSD_flags_20220422_1dataset_keep.tsv", na = "")


##############################################################################
# Read in  dataset. Make sure to change your the file name to yours
df<-read_tsv("C:/Users/jstephens/Documents/ICPI/Calc over time/Data/test_MER_Structured_Datasets_Site_IM_with_CAT_flags_FY20-22_20220318_Haiti.txt") %>%
  mutate(fiscal_year = as.character(fiscal_year))
glimpse(df)


##munge
names(df)
head(df, 10)

df_longd <- df %>%
  filter(indicator %in% c("TX_CURR", "TX_CURR_Lag1", "TX_CURR_Lag2",
                          "TX_PVLS", "TX_NEW","TX_NET_NEW", "TX_NET_NEW_SHIFT"),
         standardizeddisaggregate %in% c("Total Numerator", "Total Denominator")) %>%
  select(funding_agency, orgunituid, sitename, psnu, mech_code, numeratordenom, indicator, indicatortype,standardizeddisaggregate,
         fiscal_year, starts_with("qtr")) %>% 
  tidyr::pivot_longer(col=qtr1:qtr4,
                      names_to="qtr",
                      values_to = "val") %>%
  unite('period', c("fiscal_year","qtr"), remove=FALSE, sep="") %>% 
  unite('indicator_nd', c("indicator","numeratordenom"), sep="_")%>% 
  tidyr::pivot_wider( names_from="indicator_nd",
                      values_from = "val") %>% 
  tidyr::pivot_longer(col=qtr1_lag1_mech_change_type:qtr4_lag1_mech_change_type,
                      names_to="lag1_mech_change_type",
                      values_to = "val_lag1") %>% 
  tidyr::pivot_longer(col=qtr1_lag2_mech_change_type:qtr4_lag2_mech_change_type,
                      names_to="lag2_mech_change_type",
                      values_to = "val_lag2")



# 
# df_long <- df %>%
#   filter(indicator %in% c("TX_CURR", "TX_CURR_Lag1", "TX_CURR_Lag2",
#                           "TX_PVLS", "TX_NEW","TX_NET_NEW", "TX_NET_NEW_SHIFT"),
#          standardizeddisaggregate %in% c("Total Numerator", "Total Denominator")) %>%
#   clean_indicator() %>%
#   select(orgunituid, sitename, psnu, mech_code, numeratordenom, indicator, indicatortype,standardizeddisaggregate,
#          fiscal_year, starts_with("qtr"))%>%
#   tidyr::pivot_longer(col=qtr1:qtr4,
#                       names_to="qtr",
#                       values_to = "val") %>%
#   unite('period', c("fiscal_year","qtr"), remove=FALSE, sep="") %>% 
#   tidyr::pivot_wider( names_from="indicator",
#                       values_from = "val") %>% 
# tidyr::pivot_longer(col=qtr1_lag1_mech_change_type:qtr4_lag1_mech_change_type,
#                     names_to="lag1_mech_change_type",
#                     values_to = "val_lag1") %>% 
#   tidyr::pivot_longer(col=qtr1_lag2_mech_change_type:qtr4_lag2_mech_change_type,
#                       names_to="lag2_mech_change_type",
#                       values_to = "val_lag2") 

df_qtr1<-df_longd %>% 
  filter(qtr=="qtr1"& lag1_mech_change_type=="qtr1_lag1_mech_change_type" & lag2_mech_change_type=="qtr1_lag2_mech_change_type")
df_qtr2<-df_longd %>% 
  filter(qtr=="qtr2"& lag1_mech_change_type=="qtr2_lag1_mech_change_type" & lag2_mech_change_type=="qtr2_lag2_mech_change_type")
df_qtr3<-df_longd %>% 
  filter(qtr=="qtr3"& lag1_mech_change_type=="qtr3_lag1_mech_change_type" & lag2_mech_change_type=="qtr3_lag2_mech_change_type")
df_qtr4<-df_longd %>% 
  filter(qtr=="qtr4"& lag1_mech_change_type=="qtr4_lag1_mech_change_type" & lag2_mech_change_type=="qtr4_lag2_mech_change_type")

df_long2<-rbind(df_qtr1,df_qtr2, df_qtr3, df_qtr4) %>% 
  select(!c(lag1_mech_change_type,lag2_mech_change_type)) %>% 
  rename(lag1_mech_change_type=val_lag1, lag2_mech_change_type=val_lag2)

names(df_long2)

df_group<-df_long2 %>% 
  group_by(funding_agency,orgunituid, sitename, psnu,mech_code,indicatortype, period, fiscal_year, qtr, lag1_mech_change_type, lag2_mech_change_type) %>% 
  summarise(
    TX_CURR_N = sum(TX_CURR_N, na.rm = TRUE),
    TX_CURR_Lag1_N = sum(TX_CURR_Lag1_N, na.rm = TRUE),
    TX_CURR_Lag2_N = sum(TX_CURR_Lag2_N, na.rm = TRUE),
    TX_PVLS_D = sum(TX_PVLS_D, na.rm = TRUE),
    TX_PVLS_N = sum(TX_PVLS_N, na.rm = TRUE),
    TX_NET_NEW_SHIFT_N = sum(TX_NET_NEW_SHIFT_N, na.rm = TRUE),
    TX_NET_NEW_N = sum(TX_NET_NEW_N, na.rm = TRUE),
    TX_NEW_N = sum(TX_NEW_N, na.rm = TRUE))

df_flags<-df_group %>% 
  mutate(useVLC=case_when(TX_CURR_Lag2_N==0 | TX_PVLS_D==0 ~"drop", 
                          TRUE~"keep")) 



write_tsv(df_flags, "C:/Users/jstephens/Documents/ICPI/Calc over time/Dataout/FY20-22_HTI_NN_VLC_MSD_flags_20220422_1dataset_keep.tsv", na = "")
