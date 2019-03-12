# script for cleaning the World Values Study (WVS6)
# available from http://www.worldvaluessurvey.org/wvs.jsp
# wsv.csv file is created below 
# and then used by wvs_make_data()

library(dplyr)

WVS <- foreign::read.spss("WV6_Data_Spss_v20180912.sav", use.value.labels = FALSE, to.data.frame = TRUE)
WVS_labs <- foreign::read.spss("WV6_Data_Spss_v20180912.sav", use.value.labels = TRUE, to.data.frame = TRUE)

wv_num <- WVS %>% transmute(rightist = as.numeric(V95), # 1 == left/right == 10 
                        birthyear = V241,
                        econ_attitude1 = as.numeric(V96),
                        econ_attitude2 = as.numeric(V97),
                        econ_attitude3 = as.numeric(V98),
                        econ_attitude4 = as.numeric(V99),
                        econ_attitude5 = as.numeric(V100),
                        econ_attitude6 = as.numeric(V101),
                        values1 = as.numeric(V70),
                        values2 = as.numeric(V71),
                        values3 = as.numeric(V72),
                        values4 = as.numeric(V73),
                        values5 = as.numeric(V74),                     
                        values6 = as.numeric(V75),
                        values7 = as.numeric(V76),
                        values8 = as.numeric(V77),
                        values9 = as.numeric(V78),
                        values10 = as.numeric(V79),
                        democracy1 = as.numeric(V131),
                        democracy2 = as.numeric(V132),
                        democracy3 = as.numeric(V133),
                        democracy4 = as.numeric(V134),
                        democracy5 = as.numeric(V135),
                        democracy6 = as.numeric(V136),
                        democracy7 = as.numeric(V137),
                        democracy8 = as.numeric(V138),
                        democracy9 = as.numeric(V139))


wv_labs <- WVS_labs %>% transmute(country = as.factor(V2),
                        region = as.factor(V256),
                        religion = as.factor(V144), 
                        sex = as.factor(V240),
                        edu = as.factor(V248),
                        ethnic = as.factor(V254),
                        born_here = as.factor(V245),
                        citizen = as.factor(V246),
                        income_bracket = as.factor(V239),
                        class = as.factor(V238))

wvs <- cbind(wv_num, wv_labs)

write.csv(wvs, file="wvs.csv", row.names = FALSE)





