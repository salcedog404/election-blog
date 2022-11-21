# read in current cycle results
library(tidyverse)
library(usmap)

# code from discussion

dat_names <- read_csv('~/Dropbox/ElectionAnalytics_Midterms/data/2022_4_0_3.csv', n_max = 0) %>% names()
dat <- read_csv('~/Dropbox/ElectionAnalytics_Midterms/data/2022_4_0_3.csv', col_names = dat_names, skip = 2)


##hand enter MA, ME, and MS
dat[dat$FIPS == 25901,'Democratic'] = 153081; dat[dat$FIPS == 25901,'Republican'] = 96415
dat[dat$FIPS == 25902,'Democratic'] = 177519; dat[dat$FIPS == 25902,'Republican'] = 90791
dat[dat$FIPS == 25903,'Democratic'] = 145379; dat[dat$FIPS == 25903,'Republican'] = 82568
dat[dat$FIPS == 25904,'Democratic'] = 0; dat[dat$FIPS == 25904,'Republican'] = 0
dat[dat$FIPS == 25905,'Democratic'] = 198126; dat[dat$FIPS == 25905,'Republican'] = 70570
dat[dat$FIPS == 25906,'Democratic'] = 189753; dat[dat$FIPS == 25906,'Republican'] = 107384
dat[dat$FIPS == 25907,'Democratic'] = 144902; dat[dat$FIPS == 25907,'Republican'] = 26481
dat[dat$FIPS == 25908,'Democratic'] = 184076; dat[dat$FIPS == 25908,'Republican'] = 80961
dat[dat$FIPS == 25909,'Democratic'] = 192941; dat[dat$FIPS == 25909,'Republican'] = 131774

dat[dat$FIPS == 23901,'Democratic'] = 218630; dat[dat$FIPS == 23901,'Republican'] = 128996
dat[dat$FIPS == 23902,'Democratic'] = 151440; dat[dat$FIPS == 23902,'Republican'] = 140895

dat[dat$FIPS == 28901,'Democratic'] = 44387; dat[dat$FIPS == 28901,'Republican'] = 119673
dat[dat$FIPS == 28902,'Democratic'] = 68427; dat[dat$FIPS == 28902,'Republican'] = 101154
dat[dat$FIPS == 28903,'Democratic'] = 51628; dat[dat$FIPS == 28903,'Republican'] = 126081
dat[dat$FIPS == 28904,'Democratic'] = 39292; dat[dat$FIPS == 28904,'Republican'] = 122128

##LA and FL races with no reporting b/c no contest
dat[dat$FIPS == 22904,'Democratic'] = 0; dat[dat$FIPS == 22904,'Republican'] = 0
dat[dat$FIPS == 12905,'Democratic'] = 0; dat[dat$FIPS == 12905,'Republican'] = 0

dat = dat %>%
  mutate(RepublicanPct = Republican/(Democratic + Republican)*100,
         DemocratPct = Democratic/(Democratic + Republican)*100,
         CD = ifelse(`Geographic Name` == 'At Large','00',CD),
         STATE_FIPS = str_pad(STATE_FIPS,2,'left','0'),
         GEOID = paste0(STATE_FIPS,CD),
         state_abbr = fips_info(STATE_FIPS)$abbr,
         abbr_fips = paste(state_abbr,CD,sep = '-'),
         Winner = ifelse(RepublicanPct>50,'Republican','Democrat')) ##for consistency with map data

dat[dat$FIPS == 25904,'Winner'] = 'Democrat' ##no reporting b/c no contest
dat[dat$FIPS == 22904,'Winner'] = 'Republican' ##no reporting b/c no contest
dat[dat$FIPS == 12905,'Winner'] = 'Republican' ##no reporting b/c no contest

uncontested.cds = c("AZ-08", "PA-15", "AZ-09", "SC-03", "FL-05", "SC-04", "IL-07", "TX-06", "LA-04", "TX-11", "MA-04", "TX-25", "NY-13", "TX-31", "PA-13", "WI-06", "PA-14",
                    "AL-01", "NY-09", "AL-06", "PA-03", "CA-10", "SD-00", "FL-06", "TX-19", "FL-18", "TX-26", "LA-06", "WI-08", "ND-00",
                    "CA-15", "CA-30", "CA-16", "CA-34", "CA-29", "CA-37")

dat.contested = dat %>%
  filter(!abbr_fips%in%uncontested.cds)

# popular vote - Dems
NationalDemVote.contested = sum(dat.contested$Democratic)/(sum(dat.contested$Democratic)+sum(dat.contested$Republican))*100
NationalDemVote.all = sum(dat$Democratic)/(sum(dat$Democratic)+sum(dat$Republican))*100
NationalDemSeat = sum(ifelse(dat$Winner=='Democrat',1,0),na.rm = T)
NationalDemSeat.contested = sum(ifelse(dat.contested$Winner=='Democrat',1,0),na.rm = T)

# popular vote - Reps
NationalRepVote.contested = sum(dat.contested$Republican)/(sum(dat.contested$Democratic)+sum(dat.contested$Republican))*100
NationalRepVote.all = sum(dat$Republican)/(sum(dat$Democratic)+sum(dat$Republican))*100
NationalRepSeat = sum(ifelse(dat$Winner=='Republican',1,0),na.rm = T)
NationalRepSeat.contested = sum(ifelse(dat.contested$Winner=='Republican',1,0),na.rm = T)


# calculate voteshare margin in actual results to match with 538 national toplines - Dem-Rep
NationalVoteshareMargin.all = NationalDemVote.all - NationalRepVote.all

# voteshare margin contested only
NationalVoteshareMargin.contested = NationalDemVote.contested - NationalRepVote.contested


######### 538 data ##########
library(tidyverse)

dat3 <- read_csv("538 election-forecasts-2022/house_district_toplines_2022.csv")
dat4 <- read_csv("538 election-forecasts-2022/house_national_toplines_2022.csv")


# 538 latest model predictions - national 
dat4last <- dat4[1, ]

# join with popular vote and seatshare results
joined <- cbind(dat4last, NationalDemVote.contested, NationalRepVote.contested, NationalDemSeat, NationalRepSeat, NationalVoteshareMargin.all)

# district-level results
# clean - 538 results - separate state and district
dat3$state_abb <- stringr::str_extract(dat3$district, "^.{2}")
dat3$cd <- as.numeric(sub(".*-", "", dat3$district))
# create state fips, cd fips
# get names from abbreviations
library(usdata)
dat3$state <- abbr2state(dat3$state)

dat3 <- dat3 %>%
  mutate(st_fips = case_when(state == 'Alabama' ~ '01',
                             state == 'Alaska' ~ '02',
                             state == 'Arizona' ~ '04',
                             state == 'Arkansas' ~ '05',
                             state =='California' ~ '06',
                             state =='Colorado' ~ '08',
                             state =='Connecticut' ~ '09',
                             state =='Delaware' ~ '10',
                             state == 'Florida' ~ '12',
                             state == 'Georgia' ~ '13',
                             state == 'Hawaii' ~ '15',
                             state =='Idaho' ~ '16',
                             state =='Illinois' ~ '17',
                             state =='Indiana' ~ '18',
                             state =='Iowa' ~ '19',
                             state =='Kansas' ~ '20',
                             state =='Kentucky' ~ '21',
                             state =='Louisiana' ~ '22',
                             state =='Maine' ~ '23',
                             state =='Maryland' ~ '24',
                             state =='Massachusetts' ~ '25',
                             state =='Michigan' ~ '26',
                             state =='Minnesota' ~ '27',
                             state =='Mississippi' ~ '28',
                             state =='Missouri' ~ '29',
                             state =='Montana' ~ '30',
                             state =='Nebraska' ~ '31',
                             state =='Nevada' ~ '32',
                             state =='New Hampshire' ~ '33',
                             state =='New Jersey' ~ '34',
                             state =='New Mexico' ~ '35',
                             state =='New York' ~ '36',
                             state =='North Carolina' ~ '37',
                             state =='North Dakota' ~ '38',
                             state =='Ohio' ~ '39',
                             state =='Oklahoma' ~ '40',
                             state =='Oregon' ~ '41',
                             state =='Pennsylvania' ~ '42',
                             state =='Rhode Island' ~ '44',
                             state =='South Carolina' ~ '45',
                             state =='South Dakota' ~ '46',
                             state =='Tennessee' ~ '47',
                             state =='Texas' ~ '48',
                             state =='Utah' ~ '49',
                             state =='Vermont' ~ '50',
                             state =='Virginia' ~ '51',
                             state =='Washington' ~ '53',
                             state =='West Virginia' ~ '54',
                             state =='Wisconsin' ~ '55',
                             state =='Wyoming' ~ '56'))  
table(dat3$st_fips)

# cd fips
nums <- c(1, 2, 3, 4, 5, 6, 7, 8, 9)
dat3$cd_fips <- ifelse(dat3$cd %in% nums, 
                           paste0("0",dat3$cd), 
                       dat3$cd)

table(dat3$cd_fips)
# st_cd_fips
dat3$st_cd_fips <- paste(dat3$st_fips,
                         dat3$cd_fips,
                             sep = "")
table(dat3$st_cd_fips)
# AL districts
dat3 <- dat3 %>%
  mutate(st_cd_fips = case_when(st_cd_fips == '5601' ~ '5600',
                                st_cd_fips == '5001' ~ '5000',
                                st_cd_fips == '4601' ~ '4600',
                                st_cd_fips == '3801' ~ '3800',
                                st_cd_fips == '1001' ~ '1000',
                                st_cd_fips == '0201' ~ '0200',
                                TRUE ~ as.character(st_cd_fips)))
table(dat3$st_cd_fips)

# clean actual data
dat$st_cd_fips <- paste(dat$STATE_FIPS,
                                  dat$CD,
                        sep = "")
table(dat$st_cd_fips)


# now select latest model results
dat3last <- dat3[1:435, ]

# join with actual results - by st_cd_fips
joined2 <- left_join(dat3last, dat, by = "st_cd_fips")

# classification accuracy
# Error (National) - voteshare - predicted Dem - actual Dem
print(joined$Dem_win) # actual
50 + joined$popvote_margin # predicted
47.26209 - 47.60252 # -0.34043

# Error (National) - seatshare - predicted Dem - actual Dem
print(joined$median_seats_Dparty) # predicted
213 # actual
joined$median_seats_Dparty - 213 # -7

# classification accuracy
# Error (district) - voteshare - predicted Dem - actual Dem
# # drop NAs
# joined2 <- joined2 %>%
#   filter(!is.na(DemocratPct))

# deal with uncontested seats
t <- joined2 %>%
  filter(is.na(name_I1) & is.na(name_R1))

joined2$DemocratPct # actual
joined2$voteshare_mean_D1 # predicted
joined2$district_pv_error <- joined2$voteshare_mean_D1 - joined2$DemocratPct  # raw
joined2 <- joined2 %>%
  mutate(district_pv_error = case_when(is.na(name_I1) & is.na(name_R1) ~ 0,
                                        TRUE ~ as.numeric(district_pv_error)))
joined2 <- joined2 %>%
 filter(!is.na(district_pv_error))
# RMSE (district)
#joined2$rmse <- sqrt(mean((joined2$voteshare_mean_D1 - joined2$DemocratPct)^2))
#dif <- joined2$voteshare_mean_D1 - joined2$DemocratPct
dif <- joined2$district_pv_error
mean(dif) # -0.84
sqrt(mean(dif^2)) # 5.88

# write_csv(state_error_538, "state_error_538.csv")

# Brier scores
# predicted probability of Dem winning - outcome (1/0)
# code binary outcomes
joined2$winner_dem <- ifelse(joined2$Winner == 'Democrat', 1, 0)
#joined2$winner_rep <- ifelse(joined2$Winner == 'Democrat', 0, 1)
dif <- joined2$winner_D1 - joined2$winner_dem
brier_score_dem <- mean((joined2$winner_D1 - joined2$winner_dem)^2)
brier_score_dem <- mean(dif^2)
#brier_score_rep <- mean((joined2$winner_R1 - joined2$winner_rep)^2)

# classification accuracy
joined2$dem_win_correct <- ifelse(joined2$winner_D1 > 0.5, 1, 0)
sum(joined2$winner_dem == joined2$dem_win_correct) / 426

# districts missed 
joined2$district_pred_true <- ifelse(joined2$winner_dem == joined2$dem_win_correct, 1, 0)
tab <- joined2 %>%
  filter(district_pred_true == 0)

# error df
dif <- joined2$district_pv_error

district_error_538 <- data.frame(district_id = joined2$district, district = joined2$st_cd_fips, error = dif)

write_csv(district_error_538, "district_error_538.csv")
  

### Economist ###
# national-level estimates
library(tidyverse)

# Economist latest model predictions - national - manual from website
# voteshare and seatshare
dat5last <- matrix(c(2022, "House", "11/8/2022", 49.8, 50.2, 210, 225), ncol = 7)
colnames(dat5last) <- c("cycle", "branch", "forecastdate", "dem_pv", "rep_pv", "dem_seats", "rep_seats")
dat5last <- as.data.frame(dat5last)

# join with popular vote and seatshare results
joined_pop_econ <- cbind(dat5last, NationalDemVote.all, NationalRepVote.all, NationalDemSeat, NationalRepSeat, NationalVoteshareMargin.all)

# classification accuracy
# Error (National) - voteshare - predicted Dem - actual Dem
print(joined_pop_econ$NationalDemVote.all) # actual
print(joined_pop_econ$dem_pv) # predicted
49.8 - 47.26209 # 2.537

# Error (National) - seatshare - predicted Dem - actual Dem
print(joined_pop_econ$NationalDemSeat) # actual
print(joined_pop_econ$dem_seats) # predicted
210 - 213 # -3

# classification accuracy
# Error (district) - voteshare - predicted Dem - actual Dem
# district-level results
dat5 <- read_csv("economist house_sims.csv")
# clean - economist results - separate state and district
dat5$state_abb <- stringr::str_extract(dat5$district_id, "^.{2}")
dat5$cd <- as.numeric(sub(".*\\.", "", dat5$district_id))
# create state fips, cd fips
# get names from abbreviations
library(usdata)
dat5$state <- abbr2state(dat5$state_abb)

dat5 <- dat5 %>%
  mutate(st_fips = case_when(state == 'Alabama' ~ '01',
                             state == 'Alaska' ~ '02',
                             state == 'Arizona' ~ '04',
                             state == 'Arkansas' ~ '05',
                             state =='California' ~ '06',
                             state =='Colorado' ~ '08',
                             state =='Connecticut' ~ '09',
                             state =='Delaware' ~ '10',
                             state == 'Florida' ~ '12',
                             state == 'Georgia' ~ '13',
                             state == 'Hawaii' ~ '15',
                             state =='Idaho' ~ '16',
                             state =='Illinois' ~ '17',
                             state =='Indiana' ~ '18',
                             state =='Iowa' ~ '19',
                             state =='Kansas' ~ '20',
                             state =='Kentucky' ~ '21',
                             state =='Louisiana' ~ '22',
                             state =='Maine' ~ '23',
                             state =='Maryland' ~ '24',
                             state =='Massachusetts' ~ '25',
                             state =='Michigan' ~ '26',
                             state =='Minnesota' ~ '27',
                             state =='Mississippi' ~ '28',
                             state =='Missouri' ~ '29',
                             state =='Montana' ~ '30',
                             state =='Nebraska' ~ '31',
                             state =='Nevada' ~ '32',
                             state =='New Hampshire' ~ '33',
                             state =='New Jersey' ~ '34',
                             state =='New Mexico' ~ '35',
                             state =='New York' ~ '36',
                             state =='North Carolina' ~ '37',
                             state =='North Dakota' ~ '38',
                             state =='Ohio' ~ '39',
                             state =='Oklahoma' ~ '40',
                             state =='Oregon' ~ '41',
                             state =='Pennsylvania' ~ '42',
                             state =='Rhode Island' ~ '44',
                             state =='South Carolina' ~ '45',
                             state =='South Dakota' ~ '46',
                             state =='Tennessee' ~ '47',
                             state =='Texas' ~ '48',
                             state =='Utah' ~ '49',
                             state =='Vermont' ~ '50',
                             state =='Virginia' ~ '51',
                             state =='Washington' ~ '53',
                             state =='West Virginia' ~ '54',
                             state =='Wisconsin' ~ '55',
                             state =='Wyoming' ~ '56'))  
table(dat5$st_fips)

# cd fips
nums <- c(1, 2, 3, 4, 5, 6, 7, 8, 9)
dat5$cd_fips <- ifelse(dat5$cd %in% nums, 
                       paste0("0",dat5$cd), 
                       dat5$cd)

table(dat5$cd_fips)
# st_cd_fips
dat5$st_cd_fips <- paste(dat5$st_fips,
                         dat5$cd_fips,
                         sep = "")
table(dat5$st_cd_fips)
# clean actual data
dat$st_cd_fips <- paste(dat$STATE_FIPS,
                        dat$CD,
                        sep = "")
table(dat$st_cd_fips)

# now select latest model results
# dat5last <- dat5[1:435, ]

# join with actual results - by st_cd_fips
joined_econ <- left_join(dat5, dat, by = "st_cd_fips")

# drop NAs
joined_econ <- joined_econ %>%
  filter(!is.na(DemocratPct))
# district-level voteshare comparison 
joined_econ$DemocratPct # actual
joined_econ$avg_dem_2_party_vote_share*100 # predicted
# predicted - actual 
joined_econ$district_pv_error <- (joined_econ$avg_dem_2_party_vote_share*100) - joined_econ$DemocratPct # raw
mean(joined_econ$district_pv_error)
# RMSE (district)
joined_econ$rmse <- sqrt(mean(((joined_econ$avg_dem_2_party_vote_share*100) - joined_econ$DemocratPct)^2))
dif <- (joined_econ$avg_dem_2_party_vote_share*100) - joined_econ$DemocratPct
mean_dif <- mean(((joined_econ$avg_dem_2_party_vote_share*100) - joined_econ$DemocratPct)^2)
sqrt(mean_dif)
mean(dif) # 0.33
sqrt(mean(dif^2)) # 3.39

# for Brier scores

# Brier scores
# predicted probability of Dem winning - outcome (1/0)
# code binary outcomes
joined_econ$winner_dem <- ifelse(joined_econ$Winner == 'Democrat', 1, 0)
#joined2$winner_rep <- ifelse(joined2$Winner == 'Democrat', 0, 1)
diff <- joined_econ$dem_win_prob - joined_econ$winner_dem
brier_score_dem_econ <- mean((joined_econ$dem_win_prob - joined_econ$winner_dem)^2)
brier_score_dem_econ <- mean(diff^2)
#brier_score_rep <- mean((joined2$winner_R1 - joined2$winner_rep)^2)

# classification accuracy
joined_econ$dem_win_correct <- ifelse(joined_econ$dem_win_prob > 0.5, 1, 0)
sum(joined_econ$winner_dem == joined_econ$dem_win_correct) / 426

# districts missed 
joined_econ$district_pred_true <- ifelse(joined_econ$winner_dem == joined_econ$dem_win_correct, 1, 0)
tab2 <- joined_econ %>%
  filter(district_pred_true == 0)

# error df
diff_ <- (joined_econ$avg_dem_2_party_vote_share*100) - joined_econ$DemocratPct

district_error_economist <- data.frame(district_id = joined_econ$district_id, district = joined_econ$st_cd_fips, error = diff_)

write_csv(district_error_economist, "district_error_economist.csv")


### extra code - needed? ###

# 538 predicted Dem voteshare > Rep voteshare - not clear how to get this?
dat4last$Dem_win.x <- dat4last$ > dat3last$voteshare_chal
# actual results Dem voteshare > Rep voteshare
joined$Dem_win.y <- joined$NationalDemVote.all > joined$NationalRepVote.all
# sum
sum(joined$Trump_win.x == joined$Trump_win.y) / 51 # 96% 

# 538 predicted Dem seatshare > Rep seatshare
joined$Dem_win_seats <- joined$mean_seats_Dparty > joined$mean_seats_Rparty
# actual results Dem seatshare > Rep seatshare
joined$Dem_win_seats <- joined$mean_seats_Dparty > joined$mean_seats_Rparty



# sum(joined$Trump_win.x == joined$Trump_win.y) / 51 # 96% 
# 
# joined$state[joined$Trump_win.x != joined$Trump_win.y] # (missed 2: NC, FL)

# # Error (National): -1.9
# 
# 45.4 - 47.3 # -1.9
# 
# # RMSE (state)
# 
# dif <- joined$Trump_pv2p.x*100 - joined$Trump_pv2p.y*100
# 
# sqrt(mean(dif^2)) # 3.02
# 
# mean(dif) # -2.44
# 
# joined$state
# 
# state_error_538 <- data.frame(state = joined$state, error = dif)
# 
# write_csv(state_error_538, "state_error_538.csv")




#### old code ####
# df <- read_csv("results2020_nov16.csv", skip=1)
# 
# df <- df[,c(2, 4, 5, 6)]
# colnames(df) <- c("state", "total", "D","R")
# df$year <- 2020
# df$D_pv2p <- df$D/df$total
# df$R_pv2p <- df$R/df$total

# # read in historical results
# dfsofar <- read_csv("../../datasets/popvote_bystate_1948-2016.csv")
# 
# # merge
# head(df)
# DF <- bind_rows(df, dfsofar)
# 
# write_csv(DF, "popvote_bystate_1948-2020.csv")

#dat1 <- read_csv("electoral_college_vs_popvote.csv")

#dat2 <- read_csv("presidential_ev_probabilities_2020.csv")

# dat3 <- read_csv("presidential_state_toplines_2020.csv")

# popular vote
# df$Trump_pv2p <- df$vote2 / (df$vote1 + df$vote2)

