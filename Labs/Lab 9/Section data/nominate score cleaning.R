# read in ideology data
HSall_members_copy <- read_csv("~/Dropbox/ElectionAnalytics_Midterms/Lab sessions/issues and ideology/HSall_members_copy.csv")
ideo <- HSall_members_copy

# read in congress/year data
house_seat_share_by_party_1948_2020 <- read_csv("~/Dropbox/ElectionAnalytics_Midterms/Lab sessions/Intro/Section data/house seat share by party 1948-2020.csv")
congress_yrs <- house_seat_share_by_party_1948_2020
# mutate congress var - lowercase, keep digits only
congress_yrs <- congress_yrs %>%
  dplyr::rename(congress = Congress) 

congress_yrs$congress <- as.numeric(gsub("([0-9]+).*$", "\\1", congress_yrs$congress))

# subset to relevant years - 113-117th congress (2012-2021)
ideo_house <- ideo %>%
  filter(congress == 113 | congress == 114 | congress == 115 | congress == 116 | congress == 117 &
           chamber == 'House') %>%
  # add in 'year' ~ congress 
  mutate(year = case_when(congress == 113 ~ 2012,
                          congress == 114 ~ 2014,
                          congress == 115 ~ 2016, 
                          congress == 116 ~ 2018,
                          congress == 117 ~ 2020))
# state names
ideo_house$state <- abbr2state(ideo_house$state_abbrev)

# st_fips
ideo_house <- ideo_house %>%
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

# add in cd_fips ~ 'district_code'
nums <- c(1, 2, 3, 4, 5, 6, 7, 8, 9)
ideo_house$cd_fips <- ifelse(ideo_house$district_code %in% nums, 
                             paste0("0",ideo_house$district_code), 
                             ideo_house$district_code)
# st_cd_fips
ideo_house$st_cd_fips <- paste(ideo_house$st_fips,
                               ideo_house$cd_fips,
                               sep = "")

# filter out NAs
ideo_house <- ideo_house %>%
  filter(!is.na(state))

# merge with voteshare data at district-level
# district-level votes
dist_pv_df <- read_csv("~/Dropbox/ElectionAnalytics_Midterms/Lab sessions/fundamentals II_ incumbency/incumb_dist_1948-2020 (3).csv")

# select relevant years from voting data
dist_pv_df <- dist_pv_df %>%
  filter(year == 2012 | year == 2014 | year == 2016 | year == 2018 | year == 2020)

# merge
dist_pv_ideo <- dist_pv_df %>%
  inner_join(ideo_house, by = c('st_cd_fips', 'year'))

# select relevant vars
dist_pv_ideo <- dist_pv_ideo %>%
  select("year","state.x","st_fips.x", "cd_fips", "st_cd_fips","winner_party","RepVotes","RepCandidate","RepStatus","DemVotes","DemCandidate","DemStatus","RepVotesMajorPercent"        ,"DemVotesMajorPercent","winner_candidate","winner_candidate_inc","president_party","congress","party_code","occupancy","last_means","bioname","bioguide_id","born","died","nominate_dim1","nominate_dim2","nominate_log_likelihood","nominate_geo_mean_probability","nominate_number_of_votes"    ,"nominate_number_of_errors","conditional","nokken_poole_dim1","nokken_poole_dim2") %>%
  # winner name
  dplyr::rename(winner_name = bioname)

# models
# mutate voteshare
dist_pv_ideo <- dist_pv_ideo %>%
  mutate(winner_voteshare = case_when(winner_candidate == 'RepCandidate' ~ RepVotesMajorPercent,
                                      winner_candidate == 'DemCandidate' ~ DemVotesMajorPercent))

# mutate nominate - 0-1
dist_pv_ideo <- dist_pv_ideo %>%
  mutate(new_nom1 = abs(nominate_dim1),
         new_nom2 = abs(nominate_dim2))

# write csv
write.csv(dist_pv_ideo, "/Users/kiara/Dropbox/ElectionAnalytics_Midterms/Lab sessions/issues and ideology/ideo_pv.csv")