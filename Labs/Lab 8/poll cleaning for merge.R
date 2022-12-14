# st_cd_fips for merging
nums <- c(0, 1, 2, 3, 4, 5, 6, 7, 8, 9)
# load polls
polls_df$cd_fips <- ifelse(polls_df$seat_number %in% nums, 
                                 paste0("0",polls_df$seat_number), 
                               polls_df$seat_number)
table(polls_df$cd_fips)

polls_df <- polls_df %>%
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

# if the leading 0s are dropped for some reason
# polls_df$st_fips <- ifelse(polls_df$st_fips %in% nums, 
#                             paste0("0",polls_df$st_fips), 
#                            polls_df$st_fips)

table(polls_df$st_fips)


polls_df$st_cd_fips <- paste(polls_df$st_fips,
                             polls_df$cd_fips,
                              sep = "")

table(polls_df$st_cd_fips)

write.csv(polls_df, '~/.../polls_df.csv')
