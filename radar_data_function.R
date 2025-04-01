## Packages ##
#devtools::install_github("JaseZiv/worldfootballR")
library(worldfootballR)
library(dplyr)
library(stringr)
library(lubridate)
library(readxl)
library(openxlsx)

# Data Function
radar_data = function(){
  
  #### Prompts ####
  # Prompt user to select season
  end_year = as.numeric(readline(prompt = 'Choose season ending year (e.g 2023/2024 is 2024): '))
  
  # Get standard stats to prompt user with team/player selection
  standard_stats = fb_big5_advanced_season_stats(
    season_end_year = end_year,
    stat_type = 'standard',
    team_or_player = 'player',
    time_pause = 3)
  
  # Prompt user to select league
  league = readline(prompt = 
  'Pick one of Big 5 Leagues (La Liga, Premier League, Ligue 1, Serie A, Bundesliga): ')
  
  # Filter standard data based on league and return unique list of teams
  unique_teams = standard_stats %>% filter(Comp == league)
  print(unique(unique_teams$Squad))
  
  # Prompt user to select team
  team_name = readline(prompt = 'Copy and Paste desired team: ')
  
  # Filter standard data based on team and return unique list of players
  unique_players = unique_teams %>% filter(Squad == team_name)
  print(unique(unique_players$Player))
  
  # Prompt user to select player
  player_name = readline(prompt = 'Copy and paste desired player: ')
  
  # Prompt user to select type of radar
  radar_type = readline(prompt = 
    "Choose a position for the radar: (Centerback, Fullback, Midfielder, Winger/CAM, Striker): ")
  
  # Prompt user to determine number of 90's filter
  game_time = as.numeric(readline(prompt = 
            "How many 90's should the percentiles filter by: "))
  
  #### Centerback radar ####
  
    if(radar_type == 'Centerback'){
      
      #### Misc Stats ####
      
      # All Misc Stats
      misc_stats = fb_big5_advanced_season_stats(
        season_end_year = end_year,
        stat_type = 'misc',
        team_or_player = 'player',
        time_pause = 3)
      
      # Specific player misc
      player_misc_stats = misc_stats %>%
        filter(Player == player_name) %>%
        select(Mins_Per_90, Won_Aerial, Won_percent_Aerial, Fls) %>%
        mutate(Won_Aerial = round(Won_Aerial/Mins_Per_90,2),
               Fls = round(Fls/Mins_Per_90,2)) %>%
        select(!Mins_Per_90)
      
      # Percentile for different variables
      perc_misc_stats = misc_stats %>%
        mutate(Pos = strsplit(Pos, ',')) %>%
        filter(sapply(Pos, function(x) "DF" %in% x),
               Mins_Per_90 >= game_time) %>%
        select(Mins_Per_90, Won_Aerial, Won_percent_Aerial, Fls) %>%
        mutate(Won_Aerial = round(Won_Aerial/Mins_Per_90,2),
               Fls = round(Fls/Mins_Per_90,2)) %>%
        select(!Mins_Per_90) %>%
        reframe(Won_Aerial = quantile(Won_Aerial, probs = c(0.05, 0.95), na.rm = TRUE),,
                Fls = quantile(Fls, probs = c(0.05, 0.95), na.rm = TRUE),
                Won_percent_Aerial = quantile(Won_percent_Aerial, 
                                              probs = c(0.05, 0.95), na.rm = TRUE)) %>%
        mutate(Won_Aerial = round(Won_Aerial, 2),
               Fls = round(Fls, 2),
               Won_percent_Aerial = round(Won_percent_Aerial, 2))

      
      
      #### Defense Stats ####
      
      # All defense stats
      defense_stats = fb_big5_advanced_season_stats(
        season_end_year = end_year,
        stat_type = 'defense',
        team_or_player = 'player',
        time_pause = 3)
      
      # Specific player
      player_defense_stats = defense_stats %>%
        filter(Player == player_name) %>%
        select(Mins_Per_90, Clr, Tkl_percent_Challenges, Int, Sh_Blocks) %>%
        mutate(Clr = round(Clr/Mins_Per_90,2),
               Int = round(Int/Mins_Per_90,2),
               Sh_Blocks = round(Sh_Blocks/Mins_Per_90,2)) %>%
        select(!Mins_Per_90)
      
      # Percentiles for different variables
      perc_defense_stats = defense_stats %>%
        mutate(Pos = strsplit(Pos, ',')) %>%
        filter(sapply(Pos, function(x) "DF" %in% x),
               Mins_Per_90 >= game_time) %>%
        select(Mins_Per_90, Clr, Tkl_percent_Challenges, Int, Sh_Blocks) %>%
        mutate(Clr = round(Clr/Mins_Per_90,2),
               Int = round(Int/Mins_Per_90,2),
               Sh_Blocks = round(Sh_Blocks/Mins_Per_90,2)) %>%
        select(!Mins_Per_90) %>%
        reframe(
          Clr = quantile(Clr, probs = c(0.05, 0.95), na.rm = TRUE),
          Tkl_percent_Challenges = 
            quantile(Tkl_percent_Challenges, probs = c(0.05, 0.95), na.rm = TRUE),
          Int = quantile(Int, probs = c(0.05, 0.95), na.rm = TRUE),
          Sh_Blocks = 
            quantile(Sh_Blocks, probs = c(0.05, 0.95), na.rm = TRUE)) %>%
        mutate(Clr = round(Clr, 2),
               Tkl_percent_Challenges = round(Tkl_percent_Challenges,2),
               Int = round(Int,2),
               Sh_Blocks = round(Sh_Blocks,2))
      
      
      
      #### Passing Stats ####
      
      # All passing stats
      passing_stats = fb_big5_advanced_season_stats(
        season_end_year = end_year,
        stat_type = 'passing',
        team_or_player = 'player',
        time_pause = 3)
      
      # Specific player
      player_passing_stats = passing_stats %>%
        filter(Player == player_name) %>%
        select(Mins_Per_90, Cmp_percent_Total, Cmp_percent_Long, Final_Third) %>%
        mutate(Final_Third = round(Final_Third/Mins_Per_90,2)) %>%
        select(!Mins_Per_90)
      
      # Percentiles for different variables
      perc_passing_stats = passing_stats %>%
        mutate(Pos = strsplit(Pos, ',')) %>%
        filter(sapply(Pos, function(x) "DF" %in% x),
               Mins_Per_90 >= game_time) %>%
        select(Mins_Per_90, Cmp_percent_Total, Cmp_percent_Long, Final_Third) %>%
        mutate(Final_Third = round(Final_Third/Mins_Per_90,2)) %>%
        select(!Mins_Per_90) %>%
        reframe(
          Cmp_percent_Total = 
            quantile(Cmp_percent_Total, probs = c(0.05, 0.95), na.rm = TRUE),
          Cmp_percent_Long = 
            quantile(Cmp_percent_Long, probs = c(0.05, 0.95), na.rm = TRUE),
          Final_Third = 
            quantile(Final_Third, probs = c(0.05, 0.95), na.rm = TRUE)) %>%
        mutate(
          Cmp_percent_Total = round(Cmp_percent_Total, 2),
          Cmp_percent_Long = round(Cmp_percent_Long,2),
          Final_Third = round(Final_Third,2)
        )
      
      
      
      
      #### Possession Stats ####
      possession_stats = fb_big5_advanced_season_stats(
        season_end_year = end_year,
        stat_type = 'possession',
        team_or_player = 'player',
        time_pause = 3)
      
      player_possession_stats = possession_stats %>%
        filter(Player == player_name) %>%
        select(Mins_Per_90, PrgC_Carries) %>%
        mutate(PrgC_Carries = round(PrgC_Carries/Mins_Per_90,2)) %>%
        select(!Mins_Per_90)
      
      perc_possession_stats = possession_stats %>%
        mutate(Pos = strsplit(Pos, ',')) %>%
        filter(sapply(Pos, function(x) "DF" %in% x),
               Mins_Per_90 >= game_time) %>%
        select(Mins_Per_90, PrgC_Carries) %>%
        mutate(PrgC_Carries = round(PrgC_Carries/Mins_Per_90,2)) %>%
        select(!Mins_Per_90) %>%
        reframe(
          PrgC_Carries = 
            quantile(PrgC_Carries, probs = c(0.05, 0.95), na.rm = TRUE)) %>%
        mutate(
          PrgC_Carries = round(PrgC_Carries,2)
        )
      
      
      
      
      
      
      #### Combine player stats ####
      df_player = cbind(player_defense_stats, player_misc_stats, player_passing_stats,
                        player_possession_stats)
      df_perc = cbind(perc_defense_stats, perc_misc_stats, perc_passing_stats,
                      perc_possession_stats)
      df_id = data.frame(Name = c(player_name, '0.05 Percentile', '0.95 Percentile'))
      df = rbind(df_player, df_perc)
      df = cbind(df_id, df)
      
      df = df %>%
        select(Name, Won_Aerial, Won_percent_Aerial, Cmp_percent_Total,
               Cmp_percent_Long, PrgC_Carries, Final_Third,
               Tkl_percent_Challenges, 
               Int, Sh_Blocks, Clr, Fls)
      
      
      #### Player Information ####
      
      player_info = standard_stats %>%
        filter(Player == player_name) %>%
        select(Player, Squad, Age, Born, Comp, Season_End_Year, MP_Playing, Mins_Per_90_Playing)
      
      # Get birthday and age
      age_year_day = str_split(player_info$Age, '-')[[1]]
      age = as.numeric(age_year_day[1])
      birthday = Sys.Date() - as.numeric(age_year_day[2]) - years(age)
      
      # Write into player_info
      player_info = player_info %>%
        mutate(Age = age,
               Birthday = birthday) %>%
        select(!Born)
      
      # Game time filter value
      player_info$TimeFilter = game_time
      
      # Add position
      player_info$Position = radar_type
      
      #### Write to sheet ####
      file_name = paste('data_', gsub(" ", '', player_info$Player), '.xlsx', sep = '')
      datasets = list('Stats' = df, 'Info' = player_info)
      write.xlsx(datasets, file = file_name)}
  
  
  #### Fullback radar ####
    if(radar_type == 'Fullback'){
      
      #### Misc Stats ####
      
      # All misc stats
      misc_stats = fb_big5_advanced_season_stats(
        season_end_year = end_year,
        stat_type = 'misc',
        team_or_player = 'player',
        time_pause = 3)
      
      # Specific player
      player_misc_stats = misc_stats %>%
        filter(Player == player_name) %>%
        select(Mins_Per_90, Won_percent_Aerial, Fls) %>%
        mutate(Fls = round(Fls/Mins_Per_90,2)) %>%
        select(!Mins_Per_90)
      
      # Percentile for different variables
      perc_misc_stats = misc_stats %>%
        mutate(Pos = strsplit(Pos, ',')) %>%
        filter(sapply(Pos, function(x) "DF" %in% x),
               Mins_Per_90 >= game_time) %>%
        select(Mins_Per_90, Won_percent_Aerial, Fls) %>%
        mutate(Fls = round(Fls/Mins_Per_90,2)) %>%
        select(!Mins_Per_90) %>%
        reframe(Fls = quantile(Fls, probs = c(0.05, 0.95), na.rm = TRUE),
                Won_percent_Aerial = quantile(Won_percent_Aerial, 
                                              probs = c(0.05, 0.95), na.rm = TRUE)) %>%
        mutate(Fls = round(Fls, 2),
               Won_percent_Aerial = round(Won_percent_Aerial, 2))
      
      
      #### Defense Stats ####
      # All defense stats
      defense_stats = fb_big5_advanced_season_stats(
        season_end_year = end_year,
        stat_type = 'defense',
        team_or_player = 'player',
        time_pause = 3)
      
      # Specific player
      player_defense_stats = defense_stats %>%
        filter(Player == player_name) %>%
        select(Mins_Per_90, Clr, Tkl_percent_Challenges, "Tkl+Int") %>%
        rename(Tkl_Int = "Tkl+Int") %>%
        mutate(Tkl_Int = round(Tkl_Int/Mins_Per_90,2),
               Clr = round(Clr/Mins_Per_90,2)) %>%
        select(!Mins_Per_90)
      
      # Percentiles for different variables
      perc_defense_stats = defense_stats %>%
        mutate(Pos = strsplit(Pos, ',')) %>%
        filter(sapply(Pos, function(x) "DF" %in% x),
               Mins_Per_90 >= game_time) %>%
        select(Mins_Per_90, Clr, Tkl_percent_Challenges, "Tkl+Int") %>%
        rename(Tkl_Int = "Tkl+Int") %>%
        mutate(Tkl_Int = round(Tkl_Int/Mins_Per_90,2),
               Clr = round(Clr/Mins_Per_90,2)) %>%
        select(!Mins_Per_90) %>%
        reframe(
          Clr = 
            quantile(Clr, probs = c(0.05, 0.95), na.rm = TRUE),
          Tkl_percent_Challenges = 
            quantile(Tkl_percent_Challenges, probs = c(0.05, 0.95), na.rm = TRUE),
          Tkl_Int = quantile(Tkl_Int, probs = c(0.05, 0.95), na.rm = TRUE)) %>%
        mutate(Tkl_percent_Challenges = round(Tkl_percent_Challenges,2),
               Tkl_Int = round(Tkl_Int,2),
               Clr = round(Clr,2))
      
      
      #### Passing Stats ####
      # All passing stats
      passing_stats = fb_big5_advanced_season_stats(
        season_end_year = end_year,
        stat_type = 'passing',
        team_or_player = 'player',
        time_pause = 3)
      
      # Specific player
      player_passing_stats = passing_stats %>%
        filter(Player == player_name) %>%
        select(Mins_Per_90, Cmp_percent_Total, Final_Third, xAG, CrsPA) %>%
        mutate(Final_Third = round(Final_Third/Mins_Per_90,2),
               xAG = round(xAG/Mins_Per_90, 2),
               CrsPA = round(CrsPA/Mins_Per_90,2)) %>%
        select(!Mins_Per_90)
      
      # Percentiles for different variables
      perc_passing_stats = passing_stats %>%
        mutate(Pos = strsplit(Pos, ',')) %>%
        filter(sapply(Pos, function(x) "DF" %in% x),
               Mins_Per_90 >= game_time) %>%
        select(Mins_Per_90, Cmp_percent_Total, Final_Third, xAG, CrsPA) %>%
        mutate(Final_Third = round(Final_Third/Mins_Per_90,2),
               xAG = round(xAG/Mins_Per_90, 2),
               CrsPA = round(CrsPA/Mins_Per_90,2)) %>%
        select(!Mins_Per_90) %>%
        reframe(
          Cmp_percent_Total = 
            quantile(Cmp_percent_Total, probs = c(0.05, 0.95), na.rm = TRUE),
          Final_Third = 
            quantile(Final_Third, probs = c(0.05, 0.95), na.rm = TRUE),
          xAG = 
            quantile(xAG, probs = c(0.05, 0.95), na.rm = TRUE),
          CrsPA = 
            quantile(CrsPA, probs = c(0.05, 0.95), na.rm = TRUE),) %>%
        mutate(
          Cmp_percent_Total = round(Cmp_percent_Total, 2),
          Final_Third = round(Final_Third,2),
          xAG = round(xAG,2),
          CrsPA = round(CrsPA,2)
        )
      
      #### Possession Stats ####
      possession_stats = fb_big5_advanced_season_stats(
        season_end_year = end_year,
        stat_type = 'possession',
        team_or_player = 'player',
        time_pause = 3)
      
      player_possession_stats = possession_stats %>%
        filter(Player == player_name) %>%
        select(Mins_Per_90, PrgC_Carries) %>%
        mutate(PrgC_Carries = round(PrgC_Carries/Mins_Per_90,2)) %>%
        select(!Mins_Per_90)
      
      perc_possession_stats = possession_stats %>%
        mutate(Pos = strsplit(Pos, ',')) %>%
        filter(sapply(Pos, function(x) "DF" %in% x),
               Mins_Per_90 >= game_time) %>%
        select(Mins_Per_90, PrgC_Carries) %>%
        mutate(PrgC_Carries = round(PrgC_Carries/Mins_Per_90, 2)) %>%
        select(!Mins_Per_90) %>%
        reframe(
          PrgC_Carries = 
            quantile(PrgC_Carries, probs = c(0.05, 0.95), na.rm = TRUE)) %>%
        mutate(
          PrgC_Carries = round(PrgC_Carries,2)
        )
      
      
      
      
      
      
      #### GCA/SCA Stats ####
      sca_stats = fb_big5_advanced_season_stats(
        season_end_year = end_year,
        stat_type = 'gca',
        team_or_player = 'player',
        time_pause = 3)
      
      player_sca_stats = sca_stats %>%
        filter(Player == player_name) %>%
        select(SCA90_SCA)
      
      perc_sca_stats = sca_stats %>%
        mutate(Pos = strsplit(Pos, ',')) %>%
        filter(sapply(Pos, function(x) "DF" %in% x),
               Mins_Per_90 >= game_time) %>%
        select(SCA90_SCA) %>%
        reframe(
          SCA90_SCA = 
            quantile(SCA90_SCA, probs = c(0.05, 0.95), na.rm = TRUE)) %>%
        mutate(
          SCA90_SCA = round(SCA90_SCA,2))
      
      #### Combine player stats ####
      df_player = cbind(player_misc_stats, player_defense_stats, player_passing_stats,
                        player_possession_stats, player_sca_stats)
      df_perc = cbind(perc_misc_stats, perc_defense_stats, perc_passing_stats,
                      perc_possession_stats, perc_sca_stats)
      df_id = data.frame(Name = c(player_name, '0.05 Percentile', '0.95 Percentile'))
      df = rbind(df_player, df_perc)
      df = cbind(df_id, df)
      
      df = df %>%
        select(Name, Tkl_percent_Challenges, PrgC_Carries,
               Final_Third, CrsPA, xAG, SCA90_SCA, Cmp_percent_Total, Fls, Won_percent_Aerial,
               Clr, Tkl_Int)
      
      
      #### Player Information ####

      player_info = standard_stats %>%
        filter(Player == player_name) %>%
        select(Player, Squad, Age, Born, Comp, Season_End_Year, MP_Playing, Mins_Per_90_Playing)
      
      # Get birthday and age
      age_year_day = str_split(player_info$Age, '-')[[1]]
      age = as.numeric(age_year_day[1])
      birthday = Sys.Date() - as.numeric(age_year_day[2]) - years(age)
      
      # Write into player_info
      player_info = player_info %>%
        mutate(Age = age,
               Birthday = birthday) %>%
        select(!Born)
      
      # Game time filter value
      player_info$TimeFilter = game_time
      
      # Add position
      player_info$Position = radar_type
      
      #### Write to sheet ####
      file_name = paste('data_', gsub(" ", '', player_info$Player), '.xlsx', sep = '')
      datasets = list('Stats' = df, 'Info' = player_info)
      write.xlsx(datasets, file = file_name)
    }
  
  
  #### Midfielder radar ####
    if(radar_type == 'Midfielder'){
      
      #### Misc Stats ####
      # All misc stats
      misc_stats = fb_big5_advanced_season_stats(
        season_end_year = end_year,
        stat_type = 'misc',
        team_or_player = 'player',
        time_pause = 3)
      
      # Specific player
      player_misc_stats = misc_stats %>%
        filter(Player == player_name) %>%
        select(Mins_Per_90, Fld, Recov, Fls) %>%
        mutate(Fld = round(Fld/Mins_Per_90,2),
               Recov = round(Recov/Mins_Per_90,2),
               Fls = round(Fls/Mins_Per_90,2)) %>%
        select(!Mins_Per_90)
      
      # Percentile for different variables
      perc_misc_stats = misc_stats %>%
        mutate(Pos = strsplit(Pos, ',')) %>%
        filter(sapply(Pos, function(x) "MF" %in% x),
               Mins_Per_90 >= game_time) %>%
        select(Mins_Per_90,  Fld, Recov, Fls) %>%
        mutate(Fld = round(Fld/Mins_Per_90,2),
               Recov = round(Recov/Mins_Per_90,2),
               Fls = round(Fls/Mins_Per_90,2)) %>%
        select(!Mins_Per_90) %>%
        reframe(Fld = quantile(Fld, probs = c(0.05, 0.95), na.rm = TRUE),,
                Fls = quantile(Fls, probs = c(0.05, 0.95), na.rm = TRUE),
                Recov = quantile(Recov, probs = c(0.05, 0.95), na.rm = TRUE)) %>%
        mutate(Fld = round(Fld, 2),
               Fls = round(Fls, 2),
               Recov = round(Recov, 2))
      
      #### Defense Stats ####
      # All defense stats
      defense_stats = fb_big5_advanced_season_stats(
        season_end_year = end_year,
        stat_type = 'defense',
        team_or_player = 'player',
        time_pause = 3)
      
      # Specific player
      player_defense_stats = defense_stats %>%
        filter(Player == player_name) %>%
        select(Mins_Per_90, Tkl_percent_Challenges, Int, Tkl_Tackles) %>%
        mutate(Tkl_Tackles = round(Tkl_Tackles/Mins_Per_90,2),
               Int = round(Int/Mins_Per_90,2)) %>%
        select(!Mins_Per_90)
      
      # Percentiles for different variables
      perc_defense_stats = defense_stats %>%
        mutate(Pos = strsplit(Pos, ',')) %>%
        filter(sapply(Pos, function(x) "MF" %in% x),
               Mins_Per_90 >= game_time) %>%
        select(Mins_Per_90, Tkl_percent_Challenges, Int, Tkl_Tackles) %>%
        mutate(Tkl_Tackles = round(Tkl_Tackles/Mins_Per_90,2),
               Int = round(Int/Mins_Per_90,2)) %>%
        select(!Mins_Per_90) %>%
        reframe(
          Tkl_percent_Challenges = 
            quantile(Tkl_percent_Challenges, probs = c(0.05, 0.95), na.rm = TRUE),
          Int = quantile(Int, probs = c(0.05, 0.95), na.rm = TRUE),
          Tkl_Tackles = 
            quantile(Tkl_Tackles, probs = c(0.05, 0.95), na.rm = TRUE)) %>%
        mutate(
          Tkl_percent_Challenges = round(Tkl_percent_Challenges,2),
          Int = round(Int,2),
          Tkl_Tackles = round(Tkl_Tackles,2))
      
      #### Passing Stats ####
      # All passing stats
      passing_stats = fb_big5_advanced_season_stats(
        season_end_year = end_year,
        stat_type = 'passing',
        team_or_player = 'player',
        time_pause = 3)
      
      # Specific player
      player_passing_stats = passing_stats %>%
        filter(Player == player_name) %>%
        select(Mins_Per_90, xAG, Final_Third, Cmp_percent_Total) %>%
        mutate(
          xAG = round(xAG/Mins_Per_90, 2),
          Final_Third = round(Final_Third/Mins_Per_90,2)) %>%
        select(!Mins_Per_90)
      
      # Percentiles for different variables
      perc_passing_stats = passing_stats %>%
        mutate(Pos = strsplit(Pos, ',')) %>%
        filter(sapply(Pos, function(x) 'MF' %in% x),
               Mins_Per_90 >= game_time) %>%
        select(Mins_Per_90, xAG, Final_Third, Cmp_percent_Total) %>%
        mutate(xAG = round(xAG/Mins_Per_90, 2),
               Final_Third = round(Final_Third/Mins_Per_90,2)) %>%
        select(!Mins_Per_90) %>%
        reframe(
          xAG = 
            quantile(xAG, probs = c(0.05, 0.95), na.rm = TRUE),
          Final_Third = 
            quantile(Final_Third, probs = c(0.05, 0.95), na.rm = TRUE),
          Cmp_percent_Total = 
            quantile(Cmp_percent_Total, probs = c(0.05, 0.95), na.rm = TRUE)) %>%
        mutate(
          xAG = round(xAG,2),
          Final_Third = round(Final_Third,2),
          Cmp_percent_Total = round(Cmp_percent_Total,2)
        )
      
      
      #### Standard Stats ####
      # All standard stats
      
      # Player standard stats
      player_standard_stats = standard_stats %>%
        filter(Player == player_name) %>%
        select(Mins_Per_90_Playing, PrgC_Progression, PrgP_Progression) %>%
        mutate(PrgC_Progression = round(PrgC_Progression/Mins_Per_90_Playing,2),
               PrgP_Progression = round(PrgP_Progression/Mins_Per_90_Playing,2)) %>%
        select(!Mins_Per_90_Playing)
      
      # Percentile for different variables
      perc_standard_stats = standard_stats %>%
        mutate(Pos = strsplit(Pos, ',')) %>%
        filter(sapply(Pos, function(x) 'MF' %in% x),
               Mins_Per_90_Playing >= game_time) %>%
        select(Mins_Per_90_Playing, PrgC_Progression, PrgP_Progression) %>%
        mutate(PrgC_Progression = round(PrgC_Progression/Mins_Per_90_Playing,2),
               PrgP_Progression = round(PrgP_Progression/Mins_Per_90_Playing,2)) %>%
        select(!Mins_Per_90_Playing) %>%
        reframe(PrgC_Progression = 
                  quantile(PrgC_Progression, probs = c(0.05, 0.95), na.rm = TRUE),
                PrgP_Progression = quantile(PrgP_Progression, 
                                            probs = c(0.05, 0.95), na.rm = TRUE)) %>%
        mutate(PrgC_Progression = round(PrgC_Progression, 2),
               PrgP_Progression = round(PrgP_Progression, 2))
      
      #### Combine player stats ####
      df_player = cbind(player_misc_stats, player_defense_stats, player_passing_stats,
                        player_standard_stats)
      df_perc = cbind(perc_misc_stats, perc_defense_stats, perc_passing_stats,
                      perc_standard_stats)
      df_id = data.frame(Name = c(player_name, '0.05 Percentile', '0.95 Percentile'))
      df = rbind(df_player, df_perc)
      df = cbind(df_id, df)
      
      df = df %>%
        select(Name, Cmp_percent_Total, PrgC_Progression, PrgP_Progression,
               xAG, Final_Third, Fld, Fls, Recov, Int, Tkl_Tackles, 
               Tkl_percent_Challenges)
      #### Player Information ####
      player_info = standard_stats %>%
        filter(Player == player_name) %>%
        select(Player, Squad, Age, Born, Comp, Season_End_Year, MP_Playing, Mins_Per_90_Playing)
      
      # Get birthday and age
      age_year_day = str_split(player_info$Age, '-')[[1]]
      age = as.numeric(age_year_day[1])
      birthday = Sys.Date() - as.numeric(age_year_day[2]) - years(age)
      
      # Write into player_info
      player_info = player_info %>%
        mutate(Age = age,
               Birthday = birthday) %>%
        select(!Born)
      
      # Game time filter value
      player_info$TimeFilter = game_time
      
      # Add position
      player_info$Position = radar_type
      
      #### Write to sheet ####
      file_name = paste('data_', gsub(" ", '', player_info$Player), '.xlsx', sep = '')
      datasets = list('Stats' = df, 'Info' = player_info)
      write.xlsx(datasets, file = file_name)
      
    }
  
  #### Winger/CAM radar ####
    if(radar_type == 'Winger/CAM'){
      #### Misc Stats ####
      # All misc stats
      misc_stats = fb_big5_advanced_season_stats(
        season_end_year = end_year,
        stat_type = 'misc',
        team_or_player = 'player',
        time_pause = 3)
      
      # Specific player
      player_misc_stats = misc_stats %>%
        filter(Player == player_name) %>%
        select(Mins_Per_90, Fld) %>%
        mutate(Fld = round(Fld/Mins_Per_90,2)) %>%
        select(!Mins_Per_90)
      
      # Percentile for different variables
      perc_misc_stats = misc_stats %>%
        mutate(Pos = strsplit(Pos, ',')) %>%
        filter(sapply(Pos, function(x) any(c('MF', 'FW') %in% x)),
               Mins_Per_90 >= game_time) %>%
        select(Mins_Per_90, Fld) %>%
        mutate(Fld = round(Fld/Mins_Per_90,2)) %>%
        select(!Mins_Per_90) %>%
        reframe(Fld = quantile(Fld, probs = c(0.05, 0.95), na.rm = TRUE)) %>%
        mutate(Fld = round(Fld, 2))
      
      #### Defense Stats ####
      # All defense stats
      defense_stats = fb_big5_advanced_season_stats(
        season_end_year = end_year,
        stat_type = 'defense',
        team_or_player = 'player',
        time_pause = 3)
      
      # Specific player
      player_defense_stats = defense_stats %>%
        filter(Player == player_name) %>%
        select(Mins_Per_90, "Tkl+Int") %>%
        rename(Tkl_Int = "Tkl+Int") %>%
        mutate(Tkl_Int = round(Tkl_Int/Mins_Per_90,2)) %>%
        select(!Mins_Per_90)
      
      # Percentiles for different variables
      perc_defense_stats = defense_stats %>%
        mutate(Pos = strsplit(Pos, ',')) %>%
        filter(sapply(Pos, function(x) any(c('MF', 'FW') %in% x)),
               Mins_Per_90 >= game_time) %>%
        select(Mins_Per_90, "Tkl+Int") %>%
        rename(Tkl_Int = "Tkl+Int") %>%
        mutate(Tkl_Int = round(Tkl_Int/Mins_Per_90,2)) %>%
        select(!Mins_Per_90) %>%
        reframe(Tkl_Int = quantile(Tkl_Int, probs = c(0.05, 0.95), na.rm = TRUE)) %>%
        mutate(Tkl_Int = round(Tkl_Int,2))
      
      #### Passing Stats ####
      # All passing stats
      passing_stats = fb_big5_advanced_season_stats(
        season_end_year = end_year,
        stat_type = 'passing',
        team_or_player = 'player',
        time_pause = 3)
      
      # Specific player
      player_passing_stats = passing_stats %>%
        filter(Player == player_name) %>%
        select(Mins_Per_90, xAG, PPA) %>%
        mutate(
          xAG = round(xAG/Mins_Per_90, 2),
          PPA = round(PPA/Mins_Per_90,2)) %>%
        select(!Mins_Per_90)
      
      # Percentiles for different variables
      perc_passing_stats = passing_stats %>%
        mutate(Pos = strsplit(Pos, ',')) %>%
        filter(sapply(Pos, function(x) any(c('MF', 'FW') %in% x)),
               Mins_Per_90 >= game_time) %>%
        select(Mins_Per_90, xAG, PPA) %>%
        mutate(xAG = round(xAG/Mins_Per_90, 2),
               PPA = round(PPA/Mins_Per_90,2)) %>%
        select(!Mins_Per_90) %>%
        reframe(
          xAG = 
            quantile(xAG, probs = c(0.05, 0.95), na.rm = TRUE),
          PPA = 
            quantile(PPA, probs = c(0.05, 0.95), na.rm = TRUE),) %>%
        mutate(
          xAG = round(xAG,2),
          PPA = round(PPA,2)
        )
      #### GCA/SCA Stats ####
      sca_stats = fb_big5_advanced_season_stats(
        season_end_year = end_year,
        stat_type = 'gca',
        team_or_player = 'player',
        time_pause = 3)
      
      player_sca_stats = sca_stats %>%
        filter(Player == player_name) %>%
        select(SCA90_SCA)
      
      perc_sca_stats = sca_stats %>%
        mutate(Pos = strsplit(Pos, ',')) %>%
        filter(sapply(Pos, function(x) any(c('MF', 'FW') %in% x)),
               Mins_Per_90 >= game_time) %>%
        select(SCA90_SCA) %>%
        reframe(
          SCA90_SCA = 
            quantile(SCA90_SCA, probs = c(0.05, 0.95), na.rm = TRUE)) %>%
        mutate(
          SCA90_SCA = round(SCA90_SCA,2))
      
      #### Standard Stats ####
      # Player standard stats
      player_standard_stats = standard_stats %>%
        filter(Player == player_name) %>%
        select(Mins_Per_90_Playing, PrgC_Progression, PrgP_Progression) %>%
        mutate(PrgC_Progression = round(PrgC_Progression/Mins_Per_90_Playing,2),
               PrgP_Progression = round(PrgP_Progression/Mins_Per_90_Playing,2)) %>%
        select(!Mins_Per_90_Playing)
      
      # Percentile for different variables
      perc_standard_stats = standard_stats %>%
        mutate(Pos = strsplit(Pos, ',')) %>%
        filter(sapply(Pos, function(x) any(c('MF', 'FW') %in% x)),
               Mins_Per_90_Playing >= game_time) %>%
        select(Mins_Per_90_Playing, PrgC_Progression, PrgP_Progression) %>%
        mutate(PrgC_Progression = round(PrgC_Progression/Mins_Per_90_Playing,2),
               PrgP_Progression = round(PrgP_Progression/Mins_Per_90_Playing,2)) %>%
        select(!Mins_Per_90_Playing) %>%
        reframe(PrgC_Progression = 
                  quantile(PrgC_Progression, probs = c(0.05, 0.95), na.rm = TRUE),
                PrgP_Progression = quantile(PrgP_Progression, 
                                            probs = c(0.05, 0.95), na.rm = TRUE)) %>%
        mutate(PrgC_Progression = round(PrgC_Progression, 2),
               PrgP_Progression = round(PrgP_Progression, 2))
      
      
      #### Shooting Stats ####
      # All Shooting stats
      shooting_stats = fb_big5_advanced_season_stats(
        season_end_year = end_year,
        stat_type = 'shooting',
        team_or_player = 'player',
        time_pause = 3)
      
      # Specific player
      player_shooting_stats = shooting_stats %>%
        filter(Player == player_name) %>%
        select(Mins_Per_90, npxG_Expected, Sh_per_90_Standard, Dist_Standard,
               npxG_per_Sh_Expected) %>%
        mutate(npxG_Expected = round(npxG_Expected/Mins_Per_90,2)) %>%
        select(!Mins_Per_90)
      
      # Percentile for different variables
      perc_shooting_stats = shooting_stats %>%
        mutate(Pos = strsplit(Pos, ',')) %>%
        filter(sapply(Pos, function(x) any(c('MF', 'FW') %in% x)),
               Mins_Per_90 >= game_time) %>%
        select(Mins_Per_90, npxG_Expected, Sh_per_90_Standard, Dist_Standard,
               npxG_per_Sh_Expected) %>%
        mutate(npxG_Expected = round(npxG_Expected/Mins_Per_90,2)) %>%
        select(!Mins_Per_90) %>%
        reframe(npxG_Expected = 
                  quantile(npxG_Expected, probs = c(0.05, 0.95), na.rm = TRUE),
                Sh_per_90_Standard = quantile(Sh_per_90_Standard, 
                                              probs = c(0.05, 0.95), na.rm = TRUE),
                npxG_per_Sh_Expected = quantile(npxG_per_Sh_Expected, 
                                                probs = c(0.05, 0.95), na.rm = TRUE),
                Dist_Standard = quantile(Dist_Standard, 
                                         probs = c(0.05, 0.95), na.rm = TRUE)) %>%
        mutate(npxG_Expected = round(npxG_Expected, 2),
               Sh_per_90_Standard = round(Sh_per_90_Standard, 2),
               npxG_per_Sh_Expected = round(npxG_per_Sh_Expected, 2),
               Dist_Standard = round(Dist_Standard,2))
      
      #### Combine player stats ####
      df_player = cbind(player_misc_stats, player_defense_stats, player_passing_stats,
                        player_sca_stats, player_standard_stats, player_shooting_stats
      )
      df_perc = cbind(perc_misc_stats, perc_defense_stats, perc_passing_stats,
                      perc_sca_stats, perc_standard_stats, perc_shooting_stats
      )
      df_id = data.frame(Name = c(player_name, '0.05 Percentile', '0.95 Percentile'))
      df = rbind(df_player, df_perc)
      df = cbind(df_id, df)
      
      df = df %>%
        select(Name, npxG_Expected, npxG_per_Sh_Expected, Fld, Tkl_Int, xAG,
               PPA, PrgC_Progression, PrgP_Progression, SCA90_SCA, Dist_Standard,
               Sh_per_90_Standard)
      
      #### Player Information ####
      player_info = standard_stats %>%
        filter(Player == player_name) %>%
        select(Player, Squad, Age, Born, Comp, Season_End_Year, MP_Playing, Mins_Per_90_Playing)
      
      # Get birthday and age
      age_year_day = str_split(player_info$Age, '-')[[1]]
      age = as.numeric(age_year_day[1])
      birthday = Sys.Date() - as.numeric(age_year_day[2]) - years(age)
      
      # Write into player_info
      player_info = player_info %>%
        mutate(Age = age,
               Birthday = birthday) %>%
        select(!Born)
      
      # Game time filter value
      player_info$TimeFilter = game_time
      
      # Add position
      player_info$Position = radar_type
      
      #### Write to sheet ####
      file_name = paste('data_', gsub(" ", '', player_info$Player), '.xlsx', sep = '')
      datasets = list('Stats' = df, 'Info' = player_info)
      write.xlsx(datasets, file = file_name)
    }
  #### Striker radar ####
    if(radar_type == 'Striker'){
      #### Misc Stats ####
      # All misc stats
      misc_stats = fb_big5_advanced_season_stats(
        season_end_year = end_year,
        stat_type = 'misc',
        team_or_player = 'player',
        time_pause = 3)
      
      # Specific player
      player_misc_stats = misc_stats %>%
        filter(Player == player_name) %>%
        select(Mins_Per_90, Won_Aerial, Fld) %>%
        mutate(Fld = round(Fld/Mins_Per_90,2),
               Won_Aerial = round(Won_Aerial/Mins_Per_90,2)) %>%
        select(!Mins_Per_90)
      
      # Percentile for different variables
      perc_misc_stats = misc_stats %>%
        mutate(Pos = strsplit(Pos, ',')) %>%
        filter(sapply(Pos, function(x) 'FW' %in% x),
               Mins_Per_90 >= game_time) %>%
        select(Mins_Per_90, Won_Aerial, Fld) %>%
        mutate(Fld = round(Fld/Mins_Per_90,2),
               Won_Aerial = round(Won_Aerial/Mins_Per_90,2)) %>%
        select(!Mins_Per_90) %>%
        reframe(Fld = quantile(Fld, probs = c(0.05, 0.95), na.rm = TRUE),
                Won_Aerial = quantile(Won_Aerial, probs = c(0.05, 0.95), na.rm = TRUE)) %>%
        mutate(Fld = round(Fld, 2),
               Won_Aerial = round(Won_Aerial,2))
      
      
      #### Passing Stats ####
      # All passing stats
      passing_stats = fb_big5_advanced_season_stats(
        season_end_year = end_year,
        stat_type = 'passing',
        team_or_player = 'player',
        time_pause = 3)
      
      # Specific player
      player_passing_stats = passing_stats %>%
        filter(Player == player_name) %>%
        select(Mins_Per_90, xAG, PPA) %>%
        mutate(
          xAG = round(xAG/Mins_Per_90, 2),
          PPA = round(PPA/Mins_Per_90,2)) %>%
        select(!Mins_Per_90)
      
      # Percentiles for different variables
      perc_passing_stats = passing_stats %>%
        mutate(Pos = strsplit(Pos, ',')) %>%
        filter(sapply(Pos, function(x) 'FW' %in% x),
               Mins_Per_90 >= game_time) %>%
        select(Mins_Per_90, xAG, PPA) %>%
        mutate(xAG = round(xAG/Mins_Per_90, 2),
               PPA = round(PPA/Mins_Per_90,2)) %>%
        select(!Mins_Per_90) %>%
        reframe(
          xAG = 
            quantile(xAG, probs = c(0.05, 0.95), na.rm = TRUE),
          PPA = 
            quantile(PPA, probs = c(0.05, 0.95), na.rm = TRUE),) %>%
        mutate(
          xAG = round(xAG,2),
          PPA = round(PPA,2)
        )
      
      
      #### Possession Stats ####
      possession_stats = fb_big5_advanced_season_stats(
        season_end_year = end_year,
        stat_type = 'possession',
        team_or_player = 'player',
        time_pause = 3)
      
      player_possession_stats = possession_stats %>%
        filter(Player == player_name) %>%
        select(Mins_Per_90, PrgC_Carries, CPA_Carries) %>%
        mutate(PrgC_Carries = round(PrgC_Carries/Mins_Per_90,2),
               CPA_Carries = round(CPA_Carries/Mins_Per_90,2)) %>%
        select(!Mins_Per_90)
      
      perc_possession_stats = possession_stats %>%
        mutate(Pos = strsplit(Pos, ',')) %>%
        filter(sapply(Pos, function(x) 'FW' %in% x),
               Mins_Per_90 >= game_time) %>%
        select(Mins_Per_90, PrgC_Carries, CPA_Carries) %>%
        mutate(PrgC_Carries = round(PrgC_Carries/Mins_Per_90,2),
               CPA_Carries = round(CPA_Carries/Mins_Per_90,2)) %>%
        select(!Mins_Per_90) %>%
        reframe(
          PrgC_Carries = 
            quantile(PrgC_Carries, probs = c(0.05, 0.95), na.rm = TRUE),
          CPA_Carries = 
            quantile(CPA_Carries, probs = c(0.05, 0.95), na.rm = TRUE)) %>%
        mutate(
          PrgC_Carries = round(PrgC_Carries,2),
          CPA_Carries = round(CPA_Carries,2)
        )
      
      
      
      
      
      
      
      #### GCA/SCA Stats ####
      sca_stats = fb_big5_advanced_season_stats(
        season_end_year = end_year,
        stat_type = 'gca',
        team_or_player = 'player',
        time_pause = 3)
      
      player_sca_stats = sca_stats %>%
        filter(Player == player_name) %>%
        select(SCA90_SCA)
      
      perc_sca_stats = sca_stats %>%
        mutate(Pos = strsplit(Pos, ',')) %>%
        filter(sapply(Pos, function(x) 'FW' %in% x),
               Mins_Per_90 >= game_time) %>%
        select(SCA90_SCA) %>%
        reframe(
          SCA90_SCA = 
            quantile(SCA90_SCA, probs = c(0.05, 0.95), na.rm = TRUE)) %>%
        mutate(
          SCA90_SCA = round(SCA90_SCA,2))
      
      
      
      #### Shooting Stats ####
      # All Shooting stats
      shooting_stats = fb_big5_advanced_season_stats(
        season_end_year = end_year,
        stat_type = 'shooting',
        team_or_player = 'player',
        time_pause = 3)
      
      # Specific player
      player_shooting_stats = shooting_stats %>%
        filter(Player == player_name) %>%
        select(Mins_Per_90, npxG_Expected, Sh_per_90_Standard, Dist_Standard,
               npxG_per_Sh_Expected) %>%
        mutate(npxG_Expected = round(npxG_Expected/Mins_Per_90,2)) %>%
        select(!Mins_Per_90)
      
      # Percentile for different variables
      perc_shooting_stats = shooting_stats %>%
        mutate(Pos = strsplit(Pos, ',')) %>%
        filter(sapply(Pos, function(x) 'FW' %in% x),
               Mins_Per_90 >= game_time) %>%
        select(Mins_Per_90, npxG_Expected, Sh_per_90_Standard, Dist_Standard,
               npxG_per_Sh_Expected) %>%
        mutate(npxG_Expected = round(npxG_Expected/Mins_Per_90,2)) %>%
        select(!Mins_Per_90) %>%
        reframe(npxG_Expected = 
                  quantile(npxG_Expected, probs = c(0.05, 0.95), na.rm = TRUE),
                Sh_per_90_Standard = quantile(Sh_per_90_Standard, 
                                              probs = c(0.05, 0.95), na.rm = TRUE),
                npxG_per_Sh_Expected = quantile(npxG_per_Sh_Expected, 
                                                probs = c(0.05, 0.95), na.rm = TRUE),
                Dist_Standard = quantile(Dist_Standard, 
                                         probs = c(0.05, 0.95), na.rm = TRUE)) %>%
        mutate(npxG_Expected = round(npxG_Expected, 2),
               Sh_per_90_Standard = round(Sh_per_90_Standard, 2),
               npxG_per_Sh_Expected = round(npxG_per_Sh_Expected, 2),
               Dist_Standard = round(Dist_Standard,2))
      
      
      
      #### Combine player stats ####
      df_player = cbind(player_misc_stats, player_passing_stats,
                        player_possession_stats, player_sca_stats, player_shooting_stats)
      df_perc = cbind(perc_misc_stats, perc_passing_stats, perc_possession_stats,
                      perc_sca_stats, perc_shooting_stats)
      df_id = data.frame(Name = c(player_name, '0.05 Percentile', '0.95 Percentile'))
      df = rbind(df_player, df_perc)
      df = cbind(df_id, df)
      
      df = df %>%
        select(Name, npxG_Expected, Sh_per_90_Standard, Dist_Standard, SCA90_SCA,
               xAG, CPA_Carries, PPA, Won_Aerial, Fld, PrgC_Carries, 
               npxG_per_Sh_Expected)
      
      player_info = standard_stats %>%
        filter(Player == player_name) %>%
        select(Player, Squad, Age, Born, Comp, Season_End_Year, MP_Playing, Mins_Per_90_Playing)
      
      # Get birthday and age
      age_year_day = str_split(player_info$Age, '-')[[1]]
      age = as.numeric(age_year_day[1])
      birthday = Sys.Date() - as.numeric(age_year_day[2]) - years(age)
      
      # Write into player_info
      player_info = player_info %>%
        mutate(Age = age,
               Birthday = birthday) %>%
        select(!Born)
      
      # Game time filter value
      player_info$TimeFilter = game_time
      
      # Add position
      player_info$Position = radar_type
      
      #### Write to sheet ####
      file_name = paste('data_', gsub(" ", '', player_info$Player), '.xlsx', sep = '')
      datasets = list('Stats' = df, 'Info' = player_info)
      write.xlsx(datasets, file = file_name)
    }
  
}

radar_data()
