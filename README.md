# radarplots
There are two scripts. The R script is to be run first to gather data and then the Python script will take the data, create a radar plot and save the plot to a jpeg. These scripts will create a radar plot for one player.

## R script
Contains a function that uses worldfootballR from https://jaseziv.github.io/worldfootballR/ to scrape player data from the big 5 leagues (England, Germany, Italy, France and Spain). When the function is run, it initially prompts the user for a season end year, a league, a team name, a unique player, a player position for the radar, and the number of 90's played to base percentile data on. Based on the player position, the function will select certain data. fbref.com has a dictionary with definitions for each metric used. If a season other than the current season is selected, the player's birthday and age will not appear on the radar. The data is per 90 unless it is percentage or distance data.

The function will create an excel file with the template, 'data_playername.xlsx'. It will have two sheets: one has the data for the player and the percentiles based on the position, while the other has the player's information: player name, squad, age, competition, season end year, matches played, 90's, birthday, percentile 90's threshold used, and position. Below is the data used for each position:

Centerback: Aerial Duels Won, Aerial Win %, Pass Completion %, Long Pass Completion %, Progressive Carries, Completed Final Third Passes, % of Dribblers Tackled, Interceptions, Shots Blocked, Clearances, Fouls.

Fullback: % of Dribblers Tackled, Progressive Carries, Completed Final Third Passes, Crosses into Penalty Area, Expected Assisted Goals, Shot Creating Actions, Pass Completion %, Fouls, Aerial Win %, Clearances, Tackles & Interceptions.

Midfielder: Pass Completion %, Progressive Carries, Progressive Passes, Expected Assisted Goals, Completed Final Third Passes, Fouls Drawn, Fouls, Loose Ball Recoveries, Interceptions, Tackles, % of Dribblers Tackled.

Winger/CAM: npxG, npxG/Shot, Fouls Drawn, Tackles & Interceptions, Expected Assisted Goals, Passes into Penalty Area, Progressive Carries, Progressive Passes, Shot Creating Actions, Shot Distance (Yards), Shots.

Striker: npxG, Shots, Shot Distance (Yards), Shot Creating Actions, Expected Assisted Goals, Carries into Penalty Area, Passes into Penalty Area, Aerial Duels Won, Fouls Drawn, Progressive Carries, npxG/Shot.

## Python Script
Reads and parses through the excel file created in R. Uses radar chart from mplsoccer. Documentation here: https://mplsoccer.readthedocs.io/en/latest/gallery/radar/plot_radar.html. Saves image as jpeg.
