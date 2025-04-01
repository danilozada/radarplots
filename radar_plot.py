# Packages
import pandas as pd
from mplsoccer import Radar, FontManager, grid
import matplotlib.pyplot as plt
from matplotlib import font_manager
import math

# Font
URL5 = ('https://raw.githubusercontent.com/google/fonts/main/apache/robotoslab/'
        'RobotoSlab%5Bwght%5D.ttf')
robotto_bold = FontManager(URL5)

## Path to excel file created from R
file = "C:/~/data_TylerAdams.xlsx"

# Function to create plot and save as JPEG
def RadarPlot(file):
    # Read excel file
    df = pd.read_excel(file, engine = "openpyxl", sheet_name = None)

    # Read excel sheets
    player_info = df['Info']
    player_stats = df['Stats']

    # Get player and percentile data
    player_stats = player_stats.set_index('Name')
    low = player_stats.loc['0.05 Percentile'].values
    high = player_stats.loc['0.95 Percentile'].values
    player_vals = player_stats.iloc[0].values

    position = player_info['Position'][0]

    # Determine params
    if position == 'Centerback':
        params = ['Aerial Duels Won', 'Aerial Win %', 'Pass Completion %',
                'Long Pass Completion %', 'Progressive Carries',
                'Completed Final Third Passes', "% of Dribblers Tackled",
                'Interceptions', 'Shots Blocked', 'Clearances', 'Fouls']
        lower_is_better = ['Fouls']
        radar_position = 'Centerback'
    if position == 'Fullback':
        params = ["% of Dribblers Tackled",
          'Progressive Carries', 'Completed Final Third Passes',
          'Crosses into Penalty Area', 'Expected Assisted Goals',
          'Shot Creating Actions', 'Pass Completion %', 'Fouls', 'Aerial Win %', 'Clearances',
          'Tackles & Interceptions']
        lower_is_better = ['Fouls']
        radar_position = 'Fullback'

    if position == 'Midfielder':
        params = ['Pass Completion %', 'Progressive Carries', "Progressive Passes",
                  'Expected Assisted Goals', 'Completed Final Third Passes', 
                  "Fouls Drawn", "Fouls", 'Loose Ball Recoveries', 
                  'Interceptions', 'Tackles', "% of Dribblers Tackled"]
        lower_is_better = ['Fouls']
        radar_position = 'Midfielder'


    
    if position == 'Winger/CAM':
        params = ["npxG", "npxG/Shot", "Fouls Drawn", 'Tackles & Interceptions',
          'Expected Assisted Goals', "Passes into Penalty Area", 'Progressive Carries',
          "Progressive Passes", "Shot Creating Actions", "Shot Distance (Yards)",
          "Shots"]
        lower_is_better = ["Shot Distance (Yards)"]
        radar_position = 'Attacking Midfielder/Winger'

    if position == 'Striker':
        params = ["npxG", "Shots", "Shot Distance (Yards)", "Shot Creating Actions",
                  'Expected Assisted Goals', 'Carries into Penalty Area', 
                  'Passes into Penalty Area', 'Aerial Duels Won', 'Fouls Drawn',
                  'Progressive Carries', 'npxG/Shot']
        lower_is_better = ["Shot Distance (Yards)"]
        radar_position = 'Striker'
        

    # Player information
    player_name = player_info['Player'][0]
    player_squad = player_info['Squad'][0]
    player_birthday = player_info['Birthday'][0].date()
    player_age = math.floor((date.today() - player_birthday).days / 365)
    player_birthday_str = player_birthday.strftime('%Y-%m-%d')
    age_string = f'Age: {player_age} ({player_birthday_str})'
    league = player_info['Comp'][0]
    season = player_info['Season_End_Year'][0]
    season_str = f'{season-1}-{season}'
    league_season_str = f'{league} {season_str}'
    game_time = player_info['Mins_Per_90_Playing'][0]
    appearances = player_info['MP_Playing'][0]
    game_time_appearances_str = f'{game_time} 90s played ({appearances} appearances)'
    percentile_filter = f"Percentiles calculated from players \nwith {player_info['TimeFilter'][0]} or more 90's across Big 5 Leagues"

    # Plot
    radar = Radar(params, low, high, lower_is_better=lower_is_better,
              round_int=[False]*len(params),
              num_rings= 6,
              ring_width=0.6, center_circle_radius=1)

    fig, axs = grid(figheight=14, grid_height=0.915, title_height=0.06, endnote_height=0.025,
              title_space=0, endnote_space=0, grid_key='radar', axis=False)
    
    radar.setup_axis(ax=axs['radar'])

    rings_inner = radar.draw_circles(ax=axs['radar'], facecolor='#BEC8CE', edgecolor='#B3BCC3')

    radar_output = radar.draw_radar(player_vals, ax=axs['radar'],
                kwargs_radar={'facecolor': '#F75959', 'alpha' : 0.3},
                kwargs_rings={'facecolor': '#F75959', 'alpha' : 0.3}) 

    radar_poly, rings_outer, vertices = radar_output

    range_labels = radar.draw_range_labels(ax=axs['radar'], fontsize=18, fontproperties = robotto_bold.prop)  # draw the range labels
    param_labels = radar.draw_param_labels(ax=axs['radar'], fontsize=20, offset = 0.5, fontproperties = robotto_bold.prop)  # draw the param labels

    endnote_text = axs['endnote'].text(0.99, 0.55, 'Inspired By: StatsBomb / Rami Moghadam \nData from Fbref', fontsize=15,
                  fontproperties=robotto_bold.prop, ha='right', va='center')
    
    # Added text to top of visual
    title1_text = axs['title'].text(0.01, 0.55, player_name, fontsize=28,
                                fontproperties=robotto_bold.prop, ha='left', va='center')
    title2_text = axs['title'].text(0.01, 0.10, player_squad, fontsize=24,
                                    fontproperties=robotto_bold.prop,
                                    ha='left', va='center', color='#B6282F')

    title3_text = axs['title'].text(0.01, -0.31, age_string, fontsize=21,
                                    fontproperties=robotto_bold.prop,
                                    ha='left', va='center', color='#B6282F')


    title4_text = axs['title'].text(0.99, 0.55, radar_position, fontsize=28,
                                    fontproperties=robotto_bold.prop, ha='right', va='center')

    title5_text = axs['title'].text(0.99, 0.10, league_season_str, fontsize=24,
                                    fontproperties=robotto_bold.prop,
                                    ha='right', va='center', color='#B6282F')

    title6_text = axs['title'].text(0.99, -0.31, game_time_appearances_str, fontsize=21,
                                    fontproperties=robotto_bold.prop,
                                    ha='right', va='center', color='#B6282F')
    title1_text = axs['endnote'].text(0.01, 0.55, percentile_filter, fontsize=15,
                                fontproperties=robotto_bold.prop, ha='left', va='center')
    
    # Save image as JPEG
    plt.savefig(f"{player_name}_Radar.jpeg", format = 'jpeg')

# Function
RadarPlot(file)
