import pandas as pd

#Mundiales de futbol
db = pd.read_csv("/Users/home/Dropbox/Databases/fifa-world-cup/WorldCupMatches.csv")

db = db[['Year', 'Home Team Name', 'Home Team Goals',
         'Away Team Goals', 'Away Team Name']]

# World cup 2018
db2 = pd.read_csv("/Users/home/Dropbox/Databases/fifa-world-cup/Cup.Russia.Matches.csv")

# Create a variable of Year for Russia
db2['Year'] = 2018

db2_matches = db2[['Year','Home Team', 'Home Team Goals', 'Away Team Goals',
                   'Away Team']]

# Rename the variables
db2_matches.columns = ['Year', 'Home Team Name', 'Home Team Goals',
                      'Away Team Goals', 'Away Team Name']

matches_bind = [db, db2_matches]
WC_matches_final = pd.concat(matches_bind)

# Save dataframe to .csv file to work on it in R
WC_matches_final.to_csv('WC_matches_final.csv')

# Some analysis of matches

# Points of home team
def results(row):
    if row['Home Team Goals'] > row['Away Team Goals']:
        val = 3
    elif row['Home Team Goals'] == row['Away Team Goals']:
        val = 1
    else:
        val = 0
    return val

WC_matches_final['Home team points'] = WC_matches_final.apply(results, axis=1)
WC_matches_final.reset_index(drop=True)

# Points of away team
def results2(row):
    if row['Home Team Goals'] < row['Away Team Goals']:
        val = 3
    elif row['Home Team Goals'] == row['Away Team Goals']:
        val = 1
    else:
        val = 0
    return val


WC_matches_final['Away team points'] = WC_matches_final.apply(results2, axis=1)
WC_matches_final.reset_index(drop=True)


# Desarmar la base por equipos y total de puntos
WC_matches_results1 = WC_matches_final[['Year', 'Home Team Name',
                                        'Home Team Goals', 'Home team points']]

# Desarmar la base por equipos y total de puntos
WC_matches_results2 = WC_matches_final[['Year', 'Away Team Name',
                                        'Away Team Goals', 'Away team points']]

WC_matches_results1.columns = ['Year', 'Team', 'Team goals', 'Team points']
WC_matches_results2.columns = ['Year', 'Team', 'Team goals', 'Team points']

matches_bind2 = [WC_matches_results1, WC_matches_results2]
WC_matches_final_points = pd.concat(matches_bind2)
WC_matches_final_points = WC_matches_final_points.replace('Germany FR', 'Germany')

# Summary of points per team
WC_matches_summary_points = pd.DataFrame(WC_matches_final_points.groupby('Team').agg(
    {'Team goals': 'sum',
     'Team points': 'sum',
     'Team': 'count'}))
WC_matches_summary_points.columns = ['Team goals', 'Team points', 'Team matches']

# Who has more points?
WC_matches_summary_points['Teams'] = WC_matches_summary_points.index
WC_matches_summary_points.reset_index(drop=True, inplace=True)
WC_matches_summary_points.sort_values(['Team points'], ascending=False)

# Who has more points based in the amount of participations?
WC_matches_summary_points['proportion'] = WC_matches_summary_points['Team points']/WC_matches_summary_points['Team matches']
WC_matches_summary_points.sort_values(['proportion'], ascending=False)