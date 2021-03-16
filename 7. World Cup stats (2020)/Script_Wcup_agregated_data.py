import pandas as pd
import numpy as np
import matplotlib.pyplot as plt

#Aggregated information from world cups
db3 = pd.read_csv("/Users/home/Dropbox/Databases/fifa-world-cup/WorldCups.csv")
# Tranforms the attendance into an integer
db3['Attendance']= db3['Attendance'].replace('\.','',regex=True).astype(int)
db3 = db3.replace('Germany FR', 'Germany')
db3.at[16, 'Country'] = 'Korea Republic'

# Russia world cup
db2 = pd.read_csv("/Users/home/Dropbox/Databases/fifa-world-cup/Cup.Russia.Matches.csv")

# Order the data to make all aggregate data
db2 = db2[['Home Team',
           'Home Team Goals',  'Away Team Goals',
           'Away Team', 'Total Goals', 'Attendance']]

# Calculate all aggregated values of that data frame


db2['Year'] = 2018
db2['Country'] = 'Russia'
db2['GoalsScored'] = db2['Total Goals'].sum()

# Number of  Qualified teams

# Extract useful columns
db2_1 = db2['Home Team']
db2_2 = db2['Away Team']

# Rename columns to merge
db2_1.columns = ['Teams']
db2_2.columns = ['Teams']

# Row binding
data_merging = [db2_1, db2_2]
db2_12 = pd.concat(data_merging)

# Extract only unique cases
number = len(db2_12.unique().tolist())
db2['Qualifiedteams'] =  number

# Calculate total attendance and number of matches playes
db2['MatchesPlayed'] = db2['Home Team'].count()
db2['Total_attendance'] = db2['Attendance'].sum()

# Filter columns
db2_aggregation_var = db2[['Year', 'Country', 'GoalsScored',
                           'Qualifiedteams', 'MatchesPlayed',
                           'Total_attendance']]
# Create missing columns

db2_aggregation_var.loc[0, 'Winner'] = 'France'
db2_aggregation_var.loc[0, 'Runners-Up'] = 'Croatia'
db2_aggregation_var.loc[0, 'Third'] = 'Belgium'
db2_aggregation_var.loc[0, 'Fourth'] = 'England'

# Re organize columns
db2_aggregation_var = db2_aggregation_var[['Year', 'Country',
                                           'Winner', 'Runners-Up',
                                           'Third', 'Fourth',
                                           'GoalsScored', 'Qualifiedteams',
                                           'MatchesPlayed', 'Total_attendance']]

# Rename columns to bind
db2_aggregation_var.columns =['Year', 'Country',
                                           'Winner', 'Runners-Up',
                                           'Third', 'Fourth',
                                           'GoalsScored', 'QualifiedTeams',
                                           'MatchesPlayed', 'Attendance']

# Extract single row
db2_aggregation_var = db2_aggregation_var[0:1]
# Row binding
wc_row_bind = [db3, db2_aggregation_var]
WC_general_final = pd.concat(wc_row_bind)

# Save file
WC_general_final.to_csv('WC_general_final.csv')