import pandas as pd
import sqlite3 as sq3

conn = sq3.connect('World_cup.sqlite')
cursor = conn.cursor()

cur.execute('DROP TABLE IF EXISTS WC')

cur.execute('CREATE TABLE WC (Match int, Year int,'
            'Teams = nvarchar(50), Goals int, '
            'Rival = nvarchar(50) ')

data_WC = pd.read_csv("/Users/home/Documents/MP blog 2021/Data/World Cup/data_final_WC_matches_long.csv")

data_WC.to_sql('data_WC', conn, if_exists = replace, index = True)

cur.execute('SELECT Teams, Goals, FROM WC')

for row in cur.fetchall:
    print(row)

conn.commit()
conn.close

