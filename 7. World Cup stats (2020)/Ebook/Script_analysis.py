import pandas as pd
import sqlite3 as sq3

conn = sq3.connect('World_cup.sqlite')
cur = conn.cursor()

cur.execute('DROP TABLE IF EXISTS WC')

cur.execute('CREATE TABLE WC (Match int, Year int,'
            'Teams nvarchar(50), Goals int, '
            'Rival nvarchar(50))')

data_WC = pd.read_csv("/Users/home/Documents/MP blog 2021/Data/World Cup/data_final_WC_matches_long.csv")

data_WC.to_sql('WC', conn, if_exists='replace', index = True)

cur.execute('SELECT Year, AVG(Goals) FROM WC GROUP BY Year')


for row in cur.fetchall():
    print(row)

conn.commit()
conn.close()