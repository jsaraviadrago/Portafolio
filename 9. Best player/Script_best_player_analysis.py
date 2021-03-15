import pandas as pd
import numpy as np
import sidetable as stb

db_bplayer = pd.read_csv("/Users/home/Dropbox/Databases/Mejor jugador futbol/Mejor_deLa_historia.csv",
                         encoding = "ISO-8859-1")

db_bplayer = db_bplayer[['ï»¿Nombre','Apellido',
                         'Pais', 'Eleccion',
                         'Fecha_nacimiento', 'Posicion',
                         'Duplicate', 'Anho_Fallecio']]

db_bplayer.columns = ['Nombre','Apellido',
                         'Pais', 'Eleccion',
                         'Fecha_nacimiento', 'Posicion',
                         'Duplicate', 'Anho_Fallecio']
Unique = ["0"]
db_bplayer = db_bplayer[db_bplayer.Duplicate.isin(Unique)]


conditions = [
    (db_bplayer['Anho_Fallecio'] < 1976),
    (db_bplayer['Anho_Fallecio'] < 2003),
    (db_bplayer['Anho_Fallecio'] >= 2003)]

values = ['Vieron a Pele', 'Vieron Maradona y Pele',
          'Vieron a todos']

db_bplayer['Vio_jugador'] = np.select(conditions, values)

db_bplayer['Vio_jugador'] = db_bplayer['Vio_jugador'].replace('0', 'Vieron a todos')


# Revisar los descriptivos que vieron todos
Vio_todos = ["Vieron a todos"]
db_bplayer_allwatch = db_bplayer[db_bplayer.Vio_jugador.isin(Vio_todos)]

# Generar variables para descriptivos especificos
def descriptivos():
    eleccion = db_bplayer_allwatch.stb.freq(['Eleccion'])
    pais = db_bplayer_allwatch.stb.freq(['Pais'])
    posicion = db_bplayer.stb.freq(['Posicion'])
    x = str(input("Tabla resumen: "))
    if x == "eleccion":
        print(eleccion.head())
    elif x == "pais":
        print(pais.head())
    elif x == "posicion":
        print(posicion.head())
    else:
        print("Ningun descriptivo")
    return
descriptivos()

# Generar descriptivos por jugador
def descrip_player():
    Messi_p = ["Messi"]
    db_bplayer_Messi = db_bplayer_allwatch[db_bplayer_allwatch.Eleccion.isin(Messi_p)]
    Messi = db_bplayer_Messi.stb.freq(['Pais'])
    Pele_p = ["Pele"]
    db_bplayer_Pele = db_bplayer_allwatch[db_bplayer_allwatch.Eleccion.isin(Pele_p)]
    Pele = db_bplayer_Pele.stb.freq(['Pais'])
    Maradona_p = ["Maradona"]
    db_bplayer_Maradona = db_bplayer_allwatch[db_bplayer_allwatch.Eleccion.isin(Maradona_p)]
    Maradona = db_bplayer_Maradona.stb.freq(['Pais'])
    y = str(input("Resumen jugaores "))
    if y == "Messi":
        print(Messi.head())
    elif y == "Pele":
        print(Pele.head())
    elif y == "Maradona":
        print(Maradona.head())
    else:
        print("Ningun descritivo")
descrip_player()

