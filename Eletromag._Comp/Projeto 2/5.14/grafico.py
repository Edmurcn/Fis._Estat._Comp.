import matplotlib.pyplot as plt
import pandas as pd
import PIL
import numpy as np
from mpl_toolkits.mplot3d import Axes3D

coord = []
campo = []

with open('/home/edmurcn/Documentos/MeusProjetos/Fis._Estat._Comp./Eletromag._Comp/Projeto 2/5.14/esp_out.dat', 'r') as arquivo:
    
    for linha in arquivo:
        partes = linha.strip().split()  # Divide a linha em partes
        if len(partes) == 6:
            try:
                x, y, z, Bx, By, Bz = map(float, partes)  # Converte as partes em números float
                coord.append((x, y, z))  # Adiciona as coordenadas à lista
                campo.append((Bx, By, Bz))  # Adiciona as componentes do campo magnético à lista
            except ValueError:
                print(f"Erro ao processar a linha: {linha}")

coord = np.array(coord)
campo = np.array(campo)

fator_escala = 100 # Ajuste o valor conforme necessário
campo_escalado = campo * fator_escala

# Reduza a quantidade de vetores (selecione um subconjunto dos pontos)  
amostragem = 100 # Ajuste o valor para controlar a densidade de vetores
coord_amostradas = coord[::amostragem]
campo_amostrado = campo_escalado[::amostragem]

fig = plt.figure()
ax = fig.add_subplot(111, projection='3d')

x = coord[:, 0]
y = coord[:, 1]
z = coord[:, 2]
u = campo[:, 0]  # Componente Bx
v = campo[:, 1]  # Componente By    
w = campo[:, 2]  # Componente Bz

ax.quiver(x, y, z, u, v, w, length=0.5, normalize=False)  # Plota vetores do campo magnético
ax.set_xlabel('X')
ax.set_ylabel('Y')
ax.set_zlabel('Z')

plt.show()
