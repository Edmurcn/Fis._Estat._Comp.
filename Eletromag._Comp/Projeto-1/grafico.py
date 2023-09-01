import matplotlib.pyplot as plt
import pandas as pd
import PIL
import numpy as np

coord = []

with open('/home/edmurcn/Documentos/MeusProjetos/Fis._Estat._Comp./Eletromag._Comp/Projeto-1/dados.dat', 'r') as arquivo:
    for linha in arquivo:
        partes = linha.strip().split()  # Divide a linha em partes
        if len(partes) == 3:
            try:
                x, y, z = map(float, partes)  # Converte as partes em números float
                coord.append((x, y, z))  # Adiciona as coordenadas à lista
            except ValueError:
                print(f"Erro ao processar a linha: {linha}")

coord = np.array(coord)
fig = plt.figure()
ax = fig.add_subplot(111, projection='3d')


z = coord[:,2]

scatter = ax.scatter(coord[:, 0], coord[:, 1], coord[:, 2], c=z, cmap="inferno", s=10)

cbar = fig.colorbar(scatter, location="left")
cbar.set_label("Potencial V(x,y)")
plt.title("Potencial Final")
plt.xlabel("X")
plt.ylabel("Y")
ax.set_zlabel("V(x,y)")

plt.show()

