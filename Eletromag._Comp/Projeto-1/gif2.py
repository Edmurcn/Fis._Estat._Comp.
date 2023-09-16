import numpy as np
import matplotlib.pyplot as plt
import imageio.v2 as imageio
from PIL import Image
import os
with open('/home/edmurcn/Documentos/MeusProjetos/Fis._Estat._Comp./Eletromag._Comp/Projeto-1/fort.1', 'r') as file:
    lines = file.readlines()

frames = []
coord = []

for line in lines:
    if line.strip():
    
        partes = line.strip().split()  # Divide a linha em partes

        if len(partes) == 3:
            x, y, z = map(float, partes)  # Converte as partes em números float
            coord.append((x, y, z))  # Adiciona as coordenadas à lista

    else:
       
        coord = np.array(coord)
        fig = plt.figure(figsize=(6,6))
        ax = fig.add_subplot(111, projection='3d')
        z = coord[:,2]
        scatter = ax.scatter(coord[:, 0], coord[:, 1], coord[:, 2], c=z, cmap="inferno", s=10)
        cbar = fig.colorbar(scatter, location="left")
        cbar.set_label("Potencial V(x,y)")
        plt.title("Potencial Final")
        plt.xlabel("X")
        plt.ylabel("Y")
        ax.set_zlabel("V(x,y)")

        filename = f'frame_{len(frames):03d}.png'
        plt.savefig(filename)
        plt.close()

        #Adicionar o nome do arquivo ao frames
        frames.append(filename)
        
        coord = []

# Criar um GIF a partir dos quadros gerados

with imageio.get_writer('evolucao_potencial.gif', mode='I', duration=0.15, loop=0) as writer:
    for frame in frames:
        image = imageio.imread(frame)
        writer.append_data(image)

for frame in frames:
    os.remove(frame)