#!/bin/bash

# Compilar e executar o programa Fortran 77
gfortran Prog.f -o Prog.exe
./Prog.exe

# Script Gnuplot para exibir o gráfico na tela
gnuplot -persist << EOF
set terminal pngcairo enhanced size 800,600 font 'Verdana,12'
set output "grafico-3d.png"
set xlabel 'Eixo X'
set ylabel 'Eixo Y'
set zlabel 'Eixo Z'
set xrange [0:100]
set yrange [0:100]
set zrange [0:1.5]
set title 'Superfície 3D'
set view 
set pm3d
splot 'dados.dat' matrix using 1:2:3 with pm3d
EOF

# Limpar arquivos temporários
rm Prog.exe 
