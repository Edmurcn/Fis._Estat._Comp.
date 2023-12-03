import numpy as np
import pandas as pd
from sklearn.ensemble import RandomForestRegressor
from sklearn.preprocessing import PolynomialFeatures
from sklearn.linear_model import Ridge
from sklearn.preprocessing import MinMaxScaler

rf = RandomForestRegressor(n_estimators=60, random_state=30)
ridge = Ridge()
poly = PolynomialFeatures(degree=5)
scaler = MinMaxScaler()

# Preparando o modelo

df = pd.read_csv("dados_plataforma.csv", header=0)

data = df.to_numpy()
nrow, ncol = data.shape
X = data[:,1:ncol]
y = data[:,0]

X = scaler.fit_transform(X)
x_poly = poly.fit_transform(X)

model_rf = rf.fit(X, y)
model_poly = ridge.fit(x_poly, y)

# Recolhendo Informações

carro = []

def get_error_binario(valor):
    while True:
        if valor==1 or valor==0:
            break
        else:
            valor = int(input("Valor inconcistente, digite valores binários (0 ou 1)  "))
    return valor  

def get_error_limite(valor, limite_inf, limite_sup):
    while True:
        if limite_inf <= valor <= limite_sup:
            break
        else:
            print('')
            valor = int(input(f"Valor inconcistente, digite valores entre {limite_inf} e {limite_sup}  "))
    return valor  

print('')
present_price = float(input("Digite o preço da tabela Fipe do Carro (Valores entre 1 e 15)  "))
present_price = get_error_limite(present_price, 1, 15)
carro.append(present_price)
print('')
fuel = int(input("Qual o combustível utilizado? (Gasolina (1) - Diesel (0))  "))
fuel = get_error_binario(fuel)
carro.append(fuel)
print('')
transmission = int(input("Qual o câmbio do carro? (Manual (1) - Atomático (0))  "))
transmission = get_error_binario(transmission)
carro.append(transmission)
print('')
seller = int(input("Qual o tipo de vendedor? (Concessionária (1) - Individual (0))  "))
seller = get_error_binario(seller)
carro.append(seller)
print('')
new = int(input("O carro é novo? (Sim (1) - Não (0))  "))
new = get_error_binario(new)
carro.append(new)
print('')
year = int(input("Digite o ano do carro (menor que 2018)  "))
year = get_error_limite(year, 2005, 2018)
carro.append(year)
print('')
km = float(input("Qual a quilometragem do carro? (valores entre 0 e 120000)  "))
km = get_error_limite(km, 0, 120000)
km = int(km/10000)

for i in range(0,13):
    if i == km:
        carro.append(1)
    else:
        carro.append(0)

# Cálculo do preço

carro = np.array(carro)
carro = carro.reshape(1,-1)
carro = scaler.transform(carro)
carro_poly = poly.fit_transform(carro)

preço_carro_rf = model_rf.predict(carro)
preço_carro_poly = model_poly.predict(carro_poly)

preço_carro = (2*preço_carro_rf + preço_carro_poly)/3

print(f"O preço justo do seu carro nestas condições é {preço_carro}")