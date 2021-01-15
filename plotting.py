


import numpy as np 
import matplotlib 
import matplotlib.pyplot as plt
from netCDF4 import Dataset

#Importing data from NetCDF
rootgrp = Dataset("results.nc", "a", format="NETCDF4")
pos = np.array(rootgrp.variables['pos'][:])
ex = np.array(rootgrp.variables['ex'][:])


#Plotting position history

plt.figure
plt.axes()
plt.axis("square")
plt.axis([-1,1,-1,1])
plt.scatter(pos[0],pos[1],1)
plt.xlabel("x")
plt.ylabel("y")
plt.title("Position History of the Particle")

plt.figure()


#Setting up axes for positions for E_x
x = np.linspace(-1,1,np.shape(ex)[0]+1)
y = np.linspace(-1,1,np.shape(ex)[1]+1)


#Plotting E_x as a psuedocolour graph

plt.axes()
plt.axis("square")
plt.axis([-1,1,-1,1])
plt.pcolor(x,y,ex)
plt.colorbar()
plt.xlabel("x")
plt.ylabel("y")
plt.title(r"$\frac{\partial \phi}{\partial x}$, $E_x$",size =20)
plt.show()





