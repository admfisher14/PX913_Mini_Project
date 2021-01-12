


import numpy as np 
import matplotlib 
import matplotlib.pyplot as plt
from netCDF4 import Dataset

rootgrp = Dataset("results.nc", "a", format="NETCDF4")





pos = np.array(rootgrp.variables['pos'][:])
ex = np.array(rootgrp.variables['ex'][:])

#ex = ex/(10**36)
#print(np.std(ex))


plt.figure
plt.axes()
plt.axis("square")
plt.axis([-1,1,-1,1])
plt.scatter(pos[0],pos[1],1)
plt.xlabel("x")
plt.ylabel("y")
plt.title("Position History of the Particle")

plt.figure()


x = np.linspace(-1,1,np.shape(ex)[0]+1)
y = np.linspace(-1,1,np.shape(ex)[0]+1)



plt.axes()
plt.axis("square")
plt.axis([-1,1,-1,1])
plt.pcolor(x,y,ex)
plt.colorbar()
plt.xlabel("x")
plt.ylabel("y")
plt.title(r"$\frac{\partial \phi}{\partial x}$, $E_x$",size =20)
plt.show()





