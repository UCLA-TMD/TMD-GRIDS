import numpy as np
import matplotlib.pyplot as plt
from matplotlib.ticker import MaxNLocator
import pandas as pd
from matplotlib import rc
rc('text',usetex=True)

#returns the akk dataframe
def parse_pff():
    data = open('PFF.dat',"r").read().split('\n')[1:-1]
    data = [ x.split('  ') for x in data]
    for j in range(len(data)):
        data[j] = [x for x in data[j] if x]
        if data[j][-1]==' ':  del data[j][-1]

    data = pd.DataFrame(data,columns=['zl','u','d','s','sea']).astype(float)
    return data

#get dataframe with z as index
df = parse_pff()

#plot up the akk ff
fig,ax=plt.subplots(figsize=(10,7))
fig.suptitle('PFF Benchmarking at $Q=10.58$ GeV')

ax.set_xlim([0.2,0.6])
ax.set_xlabel(r'$z_{\Lambda}$')
ax.xaxis.set_major_locator(MaxNLocator(11))
ax.yaxis.set_major_locator(MaxNLocator(11))

ax.set_ylabel(r'z$D_{1T\;\Lambda/q}^{(1)\;\perp}(z_{\Lambda};Q^{2})$')
ax.axhline(0,color='grey')

ax.plot(df.zl,df.u,label='u')
ax.plot(df.zl,df.d,label='d')
ax.plot(df.zl,df.s,label='s')
ax.plot(df.zl,df.sea,label='sea')
ax.legend()

plt.savefig('PFF.pdf')
