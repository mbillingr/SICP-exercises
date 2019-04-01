import numpy as np
import scipy.optimize as spo
import matplotlib.pyplot as plt

x = [1_000, 10_000, 100_000, 1_000_000, 10_000_000, 100_000_000]
y = [[192053, 202179, 210671],
     [250185, 241217, 252219],
     [282318, 292632, 293837],
     [321762, 322667, 333423],
     [385437, 409067, 409867],
     [457772, 460533, 456271]]

scale, _ = spo.curve_fit(lambda ln, a: a * ln,
                         np.tile(np.log(x), (3, 1)).T.ravel(),
                         np.ravel(y))

xs = np.logspace(np.log10(x[0]), np.log10(x[-1]), 1000)

plt.semilogx(x, y, '-o')
plt.plot(xs, scale * np.log(xs), '--',
         label='$\log{n}$ (fitted)')

plt.xlabel('lower limit of prime search')
plt.ylabel('time taken to verify prime')
plt.legend()
