import numpy as np
import scipy.optimize as spo
import matplotlib.pyplot as plt

x = [1_000, 10_000, 100_000, 1_000_000]
y = [[4433, 4352, 4397], [12692, 12783, 12664], [38453, 38522, 38885], [101474, 101351, 101330]]

scale, _ = spo.curve_fit(lambda n, a: a * np.sqrt(n), np.tile(x, (3, 1)).T.ravel(), np.ravel(y))

xs = np.arange(x[0], x[-1])

plt.plot(x, y, '-o')
plt.plot(xs, scale * np.sqrt(xs), '--', label='$\sqrt{n}$ (fitted)')

plt.xlabel('lower limit of prime search')
plt.ylabel('time taken to verify prime')
plt.legend()
