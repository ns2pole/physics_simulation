import numpy as np
import matplotlib.pyplot as plt

# データ点
x = np.array([0.1, 0.2, 0.3, 0.4, 0.5])
y = np.array([0.48, 0.90, 1.28, 1.67, 2.07])

# 最小二乗法による直線回帰
coefficients = np.polyfit(x, y, 1)
a, b = coefficients

# 結果を表示
print(f"slope: a = {a:.4f}")
print(f"y-intercept: b  = {b:.4f}")
print(f"regression-line: f(x) = {a:.4f}x + {b:.4f}")

# x=0〜0.6の範囲で直線用データ
x_fit = np.linspace(0, 0.6, 100)
y_fit = a * x_fit + b

# プロット
plt.scatter(x, y, color='blue', label='data')

# 直線は描くが完全透明にする
plt.plot(x_fit, y_fit, color='red', alpha=0)

plt.xlabel('x')
plt.ylabel('y')
plt.legend()
plt.grid(True)
plt.show()

