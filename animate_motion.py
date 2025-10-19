import matplotlib.pyplot as plt
import matplotlib.animation as animation
import csv

# CSVデータ読み込み
t_vals = []
x_vals = []
y_vals = []
with open('uniform_motion.csv', newline='') as csvfile:
    reader = csv.reader(csvfile)
    for row in reader:
        t, x, y = map(float, row)
        t_vals.append(t)
        x_vals.append(x)
        y_vals.append(y)

# アニメーションの準備
fig, ax = plt.subplots()
ax.set_xlim(min(x_vals) - 1, max(x_vals) + 1)
ax.set_ylim(min(y_vals) - 10, max(y_vals) + 10)

point, = ax.plot([], [], 'ro', label="Object")
path, = ax.plot([], [], 'b--', alpha=0.5, label="Path")

# 時刻表示用のテキスト
time_text = ax.text(0.02, 0.95, '', transform=ax.transAxes,
                    fontsize=12, verticalalignment='top')

# 初期化
def init():
    point.set_data([], [])
    path.set_data([], [])
    time_text.set_text('')
    return point, path, time_text

# 各フレームの更新
def update(frame):
    x = x_vals[frame]
    y = y_vals[frame]
    t = t_vals[frame]
    point.set_data([x], [y])
    path.set_data(x_vals[:frame+1], y_vals[:frame+1])
    time_text.set_text(f"t = {t:.2f} s")
    return point, path, time_text

# アニメーションの作成
ani = animation.FuncAnimation(fig, update, frames=len(t_vals),
                              init_func=init, blit=True, interval=200)

plt.title("2D Motion Animation (X vs Y)")
plt.xlabel("X Position (m)")
plt.ylabel("Y Position (m)")
plt.legend()
plt.grid(True)
plt.tight_layout()
plt.show()
