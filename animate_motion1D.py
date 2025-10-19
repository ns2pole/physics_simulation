import matplotlib.pyplot as plt
import matplotlib.animation as animation
import csv

# CSVデータ読み込み（t, xのみ）
t_vals = []
x_vals = []
with open('motion.csv', newline='') as csvfile:
    reader = csv.reader(csvfile)
    for row in reader:
        t, x = map(float, row[:2])  # 3列目は無視
        t_vals.append(t)
        x_vals.append(x)

# アニメーション準備
fig, ax = plt.subplots()
ax.set_xlim(min(x_vals) - 1, max(x_vals) + 1)
ax.set_ylim(-1, 1)  # yは使わないので適当に上下少しだけ確保

point, = ax.plot([], [], 'ro', label="Object")
path, = ax.plot([], [], 'b--', alpha=0.5, label="Path")

time_text = ax.text(0.02, 0.95, '', transform=ax.transAxes,
                    fontsize=12, verticalalignment='top')

def init():
    point.set_data([], [])
    path.set_data([], [])
    time_text.set_text('')
    return point, path, time_text

def update(frame):
    x = x_vals[frame]
    t = t_vals[frame]
    point.set_data([x], [0])        # y=0に固定
    path.set_data(x_vals[:frame+1], [0]*(frame+1))  # y=0の線を描画
    time_text.set_text(f"t = {t:.2f} s")
    return point, path, time_text

ani = animation.FuncAnimation(fig, update, frames=len(t_vals),
                              init_func=init, blit=True, interval=200)

plt.title("1D Motion Animation (X over time)")
plt.xlabel("X Position (m)")
plt.ylabel("Y (fixed at 0)")
plt.legend()
plt.grid(True)
plt.tight_layout()
plt.show()