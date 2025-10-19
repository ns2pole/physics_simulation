import matplotlib.pyplot as plt
import matplotlib.animation as animation
import csv

# CSVデータ読み込み（ヘッダを飛ばして t, y を読む）
t_vals = []
y_vals = []
with open('motion.csv', newline='') as csvfile:
    reader = csv.reader(csvfile)
    next(reader)  # ヘッダをスキップ
    for row in reader:
        t, y = map(float, row[:2])  # 1列目: time, 2列目: position (y方向)の情報を取得
        t_vals.append(t)
        y_vals.append(y)

# アニメーション準備
fig, ax = plt.subplots()
ax.set_xlim(-1, 1)  # xは固定
ax.set_ylim(min(y_vals) - 1, max(y_vals) + 1)

point, = ax.plot([], [], 'ro', label="Object")
path, = ax.plot([], [], 'b--', alpha=0.5, label="Path")

# 右上に時間テキスト
time_text = ax.text(0.98, 0.95, '', transform=ax.transAxes,
                    fontsize=12, verticalalignment='top',
                    horizontalalignment='right')

def init():
    point.set_data([], [])
    path.set_data([], [])
    time_text.set_text('')
    return point, path, time_text

def update(frame):
    y = y_vals[frame]
    t = t_vals[frame]
    point.set_data([0], [y])              # x=0 に固定して y方向に落下
    path.set_data([0]*(frame+1), y_vals[:frame+1])  # yの軌跡を描画
    time_text.set_text(f"t = {t:.2f} s")
    return point, path, time_text

ani = animation.FuncAnimation(fig, update, frames=len(t_vals),
                              init_func=init, blit=True, interval=50)

plt.title("Free Fall Animation (Y over time)")
plt.xlabel("X Position (fixed at 0)")
plt.ylabel("Y Position (m)")
plt.legend()
plt.grid(True)
plt.tight_layout()
plt.show()
