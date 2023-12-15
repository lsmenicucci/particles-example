import matplotlib.pyplot as plt 
import numpy as np 
import sys
from pathlib import Path 
import matplotlib.animation as animation

input_file = sys.argv[1]
input_file = Path(input_file) 
assert input_file.exists(), "Input file does not exist"

def loop_reader(file):
    f = open(input_file, "r")

    n = int(f.readline().strip())
    r = np.zeros((n, 2))

    while True: 
        for i in range(n):
            line = f.readline().strip()
            if not line:
                f.close()
                print("End of file reached")
                return 

            x, y = line.split()
            r[i, 0] = float(x) 
            r[i, 1] = float(y)

        yield r

fig, ax = plt.subplots() 
ax.set_xlim(-5, 5) 
ax.set_ylim(-5, 5) 
ax.set_aspect('equal') 

line, = ax.plot([], [], 'ko', ms=4)
reader = loop_reader(input_file)

def init(): 
    line.set_data([], []) 
    return line, 

def animate(t): 
    r = next(reader)
    line.set_data(r[:, 0], r[:, 1]) 
    return line,

ani = animation.FuncAnimation(fig, animate, init_func=init, interval=5, blit=True) 
#ani.save('particles.mp4', writer="ffmpeg")
plt.show()

    


