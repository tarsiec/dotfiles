import qrcode
import sys

data = ""
n_of_pics = 1
file = open(sys.argv[1], 'r')
n_of_lines = len(file.readlines())
file.close()


with open(sys.argv[1], 'r') as f:
    for i, v in enumerate(f, start=1):
        data += v
        print(data)
        if (i % int(sys.argv[2]) == 0 and i != 0) or i == n_of_lines:
            filename = str(n_of_pics) + ".png"
            img = qrcode.make(data)
            img.save("img/"+filename)
            data = ""
            n_of_pics += 1
