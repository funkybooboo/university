# mod n
n = 899

# limit l
l = 900

# array a
a = []

for i in range(0, l):
    a.append([])
    for j in range(0, l):
        a[i].append((i * j) % n)

# get inverse
# the inverse of a number x is the number y such that (x * y) % n = 1

# search s
s = 253

# search for the inverse of s
for i in range(0, l):
    if a[s][i] == 1:
        print(f"Inverse of {s} mod {n} is: {i}", end="")
        break
