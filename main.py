import random

def main():
    A = set()
    while len(A) < 10:
        A.add(random.randint(1, 30))
    k = random.randint(1, 10)
    A = list(A)
    random.shuffle(A)
    print(A)
    print(k)
    print(get_Kth(A, k))

def find_peak(array):
    left = 0
    right = len(array) - 1
    while left < right:
        mid = (left + right) // 2
        if array[mid] < array[mid + 1]:
            left = mid + 1
        elif array[mid] > array[mid + 1]:
            right = mid
    return array[left]


def fit_line(array):
    x_mean = 0
    y_mean = 0
    for p in array:
        x_mean += p[0]
        y_mean += p[1]
    x_mean /= len(array)
    y_mean /= len(array)
    temp1 = 0
    temp2 = 0
    for p in array:
        temp1 += (p[0] - x_mean) * (p[1] - y_mean)
        temp2 += (p[0] - x_mean) ** 2
    m = temp1 / temp2
    b = y_mean - (m * x_mean)

    return m, b

def get_Kth(A, k):
    if k > len(A):
        return None
    pivot = get_pivot(A)
    B = []
    C = []
    for a in A:
        if a > pivot:
            C.append(a)
        elif a < pivot:
            B.append(a)
    if k == len(B)+1:
        return pivot
    elif k > len(B)+1:
        return get_Kth(C, k - (len(B) + 1))
    else:
        return get_Kth(B, k)

def get_pivot(A):

    return len(A) // 2

if __name__ == '__main__':
    main()