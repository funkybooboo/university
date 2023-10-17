import random


def main():
    A = set()
    l = random.randint(10, 50)
    while len(A) < l:
        A.add(random.randint(1, l*2))
    k = random.randint(1, 10)
    A = list(A)
    random.shuffle(A)
    print('list')
    print(A)
    print('k')
    print(k)
    print()
    kth = get_Kth(A, k)
    A.sort()
    left = []
    right = []
    for a in A:
        if a < kth:
            left.append(a)
        elif a > kth:
            right.append(a)
    print('kth')
    print(kth)
    print()
    print('left and size')
    print(len(left))
    print(left)
    print()
    print('right and size')
    print(len(right))
    print(right)
    print()
    if len(left) + 1 == k:
        print('Correct')
    else:
        print('Incorrect')

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

    if len(A) % 2 == 0:
        pivot = get_pivot(A, len(A) // 2)
    else:
        pivot = get_pivot(A, (len(A) // 2) + 1)
    left = []
    right = []
    for a in A:
        if a > pivot:
            right.append(a)
        elif a < pivot:
            left.append(a)

    if k == len(left) + 1:
        return pivot
    elif k > len(left) + 1:
        return get_Kth(right, k - (len(left) + 1))
    else:
        return get_Kth(left, k)


def get_pivot(A, k):

    groups = get_groups(A)

    median = get_median_of_medians(groups)

    left, right = split_A(A, median)

    if k == len(left) + 1:
        return median
    elif k > len(left) + 1:
        return get_pivot(right, k - (len(left) + 1))
    else:
        return get_pivot(left, k)


def split_A(A, median):
    left = []
    right = []
    for a in A:
        if a > median:
            right.append(a)
        elif a < median:
            left.append(a)
    return left, right


def get_median_of_medians(groups):
    medians = []
    for group in groups:
        group.sort()
        medians.append(group[len(group) // 2])
    medians.sort()
    median = medians[len(medians) // 2]
    return median


def get_groups(A):
    groups = [[]]
    count = 0
    for a in A:
        if len(groups[count]) < 5:
            groups[count].append(a)
        else:
            count += 1
            groups.append([])
            groups[count].append(a)
    return groups


if __name__ == '__main__':
    main()
