import random


def main():
    A = []
    l = random.randint(5, 10)
    while len(A) < l:
        A.append(random.randint(1, l*2))
    k = random.randint(1, l)
    random.shuffle(A)
    print('list and size')
    print(len(A))
    print(A)
    print()
    print('k')
    print(k)
    print()
    kth = get_Kth(A, k)
    left, right = split_A(A, kth)
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
    A.sort()
    if kth == A[k-1]:
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
        median = get_median(A, len(A) // 2)
    else:
        median = get_median(A, (len(A) // 2) + 1)

    left, right = split_A(A, median)

    if k == len(left) + 1:
        return median
    elif k > len(left) + 1:
        return get_Kth(right, k - (len(left) + 1))
    else:
        return get_Kth(left, k)


def get_median(A, k):

    groups = get_groups(A)

    median = get_median_of_medians(groups)

    left, right = split_A(A, median)

    if k == len(left) + 1:
        return median
    elif k > len(left) + 1:
        return get_median(right, k - (len(left) + 1))
    else:
        return get_median(left, k)


def split_A(A, median):
    left = []
    right = []
    for a in A:
        if a > median:
            right.append(a)
        else:
            left.append(a)
    left.remove(median)
    return left, right


def get_median_of_medians(groups):
    medians = []
    for group in groups:
        group.sort()
        medians.append(group[len(group) // 2])
    medians.sort()
    return medians[len(medians) // 2]


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
