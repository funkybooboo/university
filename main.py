import random


def main():
    A = []
    for i in range(10):
        A.append(random.randint(1, 30))
    k = random.randint(1, 10)
    print(selection(A, k))

def selection(A, k):
    # get pivot
    # move items bigger to the right of pivot
    # check if k is == to pivot by using the len(A)
    # if k has to be on the right side of pivot then throw away the left
    # if k has to be on the left side of pivot then throw away the right
    # keep searching until the kth element is found
    pivot_index = get_pivot_index(A)
    pivot = A[pivot_index]
    B = []
    C = []
    for a in A:
        if a > pivot:
            C.append(a)
        elif a < pivot:
            B.append(a)
    B.append(pivot)

    if k == len(B):
        return pivot
    elif k > len(B):
        return selection(C, k-len(B))
    else:
        return selection(B, k)

def get_pivot_index(A):
    return len(A) // 2

if __name__ == '__main__':
    main()