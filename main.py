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