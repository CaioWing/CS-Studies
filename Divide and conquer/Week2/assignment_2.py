def count_split_inv(B : list, C : list):
    i = 0
    j = 0
    D = []
    count = 0
    while i < len(B) and j < len(C):
        if B[i] < C[j]:
            D.append(B[i])
            i += 1
        else:
            D.append(C[j])
            count += len(B[i:])
            j += 1
    D.extend(B[i:])
    D.extend(C[j:])
    Z = count
    return D, Z

def count_array(A : list):
    if len(A) == 1:
        return A, 0
    else:
        half_array = int(len(A) / 2)
        B, x = count_array(A[:half_array])
        C, y = count_array(A[half_array:])
        D, z = count_split_inv(B = B, C = C)
    return D, x + y + z

if __name__ == '__main__':
    with open('IntegerArray.txt') as f:
        array = [int(x) for x in f]
    print(count_array(array)[1])