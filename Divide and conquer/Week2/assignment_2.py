def count_split_inv(B : list, C : list):
    """
    The function `count_split_inv` takes two sorted lists `B` and `C` as input and returns a merged list
    `D` and the count of split inversions `Z`.
    
    :param B: B is a list of integers
    :type B: list
    :param C: The parameter C is a list
    :type C: list
    :return: a tuple containing two values. The first value is a list `D` which is the merged and sorted
    list of `B` and `C`. The second value is an integer `Z` which represents the count of split
    inversions between `B` and `C`.
    """
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
    """
    The function "count_array" recursively divides an input list into two halves, counts the number of
    inversions in each half, and then counts the number of split inversions between the two halves.
    
    :param A: A is a list of integers
    :type A: list
    :return: The function `count_array` returns a tuple containing two elements. The first element is
    the sorted array `D`, and the second element is the count of inversions `x + y + z`.
    """
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