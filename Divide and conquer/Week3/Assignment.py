def SwapPositions(list : list, pos1 : int, pos2 : int):
    """Swap positions of two lists

    Args:
        list (list): target list to swap
        pos1 (int): element position to swap 
        pos2 (int): other element position to swap

    Returns:
        list: swaped list
    """
    list[pos1], list[pos2] = list[pos2], list[pos1]
    return list

def Partition(arr : list):
    """

    Args:
        arr (list): array to be partitioned
        
    Returns:
        i-1 (int): final pivot position
    """
    p = arr[0]
    rigth = len(arr)
    i = 1
    for j in range(1, rigth):
        if arr[j] < p:
            arr = SwapPositions(arr, j, i)
            i += 1
    arr = SwapPositions(arr, 0, i - 1)
    return arr, i - 1

def ChoosePivot(arr : list, mode : str = 'middle'):
    """_summary_

    Args:
        arr (list): array that contains the pivot
        mode (str, optional): pivot's mode choise. Defaults to 'middle'.

    Returns:
        int: pivot position
    """
    match mode:
        case 'middle': return len(arr)//2

        case 'first': return 0
        
        case 'last': return len(arr) - 1
        

def Quicksort(arr : list, mode : str = 'middle', count = 0) -> list:
    """
    Algorithm for quicksorting a list
    O(n*log n)

    Args:
        arr (list): array to be sorted.
        mode (str, optional): mode to select the pivot. Defaults to 'middle'.
    """
    if len(arr) > 1:
        i = ChoosePivot(arr= arr,
                        mode= mode)
        SwapPositions(arr, 0, i)
        arr, pivot_position = Partition(arr= arr)
        arr_left, count = Quicksort(arr= arr[:pivot_position],
                  mode= mode, count= count)
        count += len(arr_left) - 1
        arr_right, count = Quicksort(arr= arr[pivot_position+1:],
                  mode= mode, count= count)
        count += len(arr_right) - 1
        
        return arr_left + [arr[pivot_position]] + arr_right, count
    else:
        return arr, count

if __name__ == '__main__':
    with open('QuickSort.txt') as f:
        array = [int(x) for x in f]
    sorted_array, count = Quicksort(array, mode= 'first')
    print(count)
    
    # a = [13, 17, 3, 2, 8, 5, 1, 4, 7, 6, 112]
    # print(Quicksort(a, mode= 'first'))
    