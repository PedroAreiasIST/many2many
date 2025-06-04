namespace mm2;

public static class ListExtensions
{
    public static List<T> This<T>(this List<T> list, List<int> indices)
    {
        ArgumentNullException.ThrowIfNull(list);
        ArgumentNullException.ThrowIfNull(indices);

        var result = new List<T>();
        foreach (var index in indices) result.Add(list[index]);
        return result;
    }

    public static void SortUnique<T>(this List<T> list) where T : IComparable<T>
    {
        ArgumentNullException.ThrowIfNull(list);
        if (list.Count <= 1) return;

        list.Sort();
        for (var i = list.Count - 1; i > 0; i--)
            if (list[i].CompareTo(list[i - 1]) == 0)
                list.RemoveAt(i);
    }

    public static List<T> Intersect<T>(List<T> first, List<T> second) where T : IComparable<T>
    {
        ArgumentNullException.ThrowIfNull(first);
        ArgumentNullException.ThrowIfNull(second);

        var result = new List<T>();
        int firstIdx = 0, secondIdx = 0;
        while (firstIdx < first.Count && secondIdx < second.Count)
        {
            var comparison = first[firstIdx].CompareTo(second[secondIdx]);
            if (comparison == 0)
            {
                result.Add(first[firstIdx]);
                firstIdx++;
                secondIdx++;
            }
            else if (comparison < 0)
            {
                firstIdx++;
            }
            else
            {
                secondIdx++;
            }
        }

        return result;
    }

    public static List<T> Union<T>(List<T> first, List<T> second) where T : IComparable<T>
    {
        ArgumentNullException.ThrowIfNull(first);
        ArgumentNullException.ThrowIfNull(second);

        var result = new List<T>();
        int firstIdx = 0, secondIdx = 0;
        while (firstIdx < first.Count && secondIdx < second.Count)
        {
            var comparison = first[firstIdx].CompareTo(second[secondIdx]);
            if (comparison == 0)
            {
                result.Add(first[firstIdx]);
                firstIdx++;
                secondIdx++;
            }
            else if (comparison < 0)
            {
                result.Add(first[firstIdx]);
                firstIdx++;
            }
            else
            {
                result.Add(second[secondIdx]);
                secondIdx++;
            }
        }

        while (firstIdx < first.Count) result.Add(first[firstIdx++]);
        while (secondIdx < second.Count) result.Add(second[secondIdx++]);
        return result;
    }

    public static List<T> Difference<T>(List<T> first, List<T> second) where T : IComparable<T>
    {
        ArgumentNullException.ThrowIfNull(first);
        ArgumentNullException.ThrowIfNull(second);

        var result = new List<T>();
        int firstIdx = 0, secondIdx = 0;
        while (firstIdx < first.Count && secondIdx < second.Count)
        {
            var comparison = first[firstIdx].CompareTo(second[secondIdx]);
            if (comparison == 0)
            {
                firstIdx++;
                secondIdx++;
            }
            else if (comparison < 0)
            {
                result.Add(first[firstIdx]);
                firstIdx++;
            }
            else
            {
                secondIdx++;
            }
        }

        while (firstIdx < first.Count) result.Add(first[firstIdx++]);
        return result;
    }


    public static List<T> SymmetricDifference<T>(List<T> first, List<T> second) where T : IComparable<T>
    {
        ArgumentNullException.ThrowIfNull(first);
        ArgumentNullException.ThrowIfNull(second);

        var result = new List<T>();
        int firstIdx = 0, secondIdx = 0;
        while (firstIdx < first.Count && secondIdx < second.Count)
        {
            var comparison = first[firstIdx].CompareTo(second[secondIdx]);
            if (comparison == 0)
            {
                firstIdx++;
                secondIdx++;
            }
            else if (comparison < 0)
            {
                result.Add(first[firstIdx]);
                firstIdx++;
            }
            else
            {
                result.Add(second[secondIdx]);
                secondIdx++;
            }
        }

        while (firstIdx < first.Count) result.Add(first[firstIdx++]);
        while (secondIdx < second.Count) result.Add(second[secondIdx++]);
        return result;
    }

    public static List<int> GetSortOrder<T>(this List<T> list) where T : IComparable<T>
    {
        ArgumentNullException.ThrowIfNull(list);

        var indices = Enumerable.Range(0, list.Count).ToList();
        indices.Sort((i, j) => list[i].CompareTo(list[j]));
        return indices;
    }

    public static List<int> GetSortOrder<T>(this List<List<T>> list) where T : IComparable<T>
    {
        ArgumentNullException.ThrowIfNull(list);

        var indices = Enumerable.Range(0, list.Count).ToList();
        var comparer = new ListComparer<T>();
        indices.Sort((i, j) => comparer.Compare(list[i], list[j]));
        return indices;
    }

    public static List<int> GetDuplicatePositions<T>(this List<List<T>> list, List<int> order) where T : IComparable<T>
    {
        ArgumentNullException.ThrowIfNull(list);
        ArgumentNullException.ThrowIfNull(order);

        var duplicates = new List<int>();
        var comparer = new ListComparer<T>();

        for (var i = 1; i < order.Count; i++)
            if (comparer.Compare(list[order[i]], list[order[i - 1]]) == 0)
                duplicates.Add(order[i]);

        return duplicates;
    }


    public static List<int> GetDuplicatePositions<T>(this List<T> list, List<int> order) where T : IComparable<T>
    {
        ArgumentNullException.ThrowIfNull(list);
        ArgumentNullException.ThrowIfNull(order);

        var duplicates = new List<int>();

        for (var i = 1; i < order.Count; i++)
            if (list[order[i]].CompareTo(list[order[i - 1]]) == 0)
                duplicates.Add(order[i]);

        return duplicates;
    }


    public static int Compare<T>(List<T> first, List<T> second) where T : IComparable<T>
    {
        ArgumentNullException.ThrowIfNull(first);
        ArgumentNullException.ThrowIfNull(second);

        var minLength = Math.Min(first.Count, second.Count);
        for (var i = 0; i < minLength; i++)
        {
            var comparison = first[i].CompareTo(second[i]);
            if (comparison != 0)
                return comparison;
        }

        return first.Count.CompareTo(second.Count);
    }

    public static int CompareLists<T>(List<List<T>> first, List<List<T>> second) where T : IComparable<T>
    {
        ArgumentNullException.ThrowIfNull(first);
        ArgumentNullException.ThrowIfNull(second);

        var minLength = Math.Min(first.Count, second.Count);
        var comparer = new ListComparer<T>();

        for (var i = 0; i < minLength; i++)
        {
            var comparison = comparer.Compare(first[i], second[i]);
            if (comparison != 0)
                return comparison;
        }

        return first.Count.CompareTo(second.Count);
    }
}

public class ListComparer<T> : IComparer<List<T>>
    where T : IComparable<T>
{
    public int Compare(List<T> x, List<T> y)
    {
        if (ReferenceEquals(x, y)) return 0;
        if (x == null) return -1;
        if (y == null) return 1;
        var min = Math.Min(x.Count, y.Count);
        for (var i = 0; i < min; i++)
        {
            var c = x[i].CompareTo(y[i]);
            if (c != 0) return c;
        }

        return x.Count.CompareTo(y.Count);
    }
}