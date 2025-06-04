namespace mm2;

public class MM2M
{
    private readonly List<(int Type, int Node)> ListofMarked = [];
    private readonly M2M[,] mat;
    private readonly object syncLock = new();

    public MM2M(int ntypes)
    {
        ArgumentOutOfRangeException.ThrowIfNegativeOrZero(ntypes);
        this.ntypes = ntypes;
        mat = new M2M[ntypes, ntypes];
        for (var i = 0; i < ntypes; i++)
        for (var j = 0; j < ntypes; j++)
            mat[i, j] = new M2M();
    }

    public int ntypes { get; init; }

    public M2M this[int elementType, int nodeType]
    {
        get
        {
            ValidateTypeIndex(elementType);
            ValidateTypeIndex(nodeType);
            return mat[elementType, nodeType];
        }
        set
        {
            ValidateTypeIndex(elementType);
            ValidateTypeIndex(nodeType);
            mat[elementType, nodeType] = value ?? throw new ArgumentNullException(nameof(value));
        }
    }

    public List<(int ElemType, int Elem)> GetAllElements(int nodeType, int node)
    {
        ValidateTypeIndex(nodeType);
        ArgumentOutOfRangeException.ThrowIfNegative(node);

        lock (syncLock)
        {
            var resultSet = new HashSet<(int ElemType, int Elem)>();
            for (var et = 0; et < ntypes; et++)
            {
                if (et == nodeType) continue;

                if (node < mat[et, nodeType].Elementsfromnode.Count)
                {
                    var list = mat[et, nodeType].Elementsfromnode[node];
                    resultSet.UnionWith(list.Select(elem => (et, elem)));
                }
            }

            var res = resultSet.ToList();
            res.SortUnique();
            return res;
        }
    }

    public int NumberofNodes(int elementType, int element, int nodeType)
    {
        ValidateTypeIndex(elementType);
        ValidateTypeIndex(nodeType);
        if (element < 0)
            return 0;

        lock (syncLock)
        {
            return element >= mat[elementType, nodeType].Count ? 0 : mat[elementType, nodeType][element].Count;
        }
    }

    public int NumberofElements(int nodeType, int node, int elementType)
    {
        ValidateTypeIndex(nodeType);
        ValidateTypeIndex(elementType);
        if (node < 0)
            return 0;

        lock (syncLock)
        {
            return node >= mat[elementType, nodeType].Elementsfromnode.Count
                ? 0
                : mat[elementType, nodeType].Elementsfromnode[node].Count;
        }
    }

    public int NumberofElements(int elementType)
    {
        ValidateTypeIndex(elementType);
        lock (syncLock)
        {
            return mat[elementType, elementType].Count;
        }
    }

    public int NumberofActiveElements(int elementType)
    {
        ValidateTypeIndex(elementType);
        lock (syncLock)
        {
            var targetMatrix = mat[elementType, elementType];
            if (targetMatrix.Count == 0)
                return 0;

            var count = 0;
            for (var i = 0; i < targetMatrix.Count; i++)
                if (targetMatrix[i]?.Count > 0)
                    count++;
            return count;
        }
    }

    public void MarktoErase(int nodeType, int node)
    {
        ValidateTypeIndex(nodeType);
        ArgumentOutOfRangeException.ThrowIfNegative(node);
        lock (syncLock)
        {
            ListofMarked.Add((nodeType, node));
        }

        var lmark = DepthFirstSearchFromANode(nodeType, node);
        foreach (var pair in lmark)
            if (!ListofMarked.Contains(pair))
                MarktoErase(pair.ElemType, pair.Elem);
        ListofMarked.SortUnique();
    }

    public void MarkDuplicates(int elementType, int nodeType)
    {
        ValidateTypeIndex(elementType);
        ValidateTypeIndex(nodeType);

        lock (syncLock)
        {
            foreach (var idx in mat[elementType, nodeType].GetDuplicates())
                MarktoErase(elementType, idx);
        }
    }

    public List<(int Type, int Node)> GetAllElements(int nodeType)
    {
        ValidateTypeIndex(nodeType);

        lock (syncLock)
        {
            return Enumerable.Range(0, mat[nodeType, nodeType].Count)
                .SelectMany(n => GetAllElements(nodeType, n))
                .Distinct()
                .OrderBy(x => x)
                .ToList();
        }
    }

    public List<(int Type, int Node)> GetAllNodes(int elementType, int elementNumber)
    {
        ValidateTypeIndex(elementType);
        ArgumentOutOfRangeException.ThrowIfNegative(elementNumber);

        lock (syncLock)
        {
            var resultSet = new HashSet<(int Type, int Node)>();

            for (var nt = 0; nt < ntypes; nt++)
                if (elementNumber < mat[elementType, nt].Count)
                    resultSet.UnionWith(mat[elementType, nt][elementNumber]
                        .Select(node => (nt, node)));

            return resultSet.OrderBy(x => x).ToList();
        }
    }

    public List<(int Type, int Node)> GetAllNodes(int elementType)
    {
        ValidateTypeIndex(elementType);

        lock (syncLock)
        {
            return Enumerable.Range(0, NumberofElements(elementType))
                .SelectMany(e => GetAllNodes(elementType, e))
                .Distinct()
                .OrderBy(x => x)
                .ToList();
        }
    }

    public List<(int ElemType, int Elem)> DepthFirstSearchFromANode(int nodeType, int node)
    {
        ValidateTypeIndex(nodeType);
        ArgumentOutOfRangeException.ThrowIfNegative(node);

        lock (syncLock)
        {
            var visited = new HashSet<(int ElemType, int Elem)>();
            var stack = new Stack<(int ElemType, int Elem)>();
            stack.Push((nodeType, node));

            while (stack.Count > 0)
            {
                var curr = stack.Pop();
                if (!visited.Add(curr)) continue;

                foreach (var e in GetAllElements(curr.ElemType, curr.Elem))
                    stack.Push(e);
            }

            return visited.OrderBy(x => x).ToList();
        }
    }

    public int AppendElement(int elementType, int nodeType, List<int> nodes)
    {
        ValidateTypeIndex(elementType);
        ValidateTypeIndex(nodeType);
        ArgumentNullException.ThrowIfNull(nodes);

        lock (syncLock)
        {
            return mat[elementType, nodeType].AppendElement(nodes);
        }
    }

    public void Compress()
    {
        if (ListofMarked.Count == 0) return;

        lock (syncLock)
        {
            // Find all connected elements
            var connectedElements = new HashSet<(int ElemType, int Elem)>(ListofMarked);
            var queue = new Queue<(int ElemType, int Elem)>(connectedElements);

            while (queue.Count > 0)
            {
                var current = queue.Dequeue();
                foreach (var element in GetAllElements(current.ElemType, current.Elem))
                    if (connectedElements.Add(element))
                        queue.Enqueue(element);
            }

            // Group elements by type
            var elementsByType = Enumerable.Range(0, ntypes).Select(_ => new List<int>()).ToList();
            foreach (var (type, node) in connectedElements)
                elementsByType[type].Add(node);

            // Process each type
            for (var t = 0; t < ntypes; t++)
            {
                if (elementsByType[t].Count == 0) continue;

                elementsByType[t].Sort();
                elementsByType[t].Reverse(); // Process in descending order

                foreach (var node in elementsByType[t])
                    for (var o = 0; o < ntypes; o++)
                        if (node < mat[t, o].Count)
                            mat[t, o].ClearElement(node);
            }

            // Synchronize matrices and clear marked list
            for (var t = 0; t < ntypes; t++)
            for (var o = 0; o < ntypes; o++)
                mat[t, o].Synchronize();

            ListofMarked.Clear();
        }
    }

    public List<int> GetTypeTopOrder()
    {
        lock (syncLock)
        {
            var typeDeps = new O2M(ntypes);
            var hasDeps = false;
            for (var e = 0; e < ntypes; e++)
            for (var n = 0; n < ntypes; n++)
                if (n != e && mat[e, n].Count > 0)
                {
                    typeDeps.AppendNodeToElement(e, n);
                    hasDeps = true;
                }

            return hasDeps ? typeDeps.GetTopOrder() : Enumerable.Range(0, ntypes).ToList();
        }
    }

    public List<int> GetElementsFromNodes(int elementType, int nodeType, List<int> nodes)
    {
        ValidateTypeIndex(elementType);
        ValidateTypeIndex(nodeType);
        ArgumentNullException.ThrowIfNull(nodes);

        lock (syncLock)
        {
            return mat[elementType, nodeType].GetElementsfromnodes(nodes);
        }
    }

    public List<int> GetElementsWithNodes(int elementType, int nodeType, List<int> nodes)
    {
        ValidateTypeIndex(elementType);
        ValidateTypeIndex(nodeType);
        ArgumentNullException.ThrowIfNull(nodes);

        lock (syncLock)
        {
            return mat[elementType, nodeType].GetElementswithnodes(nodes);
        }
    }

    private void ValidateTypeIndex(int typeIndex)
    {
        if (typeIndex < 0 || typeIndex >= ntypes)
            throw new ArgumentOutOfRangeException(nameof(typeIndex), "Type index is out of range.");
    }
}