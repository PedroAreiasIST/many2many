using System.Text.Json.Serialization;
using ConcurrentCollections;

namespace mm2;

public class M2M : O2M
{
    protected readonly object syncLock = new();

    public M2M(int ReservedSize) : base(ReservedSize)
    {
        IsInSync = false;
    }

    public M2M(List<List<int>> Adjacencies) : base(Adjacencies)
    {
        IsInSync = false;
    }

    public M2M()
    {
        IsInSync = false;
    }

    [JsonInclude]
    public IReadOnlyList<IReadOnlyList<int>> Elemeloc { get; private set; } = new List<IReadOnlyList<int>>();

    [JsonInclude]
    public IReadOnlyList<IReadOnlyList<int>> Nodeloc { get; private set; } = new List<IReadOnlyList<int>>();

    private bool IsInSync { get; set; }

    [JsonInclude] public O2M Elementsfromnode { get; private set; } = new();

    public int CompareTo(M2M? other)
    {
        if (other is null) return 1;
        if (Count != other.Count) return Count.CompareTo(other.Count);
        for (var i = 0; i < Count; i++)
        {
            var comparison = ListExtensions.Compare(this[i], other[i]);
            if (comparison != 0) return comparison;
        }

        return 0;
    }

    public bool Equals(M2M? other)
    {
        if (other is null || Count != other.Count) return false;
        for (var i = 0; i < Count; i++)
            if (!ListExtensions.Compare(this[i], other[i]).Equals(0))
                return false;
        return true;
    }

    public override object Clone()
    {
        lock (syncLock)
        {
            var clone = new M2M();
            for (var i = 0; i < Count; i++)
                clone.AppendElement(new List<int>(this[i]));
            if (IsInSync) clone.Synchronize();
            return clone;
        }
    }

    public override bool Equals(object? obj)
    {
        return obj is not null && (ReferenceEquals(this, obj) || (obj is M2M other && Equals(other)));
    }

    public override int GetHashCode()
    {
        var hashCode = new HashCode();
        hashCode.Add(Count);
        for (var i = 0; i < Count; i++)
            foreach (var node in this[i])
                hashCode.Add(node);
        return hashCode.ToHashCode();
    }

    public override int AppendElement(List<int> nodes)
    {
        ArgumentNullException.ThrowIfNull(nodes);
        lock (syncLock)
        {
            IsInSync = false;
            return base.AppendElement(nodes);
        }
    }

    public void Synchronize()
    {
        lock (syncLock)
        {
            if (IsInSync) return;

            Elementsfromnode = Transpose();
            var elemPositions = GetElementPositions(this, Elementsfromnode);
            var nodePositions = GetNodePositions(this, Elementsfromnode);

            var immutableElemPositions = elemPositions.Select(list => list.AsReadOnly()).ToList();
            Elemeloc = immutableElemPositions.AsReadOnly();

            var immutableNodePositions = nodePositions.Select(list => list.AsReadOnly()).ToList();
            Nodeloc = immutableNodePositions.AsReadOnly();
            IsInSync = true;
        }
    }

    public List<int> GetElementswithnodes(List<int> nodes)
    {
        ArgumentNullException.ThrowIfNull(nodes);
        if (nodes.Count == 0) return [];
        Synchronize();
        if (nodes.Any(n => n < 0 || n >= Elementsfromnode.Count))
            return [];

        var elems = new List<int>(Elementsfromnode[nodes[0]]);
        if (elems.Count == 0) return [];

        for (var node = 1; node < nodes.Count && elems.Count > 0; node++)
        {
            var nodeElems = Elementsfromnode[nodes[node]];
            if (nodeElems.Count == 0) return [];
            elems = ListExtensions.Intersect(elems, nodeElems);
        }

        return elems;
    }

    public List<int> GetElementsfromnodes(List<int> nodes)
    {
        ArgumentNullException.ThrowIfNull(nodes);
        return GetElementswithnodes(nodes)
            .Where(e => e < Count && adjacencies[e].Count == nodes.Count)
            .ToList();
    }

    public List<int> GetElementNeighbours(int element)
    {
        ArgumentOutOfRangeException.ThrowIfNegative(element);
        ArgumentOutOfRangeException.ThrowIfGreaterThanOrEqual(element, Count);
        Synchronize();
        var neighbours = new ConcurrentHashSet<int>();
        var validNodes = this[element].Where(n => n >= 0 && n < Elementsfromnode.Count).ToList();

        Parallel.ForEach(validNodes, node =>
        {
            var nodeElements = Elementsfromnode[node];
            for (var i = 0; i < nodeElements.Count; i++)
            {
                var x = nodeElements[i];
                if (x != element)
                    neighbours.Add(x);
            }
        });

        var result = new List<int>(neighbours);
        result.SortUnique();
        return result;
    }

    public List<int> GetNodeNeighbours(int node)
    {
        ArgumentOutOfRangeException.ThrowIfNegative(node);
        Synchronize();
        ArgumentOutOfRangeException.ThrowIfGreaterThanOrEqual(node, Elementsfromnode.Count);
        var neighbours = new ConcurrentHashSet<int>();
        var validElements = Elementsfromnode[node].Where(e => e >= 0 && e < Count).ToList();
        Parallel.ForEach(validElements, elem =>
        {
            var nodes = this[elem];
            for (var i = 0; i < nodes.Count; i++)
            {
                var x = nodes[i];
                if (x != node)
                    neighbours.Add(x);
            }
        });

        var result = new List<int>(neighbours);
        result.SortUnique();
        return result;
    }

    public override void CompressElements(List<int> oldElementFromNew)
    {
        ArgumentNullException.ThrowIfNull(oldElementFromNew);
        if (oldElementFromNew.Count == 0) return;
        lock (syncLock)
        {
            IsInSync = false;
            base.CompressElements(oldElementFromNew);
            Synchronize();
        }
    }

    public override void PermuteElements(List<int> newElementFromOld)
    {
        base.PermuteElements(newElementFromOld);
        Synchronize();
    }

    public override void PermuteNodes(List<int> newNodesFromOld)
    {
        ArgumentNullException.ThrowIfNull(newNodesFromOld);
        if (newNodesFromOld.Count == 0) return;

        lock (syncLock)
        {
            IsInSync = false;
            base.PermuteNodes(newNodesFromOld);
            Synchronize();
        }
    }

    public O2M GetElementsToElements()
    {
        Synchronize();
        return this * Elementsfromnode;
    }

    public O2M GetNodesToNodes()
    {
        Synchronize();
        return Elementsfromnode * this;
    }

    public List<List<int>> GetCliques()
    {
        return GetCliques(this, Elementsfromnode);
    }
}