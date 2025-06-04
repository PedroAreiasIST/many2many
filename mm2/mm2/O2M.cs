using System.Diagnostics;
using System.Globalization;
using System.Runtime.CompilerServices;
using System.Text;
using System.Text.Json.Serialization;
using static System.Runtime.CompilerServices.MethodImplOptions;

namespace mm2;

public class O2M : IComparable<O2M>, IEquatable<O2M>, ICloneable
{
    [JsonInclude] protected List<List<int>> adjacencies;

    protected int maxNode = -1;

    public O2M()
    {
        adjacencies = [];
    }

    public O2M(int ReservedSize)
    {
        ArgumentOutOfRangeException.ThrowIfNegative(ReservedSize);
        adjacencies = new List<List<int>>(ReservedSize);
    }

    public O2M(List<List<int>> Adjacencies)
    {
        adjacencies = Adjacencies;
        maxNode = -1;
        foreach (var row in adjacencies) RecomputeMaxNode(row);
    }

    public int Count => adjacencies.Count;
    public int MaxNode => maxNode;

    public List<int> this[int i]
    {
        get
        {
            ArgumentOutOfRangeException.ThrowIfNegative(i, nameof(i));
            ArgumentOutOfRangeException.ThrowIfGreaterThanOrEqual(i, Count, nameof(i));
            return adjacencies[i];
        }
    }

    public int this[int i, int j] => adjacencies[i][j];

    public virtual object Clone()
    {
        var clone = new O2M();
        clone.maxNode = maxNode;
        clone.adjacencies = new List<List<int>>(adjacencies.Count);
        if (adjacencies != null)
            foreach (var adj in adjacencies)
                clone.adjacencies.Add(new List<int>(adj));

        Debug.Assert(clone.maxNode == maxNode, "Clone's maxNode must match original");
        return clone;
    }

    public int CompareTo(O2M? other)
    {
        if (other is null) return 1;
        return ListExtensions.CompareLists(adjacencies, other.adjacencies);
    }

    public bool Equals(O2M? other)
    {
        if (other is null) return false;
        if (ReferenceEquals(this, other)) return true;
        if (maxNode != other.maxNode || Count != other.Count) return false;
        for (var i = 0; i < Count; i++)
        {
            var a = adjacencies[i];
            var b = other.adjacencies[i];
            if (a.Count != b.Count) return false;
            for (var j = 0; j < a.Count; j++)
                if (a[j] != b[j])
                    return false;
        }

        return true;
    }

    public void Reserve(int res)
    {
        adjacencies.Capacity = Math.Max(adjacencies.Capacity, res);
    }

    public virtual void ClearElement(int element)
    {
        ArgumentOutOfRangeException.ThrowIfNegative(element, nameof(element));
        ArgumentOutOfRangeException.ThrowIfGreaterThanOrEqual(element, Count, nameof(element));
        adjacencies[element].Clear();
        maxNode = -1;
        for (var i = 0; i < adjacencies.Count; i++)
        {
            var row = adjacencies[i];
            if (row.Count > 0) RecomputeMaxNode(row);
        }
    }

    public virtual int AppendElement(List<int> nodes)
    {
        ArgumentNullException.ThrowIfNull(nodes);
        for (var i = 0; i < nodes.Count; i++)
            if (nodes[i] < 0)
                throw new ArgumentException("Node indices cannot be negative", nameof(nodes));
        var copy = new List<int>(nodes);
        adjacencies.Add(copy);
        RecomputeMaxNode(copy);
        return Count - 1;
    }

    public void AppendNodeToElement(int element, int node)
    {
        ArgumentOutOfRangeException.ThrowIfNegative(element, nameof(element));
        ArgumentOutOfRangeException.ThrowIfNegative(node);
        if (adjacencies.Count <= element)
            while (adjacencies.Count <= element)
                adjacencies.Add(new List<int>());
        adjacencies[element].Add(node);
        UpdateMaxNode(node);
    }

    public static implicit operator O2M(List<List<int>> nodes)
    {
        ArgumentNullException.ThrowIfNull(nodes);
        var rel = new O2M(nodes.Count);
        for (var i = 0; i < nodes.Count; i++)
        {
            var element = nodes[i];
            rel.AppendElement(element);
        }

        return rel;
    }

    public static implicit operator List<List<int>>(O2M o2m)
    {
        ArgumentNullException.ThrowIfNull(o2m);
        var nodes = new List<List<int>>(o2m.Count);
        foreach (var adj in o2m.adjacencies)
            nodes.Add(new List<int>(adj));
        return nodes;
    }

    public override bool Equals(object? obj)
    {
        return Equals(obj as O2M);
    }

    public override int GetHashCode()
    {
        var hash = new HashCode();
        hash.Add(maxNode);
        for (var i = 0; i < adjacencies.Count; i++)
        {
            var row = adjacencies[i];
            hash.Add(row.Count);
            for (var j = 0; j < row.Count; j++) hash.Add(row[j]);
        }

        return hash.ToHashCode();
    }

    public static bool operator ==(O2M? left, O2M? right)
    {
        return ReferenceEquals(left, right) || left?.Equals(right) == true;
    }

    public static bool operator !=(O2M? left, O2M? right)
    {
        return !(left == right);
    }

    public static bool operator <(O2M? left, O2M? right)
    {
        if (left is null) return right is not null;
        if (right is null) return false;
        return left.CompareTo(right) < 0;
    }

    public static bool operator >(O2M? left, O2M? right)
    {
        if (left is null) return false;
        if (right is null) return true;
        return left.CompareTo(right) > 0;
    }

    public static bool operator <=(O2M? left, O2M? right)
    {
        if (left is null) return true;
        if (right is null) return false;
        return left.CompareTo(right) <= 0;
    }

    public static bool operator >=(O2M? left, O2M? right)
    {
        if (left is null) return right is null;
        if (right is null) return true;
        return left.CompareTo(right) >= 0;
    }

    public override string ToString()
    {
        var sb = new StringBuilder();
        sb.AppendLine($"O2M (Count: {Count}, MaxNode: {MaxNode})");
        for (var i = 0; i < Count; i++)
        {
            sb.Append($"[{i}] -> ");
            if (i < adjacencies.Count)
            {
                var row = adjacencies[i];
                sb.AppendLine(row.Count > 0 ? string.Join(", ", row) : "<empty>");
            }
            else
            {
                sb.AppendLine("<null>");
            }
        }

        return sb.ToString();
    }

    public static List<List<int>> GetCliques(O2M elementsToNodes, O2M nodesToElements)
    {
        ArgumentNullException.ThrowIfNull(elementsToNodes);
        ArgumentNullException.ThrowIfNull(nodesToElements);

        var elemCount = elementsToNodes.Count;
        var positions = GetNodePositions(elementsToNodes, nodesToElements);
        var resultMatrix = new List<List<int>>(elemCount);

        // Initialize result matrix
        for (var e = 0; e < elemCount; e++)
        {
            var size = elementsToNodes.adjacencies[e].Count;
            resultMatrix.Add(new List<int>(new int[size * size]));
        }

        var nodeCount = nodesToElements.Count;
        Parallel.For(0, nodeCount, node =>
        {
            if (node >= positions.Count) return;

            var nodePosList = positions[node];
            var idx = 0;
            var marker = new Dictionary<int, int>();

            var elementsConnectedToNode = nodesToElements.adjacencies[node];
            foreach (var e in elementsConnectedToNode)
            {
                if (e < 0 || e >= elementsToNodes.adjacencies.Count || e >= resultMatrix.Count) continue;

                var row = elementsToNodes.adjacencies[e];
                var len = row.Count;

                if (idx >= nodePosList.Count) break;

                var rowPos = nodePosList[idx] * len;

                for (var j = 0; j < row.Count; j++)
                {
                    var n2 = row[j];
                    if (!marker.TryGetValue(n2, out var count))
                    {
                        count = marker.Count;
                        marker[n2] = count;
                    }

                    var flatIndex = rowPos + j;
                    if (flatIndex < resultMatrix[e].Count) resultMatrix[e][flatIndex] = count;
                }

                idx++;
            }
        });

        return resultMatrix;
    }

    public List<int> GetTopOrder()
    {
        // total number of nodes is the larger of:
        //  - Count (number of source‐lists you have)
        //  - MaxNode+1 (highest ID seen in any adjacency list)
        var nodeCount = Math.Max(Count, MaxNode + 1);

        // compute in‐degrees for 0..nodeCount-1
        var inDegree = new int[nodeCount];
        for (var source = 0; source < Count; source++)
            foreach (var target in adjacencies[source])
                // only count edges that point to a valid node
                if (target >= 0 && target < nodeCount)
                    inDegree[target]++;

        // start with all nodes that have no incoming edges
        var pending = new Queue<int>();
        for (var node = 0; node < nodeCount; node++)
            if (inDegree[node] == 0)
                pending.Enqueue(node);

        // Kahn’s algorithm
        var topOrder = new List<int>(nodeCount);
        while (pending.TryDequeue(out var current))
        {
            topOrder.Add(current);

            // only nodes < Count have adjacency lists
            if (current < Count)
                foreach (var neighbor in adjacencies[current])
                    if (neighbor >= 0 && neighbor < nodeCount && --inDegree[neighbor] == 0)
                        pending.Enqueue(neighbor);
        }

        // if we didn’t include every node, there’s a cycle
        if (topOrder.Count != nodeCount)
            throw new InvalidOperationException("Graph contains cycles");

        return topOrder;
    }


    public List<int> GetOrder()
    {
        return adjacencies.GetSortOrder();
    }

    public List<int> GetDuplicates()
    {
        return adjacencies.GetDuplicatePositions(GetOrder());
    }

    public virtual void CompressElements(List<int> oldElementFromNew)
    {
        ArgumentNullException.ThrowIfNull(oldElementFromNew);
        var newAdj = new List<List<int>>(oldElementFromNew.Count);
        var seen = new bool[Count];
        for (var i = 0; i < oldElementFromNew.Count; i++)
        {
            var originalIndex = oldElementFromNew[i];
            ArgumentOutOfRangeException.ThrowIfNegative(originalIndex, nameof(oldElementFromNew));
            ArgumentOutOfRangeException.ThrowIfGreaterThanOrEqual(originalIndex, Count, nameof(oldElementFromNew));
            if (seen[originalIndex])
                throw new ArgumentException($"Duplicate index {originalIndex} in {nameof(oldElementFromNew)}",
                    nameof(oldElementFromNew));
            seen[originalIndex] = true;
            newAdj.Add(new List<int>(adjacencies[originalIndex]));
        }

        adjacencies = newAdj;
        maxNode = -1;
        foreach (var row in adjacencies) RecomputeMaxNode(row);
    }

    public virtual void PermuteNodes(List<int> nodeMap)
    {
        ArgumentNullException.ThrowIfNull(nodeMap);
        maxNode = -1;
        foreach (var row in adjacencies)
        {
            for (var j = 0; j < row.Count; j++)
            {
                var originalNode = row[j];
                ArgumentOutOfRangeException.ThrowIfNegative(originalNode, nameof(nodeMap));
                ArgumentOutOfRangeException.ThrowIfGreaterThanOrEqual(originalNode, nodeMap.Count, nameof(nodeMap));
                row[j] = nodeMap[originalNode];
            }

            RecomputeMaxNode(row);
        }
    }

    public virtual void PermuteElements(List<int> newElementFromOld)
    {
        ArgumentNullException.ThrowIfNull(newElementFromOld);
        if (newElementFromOld.Count != Count)
            throw new ArgumentException($"Element map must have the same length as Count ({Count})",
                nameof(newElementFromOld));
        var newAdj = new List<List<int>>(Count);
        for (var i = 0; i < Count; i++) newAdj.Add(null!);
        var seen = new bool[Count];
        for (var originalIndex = 0; originalIndex < Count; originalIndex++)
        {
            var newPosition = newElementFromOld[originalIndex];
            ArgumentOutOfRangeException.ThrowIfNegative(newPosition, nameof(newElementFromOld));
            ArgumentOutOfRangeException.ThrowIfGreaterThanOrEqual(newPosition, Count, nameof(newElementFromOld));
            if (seen[newPosition])
                throw new ArgumentException($"Duplicate new position {newPosition} in {nameof(newElementFromOld)}",
                    nameof(newElementFromOld));
            seen[newPosition] = true;
            newAdj[newPosition] = new List<int>(adjacencies[originalIndex]);
        }

        adjacencies = newAdj;
        maxNode = -1;
        foreach (var row in adjacencies)
            if (row != null)
                RecomputeMaxNode(row);
    }

    public static List<List<int>> GetNodePositions(O2M nodesFromElement, O2M elementsFromNode)
    {
        ArgumentNullException.ThrowIfNull(nodesFromElement);
        ArgumentNullException.ThrowIfNull(elementsFromNode);
        var numNodes = elementsFromNode.Count;
        var numElements = nodesFromElement.Count;
        var nodePositions = new List<List<int>>(numNodes);
        for (var node = 0; node < numNodes; node++) nodePositions.Add(new List<int>());
        for (var element = 0; element < numElements; element++)
            if (element < nodesFromElement.adjacencies.Count)
            {
                var localNodes = nodesFromElement.adjacencies[element];
                for (var localPos = 0; localPos < localNodes.Count; localPos++)
                {
                    var node = localNodes[localPos];
                    if (node >= 0 && node < numNodes) nodePositions[node].Add(localPos);
                }
            }

        return nodePositions;
    }

    public static List<List<int>> GetElementPositions(O2M elementsToNodes, O2M nodesToElements)
    {
        ArgumentNullException.ThrowIfNull(elementsToNodes);
        ArgumentNullException.ThrowIfNull(nodesToElements);
        var eCount = elementsToNodes.Count;
        var pos = new List<List<int>>(eCount);
        for (var i = 0; i < eCount; i++)
            if (i < elementsToNodes.adjacencies.Count)
                pos.Add(new List<int>(new int[elementsToNodes.adjacencies[i].Count]));
            else
                pos.Add(new List<int>());
        Parallel.For(0, nodesToElements.Count, node =>
        {
            if (node >= 0 && node < nodesToElements.adjacencies.Count)
            {
                var elementsConnectedToNode = nodesToElements.adjacencies[node];
                for (var i = 0; i < elementsConnectedToNode.Count; i++)
                {
                    var e = elementsConnectedToNode[i];
                    if (e >= 0 && e < elementsToNodes.adjacencies.Count && e < pos.Count)
                    {
                        var row = elementsToNodes.adjacencies[e];
                        for (var j = 0; j < row.Count; j++)
                            if (row[j] == node)
                            {
                                if (j >= 0 && j < pos[e].Count)
                                    pos[e][j] = i;
                                else
                                    Debug.WriteLine(
                                        $"Warning: Write index out of bounds (e={e}, j={j}) for pos[{e}] size {pos[e].Count} in GetElementPositions.");
                                break;
                            }
                    }
                    else
                    {
                        Debug.WriteLine(
                            $"Warning: Element index {e} out of bounds for elementsToNodes count {elementsToNodes.adjacencies.Count} or pos count {pos.Count} in GetElementPositions.");
                    }
                }
            }
        });
        return pos;
    }

    public bool[,] GetBooleanMatrix()
    {
        var numRows = Count;
        var numCols = maxNode + 1;
        if (numCols <= 0) numCols = 0;
        var mat = new bool[numRows, numCols];
        for (var i = 0; i < numRows; i++)
        {
            var row = adjacencies[i];
            for (var j = 0; j < row.Count; j++)
            {
                var node = row[j];
                if (node >= 0 && node < numCols)
                    mat[i, node] = true;
                else
                    Debug.WriteLine(
                        $"Warning: Node index {node} out of bounds for boolean matrix columns {numCols} in GetBooleanMatrix.");
            }
        }

        return mat;
    }

    public static O2M GetFromBooleanMatrix(bool[,] matrix)
    {
        ArgumentNullException.ThrowIfNull(matrix);
        int r = matrix.GetLength(0), c = matrix.GetLength(1);
        var rel = new O2M(r);
        rel.maxNode = c > 0 ? c - 1 : -1;
        for (var i = 0; i < r; i++)
        {
            var list = new List<int>();
            for (var j = 0; j < c; j++)
                if (matrix[i, j])
                    list.Add(j);
            rel.adjacencies.Add(list);
            rel.RecomputeMaxNode(list);
        }

        return rel;
    }

    public static O2M GetRandomO2M(int elementCount, int nodeCount, double density, int? seed = null)
    {
        ArgumentOutOfRangeException.ThrowIfNegative(elementCount);
        ArgumentOutOfRangeException.ThrowIfNegative(nodeCount);
        if (density is < 0 or > 1)
            throw new ArgumentOutOfRangeException(nameof(density), "Density must be between 0.0 and 1.0");
        var rnd = seed.HasValue ? new Random(seed.Value) : new Random();
        var rel = new O2M(elementCount);
        for (var i = 0; i < elementCount; i++)
        {
            var list = new List<int>();
            for (var j = 0; j < nodeCount; j++)
                if (rnd.NextDouble() < density)
                    list.Add(j);
            rel.AppendElement(list);
        }

        return rel;
    }

    public string GetEPSString(
        string title = "",
        int margin = 40,
        int elemSpacing = 20,
        int nodeSpacing = 20,
        int elemRadius = 4,
        int nodeRadius = 4,
        double lineWidth = 0.5,
        string elementColor = "0 0 0", // Default black fill "R G B"
        string nodeColor = "0 0 0", // Default black fill
        string lineColor = "0.5 0.5 0.5", // Default gray stroke
        string textColor = "0 0 0", // Default black text
        int fontSize = 12,
        bool drawElementLabels = true,
        bool drawNodeLabels = true)
    {
        var sb = new StringBuilder();

        // Calculate dynamic width and height based on parameters
        var contentWidthElements = Count > 0 ? 2L * elemRadius : 0; // Width of the element column

        long nodesAreaEffectiveWidth = 0;
        if (maxNode >= 0) // If there are any nodes
        {
            nodesAreaEffectiveWidth = 2L * nodeRadius; // Base width for one node
            if (maxNode > 0)
                nodesAreaEffectiveWidth +=
                    (long)maxNode * nodeSpacing; // Additional width for spacing between multiple nodes
        }

        var totalContentWidth = contentWidthElements;
        if (maxNode >= 0)
        {
            totalContentWidth += nodeSpacing; // Gap between elements and nodes
            totalContentWidth += nodesAreaEffectiveWidth - 2L * nodeRadius; // span of node centers
            totalContentWidth += 2L * nodeRadius; // width of the last node (or first if only one)

            // Simplified version based on original logic structure for node area width part
            // This part is the width taken by nodes and the initial gap before them
            var nodeRelatedWidth = nodeSpacing + // Initial gap
                                   (maxNode > 0 ? (long)maxNode * nodeSpacing : 0) + // Space for multiple nodes
                                   2L * nodeRadius; // Width of a node representation
            totalContentWidth = 2L * elemRadius + (maxNode >= 0 ? nodeRelatedWidth : 0);
        }
        else
        {
            totalContentWidth = Count > 0 ? 2L * elemRadius : 0;
        }


        long contentHeightElements = 0;
        if (Count > 0)
        {
            contentHeightElements = 2L * elemRadius; // Base height for one element
            if (Count > 1) contentHeightElements += (long)(Count - 1) * elemSpacing; // Additional height for spacing
        }

        var finalWidth = 2L * margin + totalContentWidth;
        var finalHeight = 2L * margin + contentHeightElements;

        // Ensure minimum dimensions for the bounding box if empty.
        if (finalWidth < 2 * margin) finalWidth = 2 * margin;
        if (finalHeight < 2 * margin) finalHeight = 2 * margin;


        sb.AppendLine("%!PS-Adobe-3.0 EPSF-3.0");
        sb.AppendLine($"%%BoundingBox: 0 0 {finalWidth} {finalHeight}");
        sb.AppendLine($"%%Title: {title}");
        sb.AppendLine("%%Creator: O2M.GetEPSString");
        sb.AppendLine("%%EndComments");
        sb.AppendLine();

        // Font setup
        sb.AppendLine($"/Times-Roman findfont {fontSize} scalefont setfont");
        sb.AppendLine();

        // Title
        if (!string.IsNullOrEmpty(title))
        {
            sb.AppendLine($"{textColor} setrgbcolor");
            sb.AppendLine(
                $"{finalWidth / 2.0} {finalHeight - margin / 2.0} moveto ({EscapePostScriptString(title)}) dup stringwidth pop 2 div neg 0 rmoveto show");
            sb.AppendLine();
        }

        // Draw Elements
        if (Count > 0)
        {
            sb.AppendLine("% Elements");
            sb.AppendLine($"{elementColor} setrgbcolor");
            for (var i = 0; i < Count; i++)
            {
                var y_elem_center = finalHeight - margin - elemRadius - i * elemSpacing;
                var x_elem_center = margin + elemRadius;
                sb.AppendLine($"{x_elem_center} {y_elem_center} {elemRadius} 0 360 arc fill");

                if (drawElementLabels)
                {
                    sb.AppendLine($"{textColor} setrgbcolor");
                    // Adjust label position slightly for better aesthetics with radius
                    var labelX = x_elem_center + elemRadius + fontSize / 3.0; // Small horizontal gap
                    var labelY = y_elem_center - fontSize / 3.0; // Vertically centered approximation
                    sb.AppendLine(
                        $"{labelX.ToString(CultureInfo.InvariantCulture)} {labelY.ToString(CultureInfo.InvariantCulture)} moveto ({i}) show");
                }
            }

            sb.AppendLine();
        }

        // Draw Nodes
        if (maxNode >= 0)
        {
            sb.AppendLine("% Nodes");
            sb.AppendLine($"{nodeColor} setrgbcolor");
            for (var j = 0; j <= maxNode; j++)
            {
                // X coordinate for the center of the element representation area: margin + elemRadius
                // Width of element representation area: 2 * elemRadius
                var x_node_center = margin + 2 * elemRadius + nodeSpacing + j * nodeSpacing;
                var y_node_center = margin + nodeRadius;
                sb.AppendLine($"{x_node_center} {y_node_center} {nodeRadius} 0 360 arc fill");

                if (drawNodeLabels)
                {
                    sb.AppendLine($"{textColor} setrgbcolor");
                    // Center label above the node
                    var labelX = x_node_center;
                    var labelY = y_node_center + nodeRadius + fontSize * 0.5; // Position above node
                    sb.AppendLine(
                        $"{labelX.ToString(CultureInfo.InvariantCulture)} {labelY.ToString(CultureInfo.InvariantCulture)} moveto ({j}) dup stringwidth pop 2 div neg 0 rmoveto show");
                }
            }

            sb.AppendLine();
        }

        // Draw Connections
        if (Count > 0 && maxNode >= 0)
        {
            sb.AppendLine("% Connections");
            sb.AppendLine($"{lineWidth.ToString(CultureInfo.InvariantCulture)} setlinewidth");
            sb.AppendLine($"{lineColor} setrgbcolor");
            for (var i = 0; i < Count; i++)
            {
                // Element's center y-coordinate
                var y1_center = finalHeight - margin - elemRadius - i * elemSpacing;
                // Element's center x-coordinate
                var x1_center = margin + elemRadius;

                // Point from which lines emanate (e.g., right edge of element circle)
                var x1_line_start = x1_center + elemRadius;
                var y1_line_start = y1_center;

                if (i < adjacencies.Count)
                {
                    var row = adjacencies[i];
                    for (var k = 0; k < row.Count; k++)
                    {
                        var connectedNode = row[k];
                        if (connectedNode >= 0 && connectedNode <= maxNode)
                        {
                            // Node's center x-coordinate
                            var x2_center = margin + 2 * elemRadius + nodeSpacing + connectedNode * nodeSpacing;
                            // Node's center y-coordinate
                            var y2_center = margin + nodeRadius;

                            // Point where lines arrive (e.g., center of node)
                            var x2_line_end = x2_center;
                            var y2_line_end = y2_center;

                            sb.AppendLine(
                                $"{x1_line_start.ToString(CultureInfo.InvariantCulture)} {y1_line_start.ToString(CultureInfo.InvariantCulture)} moveto {x2_line_end.ToString(CultureInfo.InvariantCulture)} {y2_line_end.ToString(CultureInfo.InvariantCulture)} lineto stroke");
                        }
                    }
                }
            }

            sb.AppendLine();
        }


        sb.AppendLine("showpage");
        sb.AppendLine("%%EOF");

        return sb.ToString();
    }

    // Helper to escape strings for PostScript
    private static string EscapePostScriptString(string s)
    {
        if (string.IsNullOrEmpty(s)) return string.Empty;
        var sb = new StringBuilder();
        foreach (var c in s)
        {
            if (c == '(' || c == ')' || c == '\\') sb.Append('\\');
            sb.Append(c);
        }

        return sb.ToString();
    }


    [MethodImpl(AggressiveOptimization | AggressiveInlining)]
    [SkipLocalsInit]
    public O2M Transpose()
    {
        var sourceCount = Count;
        if (sourceCount == 0) return new O2M();
        var targetCount = maxNode + 1;
        if (targetCount <= 0) return new O2M(0) { maxNode = sourceCount > 0 ? sourceCount - 1 : -1 };
        var result = new O2M(targetCount) { maxNode = sourceCount > 0 ? sourceCount - 1 : -1 };
        var chunkCount = Math.Min(Environment.ProcessorCount, sourceCount);
        var chunkRanges = new (int Start, int End)[chunkCount];
        var baseSize = sourceCount / chunkCount;
        var rem = sourceCount % chunkCount;
        var cursor = 0;
        for (var c = 0; c < chunkCount; c++)
        {
            var size = baseSize + (c < rem ? 1 : 0);
            chunkRanges[c] = (cursor, cursor + size);
            cursor += size;
        }

        var partialCount = new int[chunkCount][];
        for (var c = 0; c < chunkCount; c++) partialCount[c] = new int[targetCount];
        Parallel.For(0, chunkCount, c =>
        {
            var localCounts = partialCount[c];
            var (start, end) = chunkRanges[c];
            for (var i = start; i < end; i++)
            {
                var adj = adjacencies[i];
                for (var j = 0; j < adj.Count; j++)
                {
                    var originalNode = adj[j];
                    localCounts[originalNode]++;
                }
            }
        });
        var adjacencyLists = new List<int>[targetCount];
        Parallel.For(0, targetCount, t =>
        {
            long totalFreq = 0;
            for (var c = 0; c < chunkCount; c++) totalFreq += partialCount[c][t];
            List<int> list;
            var capacity = (int)totalFreq;
            list = new List<int>(capacity);
            for (var i = 0; i < capacity; i++) list.Add(0);
            adjacencyLists[t] = list;
            long offset = 0;
            for (var c = 0; c < chunkCount; c++)
            {
                var cnt = partialCount[c][t];
                partialCount[c][t] = (int)offset;
                offset += cnt;
            }
        });
        Parallel.For(0, chunkCount, c =>
        {
            var offsets = partialCount[c];
            var (start, end) = chunkRanges[c];
            for (var i = start; i < end; i++)
            {
                var adj = adjacencies[i];
                for (var j = 0; j < adj.Count; j++)
                {
                    var targetElementIndex = adj[j];
                    {
                        var writePos = offsets[targetElementIndex]++;
                        adjacencyLists[targetElementIndex][writePos] = i;
                    }
                }
            }
        });
        for (var t = 0; t < targetCount; t++) result.adjacencies.Add(adjacencyLists[t]);
        return result;
    }

    public static O2M operator *(O2M left, O2M right)
    {
        ArgumentNullException.ThrowIfNull(left);
        ArgumentNullException.ThrowIfNull(right);
        var result = new O2M(left.Count)
        {
            maxNode = right.MaxNode,
            adjacencies = left.MaxNode < right.Count
                ? SymbolicPhaseRaw(left.adjacencies, right.adjacencies)
                : SymbolicPhaseSafe(left.adjacencies, right.adjacencies)
        };
        return result;
    }

    public static O2M operator |(O2M left, O2M right)
    {
        ArgumentNullException.ThrowIfNull(left);
        ArgumentNullException.ThrowIfNull(right);

        var elemCount = Math.Max(left.Count, right.Count);
        var maxNode = Math.Max(left.maxNode, right.maxNode);
        var result = new O2M(elemCount);

        var marker = maxNode >= 0 ? new bool[maxNode + 1] : null;

        for (var i = 0; i < elemCount; i++)
        {
            var mergedNodes = new List<int>();

            void MergeRow(List<int> row)
            {
                if (row.Count == 0) return;

                if (maxNode >= 0)
                {
                    for (var j = 0; j < row.Count; j++)
                    {
                        var node = row[j];
                        if (node >= 0 && node <= maxNode && !marker![node])
                        {
                            marker[node] = true;
                            mergedNodes.Add(node);
                        }
                    }

                    Array.Clear(marker!, 0, maxNode + 1);
                }
                else
                {
                    mergedNodes.AddRange(row);
                }
            }

            if (i < left.Count)
                MergeRow(left.adjacencies[i]);

            if (i < right.Count)
                MergeRow(right.adjacencies[i]);

            mergedNodes.SortUnique();
            result.AppendElement(mergedNodes);
            if (mergedNodes.Count > 0)
                result.RecomputeMaxNode(mergedNodes);
        }

        return result;
    }

    public static O2M operator +(O2M left, O2M right)
    {
        return left | right;
    }

    public static O2M operator &(O2M left, O2M right)
    {
        ArgumentNullException.ThrowIfNull(left);
        ArgumentNullException.ThrowIfNull(right);
        var elemCount = Math.Min(left.Count, right.Count);
        var m = Math.Max(left.maxNode, right.maxNode);
        var result = new O2M(elemCount);
        bool[]? marker = null;
        for (var i = 0; i < elemCount; i++)
        {
            var list = new List<int>();
            if (m >= 0)
            {
                if (marker == null)
                    marker = new bool[m + 1];
                else
                    Array.Clear(marker, 0, m + 1);
                if (i >= 0 && i < right.adjacencies.Count)
                {
                    var rightRow = right.adjacencies[i];
                    for (var j = 0; j < rightRow.Count; j++)
                    {
                        var n = rightRow[j];
                        if (n >= 0 && n <= m) marker[n] = true;
                    }
                }

                if (i >= 0 && i < left.adjacencies.Count)
                {
                    var leftRow = left.adjacencies[i];
                    for (var j = 0; j < leftRow.Count; j++)
                    {
                        var n = leftRow[j];
                        if (n >= 0 && n <= m)
                            if (marker[n])
                                list.Add(n);
                    }
                }
            }

            result.adjacencies.Add(list);
            if (list.Count > 0) result.RecomputeMaxNode(list);
        }

        return result;
    }

    public static O2M operator ^(O2M left, O2M right)
    {
        ArgumentNullException.ThrowIfNull(left);
        ArgumentNullException.ThrowIfNull(right);
        return (left | right) - (left & right);
    }

    public static O2M operator -(O2M left, O2M? right)
    {
        ArgumentNullException.ThrowIfNull(left);
        var elemCount = left.Count;
        var m = Math.Max(left.maxNode, right?.maxNode ?? -1);
        var result = new O2M(elemCount);
        bool[]? marker = null;
        for (var i = 0; i < elemCount; i++)
        {
            var list = new List<int>();
            if (right != null && i >= 0 && i < right.Count)
            {
                var rightRow = right.adjacencies[i];
                if (rightRow.Count > 0 && m >= 0)
                {
                    if (marker == null)
                        marker = new bool[m + 1];
                    else
                        Array.Clear(marker, 0, m + 1);
                    for (var j = 0; j < rightRow.Count; j++)
                    {
                        var n = rightRow[j];
                        if (n >= 0 && n <= m) marker[n] = true;
                    }
                }
                else if (rightRow.Count > 0 && m < 0)
                {
                }
            }
            else if (m >= 0 && marker != null)
            {
                Array.Clear(marker, 0, m + 1);
            }

            if (i >= 0 && i < left.adjacencies.Count)
            {
                var leftRow = left.adjacencies[i];
                if (leftRow.Count > 0)
                {
                    if (m >= 0 && marker == null) marker = new bool[m + 1];
                    if (m < 0 || (marker != null && m >= 0))
                        for (var j = 0; j < leftRow.Count; j++)
                        {
                            var n = leftRow[j];
                            if (m < 0 || (n >= 0 && n <= m))
                                if (m < 0 || !marker[n])
                                    list.Add(n);
                        }
                }
            }

            result.adjacencies.Add(list);
            if (list.Count > 0) result.RecomputeMaxNode(list);
        }

        return result;
    }

    [MethodImpl(AggressiveInlining)]
    private void UpdateMaxNode(int candidate)
    {
        int observed;
        do
        {
            observed = maxNode;
            if (observed >= candidate) return;
        } while (Interlocked.CompareExchange(ref maxNode, candidate, observed) != observed);
    }

    [MethodImpl(AggressiveInlining)]
    private void RecomputeMaxNode(List<int> nodes)
    {
        if (nodes.Count == 0) return;
        var localMax = -1;
        for (var i = 0; i < nodes.Count; i++)
            if (nodes[i] > localMax)
                localMax = nodes[i];
        if (localMax > -1) UpdateMaxNode(localMax);
    }

    [MethodImpl(AggressiveOptimization | AggressiveInlining)]
    [SkipLocalsInit]
    public static List<List<int>> SymbolicPhaseRaw(List<List<int>> aRows, List<List<int>> bRows)
    {
        if (aRows == null) throw new ArgumentNullException(nameof(aRows));
        if (bRows == null) throw new ArgumentNullException(nameof(bRows));

        var n = aRows.Count;
        var result = new List<int>[n]; // Array to hold the lists for parallel assignment

        Parallel.For(0, n,
            // Thread-local initializer: only a HashSet is needed per thread/task.
            () => new HashSet<int>(),
            // Loop body
            (i, loopState, localSet) =>
            {
                localSet.Clear(); // Reuse the HashSet for the current row i

                var aRow = aRows[i]; // Direct list access

                // If you are certain aRows[i] and bRows[middleNode] can never be null
                // (as implied by original code), you can remove these null checks
                // for a marginal performance gain. Otherwise, keep them for robustness.

                for (var j = 0; j < aRow.Count; j++) // Direct list access
                {
                    var middleNode = aRow[j];
                    var bRow = bRows[middleNode]; // Direct list access
                    for (var k = 0; k < bRow.Count; k++) // Direct list access
                        localSet.Add(bRow[k]);
                }

                // Create a NEW list for result[i] from the HashSet's contents.
                // This is crucial for correctness to avoid data races.
                result[i] = new List<int>(localSet);
                return localSet; // Return the localSet for potential reuse by the Parallel.For task
            },
            // Thread-local finalizer: no specific action needed here.
            localSet => { }
        );

        return result.ToList();
    }


    [MethodImpl(AggressiveOptimization | AggressiveInlining)]
    [SkipLocalsInit]
    private static List<List<int>> SymbolicPhaseSafe(List<List<int>> aRows, List<List<int>> bRows)
    {
        var n = aRows.Count;
        var result = Enumerable.Range(0, n).Select(_ => new List<int>()).ToArray();

        Parallel.For(0, n, () => (set: new HashSet<int>(), list: new List<int>()), (i, loopState, local) =>
        {
            local.set.Clear();
            local.list.Clear();

            var aRow = aRows[i];
            for (var j = 0; j < aRow.Count; j++)
            {
                var middleNode = aRow[j];
                if (middleNode >= 0 && middleNode < bRows.Count)
                {
                    var bRow = bRows[middleNode];
                    for (var k = 0; k < bRow.Count; k++) local.set.Add(bRow[k]);
                }
            }

            local.list.AddRange(local.set);
            result[i] = local.list;
            return local;
        }, local => { });

        return result.ToList();
    }
}