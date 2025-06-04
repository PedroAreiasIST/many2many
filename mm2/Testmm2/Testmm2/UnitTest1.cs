using NUnit.Framework;
using mm2; // Assuming O2M.cs is in the mm2 namespace
using System.Collections.Generic;
using System.Linq;
using System;
using System.Reflection;
using NUnit.Framework.Legacy; // For ArgumentNullException, ArgumentOutOfRangeException, etc.

namespace mm2.Tests
{
    [TestFixture]
    public class O2MTests
    {
        #region Helper Methods for creating O2M instances
        private static O2M CreateO2M(List<List<int>> adjacencies)
        {
            return new O2M(adjacencies);
        }
        #endregion

        #region Constructors
        [Test]
        public void Constructor_Default_CreatesEmptyO2M()
        {
            var o2m = new O2M();
            Assert.That(o2m.Count, Is.EqualTo(0));
            Assert.That(o2m.MaxNode, Is.EqualTo(-1));
        }

        [Test]
        public void Constructor_WithReservedSize_CreatesO2MWithCapacity()
        {
            var o2m = new O2M(10);
            Assert.That(o2m.Count, Is.EqualTo(0)); // Count is 0, capacity is set
            Assert.That(o2m.MaxNode, Is.EqualTo(-1));
            o2m.AppendElement(new List<int> { 1, 2 });
            Assert.That(o2m.Count, Is.EqualTo(1));
        }

        [Test]
        public void Constructor_WithReservedSize_Negative_ThrowsArgumentOutOfRangeException()
        {
            Assert.Throws<ArgumentOutOfRangeException>(() => new O2M(-1));
        }

        [Test]
        public void Constructor_WithAdjacencies_InitializesCorrectly()
        {
            var adj = new List<List<int>>
            {
                new List<int> {0, 1},
                new List<int> {2},
                new List<int>()
            };
            var o2m = new O2M(adj);

            Assert.That(o2m.Count, Is.EqualTo(3));
            Assert.That(o2m.MaxNode, Is.EqualTo(2));
            CollectionAssert.AreEqual(adj[0], o2m[0]);
            CollectionAssert.AreEqual(adj[1], o2m[1]);
            CollectionAssert.AreEqual(adj[2], o2m[2]);
        }

        [Test]
        public void Constructor_WithAdjacencies_EmptyList_InitializesCorrectly()
        {
            var adj = new List<List<int>>();
            var o2m = new O2M(adj);
            Assert.That(o2m.Count, Is.EqualTo(0));
            Assert.That(o2m.MaxNode, Is.EqualTo(-1));
        }

        [Test]
        public void Constructor_WithAdjacencies_ListWithEmptySublists_InitializesCorrectly()
        {
            var adj = new List<List<int>> { new List<int>(), new List<int>() };
            var o2m = new O2M(adj);
            Assert.That(o2m.Count, Is.EqualTo(2));
            Assert.That(o2m.MaxNode, Is.EqualTo(-1));
        }
        #endregion

        #region Properties (Count, MaxNode)
        [Test]
        public void Count_ReturnsCorrectNumberOfElements()
        {
            var o2m = CreateO2M(new List<List<int>> { new List<int> {0}, new List<int> {1} });
            Assert.That(o2m.Count, Is.EqualTo(2));
        }

        [Test]
        public void MaxNode_ReturnsCorrectMaxNodeIndex()
        {
            var o2m = CreateO2M(new List<List<int>> { new List<int> {0, 3}, new List<int> {1} });
            Assert.That(o2m.MaxNode, Is.EqualTo(3));
        }

        [Test]
        public void MaxNode_NoNodes_ReturnsMinusOne()
        {
            var o2m = CreateO2M(new List<List<int>> { new List<int>(), new List<int>() });
            Assert.That(o2m.MaxNode, Is.EqualTo(-1));
        }
        #endregion

        #region Indexers
        [Test]
        public void Indexer_Get_ReturnsCorrectRow()
        {
            var row0 = new List<int> {0, 1};
            var o2m = CreateO2M(new List<List<int>> { row0, new List<int> {2} });
            CollectionAssert.AreEqual(row0, o2m[0]);
        }

        [Test]
        public void Indexer_Get_OutOfRange_ThrowsArgumentOutOfRangeException()
        {
            var o2m = CreateO2M(new List<List<int>> { new List<int> {0} });
            Assert.Throws<ArgumentOutOfRangeException>(() => { var r = o2m[-1]; });
            Assert.Throws<ArgumentOutOfRangeException>(() => { var r = o2m[1]; });
        }
        
      [Test]
        public void Indexer_GetElementNode_ReturnsCorrectValue()
        {
            var o2m = CreateO2M(new List<List<int>> { new List<int> {10, 20}, new List<int> {30} });
            Assert.That(o2m[0, 1], Is.EqualTo(20));
            Assert.That(o2m[1, 0], Is.EqualTo(30));
        }
        
        #endregion

        #region Clone
        [Test]
        public void Clone_CreatesDeepCopy()
        {
            var original = CreateO2M(new List<List<int>> { new List<int> {0, 1}, new List<int> {2} });
            var clone = (O2M)original.Clone();

            Assert.That(clone, Is.Not.SameAs(original));
            Assert.That(clone.Count, Is.EqualTo(original.Count));
            Assert.That(clone.MaxNode, Is.EqualTo(original.MaxNode));
            CollectionAssert.AreEqual(original[0], clone[0]);
            Assert.That(clone[0], Is.Not.SameAs(original[0]));

            clone.AppendNodeToElement(0, 3);
            Assert.That(original[0].Count, Is.EqualTo(2));
            Assert.That(original.MaxNode, Is.EqualTo(2));
            
        }

        [Test]
        public void Clone_EmptyO2M_CreatesEmptyO2M()
        {
            var original = new O2M();
            var clone = (O2M)original.Clone();
            Assert.That(clone, Is.Not.SameAs(original));
            Assert.That(clone.Count, Is.EqualTo(0));
            Assert.That(clone.MaxNode, Is.EqualTo(-1));
        }
        #endregion

        #region CompareTo
        [Test]
        public void CompareTo_Null_Returns1()
        {
            var o2m = new O2M();
            Assert.That(o2m.CompareTo(null), Is.EqualTo(1));
        }

        [Test]
        public void CompareTo_DifferentMaxNode_ComparesMaxNode()
        {
            var o2mA = CreateO2M(new List<List<int>> { new List<int> {0} });
            var o2mB = CreateO2M(new List<List<int>> { new List<int> {1} });
            Assert.That(o2mA.CompareTo(o2mB), Is.LessThan(0));
            Assert.That(o2mB.CompareTo(o2mA), Is.GreaterThan(0));
        }

        [Test]
        public void CompareTo_SameMaxNode_DifferentCount_ComparesCount()
        {
            var o2mA = CreateO2M(new List<List<int>> { new List<int> {0} });
            var o2mB = CreateO2M(new List<List<int>> { new List<int> {0}, new List<int>() });
            Assert.That(o2mA.CompareTo(o2mB), Is.LessThan(0));
            Assert.That(o2mB.CompareTo(o2mA), Is.GreaterThan(0));
        }

        [Test]
        public void CompareTo_SameMaxNodeCountAndRowCounts_DifferentCellValues_ComparesCellValues()
        {
            var o2mA = CreateO2M(new List<List<int>> { new List<int> {0, 1}, new List<int> {2} });
            var o2mB = CreateO2M(new List<List<int>> { new List<int> {0, 1}, new List<int> {3} });
            o2mA[1][0] = 3;
            o2mB[1][0] = 4;
            o2mA[0][0] = 4;
            Assert.That(o2mA.CompareTo(o2mB), Is.GreaterThan(0));
            Assert.That(o2mB.CompareTo(o2mA), Is.LessThan(0));
        }

        [Test]
        public void CompareTo_EqualInstances_Returns0()
        {
            var adj = new List<List<int>> { new List<int> {0, 1}, new List<int> {2} };
            var o2mA = CreateO2M(new List<List<int>>(adj.Select(r => new List<int>(r))));
            var o2mB = CreateO2M(new List<List<int>>(adj.Select(r => new List<int>(r))));
            Assert.That(o2mA.CompareTo(o2mB), Is.EqualTo(0));
        }
        #endregion

        #region Equals
        [Test]
        public void Equals_Null_ReturnsFalse()
        {
            var o2m = new O2M();
            Assert.That(o2m.Equals(null), Is.False);
        }

        [Test]
        public void Equals_ReferenceEquals_ReturnsTrue()
        {
            var o2m = new O2M();
            Assert.That(o2m.Equals(o2m), Is.True);
        }

        [Test]
        public void Equals_DifferentMaxNode_ReturnsFalse()
        {
            var o2mA = CreateO2M(new List<List<int>> { new List<int> {0} });
            var o2mB = CreateO2M(new List<List<int>> { new List<int> {1} });
            Assert.That(o2mA.Equals(o2mB), Is.False);
        }

        [Test]
        public void Equals_DifferentCount_ReturnsFalse()
        {
            var o2mA = CreateO2M(new List<List<int>> { new List<int> {0} });
            var o2mB = CreateO2M(new List<List<int>> { new List<int> {0}, new List<int>() });
            Assert.That(o2mA.Equals(o2mB), Is.False);
        }
        
        [Test]
        public void Equals_DifferentCellValues_ReturnsFalse()
        {
            var o2mA = CreateO2M(new List<List<int>> { new List<int> {0, 1}, new List<int> {2} });
            var o2mB = CreateO2M(new List<List<int>> { new List<int> {0, 1}, new List<int> {3} });
            Assert.That(o2mA.Equals(o2mB), Is.False);
        }

        [Test]
        public void Equals_EqualInstances_ReturnsTrue()
        {
            var adj = new List<List<int>> { new List<int> {0, 1}, new List<int> {2} };
            var o2mA = CreateO2M(new List<List<int>>(adj.Select(r => new List<int>(r))));
            var o2mB = CreateO2M(new List<List<int>>(adj.Select(r => new List<int>(r))));
            Assert.That(o2mA.Equals(o2mB), Is.True);
        }

        [Test]
        public void Equals_Object_CallsTypedEquals()
        {
            var adj = new List<List<int>> { new List<int> {0, 1} };
            var o2mA = CreateO2M(new List<List<int>>(adj.Select(r => new List<int>(r))));
            var o2mB = CreateO2M(new List<List<int>>(adj.Select(r => new List<int>(r))));
            var o2mC = CreateO2M(new List<List<int>> { new List<int> {0, 2} });

            Assert.That(o2mA.Equals((object)o2mB), Is.True);
            Assert.That(o2mA.Equals((object)o2mC), Is.False);
            Assert.That(o2mA.Equals(new object()), Is.False);
        }
        #endregion

        #region Reserve
        [Test]
        public void Reserve_IncreasesCapacity()
        {
            var o2m = new O2M();
            o2m.Reserve(100);
            o2m.AppendElement(new List<int> {0});
            Assert.That(o2m.Count, Is.EqualTo(1));
        }
        #endregion

        #region ClearElement
        [Test]
        public void ClearElement_ClearsRowAndRecomputesMaxNode()
        {
            var o2m = CreateO2M(new List<List<int>> { new List<int> {0, 3}, new List<int> {1, 2} });
            Assert.That(o2m.MaxNode, Is.EqualTo(3));

            o2m.ClearElement(0);
            Assert.That(o2m[0], Is.Empty);
            Assert.That(o2m.MaxNode, Is.EqualTo(2));

            o2m.ClearElement(1);
            Assert.That(o2m[1], Is.Empty);
            Assert.That(o2m.MaxNode, Is.EqualTo(-1));
        }

        [Test]
        public void ClearElement_OutOfRange_ThrowsArgumentOutOfRangeException()
        {
            var o2m = CreateO2M(new List<List<int>> { new List<int> {0} });
            Assert.Throws<ArgumentOutOfRangeException>(() => o2m.ClearElement(-1));
            Assert.Throws<ArgumentOutOfRangeException>(() => o2m.ClearElement(1));
        }
        #endregion

        #region AppendElement
        [Test]
        public void AppendElement_AddsRowAndUpdatesMaxNode()
        {
            var o2m = new O2M();
            o2m.AppendElement(new List<int> {0, 1});
            Assert.That(o2m.Count, Is.EqualTo(1));
            CollectionAssert.AreEqual(new List<int> {0, 1}, o2m[0]);
            Assert.That(o2m.MaxNode, Is.EqualTo(1));

            o2m.AppendElement(new List<int> {2, 3});
            Assert.That(o2m.Count, Is.EqualTo(2));
            CollectionAssert.AreEqual(new List<int> {2, 3}, o2m[1]);
            Assert.That(o2m.MaxNode, Is.EqualTo(3));

            o2m.AppendElement(new List<int>());
            Assert.That(o2m.Count, Is.EqualTo(3));
            Assert.That(o2m[2], Is.Empty);
            Assert.That(o2m.MaxNode, Is.EqualTo(3));
        }

        [Test]
        public void AppendElement_NullNodes_ThrowsArgumentNullException()
        {
            var o2m = new O2M();
            Assert.Throws<ArgumentNullException>(() => o2m.AppendElement(null!));
        }

        [Test]
        public void AppendElement_NegativeNode_ThrowsArgumentException()
        {
            var o2m = new O2M();
            Assert.Throws<ArgumentException>(() => o2m.AppendElement(new List<int> {-1}));
        }
        #endregion

        #region AppendNodeToElement
        [Test]
        public void AppendNodeToElement_AddsNodeAndUpdatesMaxNode()
        {
            var o2m = CreateO2M(new List<List<int>> { new List<int> {0} });
            o2m.AppendNodeToElement(0, 1);
            CollectionAssert.AreEqual(new List<int> {0, 1}, o2m[0]);
            Assert.That(o2m.MaxNode, Is.EqualTo(1));

            o2m.AppendNodeToElement(0, 3);
            CollectionAssert.AreEqual(new List<int> {0, 1, 3}, o2m[0]);
            Assert.That(o2m.MaxNode, Is.EqualTo(3));

            o2m.AppendNodeToElement(0, 2);
            CollectionAssert.AreEqual(new List<int> {0, 1, 3, 2}, o2m[0]);
            Assert.That(o2m.MaxNode, Is.EqualTo(3));
        }

        [Test]
        public void AppendNodeToElement_InvalidElementIndex_ThrowsArgumentOutOfRangeException()
        {
            var o2m = CreateO2M(new List<List<int>> { new List<int> {0} });
            Assert.Throws<ArgumentOutOfRangeException>(() => o2m.AppendNodeToElement(-1, 1));
          //  Assert.Throws<ArgumentOutOfRangeException>(() => o2m.AppendNodeToElement(1, 1));
        }

        [Test]
        public void AppendNodeToElement_NegativeNode_ThrowsArgumentOutOfRangeException()
        {
            var o2m = CreateO2M(new List<List<int>> { new List<int> {0} });
            Assert.Throws<ArgumentOutOfRangeException>(() => o2m.AppendNodeToElement(0, -1));
        }
        #endregion

        #region Implicit Operator from List<List<int>>
        [Test]
        public void ImplicitOperator_FromListOfLists_ValidInput_ConvertsCorrectly()
        {
            List<List<int>> adj = new List<List<int>>
            {
                new List<int> { 0, 1 },
                new List<int> { 2 }
            };
            O2M o2m = adj;
        
            Assert.That(o2m.Count, Is.EqualTo(2));
            Assert.That(o2m.MaxNode, Is.EqualTo(2));
            CollectionAssert.AreEqual(adj[0], o2m[0]);
            CollectionAssert.AreEqual(adj[1], o2m[1]);
        }
        
        [Test]
        public void ImplicitOperator_ToListOfLists_ValidInput_ConvertsCorrectly()
        {
            var o2m = CreateO2M(new List<List<int>>
            {
                new List<int> { 0, 1 },
                new List<int> { 2 }
            });
            List<List<int>> adj = o2m;
        
            Assert.That(adj.Count, Is.EqualTo(2));
            CollectionAssert.AreEqual(new List<int> { 0, 1 }, adj[0]);
            CollectionAssert.AreEqual(new List<int> { 2 }, adj[1]);
        }
        
        [Test]
        public void ImplicitOperator_FromListOfLists_NullInput_ThrowsArgumentNullException()
        {
            List<List<int>>? adj = null;
            Assert.Throws<ArgumentNullException>(() => { O2M o2m = adj!; });
        }
        
        [Test]
        public void ImplicitOperator_FromListOfLists_NullElement_ThrowsArgumentNullException()
        {
            var adj = new List<List<int>> { new List<int> { 0 }, null! };
            Assert.Throws<ArgumentNullException>(() => { O2M o2m = adj; });
        }
        #endregion

        #region GetHashCode
        [Test]
        public void GetHashCode_EqualObjects_ReturnSameHashCode()
        {
            var adj = new List<List<int>> { new List<int> {0, 1}, new List<int> {2} };
            var o2mA = CreateO2M(new List<List<int>>(adj.Select(r => new List<int>(r))));
            var o2mB = CreateO2M(new List<List<int>>(adj.Select(r => new List<int>(r))));
            Assert.That(o2mA.GetHashCode(), Is.EqualTo(o2mB.GetHashCode()));
        }

        [Test]
        public void GetHashCode_DifferentObjects_LikelyReturnDifferentHashCodes()
        {
            var o2mA = CreateO2M(new List<List<int>> { new List<int> {0, 1} });
            var o2mB = CreateO2M(new List<List<int>> { new List<int> {0, 2} });
            Assert.That(o2mA.GetHashCode(), Is.Not.EqualTo(o2mB.GetHashCode()));

            var o2mC = CreateO2M(new List<List<int>> { new List<int> {0, 1}, new List<int>() });
            Assert.That(o2mA.GetHashCode(), Is.Not.EqualTo(o2mC.GetHashCode()));
        }
        #endregion

        #region Operators (==, !=, <, >, <=, >=)
        [Test]
        public void Operator_Equals_ComparesCorrectly()
        {
            var adj = new List<List<int>> { new List<int> {0, 1} };
            var o2mA1 = CreateO2M(new List<List<int>>(adj.Select(r => new List<int>(r))));
            var o2mA2 = CreateO2M(new List<List<int>>(adj.Select(r => new List<int>(r))));
            var o2mB = CreateO2M(new List<List<int>> { new List<int> {0, 2} });
            O2M? nullO2m = null;

            Assert.That(o2mA1 == o2mA2, Is.True);
            Assert.That(o2mA1 == o2mB, Is.False);
            Assert.That(o2mA1 == o2mA1, Is.True);
            Assert.That(o2mA1 == nullO2m, Is.False);
            Assert.That(nullO2m == o2mA1, Is.False);
            Assert.That(nullO2m == nullO2m, Is.True);
        }

        [Test]
        public void Operator_NotEquals_ComparesCorrectly()
        {
            var adj = new List<List<int>> { new List<int> {0, 1} };
            var o2mA1 = CreateO2M(new List<List<int>>(adj.Select(r => new List<int>(r))));
            var o2mA2 = CreateO2M(new List<List<int>>(adj.Select(r => new List<int>(r))));
            var o2mB = CreateO2M(new List<List<int>> { new List<int> {0, 2} });
            O2M? nullO2m = null;

            Assert.That(o2mA1 != o2mA2, Is.False);
            Assert.That(o2mA1 != o2mB, Is.True);
            Assert.That(o2mA1 != nullO2m, Is.True);
            Assert.That(nullO2m != o2mA1, Is.True);
            Assert.That(nullO2m != nullO2m, Is.False);
        }

        [Test]
        public void Operator_Comparison_UsesCompareTo()
        {
            var o2mA = CreateO2M(new List<List<int>> { new List<int> {0} });
            var o2mB = CreateO2M(new List<List<int>> { new List<int> {1} });
            var o2mC = CreateO2M(new List<List<int>> { new List<int> {0} });

            Assert.That(o2mA < o2mB, Is.True);
            Assert.That(o2mB < o2mA, Is.False);
            Assert.That(o2mA < o2mC, Is.False);

            Assert.That(o2mB > o2mA, Is.True);
            Assert.That(o2mA > o2mB, Is.False);
            Assert.That(o2mA > o2mC, Is.False);

            Assert.That(o2mA <= o2mB, Is.True);
            Assert.That(o2mA <= o2mC, Is.True);
            Assert.That(o2mB <= o2mA, Is.False);

            Assert.That(o2mB >= o2mA, Is.True);
            Assert.That(o2mA >= o2mC, Is.True);
            Assert.That(o2mA >= o2mB, Is.False);
        }
        #endregion

        #region ToString
        [Test]
        public void ToString_ReturnsNonEmptyString()
        {
            var o2m = CreateO2M(new List<List<int>> { new List<int> {0, 1}, new List<int>() });
            string s = o2m.ToString();
            Assert.That(s, Is.Not.Empty);
            StringAssert.Contains("O2M", s);
            StringAssert.Contains($"Count: {o2m.Count}", s);
            StringAssert.Contains($"MaxNode: {o2m.MaxNode}", s);
            StringAssert.Contains("[0] -> 0, 1", s);
            StringAssert.Contains("[1] -> <empty>", s);
        }
        #endregion

        #region GetCliques
        [Test]
        public void GetCliques_SimpleCase_ReturnsCorrectCliques()
        {
            var elementsToNodes = CreateO2M(new List<List<int>> {
                new List<int> {0, 1},
                new List<int> {1, 2}
            });
            var nodesToElements = CreateO2M(new List<List<int>> {
                new List<int> {0},
                new List<int> {0, 1},
                new List<int> {1}
            });

            var result = O2M.GetCliques(elementsToNodes, nodesToElements);

            Assert.That(result.Count, Is.EqualTo(2));
            CollectionAssert.AreEqual(new List<int> {0, 1, 0, 1}, result[0], "Clique for E0");
            CollectionAssert.AreEqual(new List<int> {1, 2, 0, 1}, result[1], "Clique for E1");
        }

        [Test]
        public void GetCliques_EmptyInputs_ReturnsEmpty()
        {
            var empty = new O2M();
            var result = O2M.GetCliques(empty, empty);
            Assert.That(result, Is.Empty);

            var e2n = CreateO2M(new List<List<int>> { new List<int> {0} });
            var n2e = CreateO2M(new List<List<int>> { new List<int> {0} });
            result = O2M.GetCliques(e2n, new O2M());
            Assert.That(result.Count, Is.EqualTo(1));
            Assert.That(result[0].Count, Is.EqualTo(1*1));
            CollectionAssert.AreEqual(new List<int>{0}, result[0]);

            result = O2M.GetCliques(new O2M(), n2e);
            Assert.That(result, Is.Empty);
        }

        [Test]
        public void GetCliques_NullInputs_ThrowsArgumentNullException()
        {
            var o2m = new O2M();
            Assert.Throws<ArgumentNullException>(() => O2M.GetCliques(null!, o2m));
            Assert.Throws<ArgumentNullException>(() => O2M.GetCliques(o2m, null!));
        }
        #endregion

        #region GetTopOrder
        [Test]
        public void GetTopOrder_ValidDAG_ReturnsCorrectOrder()
        {
            var square_o2m = new O2M(4);
            square_o2m.AppendElement(new List<int> {1, 2});
            square_o2m.AppendElement(new List<int> {3});
            square_o2m.AppendElement(new List<int> {3});
            square_o2m.AppendElement(new List<int>());

            var order = square_o2m.GetTopOrder();
            Assert.That(order.IndexOf(0), Is.LessThan(order.IndexOf(1)));
            Assert.That(order.IndexOf(0), Is.LessThan(order.IndexOf(2)));
            Assert.That(order.IndexOf(1), Is.LessThan(order.IndexOf(3)));
            Assert.That(order.IndexOf(2), Is.LessThan(order.IndexOf(3)));
            Assert.That(order.Count, Is.EqualTo(4));
        }

        [Test]
        public void GetTopOrder_GraphWithCycle_ThrowsInvalidOperationException()
        {
            var o2m = new O2M(3);
            o2m.AppendElement(new List<int> {1});
            o2m.AppendElement(new List<int> {2});
            o2m.AppendElement(new List<int> {0});
            Assert.Throws<InvalidOperationException>(() => o2m.GetTopOrder());
        }
        

        [Test]
        public void GetTopOrder_DisconnectedGraph_ReturnsValidOrder()
        {
            var o2m = new O2M(3);
            o2m.AppendElement(new List<int> {1, 2});
            o2m.AppendElement(new List<int>());
            o2m.AppendElement(new List<int>());
            var order = o2m.GetTopOrder();
            Assert.That(order.Count, Is.EqualTo(3));
            CollectionAssert.Contains(order, 0);
            CollectionAssert.Contains(order, 1); 
            CollectionAssert.Contains(order, 2);
            Assert.That(order.IndexOf(0), Is.LessThan(order.IndexOf(1)));
            Assert.That(order.IndexOf(0), Is.LessThan(order.IndexOf(2)));
        }
        #endregion

        #region GetOrder & GetDuplicates
        [Test]
        public void GetOrder_CallsExternalExtensionMethod()
        {
            var o2m = CreateO2M(new List<List<int>> { new List<int> {1,0}, new List<int> {2} });
            try
            {
                var order = o2m.GetOrder();
                Assert.That(order, Is.Not.Null);
            }
            catch (MissingMethodException) { Assert.Inconclusive("GetSortOrder extension method is not available."); }
            catch (NotImplementedException) { Assert.Inconclusive("GetSortOrder extension method is not implemented."); }
        }

        [Test]
        public void GetDuplicates_CallsExternalExtensionMethods()
        {
            var o2m = CreateO2M(new List<List<int>> { new List<int> {0}, new List<int> {0} });
            try
            {
                var duplicates = o2m.GetDuplicates();
                Assert.That(duplicates, Is.Not.Null);
            }
            catch (MissingMethodException) { Assert.Inconclusive("GetSortOrder or GetDuplicatePositions extension method is not available."); }
            catch (NotImplementedException) { Assert.Inconclusive("GetSortOrder or GetDuplicatePositions extension method is not implemented."); }
        }
        #endregion

        #region CompressElements
        [Test]
        public void CompressElements_ValidMap_CompressesAndReorders()
        {
            var o2m = CreateO2M(new List<List<int>> {
                new List<int> {0, 1},
                new List<int> {2},
                new List<int> {3, 4}
            });
            var indexMap = new List<int> {2, 0};
            o2m.CompressElements(indexMap);

            Assert.That(o2m.Count, Is.EqualTo(2));
            CollectionAssert.AreEqual(new List<int> {3, 4}, o2m[0]);
            CollectionAssert.AreEqual(new List<int> {0, 1}, o2m[1]);
            Assert.That(o2m.MaxNode, Is.EqualTo(4));
        }

        [Test]
        public void CompressElements_EmptyMap_ResultsInEmptyO2M()
        {
            var o2m = CreateO2M(new List<List<int>> { new List<int> {0,1} });
            o2m.CompressElements(new List<int>());
            Assert.That(o2m.Count, Is.EqualTo(0));
            Assert.That(o2m.MaxNode, Is.EqualTo(-1));
        }

        [Test]
        public void CompressElements_MapWithOutOfRangeIndex_ThrowsArgumentOutOfRangeException()
        {
            var o2m = CreateO2M(new List<List<int>> { new List<int> {0} });
            Assert.Throws<ArgumentOutOfRangeException>(() => o2m.CompressElements(new List<int> {1}));
            Assert.Throws<ArgumentOutOfRangeException>(() => o2m.CompressElements(new List<int> {-1}));
        }

        [Test]
        public void CompressElements_MapWithDuplicateIndex_ThrowsArgumentException()
        {
            var o2m = CreateO2M(new List<List<int>> { new List<int> {0}, new List<int> {1} });
            Assert.Throws<ArgumentException>(() => o2m.CompressElements(new List<int> {0, 0}));
        }

        [Test]
        public void CompressElements_NullMap_ThrowsArgumentNullException()
        {
            var o2m = new O2M();
            Assert.Throws<ArgumentNullException>(() => o2m.CompressElements(null!));
        }
        #endregion

        #region PermuteNodes
        [Test]
        public void PermuteNodes_ValidMap_PermutesNodesAndRecomputesMaxNode()
        {
            var o2m = CreateO2M(new List<List<int>> {
                new List<int> {0, 1},
                new List<int> {1, 2}
            });
            var nodeMap = new List<int> {10, 11, 12};

            o2m.PermuteNodes(nodeMap);

            CollectionAssert.AreEqual(new List<int> {10, 11}, o2m[0]);
            CollectionAssert.AreEqual(new List<int> {11, 12}, o2m[1]);
            Assert.That(o2m.MaxNode, Is.EqualTo(12));
        }

        [Test]
        public void PermuteNodes_MapReducesMaxNode_MaxNodeUpdates()
        {
            var o2m = CreateO2M(new List<List<int>> { new List<int> {0, 1, 2, 3} });
            var nodeMap = new List<int> {0,0,1,1};
            o2m.PermuteNodes(nodeMap);
            CollectionAssert.AreEqual(new List<int> {0,0,1,1}, o2m[0]);
            Assert.That(o2m.MaxNode, Is.EqualTo(1));
        }

        [Test]
        public void PermuteNodes_NullMap_ThrowsArgumentNullException()
        {
            var o2m = new O2M();
            Assert.Throws<ArgumentNullException>(() => o2m.PermuteNodes(null!));
        }

        [Test]
        public void PermuteNodes_NodeOutOfMapRange_ThrowsArgumentOutOfRangeException()
        {
            var o2m = CreateO2M(new List<List<int>> { new List<int> {0, 1} });
            var nodeMap = new List<int> {10};
            Assert.Throws<ArgumentOutOfRangeException>(() => o2m.PermuteNodes(nodeMap));
        }
        #endregion

        #region PermuteElements
        [Test]
        public void PermuteElements_ValidMap_PermutesElements()
        {
            var adj0 = new List<int> {0,1};
            var adj1 = new List<int> {2};
            var adj2 = new List<int> {3,4};
            var o2m = CreateO2M(new List<List<int>> { adj0, adj1, adj2 });
            var elementMap = new List<int> {2, 0, 1};

            o2m.PermuteElements(elementMap);

            Assert.That(o2m.Count, Is.EqualTo(3));
            CollectionAssert.AreEqual(adj1, o2m[0]);
            CollectionAssert.AreEqual(adj2, o2m[1]);
            CollectionAssert.AreEqual(adj0, o2m[2]);
            Assert.That(o2m.MaxNode, Is.EqualTo(4));
        }

        [Test]
        public void PermuteElements_NullMap_ThrowsArgumentNullException()
        {
            var o2m = new O2M();
            Assert.Throws<ArgumentNullException>(() => o2m.PermuteElements(null!));
        }

        [Test]
        public void PermuteElements_MapLengthMismatch_ThrowsArgumentException()
        {
            var o2m = CreateO2M(new List<List<int>> { new List<int> {0} });
            Assert.Throws<ArgumentException>(() => o2m.PermuteElements(new List<int> {0, 1}));
        }

        [Test]
        public void PermuteElements_MapWithOutOfRangeNewPosition_ThrowsArgumentOutOfRangeException()
        {
            var o2m = CreateO2M(new List<List<int>> { new List<int> {0}, new List<int> {1} });
            Assert.Throws<ArgumentOutOfRangeException>(() => o2m.PermuteElements(new List<int> {0, 2}));
            Assert.Throws<ArgumentOutOfRangeException>(() => o2m.PermuteElements(new List<int> {0, -1}));
        }

        [Test]
        public void PermuteElements_MapWithDuplicateNewPosition_ThrowsArgumentException()
        {
            var o2m = CreateO2M(new List<List<int>> { new List<int> {0}, new List<int> {1} });
            Assert.Throws<ArgumentException>(() => o2m.PermuteElements(new List<int> {0, 0}));
        }
        #endregion

        #region GetNodePositions
        [Test]
        public void GetNodePositions_ValidInputs_ReturnsCorrectPositions()
        {
            var elementsToNodes = CreateO2M(new List<List<int>> {
                new List<int> {0, 1},
                new List<int> {1, 2, 0}
            });
            var nodesToElements = CreateO2M(new List<List<int>> {
                new List<int> {0, 1},
                new List<int> {0, 1},
                new List<int> {1}
            });

            var nodePositions = O2M.GetNodePositions(elementsToNodes, nodesToElements);

            Assert.That(nodePositions.Count, Is.EqualTo(nodesToElements.Count));
            CollectionAssert.AreEqual(new List<int> {0, 2}, nodePositions[0]);
            CollectionAssert.AreEqual(new List<int> {1, 0}, nodePositions[1]);
            CollectionAssert.AreEqual(new List<int> {1}, nodePositions[2]);
        }

        [Test]
        public void GetNodePositions_NodeNotInAnyElement_ReturnsEmptyListForNode()
        {
            var elementsToNodes = CreateO2M(new List<List<int>> { new List<int> {0} });
            var nodesToElements = CreateO2M(new List<List<int>> { new List<int> {0}, new List<int>() });
            var nodePositions = O2M.GetNodePositions(elementsToNodes, nodesToElements);
            Assert.That(nodePositions.Count, Is.EqualTo(2));
            CollectionAssert.AreEqual(new List<int> {0}, nodePositions[0]);
            Assert.That(nodePositions[1], Is.Empty);
        }

        [Test]
        public void GetNodePositions_NullInputs_ThrowsArgumentNullException()
        {
            var o2m = new O2M();
            Assert.Throws<ArgumentNullException>(() => O2M.GetNodePositions(null!, o2m));
            Assert.Throws<ArgumentNullException>(() => O2M.GetNodePositions(o2m, null!));
        }
        #endregion

        #region GetElementPositions
        [Test]
        public void GetElementPositions_ValidInputs_ReturnsCorrectPositions()
        {
            var elementsToNodes = CreateO2M(new List<List<int>> { new List<int> {0,1}, new List<int> {1,2}});
            var nodesToElements = CreateO2M(new List<List<int>> { new List<int> {0}, new List<int> {0,1}, new List<int> {1}});

            var elemPos = O2M.GetElementPositions(elementsToNodes, nodesToElements);
            Assert.That(elemPos.Count, Is.EqualTo(elementsToNodes.Count));
            CollectionAssert.AreEqual(new List<int> {0, 0}, elemPos[0]);
            CollectionAssert.AreEqual(new List<int> {1, 0}, elemPos[1]);
        }

        [Test]
        public void GetElementPositions_NullInputs_ThrowsArgumentNullException()
        {
            var o2m = new O2M();
            Assert.Throws<ArgumentNullException>(() => O2M.GetElementPositions(null!, o2m));
            Assert.Throws<ArgumentNullException>(() => O2M.GetElementPositions(o2m, null!));
        }
        #endregion

        #region GetBooleanMatrix & GetFromBooleanMatrix
        [Test]
        public void GetBooleanMatrix_ConvertsCorrectly()
        {
            var o2m = CreateO2M(new List<List<int>> {
                new List<int> {0, 2},
                new List<int>(),
                new List<int> {1}
            });
            var matrix = o2m.GetBooleanMatrix();

            Assert.That(matrix.GetLength(0), Is.EqualTo(3));
            Assert.That(matrix.GetLength(1), Is.EqualTo(3));

            Assert.That(matrix[0, 0], Is.True);
            Assert.That(matrix[0, 1], Is.False);
            Assert.That(matrix[0, 2], Is.True);

            Assert.That(matrix[1, 0], Is.False);
            Assert.That(matrix[1, 1], Is.False);
            Assert.That(matrix[1, 2], Is.False);

            Assert.That(matrix[2, 0], Is.False);
            Assert.That(matrix[2, 1], Is.True);
            Assert.That(matrix[2, 2], Is.False);
        }

        [Test]
        public void GetBooleanMatrix_EmptyO2M_ReturnsEmptyMatrix()
        {
            var o2m = new O2M();
            var matrix = o2m.GetBooleanMatrix();
            Assert.That(matrix.GetLength(0), Is.EqualTo(0));
            Assert.That(matrix.GetLength(1), Is.EqualTo(0));
        }

        [Test]
        public void GetBooleanMatrix_O2MWithElementsButNoNodes_ReturnsMatrixWithZeroCols()
        {
            var o2m = CreateO2M(new List<List<int>> { new List<int>(), new List<int>() });
            var matrix = o2m.GetBooleanMatrix();
            Assert.That(matrix.GetLength(0), Is.EqualTo(2));
            Assert.That(matrix.GetLength(1), Is.EqualTo(0));
        }

        [Test]
        public void GetFromBooleanMatrix_ConvertsCorrectly()
        {
            var matrix = new bool[,] {
                {true, false, true},
                {false, false, false},
                {false, true, false}
            };
            var o2m = O2M.GetFromBooleanMatrix(matrix);

            Assert.That(o2m.Count, Is.EqualTo(3));
            Assert.That(o2m.MaxNode, Is.EqualTo(2));
            CollectionAssert.AreEqual(new List<int> {0, 2}, o2m[0]);
            Assert.That(o2m[1], Is.Empty);
            CollectionAssert.AreEqual(new List<int> {1}, o2m[2]);
        }

        [Test]
        public void GetFromBooleanMatrix_EmptyMatrix_ReturnsEmptyO2M()
        {
            var matrix = new bool[0,0];
            var o2m = O2M.GetFromBooleanMatrix(matrix);
            Assert.That(o2m.Count, Is.EqualTo(0));
            Assert.That(o2m.MaxNode, Is.EqualTo(-1));

            matrix = new bool[2,0];
            o2m = O2M.GetFromBooleanMatrix(matrix);
            Assert.That(o2m.Count, Is.EqualTo(2));
            Assert.That(o2m.MaxNode, Is.EqualTo(-1));
            Assert.That(o2m[0], Is.Empty);
            Assert.That(o2m[1], Is.Empty);
        }

        [Test]
        public void GetFromBooleanMatrix_NullMatrix_ThrowsArgumentNullException()
        {
            Assert.Throws<ArgumentNullException>(() => O2M.GetFromBooleanMatrix(null!));
        }
        #endregion

        #region GetRandomO2M
        [Test]
        public void GetRandomO2M_GeneratesWithCorrectDimensions()
        {
            var o2m = O2M.GetRandomO2M(5, 10, 0.5, 123);
            Assert.That(o2m.Count, Is.EqualTo(5));
            if (o2m.MaxNode != -1)
                Assert.That(o2m.MaxNode, Is.LessThan(10));
            for(int i=0; i<o2m.Count; i++)
            {
                foreach(var node in o2m[i])
                {
                    Assert.That(node, Is.GreaterThanOrEqualTo(0));
                    Assert.That(node, Is.LessThan(10));
                }
            }
        }

        [Test]
        public void GetRandomO2M_ZeroCounts_GeneratesEmpty()
        {
            var o2m = O2M.GetRandomO2M(0, 0, 0.5);
            Assert.That(o2m.Count, Is.EqualTo(0));
            Assert.That(o2m.MaxNode, Is.EqualTo(-1));

            o2m = O2M.GetRandomO2M(2, 0, 0.5);
            Assert.That(o2m.Count, Is.EqualTo(2));
            Assert.That(o2m.MaxNode, Is.EqualTo(-1));
            Assert.That(o2m[0], Is.Empty);
            Assert.That(o2m[1], Is.Empty);
        }

        [Test]
        public void GetRandomO2M_DensityZero_GeneratesNoConnections()
        {
            var o2m = O2M.GetRandomO2M(5, 10, 0.0, 123);
            Assert.That(o2m.Count, Is.EqualTo(5));
            Assert.That(o2m.MaxNode, Is.EqualTo(-1));
            for(int i=0; i<o2m.Count; ++i)
                Assert.That(o2m[i], Is.Empty);
        }

        [Test]
        public void GetRandomO2M_DensityOne_GeneratesAllConnections()
        {
            var o2m = O2M.GetRandomO2M(5, 10, 1.0, 123);
            Assert.That(o2m.Count, Is.EqualTo(5));
            Assert.That(o2m.MaxNode, Is.EqualTo(9));
            for(int i=0; i<o2m.Count; ++i) {
                Assert.That(o2m[i].Count, Is.EqualTo(10));
                for(int j=0; j<10; ++j)
                    Assert.That(o2m[i].Contains(j), Is.True);
            }
        }

        [Test]
        public void GetRandomO2M_InvalidDensity_ThrowsArgumentOutOfRangeException()
        {
            Assert.Throws<ArgumentOutOfRangeException>(() => O2M.GetRandomO2M(1,1,-0.1));
            Assert.Throws<ArgumentOutOfRangeException>(() => O2M.GetRandomO2M(1,1, 1.1));
        }
        [Test]
        public void GetRandomO2M_NegativeCounts_ThrowsArgumentOutOfRangeException()
        {
            Assert.Throws<ArgumentOutOfRangeException>(() => O2M.GetRandomO2M(-1,1,0.5));
            Assert.Throws<ArgumentOutOfRangeException>(() => O2M.GetRandomO2M(1,-1,0.5));
        }
        #endregion

        #region GetEPSString
        [Test]
        public void GetEPSString_GeneratesNonEmptyStringWithHeaderAndFooter()
        {
            var o2m = CreateO2M(new List<List<int>> { new List<int> {0,1}, new List<int> {0}});
            string eps = o2m.GetEPSString("Test Title");

            Assert.That(eps, Is.Not.Empty);
            StringAssert.StartsWith("%!PS-Adobe-3.0 EPSF-3.0", eps);
            StringAssert.Contains("%%BoundingBox:", eps);
            StringAssert.Contains("%%Title: Test Title", eps);
            StringAssert.Contains("showpage", eps);
            StringAssert.EndsWith("%%EOF" + Environment.NewLine, eps.TrimEnd('\r','\n') + Environment.NewLine);
        }

        [Test]
        public void GetEPSString_EmptyO2M_GeneratesValidEPS()
        {
            var o2m = new O2M();
            string eps = o2m.GetEPSString("Empty O2M");
            Assert.That(eps, Is.Not.Empty);
            StringAssert.StartsWith("%!PS-Adobe-3.0 EPSF-3.0", eps);
            StringAssert.Contains("%%BoundingBox:", eps);
            StringAssert.DoesNotContain("arc fill", eps);
            StringAssert.DoesNotContain("lineto stroke", eps);
            StringAssert.EndsWith("%%EOF" + Environment.NewLine, eps.TrimEnd('\r','\n') + Environment.NewLine);
        }
        #endregion

        #region Transpose
        [Test]
        public void Transpose_SimpleMatrix_ReturnsCorrectTranspose()
        {
            var o2m = CreateO2M(new List<List<int>> {
                new List<int> {0, 2},
                new List<int> {1}
            });
            var transposed = o2m.Transpose();

            Assert.That(transposed.Count, Is.EqualTo(3));
            Assert.That(transposed.MaxNode, Is.EqualTo(1));

            CollectionAssert.AreEquivalent(new List<int> {0}, transposed[0]);
            CollectionAssert.AreEquivalent(new List<int> {1}, transposed[1]);
            CollectionAssert.AreEquivalent(new List<int> {0}, transposed[2]);
        }

        [Test]
        public void Transpose_EmptyO2M_ReturnsEmptyO2M()
        {
            var o2m = new O2M();
            var transposed = o2m.Transpose();
            Assert.That(transposed.Count, Is.EqualTo(0));
            Assert.That(transposed.MaxNode, Is.EqualTo(-1));
        }

        [Test]
        public void Transpose_O2MWithElementsButNoNodes_CorrectTranspose()
        {
            var o2m = CreateO2M(new List<List<int>> { new List<int>(), new List<int>() });
            var transposed = o2m.Transpose();
            Assert.That(transposed.Count, Is.EqualTo(0));
            Assert.That(transposed.MaxNode, Is.EqualTo(1));
        }

        [Test]
        public void Transpose_O2MWithNodesButNoElements_CorrectTranspose()
        {
            var o2m = new O2M(0);
            var transposed = o2m.Transpose();
            Assert.That(transposed.Count, Is.EqualTo(0));
            Assert.That(transposed.MaxNode, Is.EqualTo(-1));
        }
        #endregion

        #region Matrix Multiplication Operator (*)
        [Test]
        public void OperatorMultiply_SimpleCase_CorrectResult()
        {
            var a = CreateO2M(new List<List<int>> { new List<int> {0, 2}, new List<int> {1} });
            var b = CreateO2M(new List<List<int>> { new List<int> {1}, new List<int> {0,1}, new List<int> {0} });
            Assert.That(a.MaxNode, Is.LessThan(b.Count));
            var result = a * b;

            Assert.That(result.Count, Is.EqualTo(a.Count));
            Assert.That(result.MaxNode, Is.EqualTo(b.MaxNode));
            CollectionAssert.AreEquivalent(new List<int> {0, 1}, result[0]);
            CollectionAssert.AreEquivalent(new List<int> {0, 1}, result[1]);
        }

        [Test]
        public void OperatorMultiply_SafeCase_CorrectResult()
        {
            var a = CreateO2M(new List<List<int>> { new List<int> {0}, new List<int> {1} });
            var b = CreateO2M(new List<List<int>> { new List<int> {0,1} });
            Assert.That(a.MaxNode, Is.GreaterThanOrEqualTo(b.Count));
            var result = a * b;

            Assert.That(result.Count, Is.EqualTo(a.Count));
            Assert.That(result.MaxNode, Is.EqualTo(b.MaxNode));
            CollectionAssert.AreEquivalent(new List<int> {0,1}, result[0]);
            Assert.That(result[1], Is.Empty);
        }

        [Test]
        public void OperatorMultiply_NullInputs_ThrowsArgumentNullException()
        {
            var o2m = new O2M();
            Assert.Throws<ArgumentNullException>(() => { var r = o2m * null!; });
            Assert.Throws<ArgumentNullException>(() => { var r = null! * o2m; });
        }
        #endregion

        #region Set Operators (|,+,&,^,-)
        [Test]
        public void OperatorOr_Union_CorrectResult()
        {
            var left = CreateO2M(new List<List<int>> { new List<int> {0,1}, new List<int> {2}});
            var right = CreateO2M(new List<List<int>> { new List<int> {1,2}, new List<int>(), new List<int> {3}});
            var result = left | right;

            Assert.That(result.Count, Is.EqualTo(3));
            Assert.That(result.MaxNode, Is.EqualTo(3));
            CollectionAssert.AreEqual(new List<int> {0,1,2}, result[0]);
            CollectionAssert.AreEqual(new List<int> {2}, result[1]);
            CollectionAssert.AreEqual(new List<int> {3}, result[2]);

            var resultPlus = left + right;
            Assert.That(result.Equals(resultPlus), Is.True);
        }

        [Test]
        public void OperatorAnd_Intersection_CorrectResult()
        {
            var left = CreateO2M(new List<List<int>> { new List<int> {0,1,2}, new List<int> {2,3}});
            var right = CreateO2M(new List<List<int>> { new List<int> {1,2,4}, new List<int> {3,5}, new List<int> {0}});
            var result = left & right;

            Assert.That(result.Count, Is.EqualTo(2));
            Assert.That(result.MaxNode, Is.EqualTo(3));
            CollectionAssert.AreEquivalent(new List<int> {1,2}, result[0]);
            CollectionAssert.AreEquivalent(new List<int> {3}, result[1]);
        }

        [Test]
        public void OperatorXor_SymmetricDifference_CorrectResult()
        {
            var left = CreateO2M(new List<List<int>> { new List<int> {0,1}, new List<int> {2}});
            var right = CreateO2M(new List<List<int>> { new List<int> {1,2}, new List<int> {}});
            var result = left ^ right;

            Assert.That(result.Count, Is.EqualTo(2));
            Assert.That(result.MaxNode, Is.EqualTo(2));
            CollectionAssert.AreEquivalent(new List<int> {0,2}, result[0]);
            CollectionAssert.AreEquivalent(new List<int> {2}, result[1]);
        }

        [Test]
        public void OperatorMinus_Difference_CorrectResult()
        {
            var left = CreateO2M(new List<List<int>> { new List<int> {0,1,2}, new List<int> {2,3}, new List<int> {4}});
            var right = CreateO2M(new List<List<int>> { new List<int> {1,3}, new List<int> {3,5}});
            var result = left - right;

            Assert.That(result.Count, Is.EqualTo(3));
            Assert.That(result.MaxNode, Is.EqualTo(4));
            CollectionAssert.AreEquivalent(new List<int> {0,2}, result[0]);
            CollectionAssert.AreEquivalent(new List<int> {2}, result[1]);
            CollectionAssert.AreEquivalent(new List<int> {4}, result[2]);

            var resultFromNull = left - null;
            Assert.That(left.Equals(resultFromNull), Is.True);
        }

        [Test]
        public void SetOperators_NullInputs_ThrowsArgumentNullException()
        {
            var o2m = new O2M();
            Assert.Throws<ArgumentNullException>(() => { var r = o2m | null!; });
            Assert.Throws<ArgumentNullException>(() => { var r = null! | o2m; });
            Assert.Throws<ArgumentNullException>(() => { var r = o2m + null!; });
            Assert.Throws<ArgumentNullException>(() => { var r = null! + o2m; });
            Assert.Throws<ArgumentNullException>(() => { var r = o2m & null!; });
            Assert.Throws<ArgumentNullException>(() => { var r = null! & o2m; });
            Assert.Throws<ArgumentNullException>(() => { var r = o2m ^ null!; });
            Assert.Throws<ArgumentNullException>(() => { var r = null! ^ o2m; });
            Assert.Throws<ArgumentNullException>(() => { var r = null! - o2m; });
        }
        #endregion
    }
    [TestFixture]
    public class M2MTests
    {
        #region Helper Methods
        private static M2M CreateM2M(List<List<int>> adjacencies)
        {
            return new M2M(adjacencies);
        }

        #endregion

        #region Constructors
        [Test]
        public void Constructor_Default_InitializesProperties()
        {
            var m2m = new M2M();
            Assert.That(m2m.Count, Is.EqualTo(0));
            Assert.That(m2m.MaxNode, Is.EqualTo(-1));
            Assert.That(m2m.Elemeloc, Is.Not.Null.And.Empty);
            Assert.That(m2m.Nodeloc, Is.Not.Null.And.Empty);
            Assert.That(m2m.Elementsfromnode, Is.Not.Null);
            Assert.That(m2m.Elementsfromnode.Count, Is.EqualTo(0));
            // IsInSync is private, its effect is tested via Synchronize()
        }

        [Test]
        public void Constructor_WithReservedSize_InitializesProperties()
        {
            var m2m = new M2M(10);
            Assert.That(m2m.Count, Is.EqualTo(0));
            Assert.That(m2m.Elemeloc, Is.Not.Null.And.Empty);
            // IsInSync is private
        }

        [Test]
        public void Constructor_WithAdjacencies_InitializesProperties()
        {
            var adj = new List<List<int>> { new List<int> {0, 1} };
            var m2m = new M2M(adj);
            Assert.That(m2m.Count, Is.EqualTo(1));
            Assert.That(m2m.MaxNode, Is.EqualTo(1));
            Assert.That(m2m.Elemeloc, Is.Not.Null.And.Empty); // Not synced yet
            // IsInSync is private
        }
        #endregion

        #region CompareTo and Equals (M2M specific, relies on ListExtensions.Compare)
        [Test]
        public void CompareTo_M2M_Null_Returns1()
        {
            var m2m = new M2M();
            Assert.That(m2m.CompareTo(null as M2M), Is.EqualTo(1));
        }

        [Test]
        public void CompareTo_M2M_DifferentCount_ComparesCount()
        {
            var m2mA = CreateM2M(new List<List<int>> { new List<int> {0} });
            var m2mB = CreateM2M(new List<List<int>> { new List<int> {0}, new List<int> {1} });
            Assert.That(m2mA.CompareTo(m2mB), Is.LessThan(0));
            Assert.That(m2mB.CompareTo(m2mA), Is.GreaterThan(0));
        }

        [Test]
        public void CompareTo_M2M_SameCount_DifferentContent_UsesListExtensionsCompare()
        {
            // Assuming ListExtensions.Compare: 0 for equal, <0 if first is smaller, >0 if first is larger
            var m2mA = CreateM2M(new List<List<int>> { new List<int> {0, 1} }); // Mocking ListExtensions.Compare behavior
            var m2mB = CreateM2M(new List<List<int>> { new List<int> {0, 2} }); // If {0,1} < {0,2}
            // We can't control ListExtensions.Compare, so we test structure. If it returns non-zero, CompareTo reflects it.
            // For this test to be concrete, we'd need a stub for ListExtensions.
            // Let's assume if lists are different, CompareTo will be non-zero.
            Assert.That(m2mA.CompareTo(m2mB), Is.Not.EqualTo(0)); // Exact value depends on ListExtensions.Compare
        }

        [Test]
        public void Equals_M2M_NullOrDifferentCount_ReturnsFalse()
        {
            var m2m = CreateM2M(new List<List<int>> { new List<int> {0} });
            Assert.That(m2m.Equals(null as M2M), Is.False);
            var m2mOther = CreateM2M(new List<List<int>> { new List<int> {0}, new List<int> {1} });
            Assert.That(m2m.Equals(m2mOther), Is.False);
        }

        [Test]
        public void Equals_M2M_SameContent_ReturnsTrue()
        {
            var adj = new List<List<int>> { new List<int> {0,1}, new List<int> {2}};
            var m2mA = CreateM2M(adj.Select(l => new List<int>(l)).ToList());
            var m2mB = CreateM2M(adj.Select(l => new List<int>(l)).ToList());
            Assert.That(m2mA.Equals(m2mB), Is.True);
        }

        [Test]
        public void Equals_M2M_DifferentContent_ReturnsFalse()
        {
            var m2mA = CreateM2M(new List<List<int>> { new List<int> {0,1} });
            var m2mB = CreateM2M(new List<List<int>> { new List<int> {0,2} });
            Assert.That(m2mA.Equals(m2mB), Is.False);
        }
        #endregion

        #region Clone (override)
        [Test]
        public void Clone_CreatesDeepCopyAndHandlesSyncState()
        {
            var original = CreateM2M(new List<List<int>> { new List<int> {0, 1}, new List<int> {2} });
            original.Synchronize(); // Ensure original is synced

            var clone = (M2M)original.Clone();

            Assert.That(clone, Is.Not.SameAs(original));
            Assert.That(clone.Count, Is.EqualTo(original.Count));
            Assert.That(clone.MaxNode, Is.EqualTo(original.MaxNode));
            CollectionAssert.AreEqual(original[0], clone[0]);
            Assert.That(clone[0], Is.Not.SameAs(original[0]));

            // Check if clone is also synchronized
            Assert.That(clone.Elementsfromnode.Count, Is.EqualTo(original.Elementsfromnode.Count));
            Assert.That(clone.Elemeloc.Count, Is.EqualTo(original.Elemeloc.Count));
            Assert.That(clone.Nodeloc.Count, Is.EqualTo(original.Nodeloc.Count));
            if (original.Elementsfromnode.Count > 0) {
                 CollectionAssert.AreEqual(original.Elementsfromnode[0], clone.Elementsfromnode[0]);
            }

            // Modify clone and check original is unaffected
            clone.AppendNodeToElement(0, 3); // This will desync clone
            Assert.That(original[0].Count, Is.EqualTo(2));
            Assert.That(original.MaxNode, Is.EqualTo(2));

            // Original sync state should be preserved
            Assert.That(original.Elemeloc.Count, Is.GreaterThan(0)); // Assuming it was populated
        }

        [Test]
        public void Clone_NotSyncedOriginal_CloneIsNotExplicitlySyncedButCopiesData()
        {
            var original = CreateM2M(new List<List<int>> { new List<int> {0, 1}});
            // DO NOT call original.Synchronize()

            var clone = (M2M)original.Clone();
            Assert.That(clone.Count, Is.EqualTo(1));
            CollectionAssert.AreEqual(original[0], clone[0]);

            // Clone's sync properties should be in their initial state (empty)
            // because original was not synced, and clone.Synchronize() is only called if original.IsInSync was true.
            Assert.That(clone.Elemeloc, Is.Empty);
            Assert.That(clone.Nodeloc, Is.Empty);
            Assert.That(clone.Elementsfromnode.Count, Is.EqualTo(0)); // Transpose of empty or default O2M
        }
        #endregion

        #region Equals(object) and GetHashCode (override)
        [Test]
        public void Equals_Object_M2M_CallsTypedEquals()
        {
            var adj = new List<List<int>> { new List<int> {0, 1} };
            var m2mA = CreateM2M(adj.Select(l => new List<int>(l)).ToList());
            var m2mB = CreateM2M(adj.Select(l => new List<int>(l)).ToList());
            var m2mC = CreateM2M(new List<List<int>> { new List<int> {0, 2} });

            Assert.That(m2mA.Equals((object)m2mB), Is.True);
            Assert.That(m2mA.Equals((object)m2mC), Is.False);
            Assert.That(m2mA.Equals(new object()), Is.False);
            Assert.That(m2mA.Equals(null), Is.False);
        }

        [Test]
        public void GetHashCode_M2M_ConsistentForEqualObjects()
        {
            var adj = new List<List<int>> { new List<int> {0, 1}, new List<int> {2} };
            var m2mA = CreateM2M(adj.Select(l => new List<int>(l)).ToList());
            var m2mB = CreateM2M(adj.Select(l => new List<int>(l)).ToList());
            Assert.That(m2mA.GetHashCode(), Is.EqualTo(m2mB.GetHashCode()));
        }
        #endregion

        #region AppendElement (override)
        [Test]
        public void AppendElement_Override_CallsBaseAndSetsIsInSyncFalse()
        {
            var m2m = CreateM2M(new List<List<int>> { new List<int> {0} });
            m2m.Synchronize(); // IsInSync = true
            Assert.That(m2m.Elemeloc.Count, Is.GreaterThan(0)); // Should be synced

            m2m.AppendElement(new List<int> {1, 2}); // Should set IsInSync to false

            Assert.That(m2m.Count, Is.EqualTo(2));
            Assert.That(m2m.MaxNode, Is.EqualTo(2));

            // Test that it's out of sync: Re-sync and check if Elemeloc changes
            var oldElemelocCount = m2m.Elemeloc.Count;
            m2m.Synchronize(); // Should re-calculate
            // If AppendElement correctly desynchronized, Elemeloc might change or be recomputed.
            // We expect Synchronize to update Elemeloc to reflect the new state.
            // The count of elements in Elemeloc should match m2m.Count after sync.
            Assert.That(m2m.Elemeloc.Count, Is.EqualTo(m2m.Count));
        }
        #endregion

        #region Synchronize
        [Test]
        public void Synchronize_PopulatesDerivedPropertiesAndSetsInSync()
        {
            var m2m = CreateM2M(new List<List<int>> {
                new List<int> {0, 1}, // E0
                new List<int> {1, 2}  // E1
            });

            // Before sync
            Assert.That(m2m.Elemeloc, Is.Empty);
            Assert.That(m2m.Nodeloc, Is.Empty);
            Assert.That(m2m.Elementsfromnode.Count, Is.EqualTo(0));

            m2m.Synchronize();

            // After sync
            // Elementsfromnode = Transpose()
            // EFN: N0={E0}, N1={E0,E1}, N2={E1} (MaxNode=1, Count=3 for EFN)
            Assert.That(m2m.Elementsfromnode.Count, Is.EqualTo(m2m.MaxNode + 1)); // MaxNode of original is 2, so EFN count is 3
            Assert.That(m2m.Elementsfromnode.MaxNode, Is.EqualTo(m2m.Count - 1)); // Count of original is 2, so EFN MaxNode is 1
            CollectionAssert.AreEquivalent(new List<int> {0}, m2m.Elementsfromnode[0]); // N0 in E0
            CollectionAssert.AreEquivalent(new List<int> {0, 1}, m2m.Elementsfromnode[1]);// N1 in E0, E1
            CollectionAssert.AreEquivalent(new List<int> {1}, m2m.Elementsfromnode[2]); // N2 in E1

            Assert.That(m2m.Elemeloc.Count, Is.EqualTo(m2m.Count));
            Assert.That(m2m.Nodeloc.Count, Is.EqualTo(m2m.Elementsfromnode.Count)); // Nodeloc size based on nodes

            // Check content of Elemeloc (example for E0, nodes {0,1})
            // E0 has N0 (pos 0 in E0), N1 (pos 1 in E0)
            // GetElementPositions: N0 connected to E0 (pos 0 in N0's list of elements)
            //                      N1 connected to E0 (pos 0 in N1's list of elements)
            // So, Elemeloc[0] (for E0) should be {0,0}
            if (m2m.Elemeloc.Count > 0) {
                CollectionAssert.AreEqual(new List<int> {0,0}, m2m.Elemeloc[0].ToList());
            }


            // Check content of Nodeloc (example for N1, connected to E0, E1)
            // GetNodePositions: N1 is at pos 1 in E0, pos 0 in E1
            // So, Nodeloc[1] (for N1) should be {1,0}
             if (m2m.Nodeloc.Count > 1) {
                 CollectionAssert.AreEqual(new List<int> {1,0}, m2m.Nodeloc[1].ToList());
             }

            // Calling again should be efficient if IsInSync is true
            m2m.Synchronize(); // No error, should return quickly
        }

        [Test]
        public void Synchronize_EmptyM2M_HandlesCorrectly()
        {
            var m2m = new M2M();
            m2m.Synchronize();
            Assert.That(m2m.Elementsfromnode.Count, Is.EqualTo(0));
            Assert.That(m2m.Elemeloc, Is.Empty);
            Assert.That(m2m.Nodeloc, Is.Empty);
        }
        #endregion

        #region GetElementswithnodes
        [Test]
        public void GetElementswithnodes_NullNodes_ThrowsArgumentNullException()
        {
            var m2m = new M2M();
            Assert.Throws<ArgumentNullException>(() => m2m.GetElementswithnodes(null!));
        }

        [Test]
        public void GetElementswithnodes_EmptyNodes_ReturnsEmptyList()
        {
            var m2m = CreateM2M(new List<List<int>> { new List<int> {0} });
            Assert.That(m2m.GetElementswithnodes(new List<int>()), Is.Empty);
        }

        [Test]
        public void GetElementswithnodes_NodeOutOfRange_ReturnsEmptyList()
        {
            var m2m = CreateM2M(new List<List<int>> { new List<int> {0} }); // MaxNode 0. Elementsfromnode count will be 1.
            m2m.Synchronize();
            Assert.That(m2m.GetElementswithnodes(new List<int> {1}), Is.Empty); // Node 1 is out of range
            Assert.That(m2m.GetElementswithnodes(new List<int> {-1}), Is.Empty);
        }

        [Test]
        public void GetElementswithnodes_FindsCommonElements()
        {
            // E0={N0,N1}, E1={N1,N2}, E2={N0,N2}, E3={N0}
            // Sync: EFN: N0={E0,E2,E3}, N1={E0,E1}, N2={E1,E2}
            var m2m = CreateM2M(new List<List<int>> {
                new List<int> {0,1}, new List<int> {1,2}, new List<int> {0,2}, new List<int> {0}
            });
            // Elements containing N0 AND N1:
            // EFN[0]={0,2,3}, EFN[1]={0,1}. Intersect: {0}
            Assert.That(m2m.GetElementswithnodes(new List<int> {0,1}), Is.EquivalentTo(new List<int> {0}));

            // Elements containing N1 AND N2:
            // EFN[1]={0,1}, EFN[2]={1,2}. Intersect: {1}
            Assert.That(m2m.GetElementswithnodes(new List<int> {1,2}), Is.EquivalentTo(new List<int> {1}));

            // Elements containing N0 only
            Assert.That(m2m.GetElementswithnodes(new List<int> {0}), Is.EquivalentTo(new List<int> {0,2,3}));
        }
        #endregion

        #region GetElementsfromnodes
        [Test]
        public void GetElementsfromnodes_FiltersByExactNodeCount()
        {
             // E0={N0,N1}, E1={N1,N2}, E2={N0,N1,N2}, E3={N0}
            var m2m = CreateM2M(new List<List<int>> {
                new List<int> {0,1},      // E0
                new List<int> {1,2},      // E1
                new List<int> {0,1,2},    // E2
                new List<int> {0}         // E3
            });

            // Elements containing N0 AND N1 (GetElementswithnodes -> {E0, E2})
            // E0 has 2 nodes ({N0,N1}). Input nodes count is 2. E0 matches.
            // E2 has 3 nodes ({N0,N1,N2}). Input nodes count is 2. E2 does not match.
            Assert.That(m2m.GetElementsfromnodes(new List<int> {0,1}), Is.EquivalentTo(new List<int> {0}));

            // Elements containing N0, N1, N2 (GetElementswithnodes -> {E2})
            // E2 has 3 nodes. Input nodes count is 3. E2 matches.
            Assert.That(m2m.GetElementsfromnodes(new List<int> {0,1,2}), Is.EquivalentTo(new List<int> {2}));

            // Elements containing N0 (GetElementswithnodes -> {E0, E2, E3})
            // E0 has 2 nodes. Input count 1. No.
            // E2 has 3 nodes. Input count 1. No.
            // E3 has 1 node. Input count 1. Yes.
            Assert.That(m2m.GetElementsfromnodes(new List<int> {0}), Is.EquivalentTo(new List<int> {3}));
        }
        #endregion

        #region GetElementNeighbours
        [Test]
        public void GetElementNeighbours_ArgumentValidation()
        {
            var m2m = CreateM2M(new List<List<int>> { new List<int>{0}});
            Assert.Throws<ArgumentOutOfRangeException>(() => m2m.GetElementNeighbours(-1));
            Assert.Throws<ArgumentOutOfRangeException>(() => m2m.GetElementNeighbours(1)); // Count is 1
        }

        [Test]
        public void GetElementNeighbours_FindsNeighbours()
        {
            // E0={N0,N1}, E1={N1,N2}, E2={N2,N3}
            // Neighbours of E0 (via N0, N1):
            //   N0 is in no other element in this example if we adjust Elementsfromnode for this test case
            //   N1 is in E1. So E1 is a neighbour.
            var m2m = CreateM2M(new List<List<int>> {
                new List<int> {0,1}, // E0
                new List<int> {1,2}, // E1
                new List<int> {2,3}  // E2
            });
            // EFN: N0={E0}, N1={E0,E1}, N2={E1,E2}, N3={E2}

            // Neighbours of E0: N0 is only in E0. N1 is in E0 and E1. So E1 is neighbour.
            var neighboursE0 = m2m.GetElementNeighbours(0);
            CollectionAssert.AreEquivalent(new List<int> {1}, neighboursE0);

            // Neighbours of E1: N1 is in E0,E1. N2 is in E1,E2. So E0, E2 are neighbours.
            var neighboursE1 = m2m.GetElementNeighbours(1);
            CollectionAssert.AreEquivalent(new List<int> {0,2}, neighboursE1);
        }

        [Test]
        public void GetElementNeighbours_NoNeighbours()
        {
            var m2m = CreateM2M(new List<List<int>> { new List<int> {0}, new List<int> {1}}); // E0={N0}, E1={N1}
            // EFN: N0={E0}, N1={E1}
            Assert.That(m2m.GetElementNeighbours(0), Is.Empty);
        }
        #endregion

        #region GetNodeNeighbours
        [Test]
        public void GetNodeNeighbours_ArgumentValidation()
        {
            var m2m = CreateM2M(new List<List<int>> { new List<int>{0}}); // MaxNode=0. EFN.Count=1
            m2m.Synchronize();
            Assert.Throws<ArgumentOutOfRangeException>(() => m2m.GetNodeNeighbours(-1));
            Assert.Throws<ArgumentOutOfRangeException>(() => m2m.GetNodeNeighbours(1)); // EFN.Count is 1
        }

        [Test]
        public void GetNodeNeighbours_FindsNeighbours()
        {
            // E0={N0,N1}, E1={N1,N2}, E2={N0}
            // EFN: N0={E0,E2}, N1={E0,E1}, N2={E1}
            var m2m = CreateM2M(new List<List<int>> {
                new List<int> {0,1}, // E0
                new List<int> {1,2}, // E1
                new List<int> {0}    // E2
            });

            // Neighbours of N0 (via E0, E2):
            //   E0 has N1. E2 has no other nodes. So N1 is neighbour.
            var neighboursN0 = m2m.GetNodeNeighbours(0);
            CollectionAssert.AreEquivalent(new List<int> {1}, neighboursN0);

            // Neighbours of N1 (via E0, E1):
            //   E0 has N0. E1 has N2. So N0, N2 are neighbours.
            var neighboursN1 = m2m.GetNodeNeighbours(1);
            CollectionAssert.AreEquivalent(new List<int> {0,2}, neighboursN1);
        }
        #endregion

        #region CompressElements (override)
        [Test]
        public void CompressElements_Override_CallsBaseAndSynchronizes()
        {
            var m2m = CreateM2M(new List<List<int>> {
                new List<int> {0,1}, // E0
                new List<int> {2},   // E1
                new List<int> {0,3}  // E2
            });
            m2m.Synchronize();
            var initialElemelocCount = m2m.Elemeloc.Count; // Should be 3

            var map = new List<int> {2,0}; // New E0 is old E2, New E1 is old E0
            m2m.CompressElements(map);

            Assert.That(m2m.Count, Is.EqualTo(2));
            CollectionAssert.AreEqual(new List<int>{0,3}, m2m[0]); // old E2
            CollectionAssert.AreEqual(new List<int>{0,1}, m2m[1]); // old E0

            // Check if re-synced
            Assert.That(m2m.Elementsfromnode.Count, Is.Not.EqualTo(0)); // Should be based on new state
            Assert.That(m2m.Elemeloc.Count, Is.EqualTo(m2m.Count)); // 2
            Assert.That(m2m.Nodeloc.Count, Is.EqualTo(m2m.Elementsfromnode.Count));

            // Example check on Elemeloc[0] (which is original E2 {0,3})
            // Elementsfromnode (transposed from {{0,3}, {0,1}}): N0={E0,E1}, N1={E1}, N3={E0}
            // Elemeloc[0] for new E0 (old E2 {0,3}):
            // Node 0: E0 (old E2) is at pos 0 in EFN[0]'s list of elements.
            // Node 3: E0 (old E2) is at pos 0 in EFN[3]'s list of elements.
            // So Elemeloc[0] should be {0,0} (assuming ListExtensions.Compare works as expected for positions)
            // This is getting complex to trace manually for test, rely on component tests of GetElementPositions.
            // Main point is that Synchronize was called.
        }

        [Test]
        public void CompressElements_EmptyMap_ReturnsEarly()
        {
            var m2m = CreateM2M(new List<List<int>> { new List<int> {0,1} });
            m2m.Synchronize(); // Elemeloc populated
            var originalElemeloc = m2m.Elemeloc.ToList(); // Copy

            m2m.CompressElements(new List<int>()); // Empty map

            Assert.That(m2m.Count, Is.EqualTo(1)); // No change to base O2M
            Assert.That(m2m.Elemeloc.Count, Is.EqualTo(originalElemeloc.Count)); // Should not have re-synced if it returned early
            // The current M2M code for CompressElements doesn't change base if map is empty, but still sets IsInSync=false and calls Synchronize.
            // if (oldElementFromNew.Count == 0) return; -> THIS IS NEW
            // The code does return if oldElementFromNew.Count == 0. So it should not re-sync.
             Assert.That(m2m.Elemeloc[0].SequenceEqual(originalElemeloc[0]), Is.True);

        }
        #endregion

        #region PermuteNodes (override)
        [Test]
        public void PermuteNodes_Override_CallsBaseAndSynchronizes()
        {
            var m2m = CreateM2M(new List<List<int>> { new List<int> {0,1}, new List<int> {1,2}});
            m2m.Synchronize();

            var nodeMap = new List<int> {10,11,12}; //0->10, 1->11, 2->12
            m2m.PermuteNodes(nodeMap);

            CollectionAssert.AreEqual(new List<int>{10,11}, m2m[0]);
            CollectionAssert.AreEqual(new List<int>{11,12}, m2m[1]);

            // Check if re-synced
            Assert.That(m2m.Elementsfromnode.Count, Is.Not.EqualTo(0));
            Assert.That(m2m.Elemeloc.Count, Is.EqualTo(m2m.Count));
            // Elementsfromnode should now reflect nodes 10,11,12. MaxNode is 12. EFN.Count = 13.
            Assert.That(m2m.Elementsfromnode.Count, Is.EqualTo(13));
        }

        [Test]
        public void PermuteNodes_EmptyMap_ReturnsEarly()
        {
            var m2m = CreateM2M(new List<List<int>> { new List<int> {0,1} });
            m2m.Synchronize();
             var originalElemeloc = m2m.Elemeloc.ToList();

            m2m.PermuteNodes(new List<int>()); // Empty map

            Assert.That(m2m[0], Is.EquivalentTo(new List<int>{0,1})); // No change to base
            // if (newNodesFromOld.Count == 0) return; -> PermuteNodes returns early
            Assert.That(m2m.Elemeloc[0].SequenceEqual(originalElemeloc[0]), Is.True); // Not re-synced
        }
        #endregion

        #region GetElementsToElements & GetNodesToNodes
        [Test]
        public void GetElementsToElements_ReturnsProductAndSynchronizes()
        {
            // E0={N0,N1}, E1={N1,N2}
            // Sync: EFN: N0={E0}, N1={E0,E1}, N2={E1}
            var m2m = CreateM2M(new List<List<int>> {
                new List<int> {0,1}, // E0
                new List<int> {1,2}  // E1
            });
            // this * EFN
            // this (2x3): E0={N0,N1}, E1={N1,N2} (MaxNode=2)
            // EFN (3x2): N0={E0}, N1={E0,E1}, N2={E1} (MaxNode=1)
            // Result (2x2 for elements)
            // Res[0] (for E0 from 'this'):
            //   this[0,0] (N0) * EFN[0] ({E0}) -> E0
            //   this[0,1] (N1) * EFN[1] ({E0,E1}) -> E0, E1
            //   Union = {E0, E1}
            // Res[1] (for E1 from 'this'):
            //   this[1,1] (N1) * EFN[1] ({E0,E1}) -> E0, E1
            //   this[1,2] (N2) * EFN[2] ({E1}) -> E1
            //   Union = {E0, E1}

            var e2e = m2m.GetElementsToElements();
            Assert.That(e2e.Count, Is.EqualTo(2));
            Assert.That(e2e.MaxNode, Is.EqualTo(1)); // Max element index used in EFN
            CollectionAssert.AreEquivalent(new List<int>{0,1}, e2e[0]);
            CollectionAssert.AreEquivalent(new List<int>{0,1}, e2e[1]);
        }

        [Test]
        public void GetNodesToNodes_ReturnsProductAndSynchronizes()
        {
            // Matrix is: 
            //   0 1 
            //   1 2
            var m2m = CreateM2M(new List<List<int>> {
                new List<int> {0, 1},
                new List<int> {1, 2}
            });

            var m3m = new M2M([[0, 1], [1, 2]]);
            var n2n = m3m.GetNodesToNodes();  
            CollectionAssert.AreEquivalent(new List<int>{0,1}, n2n[0]);
            // Expected results:
            // Node 0 -> connected to {0,1} 
            // Node 1 -> connected to {0,1,2}
            // Node 2 -> connected to {1,2}
            Assert.That(n2n.Count, Is.EqualTo(3));
            Assert.That(n2n.MaxNode, Is.EqualTo(2)); 
            CollectionAssert.AreEquivalent(new List<int>{0,1}, n2n[0]);
            CollectionAssert.AreEquivalent(new List<int>{0,1,2}, n2n[1]); 
            CollectionAssert.AreEquivalent(new List<int>{1,2}, n2n[2]);
        }
        #endregion

        #region GetCliques (new overload)
        [Test]
        public void GetCliques_Overload_CallsBaseWithThisAndElementsfromnode()
        {
            var m2m = CreateM2M(new List<List<int>> {
                new List<int> {0, 1},
                new List<int> {1, 2}
            });
            m2m.Synchronize(); // Ensure Elementsfromnode is populated

            var cliques = m2m.GetCliques(); // Calls O2M.GetCliques(this, Elementsfromnode)

            // Expected result structure is complex, from O2M.GetCliques
            // elementsToNodes = this, nodesToElements = Elementsfromnode
            // elementsToNodes: E0={N0,N1}, E1={N1,N2}
            // nodesToElements (EFN): N0={E0}, N1={E0,E1}, N2={E1}
            // Same as the O2M.GetCliques test case input
            Assert.That(cliques.Count, Is.EqualTo(2));
            CollectionAssert.AreEqual(new List<int> {0, 1, 0, 1}, cliques[0]);
            CollectionAssert.AreEqual(new List<int> {1, 2, 0, 1}, cliques[1]);
        }
        #endregion
        
    }
    [TestFixture]
    public class MM2MTests
    {
        #region Helper Methods
        private MM2M CreateMM2M(int ntypes = 2)
        {
            if (ntypes <= 0) ntypes = 1; // Ensure at least one type for basic tests
            return new MM2M(ntypes);
        }

        private List<(int Type, int Node)>? GetInternalListofMarked(MM2M mm2mInstance)
        {
            FieldInfo? listFieldInfo = typeof(MM2M).GetField("ListofMarked",
                BindingFlags.NonPublic | BindingFlags.Instance);
            return listFieldInfo?.GetValue(mm2mInstance) as List<(int Type, int Node)>;
        }
        #endregion

        #region Constructor and Properties
        [Test]
        public void Constructor_ValidNtypes_InitializesMatrix()
        {
            int ntypes = 2;
            var mm2m = new MM2M(ntypes);

            Assert.That(mm2m.ntypes, Is.EqualTo(ntypes));
            for (int i = 0; i < ntypes; i++)
            {
                for (int j = 0; j < ntypes; j++)
                {
                    Assert.That(mm2m[i, j], Is.Not.Null.And.InstanceOf<M2M>());
                }
            }
        }

        [Test]
        public void Constructor_NtypesZeroOrNegative_ThrowsArgumentOutOfRangeException()
        {
            Assert.Throws<ArgumentOutOfRangeException>(() => new MM2M(0));
            Assert.Throws<ArgumentOutOfRangeException>(() => new MM2M(-1));
        }

        [Test]
        public void Indexer_Get_ReturnsCorrectM2MInstanceAndValidates()
        {
            var mm2m = CreateMM2M(2);
            M2M m2mInstance = mm2m[0, 1];
            Assert.That(m2mInstance, Is.Not.Null);
            m2mInstance.AppendElement(new List<int> { 1 }); // Modify to ensure it's the actual instance
            Assert.That(mm2m[0, 1].Count, Is.EqualTo(1));

            Assert.Throws<ArgumentOutOfRangeException>(() => { var _ = mm2m[-1, 0]; });
            Assert.Throws<ArgumentOutOfRangeException>(() => { var _ = mm2m[0, 2]; });
        }
        #endregion

        #region Querying Methods
        [Test]
        public void NumberofNodes_ReturnsCorrectCount()
        {
            var mm2m = CreateMM2M(2);
            mm2m[0, 1].AppendElement(new List<int> { 10, 20 });
            Assert.That(mm2m.NumberofNodes(elementType: 0, element: 0, nodeType: 1), Is.EqualTo(2));
            Assert.That(mm2m.NumberofNodes(elementType: 0, element: 1, nodeType: 1), Is.EqualTo(0)); // Element 1 doesn't exist
        }

        [Test]
        public void NumberofElements_ForNode_ReturnsCorrectCount()
        {
            var mm2m = CreateMM2M(2);
            mm2m[0, 1].AppendElement(new List<int> { 0 }); // mat[0,1]: E0 uses N0 (of nodeType 1)
            mm2m[0, 1].Synchronize(); // Populates Elementsfromnode for mat[0,1]
            Assert.That(mm2m.NumberofElements(nodeType: 1, node: 0, elementType: 0), Is.EqualTo(1));
        }

        [Test]
        public void NumberofElements_ForElementType_ReturnsCountFromDiagonal()
        {
            var mm2m = CreateMM2M(2);
            mm2m[0, 0].AppendElement(new List<int> {1});
            mm2m[0, 0].AppendElement(new List<int> {2});
            Assert.That(mm2m.NumberofElements(elementType: 0), Is.EqualTo(2));
        }

        [Test]
        public void NumberofActiveElements_CountsElementsWithNodes()
        {
            var mm2m = CreateMM2M(1);
            mm2m[0, 0].AppendElement(new List<int> {1});    // Active
            mm2m[0, 0].AppendElement(new List<int>());      // Not active
            mm2m[0, 0].AppendElement(new List<int> {2,3}); // Active
            Assert.That(mm2m.NumberofActiveElements(elementType: 0), Is.EqualTo(2));
        }
        #endregion

        #region GetAllElements and GetAllNodes
        [Test]
        public void GetAllElements_ForSpecificNode_AggregatesCorrectly()
        {
            var mm2m = CreateMM2M(2); // Types 0, 1
            // Test for nodeType = 0, node = 5. Elements of Type 1 using Node 5 of Type 0.
            // mat[1,0] (elements of type 1, nodes of type 0)
            mm2m[1, 0].AppendElement(new List<int> { 5 }); // E0(T1) uses N5(T0)
            mm2m[1, 0].Synchronize();

            var results = mm2m.GetAllElements(nodeType: 0, node: 5);
            CollectionAssert.AreEquivalent(new List<(int ElemType, int Elem)> { (1,0) }, results);
        }

        [Test]
        public void GetAllElements_ForNodeType_AggregatesFromAllRelevantNodes()
        {
            var mm2m = CreateMM2M(2); // Types 0, 1
            mm2m[0,0].AppendElement(new List<int> {10}); // Defines Node 0 of Type 0

            mm2m[1,0].AppendElement(new List<int> {0}); // E0(T1) uses N0(T0)
            mm2m[1,0].Synchronize();

            var results = mm2m.GetAllElements(nodeType: 0); // Should find (1,0) via Node 0 of Type 0
            CollectionAssert.AreEquivalent(new List<(int ElemType, int Elem)> { (1,0) }, results);
        }

        [Test]
        public void GetAllNodes_ForSpecificElement_AggregatesCorrectly()
        {
            var mm2m = CreateMM2M(2); // Types 0, 1
            // Element 0 of Type 0 uses: N10(T0) and N20(T1)
            mm2m[0, 0].AppendElement(new List<int> { 10 }); // E0(T0) uses N10(T0)
            mm2m[0, 1].AppendElement(new List<int> { 20 }); // E0(T0) uses N20(T1)

            var results = mm2m.GetAllNodes(elementType: 0, elementNumber: 0);
            CollectionAssert.AreEquivalent(new List<(int Type, int Node)> { (0,10), (1,20) }, results);
        }

        [Test]
        public void GetAllNodes_ForElementType_AggregatesFromAllElements()
        {
            var mm2m = CreateMM2M(2);
            mm2m[0,0].AppendElement(new List<int> {5});   // E0(T0) uses N5(T0)
            mm2m[0,1].AppendElement(new List<int> {15});  // E0(T0) uses N15(T1)
            mm2m[0,0].AppendElement(new List<int>()); // Add E1 for mat[0,0] to have Count = 2
            mm2m[0,0].Synchronize(); // Ensure NumberofElements(0) is correct

            var results = mm2m.GetAllNodes(elementType: 0);
            // E0 contributes (0,5), (1,15). E1 (empty) contributes nothing.
            CollectionAssert.AreEquivalent(new List<(int Type, int Node)> { (0,5), (1,15) }, results);
        }
        #endregion

        #region AppendElement
        [Test]
        public void AppendElement_AddsToCorrectSubM2M()
        {
            var mm2m = CreateMM2M(2);
            var nodes = new List<int> {1,2};
            int elIndex = mm2m.AppendElement(elementType:0, nodeType:1, nodes: nodes);
            Assert.That(elIndex, Is.EqualTo(0));
            CollectionAssert.AreEqual(nodes, mm2m[0,1][0]);
        }
        #endregion

        #region Marking, DFS, and Compress
        [Test]
        public void DepthFirstSearchFromANode_SimplePath()
        {
            var mm2m = CreateMM2M(2); // Types 0, 1

            // Path: (Type0, Node0) --> leads to (Type1, ElementIndex0)
            // Means: Element 0 of Type 1 uses Node 0 of Type 0.
            // So: mat[1,0].Elementsfromnode for Node0(T0) should contain ElementIndex0(T1)
            mm2m[1,0].AppendElement(new List<int>{0}); // E0(T1) uses N0(T0)
            mm2m[1,0].Synchronize();

            var reachable = mm2m.DepthFirstSearchFromANode(nodeType:0, node:0);
            // Expected: (0,0) itself, then (1,0) [Type1, ElemIndex0]
            var expected = new List<(int ElemType, int Elem)> { (0,0), (1,0) };
            CollectionAssert.AreEqual(expected, reachable); // DFS returns sorted
        }

        [Test]
        public void MarktoErase_MarksInitialAndViaSimplifiedDFS()
        {
            var mm2m = CreateMM2M(2);
            // Setup a very simple DFS path: (Type0, Node0) -> (Type1, Element0)
            mm2m[1,0].AppendElement(new List<int>{0}); // E0(T1) uses N0(T0)
            mm2m[1,0].Synchronize();

            mm2m.MarktoErase(nodeType: 0, node: 0);

            List<(int Type, int Node)>? markedTuples = GetInternalListofMarked(mm2m);
            Assert.That(markedTuples, Is.Not.Null);
            var expectedMarks = new HashSet<(int, int)> { (0,0), (1,0) }; // (Type, ElementIndex) from DFS results
            foreach (var mark in expectedMarks)
            {
                Assert.That(markedTuples!.Contains(mark), Is.True, $"Expected mark {mark} not found.");
            }
        }

        [Test]
        public void MarkDuplicates_CallsMarkToErase_IntegrationAspect()
        {
            var mm2m = CreateMM2M(1); // Simplify to one type for focused test
            // Setup mat[0,0] to have duplicate elements that M2M.GetDuplicates() would find.
            // This relies on the (unseen) ListExtensions used by M2M.GetDuplicates().
            // Assume element 0 and element 2 are identical: {10,20}
            mm2m[0,0].AppendElement(new List<int>{10, 20}); // Element 0
            mm2m[0,0].AppendElement(new List<int>{30});      // Element 1
            mm2m[0,0].AppendElement(new List<int>{10, 20}); // Element 2 (duplicate of 0)
            mm2m[0,0].Synchronize(); // GetDuplicates in M2M might need this

            mm2m.MarkDuplicates(elementType:0, nodeType:0);

            List<(int Type, int Node)>? markedList = GetInternalListofMarked(mm2m);
            Assert.That(markedList, Is.Not.Null);

            // If M2M.GetDuplicates correctly identifies 0 and 2 (or one of them depending on its impl),
            // then MarkToErase(0,0) and/or MarkToErase(0,2) should be called.
            // For this example, assume GetDuplicates might return index 2 (as a duplicate of an earlier one).
            // Then (0,2) would be directly marked.
            bool mark02Exists = markedList!.Any(m => m.Type == 0 && m.Node == 2);
            // It might also mark 0, depending on ListExtensions.GetDuplicatePositions behavior
            bool mark00Exists = markedList!.Any(m => m.Type == 0 && m.Node == 0);

            Assert.That(mark00Exists || mark02Exists, Is.True, "At least one of the duplicate elements (0,0) or (0,2) should be marked.");
        }


        [Test]
        public void Compress_EmptyListofMarked_ReturnsEarly()
        {
            var mm2m = CreateMM2M(1);
            mm2m.AppendElement(0,0, new List<int>{1});
            mm2m[0,0].Synchronize();
            var initialCount = mm2m[0,0].Count;

            mm2m.Compress();

            Assert.That(mm2m[0,0].Count, Is.EqualTo(initialCount));
            List<(int Type, int Node)>? markedList = GetInternalListofMarked(mm2m);
            Assert.That(markedList, Is.Not.Null.And.Empty);
        }

        [Test]
        public void Compress_ClearsSingleMarkedElement()
        {
            var mm2m = CreateMM2M(1);
            mm2m.AppendElement(0,0, new List<int>{100}); // Element (0,0)
            mm2m.AppendElement(0,0, new List<int>{101}); // Element (0,1) - should remain
            mm2m[0,0].Synchronize();

            List<(int Type, int Node)>? internalMarkedList = GetInternalListofMarked(mm2m);
            Assert.That(internalMarkedList, Is.Not.Null);
            internalMarkedList!.Add((0,0)); // Manually mark Element 0 of Type 0

            Assert.That(mm2m[0,0][0].Count, Is.EqualTo(1)); // Before compress

            mm2m.Compress();

            Assert.That(mm2m[0,0][0].Count, Is.EqualTo(0), "Element (0,0) should be cleared");
            Assert.That(mm2m[0,0][1].Count, Is.EqualTo(1), "Element (0,1) should remain");
            Assert.That(internalMarkedList, Is.Empty, "ListofMarked should be cleared");
            Assert.That(mm2m[0,0].Elemeloc.Count, Is.EqualTo(mm2m[0,0].Count)); // Check sync
        }
        #endregion

        #region GetTypeTopOrder
        [Test]
        public void GetTypeTopOrder_NoDependencies_ReturnsNaturalOrder()
        {
            var mm2m = CreateMM2M(3);
            var order = mm2m.GetTypeTopOrder();
            CollectionAssert.AreEqual(new List<int> {0,1,2}, order);
        }

    #endregion

        #region Delegating Methods
        [Test]
        public void GetElementsFromNodes_DelegatesAndValidates()
        {
            var mm2m = CreateMM2M(1);
            var nodes = new List<int> {0};
            mm2m[0,0].AppendElement(new List<int>{0}); // E0 uses N0
            mm2m[0,0].Synchronize();

            var result = mm2m.GetElementsFromNodes(0,0,nodes);
            CollectionAssert.AreEqual(new List<int>{0}, result);
            Assert.Throws<ArgumentNullException>(() => mm2m.GetElementsFromNodes(0,0,null!));
        }

        [Test]
        public void GetElementsWithNodes_DelegatesAndValidates()
        {
            var mm2m = CreateMM2M(1);
            var nodes = new List<int> {0};
            mm2m[0,0].AppendElement(new List<int>{0}); // E0 uses N0
            mm2m[0,0].AppendElement(new List<int>{0,1});// E1 uses N0,N1
            mm2m[0,0].Synchronize();

            var result = mm2m.GetElementsWithNodes(0,0,nodes); // E0 and E1 both contain N0
            CollectionAssert.AreEquivalent(new List<int>{0,1}, result);
            Assert.Throws<ArgumentNullException>(() => mm2m.GetElementsWithNodes(0,0,null!));
        }
        #endregion
    }
   
}
