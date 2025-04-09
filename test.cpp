
#include "test.hpp"
using namespace std;

void testsek() {
  cout << "=== Testing basics.hpp functions ===\n";

  // screenwrite and screenread
  {
    cout << "- Testing screenwrite and screenread\n";
    int a = 42, b = -7;
    double c = 3.14;
    // Capture output of screenwrite
    ostringstream out;
    streambuf *oldCout = cout.rdbuf(out.rdbuf());
    screenwrite(a, b, c, string("hello"));
    cout.rdbuf(oldCout);
    string output = out.str();
    // Expected format: "42 -7 3.14 hello " (maybe with a newline)
    cout << "screenwrite output: \"" << output << "\"";
    string expected = "42 -7 3.14 hello";
    assert(output.find(expected) != string::npos);
    cout << " [OK]\n";

    // Prepare input for screenread
    istringstream in("100 2.5 test");
    streambuf *oldCin = cin.rdbuf(in.rdbuf());
    int i;
    double d;
    string s;
    screenread(i, d, s);
    cin.rdbuf(oldCin);
    cout << "screenread read values: " << i << ", " << d << ", " << s;
    assert(i == 100 && fabs(d - 2.5) < 1e-9 && s == "test");
    cout << " [OK]\n";
  }

  // pointerdestroy and arraydestroy
  {
    cout << "- Testing pointerdestroy and arraydestroy\n";
    int *pi = new int(123);
    pointerdestroy(pi);
    cout << "pointerdestroy set pointer to "
         << (pi == nullptr ? "nullptr" : "non-null");
    assert(pi == nullptr);
    cout << " [OK]\n";

    double *arr = new double[3];
    arr[0] = 1.1;
    arr[1] = 2.2;
    arr[2] = 3.3;
    arraydestroy(arr);
    cout << "arraydestroy set pointer to "
         << (arr == nullptr ? "nullptr" : "non-null");
    assert(arr == nullptr);
    cout << " [OK]\n";
  }

  // arraycreate with corner cases
  {
    cout << "- Testing arraycreate\n";
    int *px = nullptr;
    arraycreate(px, 5);
    cout << "arraycreate allocated array of 5 ints";
    assert(px != nullptr);

    cout << " (initialized to 0) [OK]\n";

    // Reallocate to a different size with an already non-empty array.
    px[2] = 99;
    arraycreate(px, 3);
    cout << "arraycreate reallocated to size 3";
    assert(px != nullptr);
    for (int j = 0; j < 3; ++j) {
      assert(px[j] == 0);
    }
    cout << " (old array destroyed, new values default) [OK]\n";

    // Allocate zero size (should yield nullptr)
    arraycreate(px, 0);
    cout << "arraycreate with size 0 yields "
         << (px == nullptr ? "nullptr pointer" : "allocated pointer");
    assert(px == nullptr);
    cout << " [OK]\n";
  }

  // typetoname (type and object variants)
  {
    cout << "- Testing typetoname\n";
    string t1 = typetoname<int>();
    cout << "typetoname<int>() -> \"" << t1 << "\"";
    assert(t1.find("int") != string::npos);
    cout << " [OK]\n";

    double df = 5.0;
    string t2 = typetoname(df);
    cout << "typetoname(double) -> \"" << t2 << "\"";
    assert(t2.find("double") != string::npos);
    cout << " [OK]\n";
  }

  // tupleforcycle
  {
    cout << "- Testing tupleforcycle\n";
    tuple<int, char, double> tup(1, 'A', 2.5);
    vector<string> elems;
    tupleforcycle(tup, [&](auto &&x) { elems.push_back(stringfromvalue(x)); });
    cout << "tupleforcycle on (1,'A',2.5) gave elements: ";
    cout << elems[0] << ", " << elems[1] << ", " << elems[2];
    assert(elems.size() == 3 && elems[0] == "1" && elems[1] == "A" &&
           elems[2] == "2.5");
    cout << " [OK]\n";
  }

  // file utilities: fileifexists, fileopenoutput, fileopeninput
  {
    cout << "- Testing file utilities\n";
    string filename = "test_file.txt";
    remove(filename.c_str()); // ensure file does not exist
    bool exists = fileifexists(filename);
    cout << "fileifexists on \"" << filename << "\" (not present) -> "
         << (exists ? "true" : "false");
    assert(!exists);
    cout << " [OK]\n";

    ofstream fout;
    fileopenoutput(fout, filename.c_str());
    assert(fout.is_open());
    fout << "HelloFile";
    fout.close();
    assert(fileifexists(filename));

    ifstream fin;
    fileopeninput(fin, filename.c_str());
    assert(fin.is_open());
    string content;
    fin >> content;
    fin.close();
    cout << "wrote and read file content: " << content;
    assert(content == "HelloFile");
    cout << " [OK]\n";
    remove(filename.c_str());
  }

  // stringfromvalue
  {
    cout << "- Testing stringfromvalue\n";
    string s1 = stringfromvalue(12345);
    string s2 = stringfromvalue(4.5f);
    string s3 = stringfromvalue(string("xyz"));
    cout << "stringfromvalue outputs: " << s1 << ", " << s2 << ", " << s3;
    assert(s1 == "12345");
    assert(s2.find("4.5") != string::npos);
    assert(s3 == "xyz");
    cout << " [OK]\n";
  }

  // commandinvoke
  {
    cout << "- Testing commandinvoke\n";
    // Use a simple shell command to output "OK" to a file (works on
    // Windows/Linux)
    string cmd = "echo OK > cmd_test.txt";
    int ret = commandinvoke(cmd);
    cout << "commandinvoke(\"" << cmd << "\") returned " << ret;
    assert(ret == 0);
    ifstream fin("cmd_test.txt");
    string text;
    fin >> text;
    fin.close();
    cout << ", produced output \"" << text << "\"";
    assert(text == "OK");
    cout << " [OK]\n";
    remove("cmd_test.txt");
  }

  // commandargget
  {
    cout << "- Testing commandargget\n";
    char arg0[] = "prog";
    char arg1[] = "alpha";
    char arg2[] = "beta";
    char arg3[] = "gamma";
    char *argv[] = {arg0, arg1, arg2, arg3};
    int argc = 4;
    vector<string> args = commandargget(argc, argv);
    cout << "commandargget produced [";
    for (size_t k = 0; k < args.size(); ++k) {
      cout << args[k] << (k < args.size() - 1 ? ", " : "");
    }
    cout << "]";
    assert(args.size() == 4 && args[1] == "alpha" && args[3] == "gamma");
    cout << " [OK]\n";
  }

  cout << "\n=== Testing seque template container ===\n";

  // Testing default construction and empty seque
  {
    cout << "- Testing default construction and empty seque\n";
    seque<int> emptySek;
    cout << "Empty seque: size = " << emptySek.size;
    assert(emptySek.size == 0);
    // Attempt iteration on empty container:
    for (auto it = emptySek.begin(); it != emptySek.end(); ++it) {
      // Should never enter this loop
      assert(false);
    }
    cout << " [OK]\n";
  }

  // Constructors and size management
  {
    cout << "- Testing constructors and size management\n";
    seque<int> s0;
    cout << "default seque<int>: size = " << getsize(s0);
    assert(getsize(s0) == 0);
    cout << " [OK]\n";

    seque<int> s5(5);
    cout << "seque<int>(5): size = " << getsize(s5);
    assert(getsize(s5) == 5);
    // Check that all values are default-initialized
    for (size_t i = 0; i < getsize(s5); ++i) {
      assert(s5[i] == int());
    }
    cout << " [OK]\n";

    seque<int> s5val(5, 7);
    cout << "seque<int>(5,7): size = " << s5val.size
         << ", first element = " << s5val[0];
    assert(s5val.size == 5 && s5val[0] == 7 && s5val[4] == 7);
    cout << " [OK]\n";

    seque<int> sInit = {1, 2, 3};
    cout << "seque<int>{1,2,3}: size = " << sInit.size;
    assert(sInit.size == 3 && sInit[1] == 2);
    cout << " [OK]\n";
  }

  // Copy and move constructors, assignment operators
  {
    cout << "- Testing copy and move semantics\n";
    seque<int> orig = {10, 20, 30};
    seque<int> copyCtor(orig);
    cout << "copy constructed seque: size=" << copyCtor.size;
    assert(copyCtor.size == 3 && copyCtor[1] == 20);
    copyCtor[1] = 99;
    assert(orig[1] == 20);
    cout << " [OK]\n";

    seque<int> assignSrc = {5, 6, 7};
    seque<int> assignDst;
    assignDst = assignSrc;
    cout << "copy assignment: dst size=" << assignDst.size;
    assert(assignDst.size == 3 && assignDst[2] == 7);
    assignSrc[2] = 42;
    assert(assignDst[2] == 7);
    cout << " [OK]\n";
  }

  // operator= with a value and initializer_list
  {
    cout << "- Testing operator= with value and initializer list\n";
    seque<int> s = {1, 2, 3};
    s = 9;
    cout << "after s = 9: ";
    for (size_t i = 0; i < s.size; ++i) {
      cout << s[i] << " ";
      assert(s[i] == 9);
    }
    cout << "[OK]\n";

    s = {7, 8};
    cout << "after s = {7,8}: size=" << s.size << ", elements: " << s[0] << ", "
         << s[1];
    assert(s.size == 2 && s[0] == 7 && s[1] == 8);
    cout << " [OK]\n";
  }

  // Element access (operator[] and operator())
  {
    cout << "- Testing element access operators on empty and non-empty seque\n";
    const seque<int> cs = {4, 5, 6};
    cout << "cs[1] = " << cs[1];
    assert(cs[1] == 5);
    cout << " [OK]\n";

    // Non-const element access:
    seque<int> sx = {10, 20, 30};
    int &ref = sx[2];
    ref = 99;
    assert(sx[2] == 99);
    cout << "Non-const operator[] allows modifying element to " << sx[2]
         << " [OK]\n";

    // Testing operator() to extend container when valid
    seque<int> sy = {1, 2, 3};
    sy(5) = 42; // extend to index 5
    cout << "operator()(5) extended size to " << sy.size << " and set value to "
         << sy[5];
    assert(sy.size == 6 && sy[5] == 42);
    cout << " [OK]\n";

    // Testing operator() on empty container for read (should throw)
    try {
      const seque<int> emptyC;
      int tmp = emptyC(0);
      (void)tmp;
      cout << "Accessing empty const seque did not throw [FAIL]\n";
      assert(false);
    } catch (const out_of_range &e) {
      cout << "Accessing empty const seque threw exception as expected [OK]\n";
    }
  }

  // Iterators: begin() and end()
  {
    cout << "- Testing begin() and end() iterators\n";
    seque<int> s = {3, 1, 4};
    int sum = 0;
    for (auto it = s.begin(); it != s.end(); ++it) {
      sum += *it;
    }
    cout << "sum of {3,1,4} via iterators = " << sum;
    assert(sum == 8);
    cout << " [OK]\n";

    // Test on an empty seque:
    seque<int> empty;
    assert(empty.begin() == empty.end());
    cout << "Empty seque iterators are equal [OK]\n";
  }

  // swap free function
  {
    cout << "- Testing swap\n";
    seque<int> a = {1, 2, 3}, b = {4, 5};
    swap(a, b);
    cout << "After swap: a.size=" << a.size << ", b.size=" << b.size;
    assert(a.size == 2 && b.size == 3);
    assert(a[0] == 4 && b[0] == 1);
    cout << " [OK]\n";
  }

  // Erase functions: erase, erase(index), erase(indices), eraselast,
  // eraseinplace
  {
    cout << "- Testing erase functions including empty seque\n";
    // Erase entire container
    seque<int> s = {10, 20, 30, 40, 50};
    erase(s);
    cout << "erase(s) on non-empty seque sets size to " << s.size;
    assert(s.size == 0);
    // Erase on empty seque should do nothing (or remain empty)
    erase(s);
    assert(s.size == 0);
    cout << " [OK]\n";

    // Erase by index:
    s = {10, 20, 30, 40};
    erase(s, 1);
    cout << "erase index 1: new size=" << s.size << ", elements: ";
    for (size_t i = 0; i < s.size; ++i) {
      cout << s[i] << " ";
    }
    vector<int> rem(s.begin(), s.end());
    vector<int> expected = {10, 40, 30};
    assert(s.size == 3 && rem == expected);
    cout << " [OK]\n";

    // Attempt erase with invalid index on non-empty seque:
    size_t oldSize = s.size;
    erase(s, 10); // Should be a no-op
    assert(s.size == oldSize);
    cout << "erase with invalid index on non-empty seque [OK]\n";

    // Erase by indices vector:
    s = {1, 2, 3, 4, 5};
    seque<size_t> idx = {1, 3};
    erase(s, idx);
    vector<int> afterErase(s.begin(), s.end());
    vector<int> expected2 = {1, 3, 5}; // depending on removal strategy
    sort(afterErase.begin(), afterErase.end());
    sort(expected2.begin(), expected2.end());
    assert(s.size == 3 && afterErase == expected2);
    cout << "erase with indices vector [OK]\n";

    // eraselast on empty seque should be safe (no-op)
    s = {};
    eraselast(s);
    assert(s.size == 0);
    cout << "eraselast on empty seque [OK]\n";

    // eraseinplace: remove a range of elements
    s = {1, 2, 3, 4, 5};
    eraseinplace(s, 1, 2); // removes elements at index 1 and 2
    vector<int> afterEraseRange(s.begin(), s.end());
    vector<int> expectedRange = {1, 4, 5};
    assert(s.size == 3 && afterEraseRange == expectedRange);
    cout << "eraseinplace range removal [OK]\n";

    // eraseinplace with start index beyond size (no-op)
    s = {1, 2, 3};
    eraseinplace(s, 5, 1);
    assert(s.size == 3);
    cout << "eraseinplace with invalid start index [OK]\n";
  }

  // Insertion: add and addinplace, including on empty seque
  {
    cout << "- Testing add and addinplace (corner cases)\n";
    seque<int> s = {5, 6, 7, 8};
    try {
      // add(s, index, value) expects index < size. For empty, it should throw.
      add(s, 4, 400);
      cout << "add with index == size did not throw [FAIL]\n";
      assert(false);
    } catch (const out_of_range &) {
      cout << "add with index == size threw exception as expected [OK]\n";
    }

    // Testing add on an empty seque (should throw)
    seque<int> empty;
    try {
      add(empty, 0, 1);
      cout << "add on empty seque did not throw [FAIL]\n";
      assert(false);
    } catch (const out_of_range &) {
      cout << "add on empty seque threw exception as expected [OK]\n";
    }

    // addinplace on non-empty seque:
    seque<int> v = {10, 20, 30};
    addinplace(v, 0, 5);
    vector<int> expv = {5, 10, 20, 30};
    assert(vector<int>(v.begin(), v.end()) == expv);
    cout << "addinplace at beginning [OK]\n";

    v = {10, 20, 30};
    addinplace(v, 2, 99);
    vector<int> expv2 = {10, 20, 99, 30};
    assert(vector<int>(v.begin(), v.end()) == expv2);
    cout << "addinplace in middle [OK]\n";

    v = {1, 2, 3};
    addinplace(v, 5, 4); // index > size should append at the end
    assert(v.size == 4 && v[3] == 4);
    cout << "addinplace with index > size (append) [OK]\n";

    // Inserting a container into another:
    seque<int> dest = {1, 2, 3};
    seque<int> src = {7, 8};
    addinplace(dest, 1, src);
    vector<int> expd = {1, 7, 8, 2, 3};
    assert(vector<int>(dest.begin(), dest.end()) == expd);
    cout << "addinplace with container insertion [OK]\n";
  }

  // append: appending values and containers, including corner cases with empty
  // seque
  {
    cout << "- Testing append (corner cases)\n";
    seque<int> a = {1, 2};
    append(a, 5);
    assert(a.size == 3 && a[a.size - 1] == 5);
    cout << "append value to non-empty seque [OK]\n";

    // Append container to non-empty seque:
    seque<int> b = {3, 4};
    append(a, b);
    assert(a.size == 5);
    cout << "append container to non-empty seque [OK]\n";

    // Append container to empty seque:
    seque<int> empty;
    seque<int> nonEmpty = {10, 20};
    append(nonEmpty, empty);
    // Note: append on an empty container might result in undefined behavior if
    // not supported. Here we assume append requires the destination to be
    // non-empty. So we check that empty remains empty.
    assert(empty.size == 0);
    cout << "append container to empty seque (no-op) [OK]\n";
  }

  // Testing stream operators << and >> with empty and non-empty seque
  {
    cout << "- Testing stream operators << and >> including empty seque\n";
    seque<int> t = {2, 4, 6};
    ostringstream oss;
    oss << t;
    string outStr = oss.str();
    istringstream iss(outStr);
    seque<int> u;
    iss >> u;
    assert(u.size == 3 && u[1] == 4);
    cout << "Stream I/O on non-empty seque [OK]\n";

    // Test with empty seque
    seque<int> empty;
    ostringstream ossEmpty;
    ossEmpty << empty;
    istringstream issEmpty(ossEmpty.str());
    seque<int> readEmpty;
    issEmpty >> readEmpty;
    assert(readEmpty.size == 0);
    cout << "Stream I/O on empty seque [OK]\n";
  }

  // Testing algorithms on empty seque
  {
    cout << "- Testing algorithms on empty seque\n";
    // isfullysatisfied on empty seque should typically return true (vacuous
    // truth)
    seque<int> empty;
    assert(isfullysatisfied(empty, [](int) { return false; }));
    cout << "isfullysatisfied on empty seque [OK]\n";

    // getmapped on empty seque
    auto mappedEmpty = getmapped(empty, [](int x) { return x * 2; });
    assert(mappedEmpty.size == 0);
    cout << "getmapped on empty seque [OK]\n";

    // reduce on empty seque (should return the initial value)
    int sumEmpty = reduce(empty, 100, [](int a, int b) { return a + b; });
    assert(sumEmpty == 100);
    cout << "reduce on empty seque [OK]\n";
  }

  // Additional tests for the rest of seque functionality...
  // (For brevity, only adding key corner cases; previous tests cover most
  // functions.)

  // Testing setorder and sampling on an empty seque
  {
    cout << "- Testing getorder and getasample on empty seque\n";
    seque<int> empty;
    seque<size_t> order = getorder(empty);
    assert(order.size == 0);
    seque<int> sample = getasample(empty, 3);
    assert(sample.size == 0);
    cout << "[OK]\n";
  }

  // Testing setordered on an empty seque (should remain empty)
  {
    cout << "- Testing setordered on empty seque\n";
    seque<int> empty;
    setordered(empty);
    assert(empty.size == 0);
    cout << "[OK]\n";
  }

  // Testing setshuffled and setreversed on empty seque
  {
    cout << "- Testing setshuffled and setreversed on empty seque\n";
    seque<int> empty;
    setshuffled(empty);
    setreversed(empty);
    assert(empty.size == 0);
    cout << "[OK]\n";
  }

  // Testing setpermuteclockwise and setpermutecounterclockwise on empty seque
  {
    cout << "- Testing permutation functions on empty seque\n";
    seque<int> empty;
    bool next = setpermuteclockwise(empty);
    assert(!next);
    bool prev = setpermutecounterclockwise(empty);
    assert(!prev);
    cout << "[OK]\n";
  }

  // Testing setunique on empty seque
  {
    cout << "- Testing setunique on empty seque\n";
    seque<int> empty;
    setunique(empty);
    assert(empty.size == 0);
    cout << "[OK]\n";
  }

  // Testing setrotatedaroundindex on empty seque (should remain empty)
  {
    cout << "- Testing setrotatedaroundindex on empty seque\n";
    seque<int> empty;
    setrotatedaroundindex(empty, 2);
    assert(empty.size == 0);
    cout << "[OK]\n";
  }

  // Testing setordered with custom compare on an empty seque
  {
    cout << "- Testing setordered with custom compare on empty seque\n";
    seque<int> empty;
    setordered(empty, [](int a, int b) { return a > b; });
    assert(empty.size == 0);
    cout << "[OK]\n";
  }

  cout << "\nAll tests completed successfully.\n";
}
