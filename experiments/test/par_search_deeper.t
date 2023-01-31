  $ ./btree_tester.exe init btree.bc
  $ ./btree_tester.exe add btree.bc 14 "key 14"
  $ ./btree_tester.exe add btree.bc 1 "key 1"
  $ ./btree_tester.exe add btree.bc 4 "key 4"
  $ ./btree_tester.exe add btree.bc 2 "key 2"
  $ ./btree_tester.exe add btree.bc 13 "key 13"
  $ ./btree_tester.exe add btree.bc 8 "key 8"
  $ ./btree_tester.exe add btree.bc 12 "key 12"
  $ ./btree_tester.exe add btree.bc 9 "key 9"
  $ ./btree_tester.exe add btree.bc 11 "key 11"
  $ ./btree_tester.exe add btree.bc 5 "key 5"
  $ ./btree_tester.exe add btree.bc 10 "key 10"
  $ ./btree_tester.exe add btree.bc 6 "key 6"
  $ ./btree_tester.exe add btree.bc 3 "key 3"
  $ ./btree_tester.exe add btree.bc 7 "key 7"

  $ ./btree_tester.exe print btree.bc 
  node(n=3,leaf=false,no_elts=14)
   - values=[4: "key 4"; 9: "key 9"; 12: "key 12"]
   - child(k=4):
      node(n=3,leaf=true,no_elts=3)
       - values=[1: "key 1"; 2: "key 2"; 3: "key 3"]
  
   - child(k=9):
      node(n=4,leaf=true,no_elts=4)
       - values=[5: "key 5"; 6: "key 6"; 7: "key 7"; 8: "key 8"]
  
   - child(k=12):
      node(n=2,leaf=true,no_elts=2)
       - values=[10: "key 10"; 11: "key 11"]
  
   - child(k=_):
      node(n=2,leaf=true,no_elts=2)
       - values=[13: "key 13"; 14: "key 14"]
  

  $ ./btree_tester.exe par-search btree.bc 1,2,3,4,5,6,7,8,9,10,11,12,13
  1 ==> key 1
  2 ==> key 2
  3 ==> key 3
  4 ==> key 4
  5 ==> key 5
  6 ==> key 6
  7 ==> key 7
  8 ==> key 8
  9 ==> key 9
  10 ==> key 10
  11 ==> key 11
  12 ==> key 12
  13 ==> key 13

  $ ./btree_tester.exe par-search btree.bc 11,2,7,5,10,1,4,8,13,12,9,6,3
  11 ==> key 11
  2 ==> key 2
  7 ==> key 7
  5 ==> key 5
  10 ==> key 10
  1 ==> key 1
  4 ==> key 4
  8 ==> key 8
  13 ==> key 13
  12 ==> key 12
  9 ==> key 9
  6 ==> key 6
  3 ==> key 3


