  $ ./btree_tester.exe init btree.bc
  $ ./btree_tester.exe add btree.bc 5 "key 5"
  $ ./btree_tester.exe add btree.bc 4 "key 4"
  $ ./btree_tester.exe add btree.bc 6 "key 6"

  $ ./btree_tester.exe par-search btree.bc 3,4,5,6,7
  3 ==> None
  4 ==> key 4
  5 ==> key 5
  6 ==> key 6
  7 ==> None
