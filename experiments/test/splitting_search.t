  $ ./btree_tester.exe init btree.bc 4
  $ ./btree_tester.exe add btree.bc 5 "key 5"
  $ ./btree_tester.exe add btree.bc 4 "key 4"
  $ ./btree_tester.exe add btree.bc 6 "key 6"
  $ ./btree_tester.exe add btree.bc 3 "key 3"
  $ ./btree_tester.exe add btree.bc 7 "key 7"
  $ ./btree_tester.exe add btree.bc 2 "key 2"
  $ ./btree_tester.exe add btree.bc 8 "key 8"
  $ ./btree_tester.exe add btree.bc 1 "key 1"
  $ ./btree_tester.exe add btree.bc 9 "key 9"
  $ ./btree_tester.exe add btree.bc 0 "key 0"
  $ ./btree_tester.exe add btree.bc 10 "key 10"


  $ ./btree_tester.exe search btree.bc 0
  "key 0"
  $ ./btree_tester.exe search btree.bc 1
  "key 1"
  $ ./btree_tester.exe search btree.bc 2
  "key 2"
  $ ./btree_tester.exe search btree.bc 3
  "key 3"
  $ ./btree_tester.exe search btree.bc 4
  "key 4"
  $ ./btree_tester.exe search btree.bc 5
  "key 5"
  $ ./btree_tester.exe search btree.bc 6
  "key 6"
  $ ./btree_tester.exe search btree.bc 7
  "key 7"
  $ ./btree_tester.exe search btree.bc 8
  "key 8"
  $ ./btree_tester.exe search btree.bc 9
  "key 9"
  $ ./btree_tester.exe search btree.bc 10
  "key 10"
  $ ./btree_tester.exe search btree.bc 11
  None
  $ ./btree_tester.exe search btree.bc 12
  None
