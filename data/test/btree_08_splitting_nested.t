  $ ./btree_tester.exe init btree.bc 4

  $ ./btree_tester.exe add btree.bc 10 "key 10"

  $ ./btree_tester.exe add btree.bc 9 "key 9"
  $ ./btree_tester.exe add btree.bc 11 "key 11"

  $ ./btree_tester.exe add btree.bc 8 "key 8"
  $ ./btree_tester.exe add btree.bc 12 "key 12"

  $ ./btree_tester.exe add btree.bc 7 "key 7"
  $ ./btree_tester.exe add btree.bc 13 "key 13"

  $ ./btree_tester.exe add btree.bc 6 "key 6"
  $ ./btree_tester.exe add btree.bc 14 "key 14"

  $ ./btree_tester.exe add btree.bc 5 "key 5"
  $ ./btree_tester.exe add btree.bc 15 "key 15"

  $ ./btree_tester.exe add btree.bc 4 "key 4"
  $ ./btree_tester.exe add btree.bc 16 "key 16"

  $ ./btree_tester.exe add btree.bc 3 "key 3"
  $ ./btree_tester.exe add btree.bc 17 "key 17"

  $ ./btree_tester.exe add btree.bc 2 "key 2"
  $ ./btree_tester.exe add btree.bc 18 "key 18"

  $ ./btree_tester.exe add btree.bc 1 "key 1"
  $ ./btree_tester.exe add btree.bc 19 "key 19"

  $ ./btree_tester.exe add btree.bc 0 "key 0"
  $ ./btree_tester.exe add btree.bc 20 "key 20"


  $ ./btree_tester.exe print btree.bc
  node(n=3,leaf=false,no_elts=21)
   - values=[6: "key 6"; 10: "key 10"; 14: "key 14"]
   - child(k=6):
      node(n=6,leaf=true,no_elts=6)
       - values=[0: "key 0"; 1: "key 1"; 2: "key 2"; 3: "key 3"; 4: "key 4"; 5: "key 5"]
  
   - child(k=10):
      node(n=3,leaf=true,no_elts=3)
       - values=[7: "key 7"; 8: "key 8"; 9: "key 9"]
  
   - child(k=14):
      node(n=3,leaf=true,no_elts=3)
       - values=[11: "key 11"; 12: "key 12"; 13: "key 13"]
  
   - child(k=_):
      node(n=6,leaf=true,no_elts=6)
       - values=[15: "key 15"; 16: "key 16"; 17: "key 17"; 18: "key 18"; 19: "key 19"; 20: "key 20"]
  
