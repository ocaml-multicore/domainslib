  $ ./btree_tester.exe init btree.bc
  $ ./btree_tester.exe print btree.bc
  node(n=0,leaf=true,no_elts=0)
   - values=[]
  
  $ ./btree_tester.exe add btree.bc 5 "key 5"
  $ ./btree_tester.exe add btree.bc 4 "key 4"
  $ ./btree_tester.exe add btree.bc 6 "key 6"

  $ ./btree_tester.exe print btree.bc
  node(n=3,leaf=true,no_elts=3)
   - values=[4: "key 4"; 5: "key 5"; 6: "key 6"]
  
