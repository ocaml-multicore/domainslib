  $ ./btree_tester.exe init btree.bc 3

  $ ./btree_tester.exe add btree.bc 0 "key 0"
  $ ./btree_tester.exe add btree.bc 10 "key 10"

  $ ./btree_tester.exe print btree.bc
  node(n=2,leaf=true,no_elts=2)
   - values=[0: "key 0"; 10: "key 10"]
  

  $ ./btree_tester.exe add btree.bc 1 "key 1"
  $ ./btree_tester.exe add btree.bc 9 "key 9"

  $ ./btree_tester.exe print btree.bc
  node(n=4,leaf=true,no_elts=4)
   - values=[0: "key 0"; 1: "key 1"; 9: "key 9"; 10: "key 10"]
  

  $ ./btree_tester.exe add btree.bc 2 "key 2"
  $ ./btree_tester.exe add btree.bc 8 "key 8"

  $ ./btree_tester.exe print btree.bc
  node(n=1,leaf=false,no_elts=6)
   - values=[2: "key 2"]
   - child(k=2):
      node(n=2,leaf=true,no_elts=2)
       - values=[0: "key 0"; 1: "key 1"]
  
   - child(k=_):
      node(n=3,leaf=true,no_elts=3)
       - values=[8: "key 8"; 9: "key 9"; 10: "key 10"]
  

  $ ./btree_tester.exe add btree.bc 3 "key 3"
  $ ./btree_tester.exe add btree.bc 7 "key 7"

  $ ./btree_tester.exe print btree.bc
  node(n=1,leaf=false,no_elts=8)
   - values=[2: "key 2"]
   - child(k=2):
      node(n=2,leaf=true,no_elts=2)
       - values=[0: "key 0"; 1: "key 1"]
  
   - child(k=_):
      node(n=5,leaf=true,no_elts=5)
       - values=[3: "key 3"; 7: "key 7"; 8: "key 8"; 9: "key 9"; 10: "key 10"]
  

  $ ./btree_tester.exe add btree.bc 4 "key 4"
  $ ./btree_tester.exe add btree.bc 6 "key 6"

  $ ./btree_tester.exe print btree.bc
  node(n=2,leaf=false,no_elts=10)
   - values=[2: "key 2"; 8: "key 8"]
   - child(k=2):
      node(n=2,leaf=true,no_elts=2)
       - values=[0: "key 0"; 1: "key 1"]
  
   - child(k=8):
      node(n=4,leaf=true,no_elts=4)
       - values=[3: "key 3"; 4: "key 4"; 6: "key 6"; 7: "key 7"]
  
   - child(k=_):
      node(n=2,leaf=true,no_elts=2)
       - values=[9: "key 9"; 10: "key 10"]
  

  $ ./btree_tester.exe add btree.bc 5 "key 5"

  $ ./btree_tester.exe print btree.bc
  node(n=2,leaf=false,no_elts=11)
   - values=[2: "key 2"; 8: "key 8"]
   - child(k=2):
      node(n=2,leaf=true,no_elts=2)
       - values=[0: "key 0"; 1: "key 1"]
  
   - child(k=8):
      node(n=5,leaf=true,no_elts=5)
       - values=[3: "key 3"; 4: "key 4"; 5: "key 5"; 6: "key 6"; 7: "key 7"]
  
   - child(k=_):
      node(n=2,leaf=true,no_elts=2)
       - values=[9: "key 9"; 10: "key 10"]
  


