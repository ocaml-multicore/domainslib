  $ ./btree_tester.exe init btree.bc 2

  $ ./btree_tester.exe add btree.bc 0 "key 0"
  $ ./btree_tester.exe add btree.bc 10 "key 10"

  $ ./btree_tester.exe print btree.bc
  node(n=2,leaf=true)
   - values=[0: "key 0"; 10: "key 10"]
  

  $ ./btree_tester.exe add btree.bc 1 "key 1"
  $ ./btree_tester.exe add btree.bc 9 "key 9"

  $ ./btree_tester.exe print btree.bc
  node(n=1,leaf=false)
   - values=[1: "key 1"]
   - child(k=1):
      node(n=1,leaf=true)
       - values=[0: "key 0"]
  
   - child(k=_):
      node(n=2,leaf=true)
       - values=[9: "key 9"; 10: "key 10"]
  

  $ ./btree_tester.exe add btree.bc 2 "key 2"
  $ ./btree_tester.exe add btree.bc 8 "key 8"

  $ ./btree_tester.exe print btree.bc
  node(n=2,leaf=false)
   - values=[1: "key 1"; 9: "key 9"]
   - child(k=1):
      node(n=1,leaf=true)
       - values=[0: "key 0"]
  
   - child(k=9):
      node(n=2,leaf=true)
       - values=[2: "key 2"; 8: "key 8"]
  
   - child(k=_):
      node(n=1,leaf=true)
       - values=[10: "key 10"]
  

  $ ./btree_tester.exe add btree.bc 3 "key 3"
  $ ./btree_tester.exe add btree.bc 7 "key 7"

  $ ./btree_tester.exe print btree.bc
  node(n=3,leaf=false)
   - values=[1: "key 1"; 3: "key 3"; 9: "key 9"]
   - child(k=1):
      node(n=1,leaf=true)
       - values=[0: "key 0"]
  
   - child(k=3):
      node(n=1,leaf=true)
       - values=[2: "key 2"]
  
   - child(k=9):
      node(n=2,leaf=true)
       - values=[7: "key 7"; 8: "key 8"]
  
   - child(k=_):
      node(n=1,leaf=true)
       - values=[10: "key 10"]
  

  $ ./btree_tester.exe add btree.bc 4 "key 4"
  $ ./btree_tester.exe add btree.bc 6 "key 6"

  $ ./btree_tester.exe print btree.bc
  node(n=1,leaf=false)
   - values=[3: "key 3"]
   - child(k=3):
      node(n=1,leaf=false)
       - values=[1: "key 1"]
       - child(k=1):
          node(n=1,leaf=true)
           - values=[0: "key 0"]
  
       - child(k=_):
          node(n=1,leaf=true)
           - values=[2: "key 2"]
  
   - child(k=_):
      node(n=2,leaf=false)
       - values=[7: "key 7"; 9: "key 9"]
       - child(k=7):
          node(n=2,leaf=true)
           - values=[4: "key 4"; 6: "key 6"]
  
       - child(k=9):
          node(n=1,leaf=true)
           - values=[8: "key 8"]
  
       - child(k=_):
          node(n=1,leaf=true)
           - values=[10: "key 10"]
  

  $ ./btree_tester.exe add btree.bc 5 "key 5"

  $ ./btree_tester.exe print btree.bc
  node(n=1,leaf=false)
   - values=[3: "key 3"]
   - child(k=3):
      node(n=1,leaf=false)
       - values=[1: "key 1"]
       - child(k=1):
          node(n=1,leaf=true)
           - values=[0: "key 0"]
  
       - child(k=_):
          node(n=1,leaf=true)
           - values=[2: "key 2"]
  
   - child(k=_):
      node(n=2,leaf=false)
       - values=[7: "key 7"; 9: "key 9"]
       - child(k=7):
          node(n=3,leaf=true)
           - values=[4: "key 4"; 5: "key 5"; 6: "key 6"]
  
       - child(k=9):
          node(n=1,leaf=true)
           - values=[8: "key 8"]
  
       - child(k=_):
          node(n=1,leaf=true)
           - values=[10: "key 10"]
  


