  $ ./btree_tester.exe build btree.bc 2 1,2,3,4
  node(n=1,leaf=false,no_elts=4)
   - values=[2: "key 2"]
   - child(k=2):
      node(n=1,leaf=true,no_elts=1)
       - values=[1: "key 1"]
  
   - child(k=_):
      node(n=1,leaf=true,no_elts=1)
       - values=[3: "key 3"]
  

  $ ./btree_tester.exe build btree.bc 2 1,2,3,4,5,6,7,8,9,10
  node(n=2,leaf=false,no_elts=10)
   - values=[3: "key 3"; 6: "key 6"]
   - child(k=3):
      node(n=2,leaf=true,no_elts=2)
       - values=[1: "key 1"; 2: "key 2"]
  
   - child(k=6):
      node(n=2,leaf=true,no_elts=2)
       - values=[4: "key 4"; 5: "key 5"]
  
   - child(k=_):
      node(n=2,leaf=true,no_elts=2)
       - values=[7: "key 7"; 8: "key 8"]
  

  $ ./btree_tester.exe build btree.bc 2 1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16
  node(n=1,leaf=false,no_elts=16)
   - values=[9: "key 9"]
   - child(k=9):
      node(n=2,leaf=false,no_elts=8)
       - values=[3: "key 3"; 6: "key 6"]
       - child(k=3):
          node(n=2,leaf=true,no_elts=2)
           - values=[1: "key 1"; 2: "key 2"]
  
       - child(k=6):
          node(n=2,leaf=true,no_elts=2)
           - values=[4: "key 4"; 5: "key 5"]
  
       - child(k=_):
          node(n=2,leaf=true,no_elts=2)
           - values=[7: "key 7"; 8: "key 8"]
  
   - child(k=_):
      node(n=1,leaf=false,no_elts=7)
       - values=[13: "key 13"]
       - child(k=13):
          node(n=3,leaf=true,no_elts=3)
           - values=[10: "key 10"; 11: "key 11"; 12: "key 12"]
  
       - child(k=_):
          node(n=3,leaf=true,no_elts=3)
           - values=[14: "key 14"; 15: "key 15"; 16: "key 16"]
  
