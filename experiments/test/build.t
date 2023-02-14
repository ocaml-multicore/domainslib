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
  node(n=4,leaf=false,no_elts=10)
   - values=[2: "key 2"; 4: "key 4"; 6: "key 6"; 8: "key 8"]
   - child(k=2):
      node(n=1,leaf=true,no_elts=1)
       - values=[1: "key 1"]
  
   - child(k=4):
      node(n=1,leaf=true,no_elts=1)
       - values=[3: "key 3"]
  
   - child(k=6):
      node(n=1,leaf=true,no_elts=1)
       - values=[5: "key 5"]
  
   - child(k=8):
      node(n=1,leaf=true,no_elts=1)
       - values=[7: "key 7"]
  
   - child(k=_):
      node(n=1,leaf=true,no_elts=1)
       - values=[9: "key 9"]
  

  $ ./btree_tester.exe build btree.bc 2 1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16
  node(n=3,leaf=false,no_elts=16)
   - values=[4: "key 4"; 8: "key 8"; 12: "key 12"]
   - child(k=4):
      node(n=1,leaf=false,no_elts=3)
       - values=[2: "key 2"]
       - child(k=2):
          node(n=1,leaf=true,no_elts=1)
           - values=[1: "key 1"]
  
       - child(k=_):
          node(n=1,leaf=true,no_elts=1)
           - values=[3: "key 3"]
  
   - child(k=8):
      node(n=1,leaf=false,no_elts=3)
       - values=[6: "key 6"]
       - child(k=6):
          node(n=1,leaf=true,no_elts=1)
           - values=[5: "key 5"]
  
       - child(k=_):
          node(n=1,leaf=true,no_elts=1)
           - values=[7: "key 7"]
  
   - child(k=12):
      node(n=1,leaf=false,no_elts=3)
       - values=[10: "key 10"]
       - child(k=10):
          node(n=1,leaf=true,no_elts=1)
           - values=[9: "key 9"]
  
       - child(k=_):
          node(n=1,leaf=true,no_elts=1)
           - values=[11: "key 11"]
  
   - child(k=_):
      node(n=1,leaf=false,no_elts=3)
       - values=[14: "key 14"]
       - child(k=14):
          node(n=1,leaf=true,no_elts=1)
           - values=[13: "key 13"]
  
       - child(k=_):
          node(n=1,leaf=true,no_elts=1)
           - values=[15: "key 15"]
  
