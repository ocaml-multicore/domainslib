  $ ./btree_tester.exe build btree.bc 3 1,2,3,4
  node(n=2,leaf=false,no_elts=4)
   - values=[2: "key 2"; 4: "key 4"]
   - child(k=2):
      node(n=1,leaf=true,no_elts=1)
       - values=[1: "key 1"]
  
   - child(k=4):
      node(n=1,leaf=true,no_elts=1)
       - values=[3: "key 3"]
  
   - child(k=_):
      node(n=0,leaf=true,no_elts=0)
       - values=[]
  

  $ ./btree_tester.exe build btree.bc 3 1,2,3,4,5,6,7,8,9,10
  node(n=2,leaf=false,no_elts=10)
   - values=[4: "key 4"; 8: "key 8"]
   - child(k=4):
      node(n=3,leaf=true,no_elts=3)
       - values=[1: "key 1"; 2: "key 2"; 3: "key 3"]
  
   - child(k=8):
      node(n=3,leaf=true,no_elts=3)
       - values=[5: "key 5"; 6: "key 6"; 7: "key 7"]
  
   - child(k=_):
      node(n=2,leaf=true,no_elts=2)
       - values=[9: "key 9"; 10: "key 10"]
  

  $ ./btree_tester.exe build btree.bc 3 1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16
  node(n=2,leaf=false,no_elts=16)
   - values=[6: "key 6"; 12: "key 12"]
   - child(k=6):
      node(n=2,leaf=false,no_elts=5)
       - values=[2: "key 2"; 4: "key 4"]
       - child(k=2):
          node(n=1,leaf=true,no_elts=1)
           - values=[1: "key 1"]
  
       - child(k=4):
          node(n=1,leaf=true,no_elts=1)
           - values=[3: "key 3"]
  
       - child(k=_):
          node(n=1,leaf=true,no_elts=1)
           - values=[5: "key 5"]
  
   - child(k=12):
      node(n=2,leaf=false,no_elts=5)
       - values=[8: "key 8"; 10: "key 10"]
       - child(k=8):
          node(n=1,leaf=true,no_elts=1)
           - values=[7: "key 7"]
  
       - child(k=10):
          node(n=1,leaf=true,no_elts=1)
           - values=[9: "key 9"]
  
       - child(k=_):
          node(n=1,leaf=true,no_elts=1)
           - values=[11: "key 11"]
  
   - child(k=_):
      node(n=2,leaf=false,no_elts=4)
       - values=[14: "key 14"; 16: "key 16"]
       - child(k=14):
          node(n=1,leaf=true,no_elts=1)
           - values=[13: "key 13"]
  
       - child(k=16):
          node(n=1,leaf=true,no_elts=1)
           - values=[15: "key 15"]
  
       - child(k=_):
          node(n=0,leaf=true,no_elts=0)
           - values=[]
  
