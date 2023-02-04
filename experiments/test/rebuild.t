  $ ./btree_tester.exe init btree.bc
  $ ./btree_tester.exe add btree.bc 1 "key 1"
  $ ./btree_tester.exe add btree.bc 14 "key 14"
  $ ./btree_tester.exe print btree.bc
  node(n=2,leaf=true,no_elts=2)
   - values=[1: "key 1"; 14: "key 14"]
  

  $ ./btree_tester.exe rebuild btree.bc 2,3,4,5,6,7,8,9,10,11,12,13
  node(n=2,leaf=false,no_elts=14)
   - values=[5: "key 5"; 10: "key 10"]
   - child(k=5):
      node(n=2,leaf=false,no_elts=4)
       - values=[1: "key 1"; 4: "key 4"]
       - child(k=1):
          node(n=1,leaf=true,no_elts=1)
           - values=[1: "key 1"]
  
       - child(k=4):
          node(n=1,leaf=true,no_elts=1)
           - values=[3: "key 3"]
  
       - child(k=_):
          node(n=0,leaf=true,no_elts=0)
           - values=[]
  
   - child(k=10):
      node(n=2,leaf=false,no_elts=4)
       - values=[7: "key 7"; 9: "key 9"]
       - child(k=7):
          node(n=1,leaf=true,no_elts=1)
           - values=[6: "key 6"]
  
       - child(k=9):
          node(n=1,leaf=true,no_elts=1)
           - values=[8: "key 8"]
  
       - child(k=_):
          node(n=0,leaf=true,no_elts=0)
           - values=[]
  
   - child(k=_):
      node(n=2,leaf=false,no_elts=4)
       - values=[12: "key 12"; 14: "key 14"]
       - child(k=12):
          node(n=1,leaf=true,no_elts=1)
           - values=[11: "key 11"]
  
       - child(k=14):
          node(n=1,leaf=true,no_elts=1)
           - values=[13: "key 13"]
  
       - child(k=_):
          node(n=0,leaf=true,no_elts=0)
           - values=[]
  
