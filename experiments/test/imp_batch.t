  $ ./btree_tester.exe init_impbatch btree.bc
  node(n=0,leaf=true,no_elts=0)
   - values=[]
  
  $ ./btree_tester.exe imp_insert btree.bc 10
  node(n=2,leaf=false,no_elts=10)
   - values=[10: "key 10"; 16: "key 16"]
   - child(k=10):
      node(n=3,leaf=true,no_elts=3)
       - values=[0: "key 0"; 6: "key 6"; 7: "key 7"]
  
   - child(k=16):
      node(n=2,leaf=true,no_elts=2)
       - values=[12: "key 12"; 15: "key 15"]
  
   - child(k=_):
      node(n=3,leaf=true,no_elts=3)
       - values=[17: "key 17"; 17: "key 17"; 17: "key 17"]
  

