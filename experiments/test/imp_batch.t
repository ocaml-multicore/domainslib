  $ ./btree_tester.exe init_impbatch btree.bc
  node(n=0,leaf=true,no_elts=0)
   - values=[]
  
  $ ./btree_tester.exe imp_insert btree.bc 20
  node(n=3,leaf=false,no_elts=20)
   - values=[7: "key 7"; 10: "key 10"; 16: "key 16"]
   - child(k=7):
      node(n=4,leaf=true,no_elts=4)
       - values=[0: "key 0"; 2: "key 2"; 3: "key 3"; 6: "key 6"]
  
   - child(k=10):
      node(n=4,leaf=true,no_elts=4)
       - values=[7: "key 7"; 7: "key 7"; 7: "key 7"; 8: "key 8"]
  
   - child(k=16):
      node(n=5,leaf=true,no_elts=5)
       - values=[11: "key 11"; 12: "key 12"; 12: "key 12"; 14: "key 14"; 15: "key 15"]
  
   - child(k=_):
      node(n=4,leaf=true,no_elts=4)
       - values=[16: "key 16"; 17: "key 17"; 17: "key 17"; 17: "key 17"]
  

