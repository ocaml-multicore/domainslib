  $ ./btree_tester.exe init btree.bc 2
  $ ./btree_tester.exe add-and-print btree.bc 1 "key 1"
  node(n=1,leaf=true,no_elts=1)
   - values=[1: "key 1"]
  
  $ ./btree_tester.exe add-and-print btree.bc 2 "key 2"
  node(n=2,leaf=true,no_elts=2)
   - values=[1: "key 1"; 2: "key 2"]
  
  $ ./btree_tester.exe add-and-print btree.bc 3 "key 3"
  node(n=3,leaf=true,no_elts=3)
   - values=[1: "key 1"; 2: "key 2"; 3: "key 3"]
  
  $ ./btree_tester.exe add-and-print btree.bc 4 "key 4"
  node(n=1,leaf=false,no_elts=4)
   - values=[2: "key 2"]
   - child(k=2):
      node(n=1,leaf=true,no_elts=1)
       - values=[1: "key 1"]
  
   - child(k=_):
      node(n=2,leaf=true,no_elts=2)
       - values=[3: "key 3"; 4: "key 4"]
  
  $ ./btree_tester.exe add-and-print btree.bc 5 "key 5"
  node(n=1,leaf=false,no_elts=5)
   - values=[2: "key 2"]
   - child(k=2):
      node(n=1,leaf=true,no_elts=1)
       - values=[1: "key 1"]
  
   - child(k=_):
      node(n=3,leaf=true,no_elts=3)
       - values=[3: "key 3"; 4: "key 4"; 5: "key 5"]
  
  $ ./btree_tester.exe add-and-print btree.bc 6 "key 6"
  node(n=2,leaf=false,no_elts=6)
   - values=[2: "key 2"; 4: "key 4"]
   - child(k=2):
      node(n=1,leaf=true,no_elts=1)
       - values=[1: "key 1"]
  
   - child(k=4):
      node(n=1,leaf=true,no_elts=1)
       - values=[3: "key 3"]
  
   - child(k=_):
      node(n=2,leaf=true,no_elts=2)
       - values=[5: "key 5"; 6: "key 6"]
  
  $ ./btree_tester.exe add-and-print btree.bc 7 "key 7"
  node(n=2,leaf=false,no_elts=7)
   - values=[2: "key 2"; 4: "key 4"]
   - child(k=2):
      node(n=1,leaf=true,no_elts=1)
       - values=[1: "key 1"]
  
   - child(k=4):
      node(n=1,leaf=true,no_elts=1)
       - values=[3: "key 3"]
  
   - child(k=_):
      node(n=3,leaf=true,no_elts=3)
       - values=[5: "key 5"; 6: "key 6"; 7: "key 7"]
  

  $ ./btree_tester.exe build btree.bc 4 1,2,3,4
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
  

  $ ./btree_tester.exe build btree.bc 4 1,2,3,4,5,6,7,8,9,10
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
  

  $ ./btree_tester.exe build btree.bc 4 1,2,3,4,5,6,7,8,9,10,11,12,13
  node(n=2,leaf=false,no_elts=13)
   - values=[5: "key 5"; 10: "key 10"]
   - child(k=5):
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
      node(n=2,leaf=false,no_elts=3)
       - values=[11: "key 11"; 12: "key 12"]
       - child(k=11):
          node(n=0,leaf=true,no_elts=0)
           - values=[]
  
       - child(k=12):
          node(n=0,leaf=true,no_elts=0)
           - values=[]
  
       - child(k=_):
          node(n=1,leaf=true,no_elts=1)
           - values=[13: "key 13"]
  

  $ ./btree_tester.exe build btree.bc 4 1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16
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
  

$ ./btree_tester.exe par-insert btree.bc 3
$ ./btree_tester.exe par-insert btree.bc 2
$ ./btree_tester.exe par-insert btree.bc 1
$ ./btree_tester.exe par-insert btree.bc 4
$ ./btree_tester.exe par-insert btree.bc 5
$ ./btree_tester.exe print btree.bc


$ ./btree_tester.exe par-insert btree.bc 1,2,3,4,5,6,7,8,9,10 
$ ./btree_tester.exe print btree.bc

