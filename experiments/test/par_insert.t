  $ ./btree_tester.exe init btree.bc 2
  $ ./btree_tester.exe par-insert btree.bc 1,2,3,4,5,6,7,8,9,10
  Fatal error: exception Failure("int_of_string")
  [2]

  $ ./btree_tester.exe print btree.bc
  node(n=0,leaf=true,no_elts=0)
   - values=[]
  

