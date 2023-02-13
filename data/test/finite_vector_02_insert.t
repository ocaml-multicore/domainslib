  $ ./finite_vector_tester.exe init ./buf
  $ ./finite_vector_tester.exe print ./buf
  [| _; _; _; _; _; _; _; _ |]
  $ ./finite_vector_tester.exe insert ./buf 0 1
  $ ./finite_vector_tester.exe insert ./buf 1 2
  $ ./finite_vector_tester.exe insert ./buf 2 3
  $ ./finite_vector_tester.exe insert ./buf 3 4
  $ ./finite_vector_tester.exe insert ./buf 4 5
  $ ./finite_vector_tester.exe print ./buf
  [| 1; 2; 3; 4; 5; _; _; _ |]
  $ ./finite_vector_tester.exe insert ./buf 0 0
  $ ./finite_vector_tester.exe print ./buf
  [| 0; 1; 2; 3; 4; 5; _; _ |]

Now let's try with a lower capacity (6):

  $ ./finite_vector_tester.exe init ./buf 6
  $ ./finite_vector_tester.exe print ./buf
  [| _; _; _; _; _; _ |]
  $ ./finite_vector_tester.exe insert ./buf 0 1
  $ ./finite_vector_tester.exe insert ./buf 1 2
  $ ./finite_vector_tester.exe insert ./buf 2 3
  $ ./finite_vector_tester.exe insert ./buf 3 4
  $ ./finite_vector_tester.exe insert ./buf 4 5
  $ ./finite_vector_tester.exe print ./buf
  [| 1; 2; 3; 4; 5; _ |]
  $ ./finite_vector_tester.exe insert ./buf 0 0
  $ ./finite_vector_tester.exe print ./buf
  [| 0; 1; 2; 3; 4; 5 |]

Any further inserts will raise an error:

  $ ./finite_vector_tester.exe insert ./buf 0 1
  Fatal error: exception Failure("out of capacity")
  [2]

