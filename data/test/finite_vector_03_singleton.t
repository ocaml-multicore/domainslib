  $ ./finite_vector_tester.exe singleton ./buf 1
  $ ./finite_vector_tester.exe print ./buf
  [| 1; _; _; _; _; _; _; _ |]
  $ ./finite_vector_tester.exe singleton ./buf 1 3
  $ ./finite_vector_tester.exe print ./buf
  [| 1; _; _ |]
  $ ./finite_vector_tester.exe singleton ./buf 1 4
  $ ./finite_vector_tester.exe print ./buf
  [| 1; _; _; _ |]
  $ ./finite_vector_tester.exe singleton ./buf 1 1
  $ ./finite_vector_tester.exe print ./buf
  [| 1 |]
