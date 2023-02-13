  $ ./finite_vector_tester.exe init ./buf
  $ ./finite_vector_tester.exe insert ./buf 0 1
  $ ./finite_vector_tester.exe insert ./buf 0 2
  $ ./finite_vector_tester.exe insert ./buf 0 3
  $ ./finite_vector_tester.exe insert ./buf 0 4
  $ ./finite_vector_tester.exe insert ./buf 0 5
  $ ./finite_vector_tester.exe insert ./buf 0 6
  $ ./finite_vector_tester.exe insert ./buf 0 7
  $ ./finite_vector_tester.exe print ./buf
  [| 7; 6; 5; 4; 3; 2; 1; _ |]

  $ ./finite_vector_tester.exe drop_last ./buf
  $ ./finite_vector_tester.exe print ./buf
  [| 7; 6; 5; 4; 3; 2; _; _ |]
  $ ./finite_vector_tester.exe drop_last ./buf
  $ ./finite_vector_tester.exe drop_last ./buf
  $ ./finite_vector_tester.exe drop_last ./buf
  $ ./finite_vector_tester.exe drop_last ./buf
  $ ./finite_vector_tester.exe drop_last ./buf
  $ ./finite_vector_tester.exe print ./buf
  [| 7; _; _; _; _; _; _; _ |]

  $ ./finite_vector_tester.exe drop_last ./buf
  $ ./finite_vector_tester.exe print ./buf
  [| _; _; _; _; _; _; _; _ |]

Any further drop raises an error:

  $ ./finite_vector_tester.exe drop_last ./buf
  Fatal error: exception Invalid_argument("attempt to drop last on empty array")
  [2]

We can also insert into an empty buffer:

  $ ./finite_vector_tester.exe insert ./buf 0 0
  $ ./finite_vector_tester.exe print ./buf
  [| 0; _; _; _; _; _; _; _ |]

But only at the 0th index:

  $ ./finite_vector_tester.exe drop_last ./buf
  $ ./finite_vector_tester.exe insert ./buf 1 10
  Fatal error: exception Invalid_argument("invalid index for insert")
  [2]

