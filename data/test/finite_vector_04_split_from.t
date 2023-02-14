  $ ./finite_vector_tester.exe init ./buf
  $ ./finite_vector_tester.exe insert ./buf 0 1
  $ ./finite_vector_tester.exe insert ./buf 1 2
  $ ./finite_vector_tester.exe insert ./buf 2 3
  $ ./finite_vector_tester.exe insert ./buf 3 4
  $ ./finite_vector_tester.exe insert ./buf 4 5
  $ ./finite_vector_tester.exe insert ./buf 5 6
  $ ./finite_vector_tester.exe insert ./buf 6 7
  $ ./finite_vector_tester.exe print ./buf
  [| 1; 2; 3; 4; 5; 6; 7; _ |]

Now, let's save a copy of the vector:

  $ cp ./buf ./buf-bk

Splitting from index 0:

  $ ./finite_vector_tester.exe split_from ./buf ./buf-split 0

  $ ./finite_vector_tester.exe print ./buf
  [| _; _; _; _; _; _; _; _ |]

  $ ./finite_vector_tester.exe print ./buf-split
  [| 1; 2; 3; 4; 5; 6; 7; _ |]

  $ cp ./buf-bk ./buf

Splitting from index 1:

  $ ./finite_vector_tester.exe split_from ./buf ./buf-split 1

  $ ./finite_vector_tester.exe print ./buf
  [| 1; _; _; _; _; _; _; _ |]

  $ ./finite_vector_tester.exe print ./buf-split
  [| 2; 3; 4; 5; 6; 7; _; _ |]

  $ cp ./buf-bk ./buf

Splitting from index 3:

  $ ./finite_vector_tester.exe split_from ./buf ./buf-split 3

  $ ./finite_vector_tester.exe print ./buf
  [| 1; 2; 3; _; _; _; _; _ |]

  $ ./finite_vector_tester.exe print ./buf-split
  [| 4; 5; 6; 7; _; _; _; _ |]

  $ cp ./buf-bk ./buf

Splitting from index 6:

  $ ./finite_vector_tester.exe split_from ./buf ./buf-split 6

  $ ./finite_vector_tester.exe print ./buf
  [| 1; 2; 3; 4; 5; 6; _; _ |]

  $ ./finite_vector_tester.exe print ./buf-split
  [| 7; _; _; _; _; _; _; _ |]

  $ cp ./buf-bk ./buf

Splitting from index 7:

  $ ./finite_vector_tester.exe split_from ./buf ./buf-split 7

  $ ./finite_vector_tester.exe print ./buf
  [| 1; 2; 3; 4; 5; 6; 7; _ |]

  $ ./finite_vector_tester.exe print ./buf-split
  [| _; _; _; _; _; _; _; _ |]

  $ cp ./buf-bk ./buf

Let's try at max capacity:

  $ ./finite_vector_tester.exe init ./buf
  $ ./finite_vector_tester.exe insert ./buf 0 1
  $ ./finite_vector_tester.exe insert ./buf 1 2
  $ ./finite_vector_tester.exe insert ./buf 2 3
  $ ./finite_vector_tester.exe insert ./buf 3 4
  $ ./finite_vector_tester.exe insert ./buf 4 5
  $ ./finite_vector_tester.exe insert ./buf 5 6
  $ ./finite_vector_tester.exe insert ./buf 6 7
  $ ./finite_vector_tester.exe insert ./buf 7 8
  $ ./finite_vector_tester.exe print ./buf
  [| 1; 2; 3; 4; 5; 6; 7; 8 |]

Now, let's save a copy of the vector:

  $ cp ./buf ./buf-bk

Splitting from index 0:

  $ ./finite_vector_tester.exe split_from ./buf ./buf-split 0

  $ ./finite_vector_tester.exe print ./buf
  [| _; _; _; _; _; _; _; _ |]

  $ ./finite_vector_tester.exe print ./buf-split
  [| 1; 2; 3; 4; 5; 6; 7; 8 |]

  $ cp ./buf-bk ./buf

Splitting from index 1:

  $ ./finite_vector_tester.exe split_from ./buf ./buf-split 1

  $ ./finite_vector_tester.exe print ./buf
  [| 1; _; _; _; _; _; _; _ |]

  $ ./finite_vector_tester.exe print ./buf-split
  [| 2; 3; 4; 5; 6; 7; 8; _ |]

  $ cp ./buf-bk ./buf

Splitting from index 3:

  $ ./finite_vector_tester.exe split_from ./buf ./buf-split 3

  $ ./finite_vector_tester.exe print ./buf
  [| 1; 2; 3; _; _; _; _; _ |]

  $ ./finite_vector_tester.exe print ./buf-split
  [| 4; 5; 6; 7; 8; _; _; _ |]

  $ cp ./buf-bk ./buf

Splitting from index 6:

  $ ./finite_vector_tester.exe split_from ./buf ./buf-split 6

  $ ./finite_vector_tester.exe print ./buf
  [| 1; 2; 3; 4; 5; 6; _; _ |]

  $ ./finite_vector_tester.exe print ./buf-split
  [| 7; 8; _; _; _; _; _; _ |]

  $ cp ./buf-bk ./buf

Splitting from index 7:

  $ ./finite_vector_tester.exe split_from ./buf ./buf-split 7

  $ ./finite_vector_tester.exe print ./buf
  [| 1; 2; 3; 4; 5; 6; 7; _ |]

  $ ./finite_vector_tester.exe print ./buf-split
  [| 8; _; _; _; _; _; _; _ |]

  $ cp ./buf-bk ./buf

Splitting from index 8:

  $ ./finite_vector_tester.exe split_from ./buf ./buf-split 8

  $ ./finite_vector_tester.exe print ./buf
  [| 1; 2; 3; 4; 5; 6; 7; 8 |]

  $ ./finite_vector_tester.exe print ./buf-split
  [| _; _; _; _; _; _; _; _ |]

  $ cp ./buf-bk ./buf

