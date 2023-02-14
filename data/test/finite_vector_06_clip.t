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

Save a copy of the vector:

  $ cp ./buf ./buf-bk

Clip 0:
 
  $ ./finite_vector_tester.exe clip ./buf 0
  $ ./finite_vector_tester.exe print ./buf
  [| _; _; _; _; _; _; _; _ |]

  $ ./finite_vector_tester.exe insert ./buf 0 1
  $ ./finite_vector_tester.exe print ./buf
  [| 1; _; _; _; _; _; _; _ |]


  $ cp ./buf-bk ./buf


Clip 1:
 
  $ ./finite_vector_tester.exe clip ./buf 1

  $ ./finite_vector_tester.exe print ./buf
  [| 1; _; _; _; _; _; _; _ |]

  $ cp ./buf-bk ./buf


Clip 4:
 
  $ ./finite_vector_tester.exe clip ./buf 4

  $ ./finite_vector_tester.exe print ./buf
  [| 1; 2; 3; 4; _; _; _; _ |]

  $ cp ./buf-bk ./buf

Clip 6:
 
  $ ./finite_vector_tester.exe clip ./buf 6

  $ ./finite_vector_tester.exe print ./buf
  [| 1; 2; 3; 4; 5; 6; _; _ |]

  $ cp ./buf-bk ./buf

Clip 7:
 
  $ ./finite_vector_tester.exe clip ./buf 7

  $ ./finite_vector_tester.exe print ./buf
  [| 1; 2; 3; 4; 5; 6; 7; _ |]

  $ cp ./buf-bk ./buf

Clip 8:
 
  $ ./finite_vector_tester.exe clip ./buf 8
  Fatal error: exception Invalid_argument("attempt to clip larger than size")
  [2]
  $ cp ./buf-bk ./buf

Clip 8 at max capacity:
 
  $ ./finite_vector_tester.exe insert ./buf 7 8
  $ ./finite_vector_tester.exe clip ./buf 8
  Fatal error: exception Invalid_argument("index out of bounds")
  [2]
  $ ./finite_vector_tester.exe print ./buf
  [| 1; 2; 3; 4; 5; 6; 7; 8 |]

  $ cp ./buf-bk ./buf


