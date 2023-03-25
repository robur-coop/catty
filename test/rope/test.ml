let rope_to_string rope =
  let len = Rope.String.length rope in
  let buf = Buffer.create len in
  Rope.String.iter_range (Buffer.add_char buf) rope 0 len;
  Buffer.contents buf

let test01 =
  Alcotest.test_case "test01" `Quick @@ fun () ->
  let cursor = Rope.String.Cursor.create Rope.String.empty 0 in
  let cursor = Rope.String.Cursor.insert_char cursor 'a' in
  let cursor = Rope.String.Cursor.insert_char cursor 'b' in
  let cursor = Rope.String.Cursor.insert_char cursor 'c' in
  let cursor = Rope.String.Cursor.delete cursor in
  let cursor = Rope.String.Cursor.delete cursor in
  Alcotest.(check string)
    "to_string"
    Rope.String.(rope_to_string (Cursor.to_rope cursor))
    "a"

let test02 =
  Alcotest.test_case "test02" `Quick @@ fun () ->
  let cursor = Rope.String.Cursor.create Rope.String.empty 0 in
  let cursor = Rope.String.Cursor.insert_char cursor 'a' in
  let cursor = Rope.String.Cursor.move_forward cursor 1 in
  let cursor = Rope.String.Cursor.insert_char cursor 'b' in
  let cursor = Rope.String.Cursor.move_forward cursor 1 in
  let cursor = Rope.String.Cursor.insert_char cursor 'c' in
  let cursor = Rope.String.Cursor.move_forward cursor 1 in
  let cursor = Rope.String.Cursor.move_backward cursor 1 in
  let cursor = Rope.String.Cursor.delete cursor in
  let cursor = Rope.String.Cursor.move_backward cursor 1 in
  let cursor = Rope.String.Cursor.delete cursor in
  Alcotest.(check string)
    "to_string"
    Rope.String.(rope_to_string (Cursor.to_rope cursor))
    "a"

let () = Alcotest.run "rope" [ ("simple", [ test01; test02 ]) ]
