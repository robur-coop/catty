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

let test03 =
  Alcotest.test_case "test03" `Quick @@ fun () ->
  let module Rp = Rope.String in
  let rec loop c = function
    | 0 -> c
    | n ->
        let c = Rp.Cursor.insert_char c 'A' in
        loop (Rp.Cursor.move_forward c 1) (pred n)
  in

  let c = loop (Rp.Cursor.create Rp.empty 0) 512 in
  let r = Rp.Cursor.to_rope c in
  Rp.iter_range ignore r 0 (Rp.length r)

let () = Alcotest.run "rope" [ ("simple", [ test01; test02; test03 ]) ]
