open Nottui
open Notty

let src = Logs.Src.create "kit.prompt"

module Log = (val Logs.src_log src : Logs.LOG)

let neg x = -x

type t =
  { command : string -> unit
  ; message : string -> unit
  ; cursor : Rp.Cursor.cursor
  ; history_command : History.t
  ; history_message : History.t
  }

let make ?(len = 10) command message =
  let cursor = Rp.Cursor.create Rp.empty 0 in
  { command
  ; message
  ; cursor
  ; history_command = History.make len
  ; history_message = History.make len
  }

module Utils = struct
  let move_cursor ?(visual = true) ~hook cursor = function
    | `Left ->
        let position = Rp.Cursor.position cursor in
        (if position > 0 then
           let cursor = Rp.Cursor.move_backward cursor 1 in
           hook cursor);
        `Handled
    | `Right ->
        let position = Rp.Cursor.position cursor in
        let rope = Rp.Cursor.to_rope cursor in
        let len = Rp.length rope in
        let len = if visual then len - 1 else len in
        (if position < len then
           let cursor = Rp.Cursor.move_forward cursor 1 in
           hook cursor);
        `Handled

  let is_print = function '\x21' .. '\x7e' | ' ' -> true | _ -> false

  let render_cursor ~width cursor =
    let rope = Rp.Cursor.to_rope cursor in
    let position = Rp.Cursor.position cursor in
    let length = Rp.length rope in
    let offset = if position >= width then position - width else 0 in
    let rope = Rp.sub rope offset (length - offset) in
    (* XXX(dinosaure): shift our text according to [offset]. *)
    let length = Rp.length rope in
    let left, middle, right =
      match position >= 0 && position < length with
      | true ->
          ( Rp.sub rope 0 position
          , Some (Rp.get rope position)
          , Rp.sub rope (position + 1) (length - position - 1) )
      | false -> (rope, None, Rp.empty)
    in
    let middle =
      match middle with
      | None -> I.uchar A.empty (Uchar.of_char ' ') 1 1
      | Some uchar -> I.uchar A.empty uchar 1 1
    in
    ( I.hcat [ I.strf "%a" Rp.print left; middle; I.strf "%a" Rp.print right ]
    , position - offset )
end

module Insertion = struct
  let handler ~(hook : ?mode:_ -> _ -> unit) state = function
    | `ASCII chr, [] when Utils.is_print chr ->
        let cursor = state.cursor in
        let cursor = Rp.Cursor.insert_char cursor (Uchar.of_char chr) in
        let cursor = Rp.Cursor.move_forward cursor 1 in
        hook { state with cursor };
        `Handled
    | `Uchar uchar, [] ->
        let cursor = state.cursor in
        let cursor = Rp.Cursor.insert_char cursor uchar in
        let cursor = Rp.Cursor.move_forward cursor 1 in
        hook { state with cursor };
        `Handled
    | `Escape, [] ->
        let len = Rp.length (Rp.Cursor.to_rope state.cursor) in
        let pos = Rp.Cursor.position state.cursor in
        let cursor =
          if pos > 0 && pos = len then Rp.Cursor.move_backward state.cursor 1
          else state.cursor
        in
        Log.debug (fun m -> m "mode: Insertion -> Normal");
        hook ~mode:`Normal { state with cursor };
        `Handled
    | `Backspace, [] ->
        let position = Rp.Cursor.position state.cursor in
        (if position > 0 then
           let cursor = state.cursor in
           let cursor = Rp.Cursor.move_backward cursor 1 in
           let cursor = Rp.Cursor.delete cursor in
           hook { state with cursor });
        `Handled
    | `Arrow `Left, [] ->
        let hook cursor = hook { state with cursor } in
        Utils.move_cursor ~visual:false ~hook state.cursor `Left
    | `Arrow `Right, [] ->
        let hook cursor = hook { state with cursor } in
        Utils.move_cursor ~visual:false ~hook state.cursor `Right
    | `Arrow `Up, [] ->
        let cursor = History.move_backward state.history_message state.cursor in
        hook { state with cursor };
        `Handled
    | `Arrow `Down, [] ->
        let cursor = History.move_forward state.history_message state.cursor in
        hook { state with cursor };
        `Handled
    | `Enter, [] ->
        History.fill state.history_message state.cursor;
        let rope = Rp.Cursor.to_rope state.cursor in
        let msg =
          let len = Rp.length rope in
          let buf = Buffer.create len in
          Rp.iter_range (Uutf.Buffer.add_utf_8 buf) rope 0 len;
          Buffer.contents buf
        in
        state.message msg;
        hook { state with cursor = Rp.Cursor.create Rp.empty 0 };
        `Handled
    | _ -> `Unhandled
end

(* Line editing from the [normal] mode:
   - Move the beginning of line            [0]
   - Move left                             [h] or [<-] or [<Backspace>]
   - Move right                            [l] or [->]
   - TODO: Move to previous word           ([b])
   - TODO: Move to next word               ([w])
   - Move to end of line:                  [$]
   - Delete current character:             [x] or [<Delete>]
   - TODO: Delete to the beginning of line ([d0])
   - TODO: Delete to end of line           ([d$])
   - TODO: Delete next word                ([dw])
   - TODO: Delete previous word            ([db])
   - TODO: Delete the line                 ([dd])
   - TODO: Insert a newline without sending message
   - TODO: Complete nickname               ([<Tab>])

   - TODO: [cw]? (Delete next word and go to [insert] mode)
   - TODO: [r]<letter>? (Replace current character by the given one)
*)

module Normal = struct
  let handler ~(hook : ?mode:_ -> _ -> unit) state = function
    | (`ASCII 'h' | `Arrow `Left | `Backspace), [] ->
        let hook cursor = hook { state with cursor } in
        Utils.move_cursor ~hook state.cursor `Left
    | (`ASCII 'l' | `Arrow `Right), [] ->
        let hook cursor = hook { state with cursor } in
        Utils.move_cursor ~hook state.cursor `Right
    | `ASCII 'i', [] ->
        hook ~mode:`Insertion state;
        `Handled
    | `ASCII ':', [] ->
        Log.debug (fun m -> m "mode: Normal -> Command");
        let cursor = Rp.Cursor.create Rp.empty 0 in
        hook ~mode:(`Command cursor) state;
        `Handled
    | `ASCII 'a', [] ->
        let position = Rp.Cursor.position state.cursor in
        let rope = Rp.Cursor.to_rope state.cursor in
        if position < Rp.length rope then
          let cursor = Rp.Cursor.move_forward state.cursor 1 in
          hook ~mode:`Insertion { state with cursor }
        else hook ~mode:`Insertion state;
        `Handled
    | `ASCII 'x', [] | `Delete, [] ->
        let len = Rp.length (Rp.Cursor.to_rope state.cursor) in
        let pos = Rp.Cursor.position state.cursor in
        (if len > 0 then
           let cursor = Rp.Cursor.delete state.cursor in
           let cursor =
             if pos = len - 1 && pos > 0 then Rp.Cursor.move_backward cursor 1
             else cursor
           in
           hook { state with cursor });
        `Handled
    | `ASCII '0', [] ->
        hook
          { state with
            cursor = Rp.Cursor.(move state.cursor (neg (position state.cursor)))
          };
        Log.debug (fun m -> m "Move to the beginning.");
        `Handled
    | `ASCII '$', [] ->
        let len = Rp.length (Rp.Cursor.to_rope state.cursor) in
        let pos = len - 1 - Rp.Cursor.position state.cursor in
        hook { state with cursor = Rp.Cursor.move state.cursor pos };
        `Handled
    | _ -> `Unhandled
end

module Command = struct
  let handler ~cursor ~(hook : ?mode:_ -> _ -> unit) state = function
    | `ASCII chr, [] when Utils.is_print chr ->
        let cursor = Rp.Cursor.insert_char cursor (Uchar.of_char chr) in
        let cursor = Rp.Cursor.move_forward cursor 1 in
        hook ~mode:(`Command cursor) state;
        `Handled
    | `Uchar uchr, [] ->
        let cursor = Rp.Cursor.insert_char cursor uchr in
        let cursor = Rp.Cursor.move_forward cursor 1 in
        hook ~mode:(`Command cursor) state;
        `Handled
    | `Escape, [] ->
        Log.debug (fun m -> m "mode: Command -> Normal");
        hook ~mode:`Normal state;
        `Handled
    | `Backspace, [] ->
        let position = Rp.Cursor.position cursor in
        (if position > 0 then
           let cursor = cursor in
           let cursor = Rp.Cursor.move_backward cursor 1 in
           let cursor = Rp.Cursor.delete cursor in
           hook ~mode:(`Command cursor) state);
        `Handled
    | `Arrow `Left, [] ->
        let hook cursor = hook ~mode:(`Command cursor) state in
        Utils.move_cursor ~visual:false ~hook cursor `Left
    | `Arrow `Right, [] ->
        let hook cursor = hook ~mode:(`Command cursor) state in
        Utils.move_cursor ~visual:false ~hook cursor `Right
    | `Arrow `Up, [] ->
        let cursor = History.move_backward state.history_command cursor in
        hook ~mode:(`Command cursor) state;
        `Handled
    | `Arrow `Down, [] ->
        let cursor = History.move_forward state.history_command cursor in
        hook ~mode:(`Command cursor) state;
        `Handled
    | `Enter, [] ->
        History.fill state.history_command cursor;
        let rope = Rp.Cursor.to_rope cursor in
        let cmd =
          let len = Rp.length rope in
          let buf = Buffer.create len in
          Rp.iter_range (Uutf.Buffer.add_utf_8 buf) rope 0 len;
          Buffer.contents buf
        in
        hook ~mode:(`Command (Rp.Cursor.create Rp.empty 0)) state;
        Log.debug (fun m -> m "Send a new command: %S" cmd);
        state.command cmd;
        `Handled
    | _ -> `Unhandled
end

module User_prompt = struct
  let render ~cursor ~y ~w state mode =
    let mode = Mode.pretty mode in
    let text, position =
      Utils.render_cursor ~width:(max 0 (w - 3)) state.cursor
    in
    Lwd.set cursor (position + 2, y);
    I.hcat [ mode; I.char A.empty ' ' 1 1; text ]
end

module Command_prompt = struct
  let render ~cursor ~y ~w mode =
    match mode with
    | `Command cursor' ->
        let text, position =
          Utils.render_cursor ~width:(max 0 (w - 3)) cursor'
        in
        Lwd.set cursor (position + 2, y + 1);
        I.hcat
          [ I.uchars A.empty [| Uchar.of_char ' '; Uchar.of_int 0x0589 |]
          ; text
          ]
    | _ -> I.uchars A.empty [| Uchar.of_char ' '; Uchar.of_int 0x0589 |]
end

type cursor = int * int

let make ~command ~message cursor status mode _w =
  let ( let* ) x f = Lwd.bind ~f x in
  let ( let+ ) x f = Lwd.map ~f x in
  let ( and+ ) = Lwd.map2 ~f:(fun x y -> (x, y)) in

  let state = Lwd.var (make command message) in
  let position = Lwd.var (0, 0) in
  (* XXX(dinosaure): absolute position [y] and [w]. *)
  let hook ?mode:mode' state' =
    Option.iter (Lwd.set mode) mode';
    Lwd.set state state'
  in

  let handler state mode key =
    match mode with
    | `Normal -> Normal.handler ~hook state key
    | `Insertion -> Insertion.handler ~hook state key
    | `Command cursor -> Command.handler ~cursor ~hook state key
    | `Visual -> assert false
  in

  let update_prompts state mode (y, w) =
    let user = User_prompt.render ~cursor ~y ~w state mode in
    let command = Command_prompt.render ~cursor ~y ~w mode in
    Ui.keyboard_area (handler state mode) (Ui.atom (I.vcat [ user; command ]))
  in

  let update_position ~x:_ ~y ~w ~h:_ () =
    let y', w' = Lwd.peek position in
    if y' <> y || w' <> w then Lwd.set position (y, w)
  in

  let* prompts =
    let+ state = Lwd.get state
    and+ position = Lwd.get position
    and+ mode = Lwd.get mode in
    update_prompts state mode position
  in

  let* status = Lwd.map ~f:Status.render (Lwd.get status) in

  let document = Ui.vcat [ prompts; status ] in
  let document = Ui.transient_sensor update_position document in
  Lwd.return document
