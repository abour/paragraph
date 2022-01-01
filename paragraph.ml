type custom_message = Error | Success of int;;

type 'a node = {
  listen_stream : 'a;
  notify_streams : 'a list;
  value : string;
};;

let rec create_pipeline2 nodes = 
  match nodes with
  | [] -> []
  | a::[] -> [a]
  | a::b::c ->  match (a,b) with ({
    listen_stream = ls1;
    notify_streams = ns1;
    value = v1;
  },
  {
    listen_stream = ls2;
    _
  }) -> {listen_stream = ls1; notify_streams = List.cons ls2 ns1; value = v1}::(create_pipeline2 (b::c));;

let create_pipeline elems = 
  let nodes = List.map (fun s -> ({listen_stream = (Eio.Stream.create 1); notify_streams = []; value = s})) elems in
  create_pipeline2 nodes;;

let listen nodes = 
  Eio.Std.Switch.run @@ fun sw ->
  List.iter ( fun x -> 
    let {listen_stream = listen_stream; notify_streams = notify_streams; value = value} = x in
      Eio.Std.Fibre.fork ~sw
      (fun () -> 
        Eio.Std.traceln "Node listening: %s" value;
        Eio.Std.Fibre.yield ();
        let _ = Eio.Stream.take listen_stream in
        Eio.Std.traceln "YOUHOU node executed:  %s" value;
      List.iter (fun stream -> Eio.Stream.add stream "1") notify_streams;)
    ) nodes;;

Eio_main.run @@ fun _ ->
  let numbers_list = ["1"; "2"; "5"; "7"] in
  let pipeline = create_pipeline numbers_list in
  Eio.Std.Fibre.both
  (
    fun () -> Eio.Std.traceln "hello you";
    match pipeline with
    | [] -> Eio.Std.traceln "pipeline is empty";
    | a::_ ->      
      let {listen_stream = listen_stream; value = value; _} = a in
      Eio.Std.traceln "notify the head of the list: %s" value;
      Eio.Stream.add listen_stream "1";
      Eio.Std.traceln "head of the list has been notified";
  )
  (fun () -> listen pipeline);;