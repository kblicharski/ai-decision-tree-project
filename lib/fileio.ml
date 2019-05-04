let read_file (file_name: string) =
  let data = Csv.load file_name in
  Csv.print data
  (* let in_channel = open_in file_name in
  try
    while true do
      let line = input_line in_channel in
      print_endline line
    done
  with End_of_file ->
    close_in in_channel *)
