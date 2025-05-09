module Internal_formatter = Formatter
open Core

module Formatter = struct
  let data_type = Datatype.Text_html

  let create ?(border = false) () =
    let rows_to_output ?headers rows =
      let count = List.length rows in
      let buf = Buffer.create (count * 16) in
      let add str = Buffer.add_string buf str in
      add "<html><body>";
      add "<!--StartFragment-->";
      add "<table";
      if border then add " border=\"1\"";
      add " style=\"border-collapse: collapse;\">";
      (match headers with
       | None -> ()
       | Some headers ->
         add "<thead><tr>";
         List.iter
           ~f:(fun h ->
             add "<th>";
             add h;
             add "</th>")
           headers;
         add "</tr></thead>");
      add "<tbody>";
      List.iter
        ~f:(fun row ->
          add "<tr>";
          List.iter
            ~f:(fun cell ->
              add "<td>";
              add cell;
              add "</td>")
            row;
          add "</tr>")
        rows;
      add "</tbody></table>";
      add "<!--EndFragment-->";
      add "</body></html>";
      Buffer.contents buf
    in
    Internal_formatter.create data_type rows_to_output
  ;;
end
