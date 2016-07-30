(* Simple OCaml script for performing apt-get update && apt-get dist-upgrade
   without user intervention and logging the result. *)

open Core.Std

module AptV2 = struct
  open Core_extended

  type apt_command = Update | DistUpgrade
  type output = { cmd : (string * string list) ; stdout : string option ; stderr : string option }
  type error_msg = { cmd : (string * string list) ; stderr : string ; stdout : string ; status : Shell.Process.status }

  let apt_command_line apt_command =
    match apt_command with
    | Update -> [ "update" ]
    | DistUpgrade -> [ "dist-upgrade" ]

  let command apt_command =
    let default_args = ["DEBIAN_FRONTEND=noninteractive" ; "apt-get"] in
    let argv = apt_command_line apt_command in
    let arguments = List.join [default_args ; argv] in
    ("env", arguments)

  let run_apt apt_command =
    let (command, argv) = command apt_command in
    try
      let output = Shell.run_lines command argv in
      Ok { cmd = (command, argv) ; stdout = Some (String.concat ~sep:"\n" output) ; stderr = None }
    with
    | Shell.Process.Failed err ->
      let output = err.Shell.Process.stdout in
      let output_err = err.Shell.Process.stderr in
      let exit_code = err.Shell.Process.status in
      Error { cmd = (command, argv) ; stderr = output_err ; stdout = output ; status = exit_code}

  let string_of_apt_result (result : (output, error_msg) Result.t)  =
    let cmdformat cmd argv = cmd ^ " " ^ (String.concat ~sep:" " argv) in
    match result with
    | Ok { cmd } ->
      let (command, argv) = cmd in
      "Command: " ^ (cmdformat command argv) ^ " run successfully."
    | Error { cmd ; stderr ; stdout }->
      let (command, argv) = cmd in
      "Error running command : " ^ (cmdformat command argv) ^ "\nSTDOUT = " ^ stdout ^ "\nSTDERR = " ^ stderr

  let run_update () =
    run_apt Update

  let run_upgrade () =
    run_apt DistUpgrade

  let install_updates () =
    let result =
      let open Result.Monad_infix in
      run_update () >>= fun update_stdout ->
      run_upgrade ()
    in
    string_of_apt_result result

end

let () =
  let output = AptV2.install_updates () in
  Printf.printf "\nUpdater output\n%s\n" output

