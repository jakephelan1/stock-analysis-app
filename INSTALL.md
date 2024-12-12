Download the code. Before you run anything, make sure you use `opam install` to install the following:
- lwt_ssl 1.2.0
- lwt_ppx 5.8.0
- cohttp-lwt-unix 5.3.0
- opium 0.20.0
- yojson 2.2.2
From there, running "dune exec bin/main.exe" should generate a pop-up window (in VSCODE or the IDE you are using), which contains a button that says "Open In Browser". Clicking on the button will lead you to the demo. Another option to view the demo is to run "dune exec bin/main.exe", open a browser of your choice, and enter "localhost:3000". You must enter "localhost:3000" while your terminal is still running "dune exec bin/main.exe". These two options should both open the demo.
