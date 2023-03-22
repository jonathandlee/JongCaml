This project requires Bogue to run, please install bogue by opening up your device's terminal and entering 'opam install bogue'. After that, given that you are reading this, we assume you have unzipped the directory. 
Now, run in order:
dune build
dune exec ./bin/main.exe

From there, click the discard button and use the console to type which button you want to discard! 

Please note that the Bogue library is known to have issues on windows systems using wsl due to insufficient permissions to access graphics drivers; please consider using an ocaml virtual machine at https://cs3110.github.io/textbook/chapters/appendix/vm.html if you are using a windows machine in order to run the application on a unix based operating system