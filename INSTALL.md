This project requires Bogue to run, please install bogue by opening up your device's terminal and entering 'opam install bogue'. After that, given that you are reading this, we assume you have unzipped the directory. 
Now, assuming you are in the root directory, run in order:
dune build
make run

From there, click the discard button and use the console to type which button you want to discard! 

Please note that the Bogue library is known to have issues on windows systems using wsl due to insufficient permissions to access graphics drivers; please consider using an ocaml virtual machine at https://cs3110.github.io/textbook/chapters/appendix/vm.html if you are using a windows machine in order to run the application on a unix based operating system

Some additional make-targets are also present for the grader's convenience:

make loc: Checks lines of code, runs loc on only relevant modules. No need to remove build; this make-target ignores all files in build for you!

make doc: creates an html file of all of our module's specifications!

make opendoc: opens a folder containing these html files. From there, you can choose which one to open in your browser!

make test: runs the test suite!

make bisect: runs bisect! This one is less relevant to graders and more for our own sanity.