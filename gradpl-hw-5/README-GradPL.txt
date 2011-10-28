                Graduate Programming Languages - Homework 5 
                           Test Input Generator
                                (version 1)

This README assumes you are using Linux. It is your responsibility to make
the project work (which may involve switching to Linux or adapting this
code to your platform). 

#1. Download the Z3 theorem prover from Microsoft Research. Check the
course webpage and/or
        
http://research.microsoft.com/en-us/um/redmond/projects/z3/download.html

#2. Unpack the version of Z3 that corresponds to your architecture. 

Example (may not work on your machine):

        tar xf z3-x64-3.0.tar.gz

#3. Install camlidl and libgomp.

Example (may not work on your machine):

        su
        yum install ocaml-camlidl ocaml-camlidl-devel libgomp

#4. Figure out where $OCAMLLIB is on your computer. Notably, that directory
contains "pervasives.mli". 

Example (may not work on your machine):

        ls /usr/lib64/ocaml/pervasives.mli
        export OCAMLLIB=/usr/lib64/ocaml

You might also check /usr/lib/ocaml, /usr/local/lib/ocaml, etc. 

#5. Build the OCaml interface for Z3, using the $OCAMLLIB location you found
before. 

Example (may not work on your machine):

        cd z3/ocaml
        ./build-lib.sh $OCAMLLIB 

For me, this often generates a number of warnings like: 

z3_stubs.c: In function ‘caml_z3_error_handler’:
z3_stubs.c:24:85: warning: initialization discards qualifiers from pointer
target type

... but the compile succeeds. 

#6. Download the CIL toolkit for C programs from Sourceforge. Check the
course webpage and/or

http://sourceforge.net/projects/cil/

#7. Unpack CIL.

Example: 

        tar xf cil-1.3.7.tar.gz 

#8. Build CIL. 

Example: 

        cd cil-1.3.7
        sh configure
        make

#9. Build the Test Input Generator: tigen. 

If you've unpacked CIL and Z3 so that the contents of the current directory
look like this: 

        cil-1.3.7/
        README-GradPL.txt
        tigen/
        z3/

... you're golden, and tigen should "make" out of the box. If not, you'll
have to edit the top of the Makefile to point it at your directories. 

        cd tigen
        vi Makefile
        make

#10. Try It Out!

tigen comes with three handy "make" targets. The first,  

        make test

... copies all of the "master copies" of the few test programs I've
provided for you over from "tigen/test-sources/" to a temporary home
in "tigen/test/". It then calls "tigen" on each one to generate test cases.
You'll find the test cases in "tigen/test/FOO/test0001.c" and the like. 

The second, 

        make eval

... runs the test cases you produced and compared your statement coverage
to the known maximum. 

Finally,

        make clean

... will remove "tigen/test/" and clean things up. You can then make
improvements to tigen and retest later. 

Good luck! 
        
        - Wes Weimer 
