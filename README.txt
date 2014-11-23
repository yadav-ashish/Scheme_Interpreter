1. Download DrRacket.
2. Open the files interpreter.scm and library.scm.
3. Run interpreter.scm by using the following command
	
	>load "interpreter.scm"          ;loading your interpreter into Scheme system 
	>repl				 ;invoking your interpreter
	--> ;"Your scheme commands"

4. You can also run another interpreter inside this interpreter like this

	>load "interpreter.scm"
	>repl
	--> (load "library.scm")         ;load the library file
	--> (load "interpreter.scm")     ;loading your intrepreter into your interpreter
	--> (repl)   			 ; invoking your 2nd interpreter
	--> ;"Your scheme commands" like the following.
	--> (define (fac x) (if (= x 0) 1 (* x (fac (- x 1)))))  ;defining factorial
	--> (fac 4)                                              ;calling factorial
	--> 24
	--> (exit)                          			; exiting 2nd interpreter
	--> (exit)                          			; exiting 1st interpreter
	>                                			; back in scheme system
	