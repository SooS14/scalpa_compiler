
##
## 	The program --- Is-vowel.s will check if an entered character is a vowel.
##
##		- It will ask the user for a letter (character),
##		  
##		- then call a procedure ("vowelp") to check if it is a vowel.
##		- It will return a value to the calling function, 
##		  0 means no, and 1 means yes,
##		- and then print a message from the calling program.
##		
##		v0 - used for syscalls
##		   - holds 0 or 1 after the call to "vowelp"
##		t0 - holds the returned value from the procedure call
## 		a0 - points to strings to be printed 
##		   - also used to pass a letter to "vowelp" 
##
##

#################################################
#						#
#               text segment			#
#						#
#################################################

	.text
	.globl main
main:
	.data
	letter:		.space 5
	prompt1:	.asciiz "Enter a letter: "
	
	message:	.asciiz "The character entered is not a vowel."
	ans1:		.asciiz "The character entered is a vowel. "
	endl:		.asciiz "\n"	   

##
## 	end of file Is-vowel.s
