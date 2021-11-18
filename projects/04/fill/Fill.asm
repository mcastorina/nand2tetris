// This file is part of www.nand2tetris.org
// and the book "The Elements of Computing Systems"
// by Nisan and Schocken, MIT Press.
// File name: projects/04/Fill.asm

// Runs an infinite loop that listens to the keyboard input.
// When a key is pressed (any key), the program blackens the screen,
// i.e. writes "black" in every pixel;
// the screen should remain fully black as long as the key is pressed. 
// When no key is pressed, the program clears the screen, i.e. writes
// "white" in every pixel;
// the screen should remain fully clear as long as no key is pressed.

(LOOP1)
// loop until KBD != 0
@KBD
D=M
@LOOP1
D;JEQ

// blank screen
@SCREEN
D=A
@save
M=D
(FILL_BLACK)

@save    // load saved address
A=M
M=-1     // write 0xffff to address
AD=A+1   // increment address
@save    // save address
M=D
@KBD     // load end address
D=A-D    // compare with current address
@FILL_BLACK
D;JGT    // keep looping if needed

(LOOP2)
@KBD
D=M
@LOOP2
D;JNE

// reset screen
@SCREEN
D=A
@save
M=D
(FILL_WHITE)

@save    // load saved address
A=M
M=0      // write 0x0000 to address
AD=A+1   // increment address
@save    // save address
M=D
@KBD     // load end address
D=A-D    // compare with current address
@FILL_WHITE
D;JGT    // keep looping if needed

@LOOP1
0;JMP
