cynthcart-dsd

forked from original cynthcart, coded by Paul Slocum 
source downloaded from: http://www.qotile.net/cynthcart.html

purpose: create a kickass c64 instrument for Double Score Dungeon chiptune band needs
https://www.facebook.com/doublescoredungeon

requested features:
- set tempo by tapping 
- tempo based effects like PWM, vibrato, fast arpeggio etc.
- playing mode with Keyboard + Joystic for Guitar of Wor. Similar to guitar hero.


build:

make.bat

it will compile the cynth-dsd.asm into cynth-dsd.prg
and start it with WinVICE x64.exe ... add it to your path.

v0.5 milestone - GUITAR OF WOR demo:

3 strings guitar emulation:

q w e r t y u i o p @ * 
 a s d f g h j k l : ; =
S z x c v b n m , . / S

3 strings 3 SID channels

push button on a string + joystick stroke = note ... like picking a guitar

low notes on the right, high notes on the left. Just like frets on a guitar neck
higher note masks the lower frets on the same string

advanced techniques:

hammer: pick a note, and push an upper fret ... backpick when release the upper fret
lean: like hammer, but with pressing the space bar: pitch up, release pich down
		 
tunes:

standard low: E A D
standard up: G B E

and these upper and lower an octave too

wavefors:

clean
distorsion