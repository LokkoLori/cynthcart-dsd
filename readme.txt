cythcart-dsd
rethink cythcart from Paul Slocum 
original code downloaded from: http://www.qotile.net/cynthcart.html

purpuse: create a kickass c64 instrumet for Double Score Dungeon chiptune band (a one man show)
https://www.facebook.com/doublescoredungeon

requested features:
- set tempo by tapping 
- tempo based effects like PWM, vibrato, fast arpeggio etc.
- playing mode with Keyboard + Joystic for Guitar of Wor. Similar to guitar hero.


build:

make.bat

it will compile the cynth-dsd.asm into cynth-dsd.prg
and start it with WinVICE x64.exe ... pls add it to you path.

v0.5 target>

3 "string" guitar "emulation:

q w e r t y u i o p @ * 
 a s d f g h j k l : ; =
S z x c v b n m , . / S

3 string 3 SID channel

push button on string + joystick stroke = note ... like picking a guitar

low notes on right high, notes on left. Just like the bunds on a guitar neck
high notes mask the lower bunds on the same string

advanced techniques:

hammer: pick a note, and push an upper bund ... backpick when release the upper bound
lean: like hummer, but with pressing the space bar: pitch up, release pich down
tapping: rightmost button is the toggle empty string button. 
         When toggole on, the joy strike sound the base note of the string.
		 When tap a bund key it sounds its note

		 
tunings:

standard low: E A D
standard up: G B E

and these upper and lower an octave too

wavefors:

clean
distorsion