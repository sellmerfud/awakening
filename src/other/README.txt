

README file for the awakening console application.
-----------------------------------------------------------------------
The awakening program is an implementation of the solo AI for
the board game Labyrinth: The Awakening, 2010 - ?, 
designed by Trevor Bender and published by GMT Games.

The program is implemented in the Scala programming language
and therefore requires the Java JVM.  It is a text based 
console application and should run on Mac OSX, Windows and
Linux.

Currently this program only supports the cards from the 
Awakening expansion. I may add support for the cards from
the original Labyrinth game in future.


To run the program
-----------------------------------------------------------------------
1. Ensure that Java is installed on you system and that the java 
   executable can be found on your PATH.
   
2. Run the awakening script located in the same directory
   as this README file. (On Windows, use the awakening.cmd script)

3. If all is well the game will prompt you for a scenario and you
   will be off an running.
   
   
More Details
-----------------------------------------------------------------------
1. You will be presented with a Command: prompt.  You can type
   'help' to get a list of valid commands.  You can type 
   'help <command>' to get more detailed help on a particular command.
   
2. Any time the game prompts you for a value (a command, a country name, etc.),  
   you only need to enter the first few characters of the value.  For example,
   if you want to enter "Saudi Arabia" as the target of an event, you can simply
   type 'Sau'
   
3. Your game will automatically be saved after each card play.
   When you resume a game in progress the game will load the most
   recent save file.
   The games are saved in a 'games' subdirectory that is created
   in the same directory as this README file.  Each game will have
   its own subdirectory within 'games'.  To delete a saved game
   simply delete its corresponding subdirectory.
   
4. There is a 'resolve plot' command, but you should rarely need to 
   use it.  The game will detect when plots need to be resolved
   automatically.
 
5. Once all cards have been played by both sides, use the 'end turn'
   command to do all of the end of turn stuff.  If there are 
   unresolved plots on the map, they will be resolved first.  
   
6. You can use the 'rollback' command to rollback to the beginning
   of a previous card play, or the beginning of a previous turn.
   
7. Look at the help for the 'show', 'history' commands.  The are
   very useful for making sure your board is in sync with the game.
   If you find any inconsistencies, you can use the 'adjust' command.
   

Configuration file
-----------------------------------------------------------------------
There is a file called 'awakening_config'.  You can use this 
to set default parameters if you like, so that you will not 
be prompted every time you start a new game.  See the 
comments in that file for more information.


Command line
-----------------------------------------------------------------------
Type 'awakening --help' (awakening.cmd --help on Windows) to
see the available command line options.  Any option typed
on the command line will override a value set in the
configuration file.


Copyright (c) 2017 Curt Sellmer
-----------------------------------------------------------------------
Permission is hereby granted, free of charge, to any person obtaining
a copy of this software and associated documentation files (the
"Software"), to deal in the Software without restriction, including
without limitation the rights to use, copy, modify, merge, publish,
distribute, sublicense, and/or sell copies of the Software, and to
permit persons to whom the Software is furnished to do so, subject to
the following conditions:

The above copyright notice and this permission notice shall be
included in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE
LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION
OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION
WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
