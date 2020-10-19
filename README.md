## Overview
This is a console application that implements the AI Bots for the board game
*Labyrinth: The Awakening, 2010 - ?*, designed by Trevor Bender and published by GMT Games.
The AI Bots were designed by Adam Zahm.

This program supports the scenarios and event cards from the original Labyrinth game as well
as those for the Awakening explansion and the Forever War expansion.
You must have a copy of the board game in order to use this program.

You can play as either the US or the Jihadist player and the AI will take the opposing side.

## Running the program

This is a Scala program, so you will need to have the Java JVM installed and make sure that
the `java` command is on your `PATH`

There are two scripts provided that will start the program.

* `awakening` -- As bash script used on Mac and Linux
* `awakening.cmd` -- A Windows command file for use on Windoze

## Downloading the package

You can download the zip file with the latest release (version 4.7) [here][1]

If you are upgrading to a newer version and the **major** version numbers of the two versions
are the same, then any saved games in progress will be compatible.

Simply copy the *games* folder from the older *awakening-x.x* directory to the 
new *awakening-x.x* directory.


[1]: https://www.dropbox.com/s/1rl29k9t4ypkv6x/awakening-4.7.zip?dl=0

## Using the program

When you first run the program, it will prompt you to enter:

1. The scenario that you wish to play
2. Whether you are playing a single scenario or a campaign game.
3. The side you wish to play (US or Jihadist)
4. The difficulty level
5. A name for your game (so your progress can be saved)

Your game is saved after each card play, plot resolution, etc.  You can use the 
`show plays` command to see the cards that have been played during the current turn.

The next time you run the program, it will ask if you want to resume a saved game.

## Entering commands
To enter a card play for the Jihadist side simply enter `j 121`.  This indicates that the
Jihadist plays card #121.  For a US card play you would enter `u 200`.

Use the `help` command to see all of the the available commands.  You can get further help for 
a specific command by typing its name after `help`. For example for help on the `show` 
command type `help show`.

The `show` command allows you to inspect the current state of the board.

The `history` command allow you to review the current turn, previous turns or the 
entire game log.

The `rollback` command will let you restart the game from:

* the beginning of a previous card during the current turn
* the beginning of a previous turn

You can also abort the current card that you are playing at most prompts by entering `abort`.


All commands can be shortened to the prefix of the command name that is unique.  The `j 121` 
command is actually shorthand for `jihadist 121`.

In fact this use of abbreviated prefixes works at every prompt in the game.  So if you are
choosing the country where you want to conduct a Jihad, you can enter `sau` to indicate 
`Saudi Arabia` or `uk` for `United Kingdom`.  If the prefix you type is not unqiue, the 
program will display the valid choices.

There is a `resolve plots` command, but you will rarely need to use it.  The program will
detect when a new action phase has started and if there are unresolved plots, they will be
resloved.

When all cards for a turn have been played, use the `end turn` command to perform the
end of turn housekeeping.

When playing a campaign game and you come to the end of a particular deck of cards
you must enter the appropriate command to let the software know that this has occurred:

    add awakening - The game starts using the Awakeing expansion rules.
                    The Bots use the appropriate priorities for Awakening.
    add forever   - The Bots use the appropriate priorities for Forever War.


## License

    Copyright (c) 2017 Curt Sellmer
    
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
