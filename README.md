## Overview
This is a console application that implements the AI Bots for the board game
*Labyrinth: The Awakening, 2010 - ?*, designed by Trevor Bender and published by GMT Games.
The AI Bots were designed by Adam Zahm.

This program supports the scenarios and event cards from the original Labyrinth game as well
as those for the Awakening explansion and the Forever War expansion.
You must have a copy of the board game in order to use this program.

You can play as either the US or the Jihadist player and the AI will take the opposing side.

## Downloading the package

You can use this link to download the latest release: [Version 5.5][1]

If you are upgrading to a newer version and the **major** version numbers of the two versions
are the same, then any saved games in progress will be compatible.

Simply copy the *games* folder from the older *awakening-x.x* directory to the 
new *awakening-x.x* directory.


[1]: https://github.com/sellmerfud/awakening/releases/download/v5.5/awakening-5.5.zip

## Running the program

This is a Scala program, so you will need to have the Java JVM installed and make sure that
the `java` command is on your `PATH`

There are two scripts provided that will start the program.

* `awakening` -- As bash script used on Mac and Linux
* `awakening.cmd` -- A Windows command file for use on Windows

## Using the program

When you first run the program, it will prompt you to enter:

1. The scenario that you wish to play
2. Whether you are playing a single scenario or a campaign game.
3. The side you wish to play (US or Jihadist)
4. The difficulty level
5. A name for your game (so your progress can be saved)

Your game is saved after each action: card play, plot resolution, etc.  You can use the 
`show actions` command to see the actions that have been performed during the current turn.

When you run the program after you have one or more saved games then you will be asked
if you want to resume a saved game.

## Action Prompt
The program displays a prompt allowing you to enter the action that you wish to perform.
At the start of a new game the prompt will look this this:
```
========================================================
| Jihadist Bot's 1st action phase               Turn 1 |
| 0 Cards played                             9 in hand |
| 0 Ops in reserve                    102 in draw pile |
========================================================

Play card
--------------------------------------------------------
Show | History | Adjust | Quit | ?
========================================================
Action:
```
Here we can see that we are in the first turn of the game and it is the Jihadist Bot's
action phase.  Next we see the available actions.  The action(s) above the single dashed line are those
that pertain to game play.  The actions below the line allow you to view and manage the game status.

To enter a command you type its name.  And for convenience you can simply type the first character of
the command name.  Most commands accept arguments which can be typed following the command name.  If you
enter a command without any arguments, then you will be prompted to enter the necessary values.
You can get help for any command by following the entering the single argument *help* or *?*. For example
to get help for the *History* command you would type: `history help` or more simply `h ?`.

Normally, the only command above the dashed line is the *Play card* command.  This allows you to enter
the next card to be played for the for the active side (*Jihadist in our example*).  So to have the
Jihadist play a card you would enter `p` followed by the number of the card. 
The *Play card* command is the most common command used in the game, so for this command you can omit the command name
and simply enter the card number.  

For example the following are all ways to play the *Leak* card which is card number 85.
```
play 85
p 85
85
```

Once two cards have been played during an action round you will be asked if the action phase should
be ended.  Normally you will answer yes so that the game is advanced to the next action round for the
opposing side.  If you answer no, then you will be shown the action prompt allowing you to inspect game
state, roll back the last card play, etc.
In this case the only command available above the dashed line will be to end the action phase.
```
Two cards have been played.
End the current Jihadist action phase? (y/n) n

========================================================
| Jihadist Bot's 1st action phase               Turn 1 |
| 2 Cards played                             7 in hand |
| 0 Ops in reserve                    102 in draw pile |
========================================================

End action phase
--------------------------------------------------------
Show | History | Rollback | Inspect | Adjust | Quit | ?
========================================================
Action:
```

## Entering Country names

When the game prompts for the name of a country, you can abbreviate the name of the country
by using the shortest unique prefix.  For example you can shorten `Saudi Arabia` to `sau`.
(Upper and lower case letters are matched equally).
For the `United States` and `United Kingdom` you can use `us` and `uk` respectively.

If the prefix you type is not unique, the program will display a menu with the valid choices:
```
'ca' is ambiguous.  Choose one:
------------------------------------------------------------------------
1: Canada
2: Caucasus
3: None of the above
------------------------------------------------------------------------
Selection:
```

In fact this abbreviation of values is not limited to country names.  It works for all of the prompts used in the game.

## Multi Deck and Campaign Games

When playing a multi-deck game the software detects when the draw pile does not have enough cards to fill
both hands and will instruct you to reshuffle the discard pile.  When you reach the end of the last
deck the software will display the Victory information.

When playing a campaign game and you come to the end of a particular deck of cards,
the software will instruct you to add the next deck for the appropriate expansion.
When moving from the *Labyrinth* deck to the *Awakening* deck, the software will begin to
enforce the appropriate rule changes associated with the *Awakening* expansion.



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
