# Paddle Splat
This program is intended to be
a minimal example of a framework that can be used as a template
or a skeleton for more complex games.

## Features Completed
* Standard model-view-controller pattern
* Built on the gloss game and simulation framework
* Programmed entirely in Haskell
* Player actions factored into an API

## Potential Extensions (patterns from Brave Splat)
* Separate modules into self-contained directories and files
* Incorporate game art
* Incorporate sound effects and music 
* Dialog boxes to manipulate options
* Save and Restore Games
* Playback saved games
* Continue saved games 
* Incorporation of AI to play vs player
* Remote multi-player
* Introduction of spash screen and credits
* Add automated testing

## Notes
### Scale and Customization
Applying this framework to a game as simple os Paddle Splat
may seem like overkill -- and it is.  But the point is to
promote and understanding of the framework -- how the program
is decoomposed into sparate modules each with specific functions
and responsibilities.  The patterns that arise from separating
in this way are our focues.  These patterns recur and can be
used to organize the code for many different games.  This
re-use of programming patterns should speed development and
aid development.  We can use the same patterns to develop
a family of games by re-using our solutions to common problems
that arise in designing, building, testing, and deploying games
to the game playing community.

### Customization and Scale
Not every game will need all the features present in the framework.
We should make the framework sub-settable so that games do not need
to incorporate support for features that are not needed.  For example
a Ping-pong like game may not benefit from a capability to save and
playback games.  So it should not pay for the added complexity to
include those features.

Some games are real-time and others are turn-based.  A framework
should take those differences into account and be customizable to
support either.

As the codebase for a game gets bigger, the more it becomes desirable
to divide the game into more files.  As the codebase of a game grows
it is common to break files into smaller files, or intoduce new 
sub-directories to group files around a specific concept.
The framework should allow for this flexibility.
