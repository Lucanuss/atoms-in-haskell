Functional Programming Project

Concept:
The player controls the movement of a white pixel. The player has to
evade red pixels that spawn at an increasing rate at the top of the screen
and move with constant velocity over the screen while getting deflected at
the borders of the screen. If the player touches a red pixel, the game is over.
Red pixels eventually turn green. When the player collides with green pixels,
an explosion occurs that will destroy all the red and green pixels in a certain 
radius. The player is awarded with points for every destroyed pixel, using the
formula 

	points_awarded = (pixels_destroyed) ^ 2
	
After touching a red pixel, the total score is displayed and the player can start
a new round.


Player Interaction:
The player can move the white pixel using the WASD keys.
When displaying the score, the player can restart using SPACE.


How it works:
Every frame, the following procedure is executed:

IF not playing
	display score
ELSE
	handle input -> move white pixel
	update pixel positions & remaining time to turn green
	spawn new red pixel if needed
	detect collisions (set explosion, playing)
	IF explosion
		destroy pixels, update score
	display everything

	
A few technical details:
While the player has integer coordinates, all the other pixels have float coordinates.
The system has its origin in the top left corner, positive x is to the right, positive y
is down. The center of a display-pixel is (x.5, y.5) in the internal system, meaning that the 
white player-pixel on (x, y) is displayed on display coordinates (x, y) and red and 
green pixels are displayed on (floor x, floor y).
While the position in the top row and the movement vector of new pixels is selected pseudo-randomly
(using a Blum-Blum-Shub generator), the spawn is always in the half of the screen where the player is
currently not and the direction is away from the player in order to avoid instant hittings.

Notes for making:
Main.hs needs to be compiled with stack ghc using the command "stack ghc -- --make Main.hs"