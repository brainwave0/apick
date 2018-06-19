# apick
An activity recommender for bored people

## Why
I always keep a list of things I'd like to do, but sometimes it is difficult to pick one to do.

## How
The names of activities are entered manually, and rated one-by-one as the user trys them. The app uses upper confidence bound to recommend activities.

## Usage
Add some activities.

    $ apick add "Game of Thrones"
    Game of Thrones
    $ apick add "bowling"
    bowling
    ...

The top recommendation is shown automatically after each command. To show the current recommendation without making any modifications:

    $ apick show
    Game of Thrones

Try it, then when you're done, give it a rating from 0 to 1.

    $ apick rate 0.5
    Game of Thrones

After watching the whole series:

    $ apick finish
    Call of Duty

If playing Call of Duty is inconvenient right now, or you're tired of playing it:

    $ apick skip
    code

    