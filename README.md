#Glove : a terminal abstraction

Glove is a very small Haskell library that lets you abstract a terminal.
The goal is mostly to provide an easy way to manipulate a console for
roguelikes, without any particular graphical implementation in mind.
You can thus use Glove with a Curse library, a sophisticated OpenGL
codebase, etc.

##Usage

The main module is the Glove.Screen file, that offers a Free Monad
modelizing the modification of a terminal.

It uses the types defined in Glove.Types. Basically, a Terminal is
perceived as a series of Tiles. A Tile is defined as a character,
a foreground and a background color, and a flag to know if the tile
has been rendered or not.

A default implementation using Data.Vector is available in Glove.Grid.
See the tests to know how to use it. There is most likely ways to provide 
a much more efficient implementation than this one. Feel free to propose
a better version than mine.

##Coming next

- First of all, the Cane library, a SDL based library that, coupled
with Glove, gives all the tools to handle the UI part of a roguelike.

- A bunch of utilies function, particularly to implement string writing.

- Off-screen consoles to improve the current model. This could modify
the API.
