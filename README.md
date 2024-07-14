```
         ######  ##########  #########  ###  ###  ########  ###  ###
       #######  ##########  #########  ###  ###  ########  ###  ###
     ###  ###    #####     ###        ###  ###  ###       ###  ###
   ####  ###       #####  ###        ###  ###  ########  ########
 ##########   #########  #########  ###  ###  ###       ####
###    ###   #########  #########  ###  ###  ###       ####
```

This is a command line utility to help transform images into ASCII art. It was intended to transform line art, memes or math diagrams. Outputs do require manual edit to shine, however they provide a nice starting point to work from.
Examples of generated output and final results are shown at the end of the README.

```
asciify [OPTIONS] FILE

Common flags:
  -u --uarg      Unclutter output
  -l --lines=20  Desired number of lines in the ouput
  -t --targ=0.5  0.0-1.0 less/more details
  -? --help      Display help message
  -V --version   Print version information
```

## Installation

Requirements: ghc, cabal
```
cabal build
```

## Examples

cabal run . -- [assets/pepe.jpg](assets/pepe.jpg) -t 0.2 -u
```
   ______    ___                                     ________  ____
 /        \/-   \-_                                 /    ____\/    \_
/   ___---\_   ___ \|                              /   _/    \_---___\__
   /        ---   - \\                            |   /        \__      \_
    __________-|    __ \                          |   ___________ \ _____ \
  /_ \_/    \_\-| _#####_|                        |  /___________\|/______\|
  - \____###- --_/##__/\#|                        |  \__ ##.#    | | #.#   \
      \  #_#  |_/\###_\/_|                        |      \####___| |_###___/
      --\_/ -\_/|___- __\     manual edit         |       ------\  |___    \
 |#_       _/-      \   \_     ---------------->  |   __      __/      \    |
|--#_\___--             _#|                       | |__\___               _|
  \-_\/\_\--_/__------_-#                         |   \____ \______________/
     - \__ - -  ------/ /                         |      \_________________|
          -\-_________--|                         |           \___________/
                 /   \-_                          |              __/    \____
             |--/-\____/ \\                       |             | |__/  /   \
               \--__   _   \                      |              \  \__/  /  \
               \ |\__ /  / _\                     |               \  \/__/__/ \
```

cabal run . -- [assets/cube.png](assets/cube.png) -u
```
        _________________                           __________________
     __-/             _ #                         _/|               _/|
   _/  |           __-/ #                       _/  |             _/  |
 |-\___ \____------     #     manual edit     #/----------------#/    |
 |     |          |     #|   ------------->   |     |           |     |
 |     |          |     |                     |     |           |     |
 |     |          |     |                     |     |           |     |
 |     |_         |     |                     |     |           |     |
 |      _   _ __ _|\ ___/|                    |     #___________|_____#
 |    /_ --     - |/   _-                     |   _/            |   _/
 |  |/-           |_ _-                       | _/              | _/
 |__|______________#-                         #'________________#/
```

cabal run . -- [assets/wojak.jpg](assets/wojak.jpg) -t 0.2 -u
```
         __--- \_                                   ____-----___
      /_-         \\                               /            \_
     /              -_                            /               \
    _       ---_----\|_                          /       ---'----\ \
    /     __/__ \____ |                         /      __/---\____  |
   |        -  _   | \ _                        |      '          ' |
    _      |\##|   _/_ \    manual edit         |       |###.  .###||
     \  /         |    \   -------------->       \             |    |
     \| -      /   \   _                          \|        /   \   |
      | |_     \|-|-  _|                           | |_     '=-='   |
      |  | \_ |-____ /                             | | \_  -----.  /
     _       \\    / /                            /  |   \________/
   / | _/      _- \____                       ___/             \____
_--        -\ /        -                     -           \ /        -
```
