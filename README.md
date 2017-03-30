# How to run Checkers

 Program is runned by GHCI. Main function, to run the game is

```playerVsComputer:: Board -> Bool -> IO String```

### Example:
`playerVsComputer initBoard False`

### Arguments
*  `Board` - initial board(`initBoard` declared in Board.hs)
*  `Bool` - Boolean telling whether user wants play with black or white figures
    * True - user is playing black figures
    * False - user is playing white figures
  
  ### Remarks
* user is always making first move, no matter if he's playing black or white figures
* when prompted user should enter sequnce of moves to make in PDN format 
 * type `q` to force quit the game 




