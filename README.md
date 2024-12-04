This is a chess-engine implementation in swi-prolog. 

## How to run

To predict the next best move for the given pgn file.
```bash
swipl -t halt -f -q -O ./src/main.pl -- {pgn_file}
```

To output all possible moves in the last position of the given pgn file.
```bash
swipl -t halt -f -q -O src/main.pl -- {pgn_file} TEST
```
