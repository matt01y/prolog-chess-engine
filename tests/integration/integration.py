import chess
import chess.pgn
import subprocess

endgame_file = "../examples/endgame.pgn"
kings_file = "../examples/kings.pgn"
london_file = "../examples/london.pgn"

examples = [ endgame_file, kings_file, london_file ]

swipl_command = "swipl ../../src/main.pl -- "

for example in examples:
    result = subprocess.run("swipl ../../src/main.pl -- " + example + " TEST", shell=True, capture_output=True)

    with open(example, "r") as f:
        game = chess.pgn.read_game(f)
    board = game.board()
    for move in game.mainline_moves():
        board.push(move)
    if (len(result.stdout.splitlines()) != board.legal_moves.count()):
        print(f"ERROR: in file {example}")
        print("Expected: " + str(board.legal_moves.count()) + " moves")
        print("Got: " + str(len(result.stdout.splitlines())))
    else:
        print(f"SUCCESS: in file {example}")

