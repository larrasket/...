import chess
import chess.svg
import chess.pgn
import sys

pgn_file = sys.argv[1]
image_file = sys.argv[2]

# Read pgn file:
with open(pgn_file) as f:
    game = chess.pgn.read_game(f)

# Go to the end of the game and create a chess.Board() from it:
game = game.end()
board = game.board()

# Generate the SVG image:
boardsvg = chess.svg.board(board=board)

# Write the SVG image to the specified file:
with open(image_file, "w") as f:
    f.write(boardsvg)

