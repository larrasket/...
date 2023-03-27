import chess
import chess.svg
import chess.pgn
import sys
import os

notation_file = sys.argv[1]
image_file = sys.argv[2]

try:
    _, ext = os.path.splitext(notation_file)
    if ext == ".pgn":
        with open(notation_file) as f:
            game = chess.pgn.read_game(f)
            game = game.end()
            board = game.board()
    elif ext == ".fen":
        board = chess.Board(open(notation_file).read())
    else:
        raise ValueError("Invalid file format. Only '.pgn' and '.fen' are supported.")
    boardsvg = chess.svg.board(board=board)
    with open(image_file, "w") as f:
        f.write(boardsvg)
except Exception as e:
    raise e
