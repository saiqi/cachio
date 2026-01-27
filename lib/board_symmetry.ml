let canonical_hash board =
  let h0 = Board.hash board in
  let h1 = Board.hash (Board.rotate board) in
  if h0 < h1 then h0 else h1
