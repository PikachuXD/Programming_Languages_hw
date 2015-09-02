-- HASKELL
import Data.List        -- contains sortBy

split (c:p:tail) = (p, c):(split tail)
split [] = []

rEdge e' e = (filter (/=e') e)
rVert v' v = (filter (/=v') v)

outG node edge = filter ((==node) . fst) edge

noInE v e = filter (flip notElem $ map snd e) v

topSort v e = topSort' v e (sort $ noInE v e) []
  where topSort' _ [] [] l = reverse l
        topSort' _ _  [] l = ["cycle"]
        topSort' v e  (n:s) l = topSort' v' e' (sort ( noInE v' e' )) (n:l)
          where v' = rVert n v
                e' = foldr rEdge e (outG n e)

main = do

        all_input_as_one_string <- getContents

        -- Next, split that string up into lines.
        --   There is a convenient built-in function to do this for us.
        let all_input_as_lines = lines all_input_as_one_string
        -- Third, sort those lines.
        --   How do we compare lines 'x' and 'y'? In reverse order!
        let edges = split all_input_as_lines
        let verts = nub all_input_as_lines

        let finalLines = topSort verts edges
        -- Finally, print out the sorted list.
        mapM putStrLn finalLines
        -- We use mapM ("monadic map") here because the type of main
        -- should be "IO t" and using vanilla map yields type
        -- [IO ()]. You can see Haskell's error message for that if
        -- you replace mapM by map above and try to recompile.
