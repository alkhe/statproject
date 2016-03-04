import Graphics.Rendering.Chart.Easy
import Graphics.Rendering.Chart.Backend.Diagrams

titles = ["Cash"]

values :: [ (String,[Double]) ]
values =
  [ ("Jun", [20])
  , ("Jul", [45])
  , ("Aug", [30])
  , ("Sep", [10])
  , ("Oct", [20])
  ]

main = toFile def "example11_big.svg" $ do
    layout_title .= "Sample Bars"
    layout_title_style . font_size .= 10
    layout_x_axis . laxis_generate .= autoIndexAxis (map fst values)
    plot $ fmap plotBars $ bars titles (addIndexes (map snd values))
