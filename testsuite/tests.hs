import Test.Hspec
import Control.Monad.State
import Glove.Types
import Glove.Screen

main = hspec spec

testBasicSetters :: (Monad m) => ScreenAction m ()
testBasicSetters = do putAt (1,1) '@'
                      colorizeFront (0,0) black
                      colorizeBack (0,0) white
                      setAt (3,3) (Tile 'v' green red True)

testBasicGetters :: (Monad m) => ScreenAction m Tile
testBasicGetters = do putAt (1,1) '@'
                      readAt (1,1)

testMockRenderSetter :: (Monad m) => ScreenAction m ()
testMockRenderSetter = do putAt (1,1) '@'
                          rendered

testReinit :: (Monad m) => ScreenAction m ()
testReinit = do putAt (1,1) '@'
                clearScreen

testConsole = (initConsole black white ' ' 80 60)

runTest f = execState (run f) testConsole
runAndEvaluateTile p f = let cons = runTest f
                             grid = _grid cons in
                             grid !!! p
runRead f = evalState (run f) testConsole

spec = do
    describe "Glove provide setters to modify a grid." $ do
        it "And allows putting characters at precise location" $ do
            runAndEvaluateTile (1,1) testBasicSetters 
                `shouldBe` Tile '@' black white True
            runAndEvaluateTile (5,5) testBasicSetters 
                `shouldBe` Tile ' ' black white True
        it "It also lets you modifying foreground and background colors" $ do 
            runAndEvaluateTile (0,0) testBasicSetters
                `shouldBe` Tile ' ' white black True
            runAndEvaluateTile (6,6) testBasicSetters
                `shouldBe` Tile ' ' black white True
        it "Or you can set everything directly" $ do
            runAndEvaluateTile (3,3) testBasicSetters
                `shouldBe` Tile 'v' green red True
        it "It can also reinitialize the console to its basic setting" $ do
            runAndEvaluateTile (1,1) testReinit
                `shouldBe` Tile ' ' black white True
    describe "Glove also allows you to get details on the grid." $ do
        it "It lets you read the value of specific area." $ do
            runRead testBasicGetters
                `shouldBe` Tile '@' black white True
    describe "In order to improve rendering..." $ do
        it "It keeps track of updated / not updated tiles" $ do
            runAndEvaluateTile (1,1) testMockRenderSetter
                `shouldBe` Tile '@' black white False
