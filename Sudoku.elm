module Sudoku exposing (..)

import Array exposing (..)
import Browser
import Browser.Events
import Html exposing (Html)
import Html.Events as Events
import Html.Attributes as Attr
import Json.Decode as Decode
import Random exposing (Generator)
import Color exposing (Color)
import Html exposing (a)
import Html
import Html


-- SUDOKU BOARD PRESET FUNCTIONS --------------------------------------------------------------
-- presets from: https://github.com/dimitri/sudoku/blob/master/easy50.txt
-- and from http://bert.stuy.edu/pbrooks/fall2018/materials/ai/Sudoku-boards.txt

-- presetted boards separated by level
easy =
    [ "1..92....|524.1....|.......7.|.5...81.2|.........|4.27...9.|.6.......|....3.945|....71..6"
    , "361.259..|.8.96..1.|4......57|..8...471|...6.3...|259...8..|74......5|.2..18.6.|..547.329"
    , "..41..527|2137.....|..7624...|35.27....|....3.875|.4...6.13|472.1..5.|.31.62..9|9.....18."
    , "...9....2|.5.1234..|.3....16.|9.8......|.7.....9.|......2.5|.91....5.|..7439.2.|4....7..."
    , "38.......|...4..785|..9.2.3..|.6..9....|8..3.2..9|....4..7.|..1.7.5..|495..6...|.......92"
    , "...158...|..2.6.8..|.3.....4.|.27.3.51.|.........|.46.8.79.|.5.....8.|..4.7.1..|...325..."
    , ".1.5..2..|9....1...|..2..8.3.|5...3...7|..8...5..|6...8...4|.4.1..7..|...7....6|..3..4.5."
    , ".8.....4.|...469...|4.......7|..59.46..|.7.6.8.3.|..85.21..|9.......5|...781...|.6.....1."
    , "9.42....7|.1.......|...7.65..|...8...9.|.2.9.4.6.|.4...2...|..16.7...|.......3.|3....57.2"
    , "...7..8..|..6....31|.4...2...|.24.7....|.1..3..8.|....6.29.|...8...7.|86....5..|..2..6..." ]
medium =
    [ "2..6.....|6...51.4.|.7.......|....3..14|..5.6....|.19.4..5.|..6....25|...9...8.|89....4.."
    , ".15.2...9|.4....7..|.27..8...|95...32..|7.......6|..62...15|...6..92.|..4....8.|2...3.65."]
hard =
    [ "..6.9....|17...3.9.|...7....5|...5..6..|.9..3.2..|..4..21..|...978...|.4...5.8.|.....6..."
    , "...92....|..68.3...|19..7...6|23..4.1..|..1...7..|..8.3..29|7...8..91|...5.72..|....64..."
    , ".6.5.4.3.|1...9...8|.........|9...5...6|.4.6.2.7.|7...4...5|.........|4...8...1|.5.2.3.4."
    , "7.....4..|.2..7..8.|..3..8.79|9..5..3..|.6..2..9.|..1.97..6|...3..9..|.3..4..6.|..9..1.35"]

-- answers to presetted boards separated by level
easy_ans = (Array.map solve_board (Array.map string_to_board (Array.fromList easy)))
med_ans = (Array.map solve_board (Array.map string_to_board (Array.fromList medium)))
hard_ans = (Array.map solve_board (Array.map string_to_board (Array.fromList hard)))

-- 2 ways of converting different types of presetted strings to our string format
convert_1 : String -> String
convert_1 str =
    String.replace "_" "."
        (String.replace "," "" (String.replace "\n" "|" str))
convert_2 : String -> String
convert_2 str =
    let
        first = String.replace "0" "." str
        foo s =
            case (String.dropLeft 9 s) of
                "" -> [String.left 9 s]
                rest -> (String.left 9 s) :: (foo rest)
    in
        String.join "|" (foo first)


------------------- STRING <--> BOARD ----------------------
-- converts each character in the preset string into an (Int, Bool)
convert_char : Char -> (Int, Bool)
convert_char num =
    case num of
        '.' -> (0, True)
        _ -> ((Char.toCode num) - 48, False)

-- calls convert_char to convert a string into a Sudoku row
string_to_row : String -> Array (Int, Bool)
string_to_row str =
    let
        list = String.toList str |> Array.fromList
    in
        Array.map convert_char list

-- converts a string to a board (input of model)
string_to_board : String -> Array (Array (Int, Bool))
string_to_board str =
    let
        rows = String.split "|" str |> Array.fromList
    in
        Array.map string_to_row rows

-- END STRING <--> BOARD -----------------------------------

-- SOLVER ALGORITHM ----------------------------------------
{- https://www.youtube.com/watch?v=tvP_FZ-D9Ng -}

-- finds the next empty cell in a row
find_next_empty_in_row : Int -> Int -> Array (Array (Int, Bool)) -> Maybe (Int, Int)
find_next_empty_in_row r c board =
    case get_2D r c board of
        Nothing -> Nothing
        Just (n, b) ->
            if n == 0
                then Just (r, c)
            else
                find_next_empty_in_row r (c+1) board

-- starts at r=0, c=0; finds the next empty cell on the board
find_next_empty_in_board : Int -> Int -> Array (Array (Int, Bool)) -> Maybe (Int, Int)
find_next_empty_in_board r c board =
    case find_next_empty_in_row r c board of
        Nothing -> 
            if r <= 8
                then find_next_empty_in_board (r+1) c board
            else
                Nothing
        Just (row, col) -> Just (row, col) 

-- given the column number, an initial r=0, and an initial result = Array.empty,
-- and the board, return an array of all elements in the column
find_col : Int -> Int -> Array (Array (Int, Bool)) -> Array (Int, Bool) -> Array (Int, Bool)
find_col r c board result =
    if r > 8 
        then result
    else
        case get_2D r c board of
            Nothing -> Debug.todo "find_col: Not going to get here"
            Just x -> find_col (r+1) c board (Array.push x result)

-- finds all elements in the 3x3 box
find_3x3 : Int -> Int -> Array (Array (Int, Bool)) -> Array (Int, Bool)
find_3x3 r c board =
    let
        row_start = (r // 3) * 3
        col_start = (c // 3) * 3

        foo row col row_c col_c result =
            case get_2D row col board of
                Nothing -> Debug.todo "find_3x3: Not going to get here"
                Just x ->
                    if row_c > 1 && col_c > 1 then
                        Array.push x result
                    else if col_c > 1 then
                        foo (row+1) col_start (row_c+1) 0 (Array.push x result)
                    else 
                        foo row (col+1) row_c (col_c+1) (Array.push x result)
    in
        foo row_start col_start 0 0 Array.empty

-- returns all elements in row
find_row : Int -> Array (Array (Int, Bool)) -> Array (Int, Bool)
find_row r board =
    case Array.get r board of
        Nothing -> Debug.todo "find_row: Not gonna reach here"
        Just x -> x


{- given an array of (Int, Bool) (could be from the row, column or 3x3 square)
and returns true if the guess is valid in the array -}
arr_is_valid : Int -> Array (Int, Bool) -> Bool
arr_is_valid guess arr =
    let
        inside = Array.filter (\x -> guess == Tuple.first x) arr
    in
        Array.isEmpty inside

-- returns true if the guess is valid
is_valid : Array (Array (Int, Bool)) -> Int -> Int -> Int -> Bool
is_valid board guess r c =
    let
        row = find_row r board
        col = find_col 0 c board Array.empty
        box = find_3x3 r c board
    in
        arr_is_valid guess row &&
        arr_is_valid guess col &&
        arr_is_valid guess box


-- solves the Sudoku board using a recursive backtracking algorithm
solve_board : Array (Array (Int, Bool)) -> (Bool, Array (Array Int))
solve_board board =
    let
        -- acts like a for loop where it checks whether guesses are valid
        -- and places them on the board if they are
        foo guess r c =
            if guess > 9 then
                (False, Array.map (Array.map (\(x,b) -> x)) board)
            else
                if is_valid board guess r c then
                    let
                        solved_board = solve_board (set_2D r c (guess, True) board)
                    in
                        if Tuple.first solved_board then
                            solved_board
                        else foo (guess+1) r c
                else 
                    foo (guess+1) r c
    in
        case find_next_empty_in_board 0 0 board of
            Nothing ->
                let
                    answers = Array.map (Array.map (\(x,b) -> x)) board
                in
                (True, answers)
            Just (row, col) -> foo 1 row col

-- END SOLVER ALGORITHM ------------------------------------

-- RANDOMLY CHOOSE BOARD -----------------------------------

-- generates a random number based on the level list given
boardGenerator : List String -> Generator Int
boardGenerator level =
    Random.int 0 ((List.length level)-1)

-- given a random number, use it as the index for the given level
choose_board : Int -> List String -> String
choose_board rand level = 
    let
        foo i list =
            case (i, list) of
                (0, x::_) -> x
                (_, _::xs) -> foo (i-1) xs
                (_, _) -> Debug.todo "choose_board: invalid index"
    in
        foo rand level


-- FUNCTIONS FOR BUTTONS -----------------------------------
-- check if a box matches at r, c
checkCell : Int -> Int -> Model -> Bool
checkCell r c model =
    let 
        user_answer = get_2D r c model.input
        correct_answer = get_2D r c model.answers
    in
        case (user_answer, correct_answer) of
            (Just (x,_), Just y) -> x == y
            (_, _) -> False

-- updates model to hold the correct answer at (r, c)
revealCell : Int -> Int -> Model -> Model
revealCell r c model =
    let
        answer = get_2D r c model.answers
        (last_r, last_c) = model.lastPos
        clickColor = set_2D last_r last_c clicked_col model.color
    in
        case answer of
            Nothing -> model
            Just x -> 
                let
                    new_model = { answers = model.answers
                                , input = set_2D r c (x, True) model.input
                                , lastPos = model.lastPos
                                , color = clickColor
                                , level = model.level
                                , display_msg = model.display_msg }
                in
                    if checkAnswers new_model then
                        { answers = model.answers
                        , input = set_2D r c (x, True) model.input
                        , lastPos = model.lastPos
                        , color = clickColor
                        , level = model.level
                        , display_msg = ("fuchsia", "Congratulations! You solved it!") }
                    else
                        new_model

-- helper for checkRow
checkButton : Int -> (Int, Bool) -> Bool
checkButton answer input =
    answer == Tuple.first input

-- helper for checkAnswers
checkRow : Array Int -> Array (Int, Bool) -> Bool
checkRow answers inputs =
    let
        rowList_answers = Array.toList answers
        rowList_inputs = Array.toList inputs
        list_bools = List.map2 checkButton rowList_answers rowList_inputs
    in
        List.foldr (&&) True list_bools

-- checks if input matches the answers
checkAnswers : Model -> Bool
checkAnswers model =
    let
        list_answers = Array.toList model.answers
        list_inputs = Array.toList model.input
        list_bools = List.map2 checkRow list_answers list_inputs
    in
        List.foldr (&&) True list_bools

-- resets the input to the original preset
resetBoard : Array (Array (Int, Bool)) -> Array (Array (Int, Bool))
resetBoard board =
    let
        resetRow row =
            let
                resetCell cell =
                    case cell of
                        (_, True) -> (0, True)
                        _ -> cell
            in
                Array.map resetCell row
    in
        Array.map resetRow board

-- MAIN ----------------------------------------------------
type alias Flags =
    ()

-- initializes an easy board on startup
init : Flags -> ( Model, Cmd Msg )
init () =
    (update (Initialize "easy")
            { answers = Array.empty
            , input = Array.empty
            , lastPos = (-1, -1)
            , color = Array.repeat 9 (Array.repeat 9 default_col)
            , level = ("easy", easy)
            , display_msg = ("transparent", "")
            })


main : Program Flags Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



-- MODEL ---------------------------------------------------

type alias Model =
    { answers : Array (Array Int) -- stores the answers to the sudoku puzzle
    , input : Array (Array ( Int, Bool )) -- the input that the user types. if false, that means it's a preset
    , lastPos : (Int, Int) -- stores the last clicked position
    , color : Array (Array Color) -- stores the colors of each element of the board
    , level : (String, List String) -- (level name, list of boards in the level)
    , display_msg : (String, String) -- the display message
    }        

-- UPDATE --------------------------------------------------
type Msg
    = StoreLast Int Int -- stores the last clicked position
    | KeyDown String -- updates the board based on the key the user presses
    | Click String -- updates the board based on the button the user presses
    | GenerateBoard Int -- generates a board given a random int
    | Initialize String -- calls GenerateBoard given a level

-- given a row and column, set v at r, c in a 2D array
set_2D : Int -> Int -> a -> Array (Array a) -> Array (Array a)
set_2D r c v arr =
    case Array.get r arr of
        Nothing -> arr
        Just x ->
            Array.set r (Array.set c v x) arr

-- given a row and column, get the element at r, c in a 2D array
get_2D : Int -> Int -> Array (Array a) -> Maybe a
get_2D r c arr =
    case Array.get r arr of
        Nothing -> Nothing 
        Just x ->
            case Array.get c x of
                Nothing -> Nothing
                Just v -> Just v

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Initialize new_level ->
            let
                level_ =
                    case new_level of
                        "medium" -> medium
                        "hard" -> hard
                        _ -> easy
            in
                ( { model | level = (new_level, level_)
                , display_msg = ("transparent", "")
                } , Random.generate GenerateBoard (boardGenerator level_)
                )
        GenerateBoard num ->
            let
                new_board = choose_board num (Tuple.second model.level) |> string_to_board
                new_ans =
                    case Tuple.first model.level of
                        "medium" -> med_ans
                        "hard" -> hard_ans
                        _ -> easy_ans
            in
                case Array.get num new_ans of
                    Nothing -> Debug.todo "GenerateBoard: ERROR"
                    Just ans_ ->
                        ( { model | answers = Tuple.second ans_
                          , input = new_board } , Cmd.none)
        StoreLast r c ->
            case get_2D r c model.input of
                Nothing -> ( model, Cmd.none )
                Just (_, b) ->
                    if b then
                        let
                            (last_r, last_c) = model.lastPos
                            resetColor = set_2D last_r last_c default_col model.color
                        in
                            ({ model 
                            | lastPos = (r, c)
                            , color = set_2D r c clicked_col resetColor
                            }, Cmd.none)
                    else
                        ( model, Cmd.none )
        KeyDown key -> 
            let
                next_c row col func =
                    case get_2D row col model.input of
                        Nothing -> -1
                        Just (_, b) ->
                            if b then
                                col
                            else
                                next_c row (func col) func
                
                next_r row col func =
                    case get_2D row col model.input of
                        Nothing -> -1
                        Just (_, b) ->
                            if b then
                                row
                            else
                                next_r (func row) col func
                (r, c) = model.lastPos
            in
            case String.toInt key of
                Nothing ->
                    if key == "Backspace" || key == "Delete" then
                        ({model | input = set_2D r c (0, True) model.input}, Cmd.none)
                    else if key == "ArrowRight" && c < 8 then
                        update (StoreLast r (next_c r (c+1) (\x -> x+1))) model
                    else if key == "ArrowLeft" && c > 0 then
                        update (StoreLast r (next_c r (c-1) (\x -> x-1))) model
                    else if key == "ArrowUp" && r > 0 then
                        update (StoreLast (next_r (r-1) c (\x -> x-1)) c) model
                    else if key == "ArrowDown" && r < 8 then
                        update (StoreLast (next_r (r+1) c (\x -> x+1)) c) model
                    else
                        ( model, Cmd.none )
                Just num ->
                    if num < 10 && num > 0 then
                        let
                            user_answers = { model | input = set_2D r c (num, True) model.input }
                        in
                            if checkAnswers user_answers then
                                ({ model | input = set_2D r c (num, True) model.input
                                , display_msg = ("fuchsia", "Congratulations! You solved it!") }
                                , Cmd.none)
                            else
                                (user_answers, Cmd.none)
                    else
                        ( model, Cmd.none )
        Click str ->
            let
                (r, c) = model.lastPos
            in
                case str of
                    "cell" ->
                        if not (checkCell r c model)
                            then 
                                ({ model | color = set_2D r c Color.red model.color 
                                }, Cmd.none)
                        else
                            ( model, Cmd.none )
                    "reset" ->
                        ( { answers = model.answers
                          , input = resetBoard model.input
                          , lastPos = model.lastPos
                          , color = model.color
                          , level = model.level
                          , display_msg = ("transparent", "")
                        } , Cmd.none)
                    "reveal" -> ( revealCell r c model, Cmd.none )
                    _ -> (model, Cmd.none)


keyDecoder : Decode.Decoder String
keyDecoder =
  Decode.field "key" Decode.string

subscriptions : Model -> Sub Msg
subscriptions _ =
    Browser.Events.onKeyDown
    (Decode.map (\key -> KeyDown key) keyDecoder)



-- VIEW

-- the two default colors 
default_col = Color.rgb255 204 153 255
clicked_col = Color.rgb255 178 102 255

-- the 4 dimensions we can change (all other dimensions are based on these) 
board_left = 45
board_top = 20
board_dim = 60
button_border = 0.2

row_width = board_dim
row_height = board_dim / 9

button_dim = (((toFloat board_dim) - button_border)) / 9
font_size = button_dim / 2.0

square_dim = button_dim * 3.0 - 2.0 * button_border
square_border = square_dim / 40

-- helper to allow us to give a number and convert it to a string for Attributes
dimVH : Float -> String -> String
dimVH num str =
    Debug.toString num ++ str

-- converts strings into an Attribute message
addStyle : List ( String, String ) -> List (Html.Attribute msg)
addStyle style =
    List.map (\( k, v ) -> Attr.style k v) style


view : Model -> Html Msg
view model =
    let
        -- the container that the sudoku board is in
        container = 
            [ ( "height", dimVH board_dim "vh")
            , ( "width", dimVH board_dim "vh")
            , ( "left", dimVH (board_left+square_dim) "vh")
            , ( "top", dimVH board_top "vh")
            , ( "position", "absolute")
            ]

        -- the style of the sudoku board
        boardstyle =
            [ ( "height", dimVH board_dim "vh")
            , ( "width", dimVH board_dim "vh")
            , ( "left", dimVH (board_left+square_dim) "vh")
            , ( "top", dimVH board_top "vh")
            , ( "text-align", "center")
            , ( "display", "table" )
            , ( "color", "#393b44" )
            , ( "position", "absolute")
            ]
        
        -- the style of each black box which created the sudoku oulines
        square = 
            [ ( "height", dimVH square_dim "vh" )
            , ( "width", dimVH square_dim "vh" )
            , ( "z-index", "20")
            , ( "border", dimVH square_border "vh solid black") 
            , ( "position", "absolute")
            , ( "pointer-events", "none")
            ]

        -- the container for the side command buttons
        buttons = 
            [ ( "height", dimVH (button_dim*4) "vh" )
            , ( "width", dimVH (button_dim*3) "vh" )
            , ( "display", "inline-block" )
            , ( "position", "absolute")
            , ( "cursor", "pointer" )
            , ( "top", dimVH (board_top+2.5*row_height-2*button_border-button_dim) "vh")
            , ( "left", dimVH (4*square_dim + board_left+3*button_border+square_dim) "vh")
            ]
        
        -- the style of each side command button
        individual_button =
            [ ( "height", dimVH (button_dim/3*4) "vh" )
            , ( "width", dimVH (button_dim*3) "vh" )
            , ( "margin-top", dimVH 2 "vh")
            , ( "position", "relative")
            , ( "font-family", "'Lucida Console', monospace")
            , ( "color", "white")
            , ( "cursor", "pointer" )
            , ( "background", "#C654FF" )
            , ( "font-size", dimVH (font_size) "vh")
            , ( "border-radius", "5%")
            ]

        -- style of the message that displays congratulations
        msg_style = 
            [ ( "font-family", "'Apple Chancery', cursive" )
            , ( "height", dimVH (board_dim/2.5) "vh" )
            , ( "width", dimVH board_dim "vh" )
            , ( "font-size", dimVH (font_size*2) "vh" )
            , ( "color", "white" )
            , ( "text-align", "center" )
            , ( "z-index", "20")
            , ( "top", dimVH (board_top-2*button_border+2*button_dim) "vh")
            , ( "position", "absolute")
            , ( "left", dimVH (board_left+square_dim) "vh" )
            , ( "background", (Tuple.first model.display_msg) )
            , ( "border-radius", "25px" )
            , ( "pointer-events", "none")
            ]
        
        -- style of the webpage title
        title = 
            [ ( "font-family", "'Lucida Console', monospace")
            , ( "font-size", dimVH (font_size*2) "vh" )
            , ( "color", "rebeccapurple" )
            ]
            
        -- function that allows us to view a row of the sudoku board
        viewRow r row =
            let
                rowstyle =
                    [ ( "display", "table-row")
                    , ( "height", dimVH row_height "vh" )
                    , ( "width", dimVH row_width "vh" )
                    , ( "padding", "0px 0px")
                    , ( "box-sizing", "border-box")
                    , ( "line-height", "0")
                    ]

                divstyle =
                    [ ( "float", "left" ) ]

                -- helper that converts the (Int, Bool) tuple into a string
                -- v is (Int, Bool)
                tupleToString v =
                    case v of
                    (0, _) -> " "
                    (x, _) -> Debug.toString x
                
                -- a helper that allows us to view each cell of the board
                viewButton c v =
                    let
                        col = Color.toCssString
                            (case get_2D r c model.color of
                                Just x -> x
                                Nothing -> default_col)

                        -- style of each cell
                        buttonstyle =
                            [ ( "height", dimVH button_dim "vh" )
                            , ( "width", dimVH button_dim "vh" )
                            , ( "justify-content", "center" )
                            , ( "font-size", "100%")
                            , ( "font-family", "monospace")
                            , ( "background", col)
                            , ( "border", dimVH button_border  "vh solid black")
                            , ( "border-radius", "0px")
                            , ( "color", "white")
                            , ( "font-size", dimVH font_size  "vh")
                            ]
                        
                    in
                    Html.div (addStyle divstyle)
                        [ Html.button
                            ((Events.onClick (StoreLast r c)) :: (addStyle buttonstyle))
                            [ Html.text (tupleToString v) ]
                        ]
            in
                Html.div (addStyle rowstyle)
                    (Array.indexedMap viewButton row |> toList)
    in
        Html.div []
        [ Html.div (addStyle container) []
        , Html.select -- view the dropdown menu
            ((Events.onInput Initialize)
            :: (Attr.style "left" (dimVH (board_left-square_dim) "vh"))
            :: (Attr.style "top" (dimVH (board_top-square_dim) "vh"))
            :: (addStyle individual_button))
                [ Html.option [Attr.value "easy"] [Html.text "Easy"]
                , Html.option [Attr.value "medium"] [Html.text "Medium"]
                , Html.option [Attr.value "hard"] [Html.text "Hard"]
                ]
        , Html.div -- title
            [ Attr.style "text-align" "center"
            , Attr.style "width" (dimVH board_dim "vh")
            , Attr.style "padding-left" (dimVH (board_left-0.75*button_border+square_dim-button_dim*3) "vh")
            , Attr.style "display" "inline-block"
            ]
            [ Html.h1 (addStyle title) [Html.text "Sudoku"]
            ]
        , Html.div (addStyle boardstyle)
            (Array.indexedMap viewRow model.input |> toList) -- the board
        , Html.div -- the squares
            ((addStyle square) ++
                [ Attr.style "top"
                    (dimVH (board_top-2*button_border) "vh")
                , Attr.style "left"
                    (dimVH (board_left-0.75*button_border+square_dim) "vh")]) []
        , Html.div
            ((addStyle square) ++
                [ Attr.style "top"
                    (dimVH (board_top-2*button_border) "vh")
                , Attr.style "left"
                    (dimVH (square_dim + board_left + button_border+square_dim) "vh")]) []
        , Html.div
            ((addStyle square) ++
                [ Attr.style "top"
                    (dimVH (board_top-2*button_border) "vh")
                , Attr.style "left"
                    (dimVH (2*square_dim + board_left+3*button_border+square_dim) "vh")]) []
        , Html.div
            ((addStyle square) ++
                [ Attr.style "top"
                    (dimVH (board_top+3*row_height-button_border) "vh")
                , Attr.style "left"
                    (dimVH (board_left-0.75*button_border+square_dim) "vh")]) []
       , Html.div
            ((addStyle square) ++
                [ Attr.style "top"
                    (dimVH (board_top+3*row_height-button_border) "vh")
                , Attr.style "left"
                    (dimVH (square_dim + board_left + button_border+square_dim) "vh")])
            []
        , Html.div
            ((addStyle square) ++
                [ Attr.style "top"
                    (dimVH (board_top+3*row_height-button_border) "vh")
                , Attr.style "left"
                    (dimVH (2*square_dim + board_left+3*button_border+square_dim) "vh")])
            []
        , Html.div
            ((addStyle square) ++
                [ Attr.style "top"
                    (dimVH (board_top+6*row_height-0.5*button_border) "vh")
                , Attr.style "left"
                    (dimVH (board_left-0.75*button_border+square_dim) "vh")])
            []
        , Html.div
            ((addStyle square) ++
                [ Attr.style "top"
                    (dimVH (board_top+6*row_height-0.5*button_border) "vh")
                , Attr.style "left"
                    (dimVH (square_dim + board_left + button_border+square_dim) "vh")])
            []
        , Html.div
            ((addStyle square) ++
                [ Attr.style "top"
                    (dimVH (board_top+6*row_height-0.5*button_border) "vh")
                , Attr.style "left"
                    (dimVH (2*square_dim + board_left+3*button_border+square_dim) "vh")])
            []
        , Html.h1
            (addStyle msg_style)
            [Html.text (Tuple.second model.display_msg)] -- the message
        , Html.div (addStyle buttons) -- the side buttons
            [ Html.button
                ((Events.onClick (Click "cell"))
                    :: (addStyle individual_button))
                [ Html.text "Check Cell"]
            , Html.button
                ((Events.onClick (Click "reveal"))
                    :: (addStyle individual_button))
                [ Html.text "Reveal Cell"] 
            , Html.button
                ((Events.onClick (Click "reset"))
                    :: (addStyle individual_button))
                [ Html.text "Reset Board"] 
            , Html.button
                ((Events.onClick (Initialize (Tuple.first model.level)))
                    :: (addStyle individual_button))
                [ Html.text "New Game"]
            ]
        ]
