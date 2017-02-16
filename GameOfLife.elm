module GameOfLife exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (on)
import Tuple
import Json.Decode as Decode exposing (field, at, andThen, Decoder)
import Time
import Random
import Date
import Task



target : Decoder a -> Decoder a
target decoder =
  field "target" decoder


className : Decoder String
className =
  at [ "className" ] Decode.string 


main =
  Html.program
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }


max_size = 20


init : ( Model, Cmd Msg )
init =
  ( initModel , Cmd.none )


type alias Cell = 
  {
    neighbours : List(Int, Int), position : (Int, Int), status : Maybe Int 
  }

type alias Model =
  { 
    seed : Int
  , auto :Bool
  , cells : List Cell
  , size : Int
  }
    
initModel : Model
initModel =
  {
    seed = 1020
  , auto = False
  , cells = initial_cell_list max_size
  , size = max_size    
  }


type Msg = Position String|Next|Auto|Random|Randomize (List Int)


if_out_of_matrix row_length int =
  if int == -1 then
    row_length - 1
  else if int >= row_length then
    0
  else
    int


l_ind_to_m_ind: Int-> Model ->  (Int, Int)
l_ind_to_m_ind int  model=
    ( int//model.size, int%model.size)


m_ind_to_l_ind: (Int, Int)-> Model ->  Int
m_ind_to_l_ind tuple model=   
     (Tuple.first tuple)* model.size + (Tuple.second tuple)


initial_neighbours_m_indecies : Int -> (Int, Int) -> List (Int, Int)
initial_neighbours_m_indecies row_length m_index =
  let first m_index= (if_out_of_matrix row_length ((Tuple.first (m_index))-1)
               , if_out_of_matrix row_length (Tuple.second (m_index)-1))
               
      second m_index= (if_out_of_matrix row_length (Tuple.first (m_index)-1)
               , if_out_of_matrix row_length (Tuple.second (m_index)))
               
      third m_index=(if_out_of_matrix row_length (Tuple.first (m_index)-1)
               , if_out_of_matrix row_length (Tuple.second (m_index)+1))
               
      fourth m_index= (if_out_of_matrix row_length (Tuple.first (m_index))
               , if_out_of_matrix row_length (Tuple.second (m_index)-1))
               
      fivth m_index= (if_out_of_matrix row_length (Tuple.first (m_index))
               , if_out_of_matrix row_length (Tuple.second (m_index)+1))
               
      sixth m_index= (if_out_of_matrix row_length (Tuple.first (m_index)+1)
               , if_out_of_matrix row_length (Tuple.second (m_index)-1))
               
      seventh m_index=(if_out_of_matrix row_length (Tuple.first (m_index)+1)
               , if_out_of_matrix row_length (Tuple.second (m_index)))
      eighth m_index=(if_out_of_matrix row_length (Tuple.first (m_index)+1)
               , if_out_of_matrix row_length (Tuple.second (m_index)+1))
  in first m_index::second m_index::third m_index::fourth m_index::fivth m_index::sixth m_index
  ::seventh m_index::eighth m_index::[]


initial_cell_list : Int -> List Cell
initial_cell_list x =
  let create_cells row_length x a = 
    if a < x then
      {
       position = (a//max_size, a%max_size)
      , neighbours = initial_neighbours_m_indecies row_length (a//max_size, a%max_size)
      , status = Just 0
      }::create_cells row_length  x (a+1)
    else
      []    
  in create_cells x (x*x) 0
 

cell_was_true_and_now_is :  Cell -> List (Int, Int)-> Cell
cell_was_true_and_now_is cell list=
  let length = List.length (List.filter (\n -> n == cell.position) list) 
  in if length == 2 then
    {cell| status = Just 1}
  else if length == 3 then
    {cell| status = Just 1}
  else
    {cell| status = Just 0}


cell_was_false_and_now_is : Cell -> List (Int, Int)-> Cell
cell_was_false_and_now_is cell list=
    let length = List.length (List.filter (\n -> n == cell.position) list) 
    in if length == 3 then
      {cell| status = Just 1}
    else
      {cell| status = Just 0}
  

refreshed_cell :  Model -> Cell -> Cell
refreshed_cell model cell =
    let changable_cells_positions = cell_postions_waiting_for_changes model
    in case cell.status of 
        Nothing ->
          {neighbours = [(12002, 20202) ], position = (12002, 20202), status = Just 0}
        Just 0 ->
          cell_was_false_and_now_is cell changable_cells_positions
        Just 1 ->
          cell_was_true_and_now_is cell changable_cells_positions
        Just _ ->
          {neighbours = [(  12002, 20202) ], position = (12002, 20202), status = Just 0}

  
get_alive_only : List Cell -> List Cell
get_alive_only list =
  List.filter (\n -> n.status == Just 1) list


get_neighbours_of_alive : List Cell -> List (Int, Int)
get_neighbours_of_alive alives = 
  case alives of 
    [] ->
      []
    x::xs ->
       List.append x.neighbours (get_neighbours_of_alive xs)



cell_postions_waiting_for_changes : Model -> List (Int, Int)
cell_postions_waiting_for_changes model =
  let alives = get_alive_only model.cells
  in get_neighbours_of_alive alives


next_generation :  Model -> List Cell
next_generation model=  
  List.map (refreshed_cell model) model.cells


random_cells : List Int -> List Cell-> List Cell
random_cells list cells=
  List.map2 (\n a -> if n == 0 then {a|status = Just 0} else {a|status = Just 1}) list cells



-- UPDATE
decode : Decoder String
decode =
  target<|className


get_cell : String -> Model-> Cell
get_cell str model =
  let number = Result.withDefault 0 (String.toInt str)
  in let index = ((number//model.size), (number%model.size))
      in List.filter (\n->n.position == index) model.cells|>List.head|>Maybe.withDefault {
      neighbours = [(1, 2)], position = (111, 111), status = Just 0
    }
          

change_color : Cell -> Cell
change_color cell =
 if .status cell == Just 1 then
    {cell|status = Just 0}
  else
     {cell|status = Just 1}



cell_should_be_changed : Cell -> Cell -> Cell
cell_should_be_changed cell x =
  if cell == x then
    change_color x
  else
    x
    

changedByClick : Cell -> Model->List Cell
changedByClick cell model=
  List.map (cell_should_be_changed cell) model.cells


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  case msg of
    Position str->
      let cell = get_cell str model
      in if not model.auto then
          ({
          model|
            cells = changedByClick cell model
          }
          , Cmd.none)
        else
          (model, Cmd.none)
    Next ->
      ({model|cells = next_generation model}, Cmd.none)
    Auto ->
      if model.auto == True then
        ({model|auto = False}, Cmd.none)
      else
        ({model|auto = True}, Cmd.none)
    Random ->
      if model.auto /= True then
        (model, Random.generate Randomize (Random.list (model.size*model.size) (Random.int 0 1)))
      else
        (model, Cmd.none)
    Randomize  list->
      ({model|cells = random_cells list model.cells}, Cmd.none)

-- SUBSCRIPTIONS 


subscriptions : Model -> Sub Msg
subscriptions model =
  if model.auto == True then
    Time.every Time.second (always Next)
  else
    Sub.none


-- VIEW
separate_cell : Model -> Cell-> Html Msg
separate_cell  model cell =
  let color =
    if cell.status == Just 1 then
      "blue"
    else
      "gray"
   in Html.map Position  (
      td
        [ on "click" decode, 
          class (m_ind_to_l_ind cell.position model|>toString)
        , style[( "width", "16px" ), ( "height", "16px" ), ( "backgroundColor", color)]
        ]
        []
    )


fill_the_row :  Int -> Model -> List(List (Html Msg))
fill_the_row size model=
  let fill_the_row1 list size model_size =
    if size==model_size then
        (List.map (separate_cell model) (List.take model_size list)) ::[]
    else
        (List.map (separate_cell model) (List.take model_size list)) ::fill_the_row1 (List.drop model_size list) (size+1) model_size
  in fill_the_row1 model.cells size model.size


rows : Model -> List(Html Msg)
rows model =
    List.map (\n->tr[](n)) (fill_the_row 1 model)


auto_button model =
  let btn = 
    if not model.auto then
      {color ="green", name = "START"}
    else
      {color ="red", name = "STOP"}
  in button [ Html.Events.onClick Auto, style[( "backgroundColor", btn.color), ("font-size","24px")] ] [ text btn.name ]


random_button model =
  let color = 
    if not model.auto then
      "blue"
    else
      "gray"
  in button [ Html.Events.onClick Random, style[( "backgroundColor", color), ("font-size","24px")] ] [ text "RANDOM" ]


view : Model -> Html Msg
view model =
  div[][
    table[][tbody[](rows model)]
  , div [][auto_button model]
  , div [][random_button model]
  ]
