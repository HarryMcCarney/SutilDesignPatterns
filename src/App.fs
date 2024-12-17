module App

open Sutil
open Sutil.CoreElements


type GlobalState = 
    {
        UserName : string
        Age : int
    }

type Cmd =
    | UpdateAge of int

// Child compoent that uses a local store for its transient local state and a dispatch to write back to parent state
let childComponent (age: int) (dispatch : Cmd -> unit ) = 

    let childStore = Store.make age

    Html.div
        [
            Html.h1 "Child Component with local store"
            Bind.el(childStore |> Store.map (fun x -> x), fun a -> Html.div (sprintf "Child Age: %i" a))
            Html.div [
                Html.button [
                    Html.text "Update Child Age"
                    Ev.onClick (fun _ -> 
                        Store.modify  (fun age -> age + 1) childStore
                    )
                ]

                Html.button [
                    Html.text "Update Parent Age"
                    Ev.onClick (fun _ -> 
                        dispatch (UpdateAge (childStore.Value)))
                ]
            ]
        ]
  

// Child component that uses a local elmish loop for its local operations and a global dispatch fn to write back to parent state
module ChIldComponentWithLocalElmish =

    type ChildCmd =
        | UpdateChildAge of int
    
    type ChildState = 
        {
            Age : int
        }
        
    let childUpdates (childCmd : ChildCmd) (childState : ChildState) = 
            match childCmd with
            | UpdateChildAge age -> {childState with Age = age}
               

    let childComponentWithLocalElmish (age: int) (dispatch : Cmd -> unit ) = 

        let childInit() = {
            Age = age
        }

        let childStore, childDispatch = () |> Store.makeElmishSimple childInit childUpdates ignore

        Html.div
            [
                Html.h1 "Child Component with Local Elmish Loop"
                Bind.el(childStore |> Store.map (fun x -> x), fun a -> Html.div (sprintf "Child Age: %i" a.Age))
                Html.div [
                    Html.button [
                        Html.text "Update Child Age"
                        Ev.onClick (fun _ -> 
                            childDispatch (UpdateChildAge (childStore.Value.Age + 1))
                            )
                        
                    ]

                    Html.button [
                        Html.text "Update Parent Age"
                        Ev.onClick (fun _ -> 
                            dispatch (UpdateAge (childStore.Value.Age)))
                    ]
                ]
            ]
   

let app() =

    let init() = {
            UserName = "John"
            Age = 30
        }

    let globalUpdate (cmd : Cmd) (globalState : GlobalState) = 
        match cmd with
        | UpdateAge age -> 
            {globalState with Age = age } 

    let state, dispatch = () |> Store.makeElmishSimple init globalUpdate ignore

    [
        Html.div state.Value.UserName
        Bind.el(state |> Store.map (fun x -> x.Age), fun a -> Html.div (sprintf "Parent Age: %i" a))
        childComponent state.Value.Age dispatch
        ChIldComponentWithLocalElmish.childComponentWithLocalElmish state.Value.Age dispatch
    ]
    |> fragment

app() |> Program.mount


